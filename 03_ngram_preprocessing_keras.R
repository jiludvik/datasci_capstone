# Dependencies
library(tidyverse)
library(textclean)
library(tidytext)
library(hunspell)
library(tictoc)

## SUPPORTING FUNCTIONS

### LOAD_SOURCE_DATA: Function to load source data from a specified directory
load_source_data<-function(input_path){
        rows=list(); i=0
        for (filename in dir(input_path)) {
                i=i+1  
                rows[[i]] <- as_tibble(read_lines(paste0(input_path,filename))) %>%
                        mutate(row_no=row_number())
                head(rows[[i]])
        }
        return(rows)
}

### SAMPLE_ROWS: Function to sample data from a list of data frames with source data
sample_rows<-function(source,target_sample_rate=0.1){
  set.seed(0)
  rows_sample=data.frame()
  rows_sample_rate=c(0.7,1,2)*target_sample_rate
  for (i in 1:3) {
    part<- source[[i]]%>% sample_frac(rows_sample_rate[i])
    rows_sample<- rbind(rows_sample, part)
  }
  rows_sample<-rows_sample %>% distinct(row_no, .keep_all = TRUE)
  head(rows_sample)
  return(rows_sample)
}


### SPLIT_TESTTRAIN: Function to split test and training data sets and save them to disk
split_testtrain<-function(source, split_rate=c(0.6,0.2,0.2)){
  #browser()
  train<-source %>% sample_frac(split_rate[1])
  no_train<- source %>% anti_join(train, by="row_no")
  valid<-no_train %>% sample_frac(split_rate[2]/(split_rate[2]+split_rate[3]))
  test<-no_train %>% anti_join(valid, by="row_no")
  nrow(source)
  nrow(rbind(test, train, valid))
  nrow(test)/nrow(source)
  nrow(valid)/nrow(source)
  nrow(train)/nrow(source)
  message('...splitting of test and training data sets complete')
  ret<-list("train"=train, "valid"=valid, "test"=test)
  return(ret)
}

### PRECLEAN_ROWS: Function to clean training data sample rows
preclean_rows<-function(source){
        rows_clean<-source %>% 
                mutate(value=value%>%
                         replace_hash() %>%
                         replace_tag() %>%
                         replace_url(replacement = "") %>%
                         str_remove_all(pattern = "(?![.,!])[[:punct:]]") %>% 
                         str_remove_all(pattern = "[0-9]") %>%
                         str_replace_all(pattern = "[.]", replacement = " .") %>% 
                         str_replace_all(pattern = "[!]", replacement = " !") %>% 
                         str_replace_all(pattern = "[,]", replacement = " ,") %>%
                         replace_contraction() %>%
                         replace_word_elongation() %>%
                         replace_internet_slang(replacement ="") %>%
                         replace_names() %>%
                         str_replace_all(pattern = "\\<.*\\d\\.*\\>", replacement = " ,")%>%
                         replace_white()) %>%
                filter(value!='' | !is.na(value))
        message('Rows pre-cleaning complete')
        return(rows_clean)
}

#clean<- preclean_rows(source_data_sample)

### LOG10_FLOOR: find order of magnitide
log10_floor <- function(x) {10^(floor(log10(x)))}

### GENERATE_NGRAMS: Function generates data set with bgrams and their statistics
generate_ngrams<-function(source, n=2, cleanse=TRUE, spellcheck=FALSE, descriptive_stats=FALSE, separate=TRUE, stopwords=NULL){
  message('Generating ', n,'-grams')
  word_cols<-sapply(1:n, function(x) {paste0('word',x)})
  ngrams<- source %>% 
    unnest_tokens(input=value, output=term, token= "ngrams", n=n) %>%
    filter(!is.na(term))
  message('...Tokenisation complete')
  if (cleanse){
    ngrams<- separate(ngrams, term, word_cols, sep = " ", remove = FALSE)
    for (word in word_cols) {
      ngrams<- filter(ngrams, !(word %in% stopwords$term))
      gc()
    }
    ngrams<-select(ngrams, -starts_with('word'))
    message('...Removal of stopwords complete')
  }
  if (spellcheck){
    ngrams<- separate(ngrams, term, word_cols, sep = " ", remove = FALSE)
    for (i in 1:n) {
      #browser()
      filter(ngrams, hunspell_check(unlist(ngrams[i+2]), dict = dictionary("en_US")))
      gc()
    }
    ngrams<-select(ngrams,-starts_with('word'))
    message('...Spellcheck complete')
  }
  if (descriptive_stats){
    #browser()
    ngrams<- ngrams %>%
      group_by(term) %>%
      summarise(term_instances_in_sample=n()) %>%
      ungroup()%>%
      mutate(total=sum(term_instances_in_sample), term_freq=term_instances_in_sample/total) %>% 
      arrange(by=desc(term_instances_in_sample))
    message('...Term frequencies calculated')
    ngrams<- ngrams %>%
      arrange(by=desc(term_freq)) %>% # arrange by term frequency
      mutate(rank = row_number(), # Add rank
             cum_term_freq =cumsum(term_freq)) # Add cumulative term frequency
    message ('...Cumulative term frequencies calculated')
  }
  if (separate){
    ngrams<-separate(ngrams, term, word_cols, sep = " ", remove = FALSE)
  }
  return(ngrams)
}

# PRINT_NDIST_SUMMARY: Function to return information on counts of ngrams
print_ndist_summary<-function(df, range){
  ngram=sapply(gregexpr("[[:alpha:]]+", df[1,1]), function(x) sum(x > 0))
  for (i in range){
    print(paste0('Unique ',ngram,'-grams with more than ',i,' instances :',
                nrow(filter(df,term_instances_in_sample>=i))))
  }
}

get_ndist_summary<-function(df, range){
        ret<-integer()
        for (i in range){
                ret<-append(ret, nrow(filter(df,term_instances_in_sample>=i)))
        }
        return(ret)
}

#GET_UNIQUE_TERM_NO: Function to calculate unique numbers of terms required by different language coverages
get_unique_term_no <- function(df){
 return(c(which(df$cum_term_freq>=0.3)[1],which(df$cum_term_freq>=0.4)[1],which(df$cum_term_freq>=0.5)[1], which(df$cum_term_freq>=0.6)[1], which(df$cum_term_freq>=0.7)[1], which(df$cum_term_freq>=0.8)[1], which(df$cum_term_freq>=0.9)[1], which(df$cum_term_freq>=0.95)[1]))
}

# GET_COVERAGE: Function to calculate language coverage achieved by specified number of ngrams
get_coverage <- function(df, unique_ngrams){
  print(round(filter(df, rank==unique_ngrams)$cum_term_freq, 4))
}


# GENERAT_TRAIN_NGRAMS_KERAS: Generation of bi, tri, quadri and pentagrams for prediction model using keras deep learning
generate_train_ngrams_keras<-function(source, n_min=3, n_max=5, stopwords){
  ngrams=list()
  stopwords<-toxic_words
  for (i in n_min:n_max) {
    ngrams[[i-n_min+1]]<-generate_ngrams(source=source, n=i, cleanse=TRUE, spellcheck=FALSE, descriptive_stats = TRUE, separate=FALSE, stopwords=stopwords)
  }
  return(ngrams)
}


## GENERATE_TEST_NGRAMS_KERAS: Generates a df with predictor and response columns from text rows for keras prediction
generate_test_ngrams_keras<-function(source, n_min=3, n_max=5, stopwords=toxic_words){
  ngram<-list()
  #set min and max number of words (don't exceed 9) for debugging
  #n_min=2; n_max=9
  #sample data (for debugging)
  #source<-read_rds('intermediate/split10/test_rows.rds') %>% sample_n(1000)
  #generate ngrams, separate and combine columns for filtering and prediction
  for (i in n_min:n_max) {
    index<-i-n_min+1 # index starting at 1 used to use ngrams in a list
    empty_cols=1:(n_max-i) # word number of empty words that need to be used to pad short ngrams
    predictor_wordno<- as.character(1:i-1) # word number of predictor words
    response_index<-i+3 # index of the response
    ngram[[index]]<-generate_ngrams(source=source, # generate ngrams
                                    n=i, 
                                    cleanse=FALSE, 
                                    spellcheck=FALSE, 
                                    descriptive_stats=FALSE, 
                                    separate=TRUE, 
                                    stopwords=stopwords) %>%
      unite ("predictor", # create new predictor column
             ends_with(predictor_wordno), 
             sep=" ", 
             remove=FALSE)
    # create response column
    ngram[[index]]$response<-ngram[[index]][[response_index]]
    # rename words so that they are all align to the right (across all ngrams)
    ngram[[index]]<-rename_with(.data=ngram[[index]], 
                .fn=function(x) {
                  current_wordno<-as.integer(substr(x, nchar(x), nchar(x)))
                  new_wordno<-current_wordno+n_max-i
                  return(paste0('word',new_wordno))},
                .col=starts_with('word'))# %>%
    # create empty 'wordx' columns for shorter ngrams
    if (i<n_max) {
    for (j in empty_cols) ngram[[index]][paste0('word',j)]<-'' 
    }
  }
  #create empty data frame for merged test data
  ngram_cols<-c('row_no', 'term', 'predictor', 'response', paste0('word',1:n_max))
  ngrams<-data.frame(matrix(ncol=length(ngram_cols), nrow=0))
  names(ngrams)<-ngram_cols
  ngrams[,1]<-as.integer(ngrams[,1])
  ngrams[, 2:length(ngram_cols)]<-sapply(ngrams[, 2:length(ngram_cols)], as.character)
  #merge ngram-specific dataframes into a single data frame
  for (i in 1:(n_max-n_min+1))ngrams<-add_row(ngrams, ngram[[i]])
  #filter out all duplicate terms
  ngrams<-distinct(ngrams, term, .keep_all=TRUE)
  #generate list to group_by and filter ngram data frame
  words_to_groupby<-list()
  for (i in n_max:2) words_to_groupby[[paste0('word',i-1)]]<-paste0('word',i:n_max)
  # group data frame by every groupby vector in list and filter out preceding word==''
  # this represents ngram tails that exists in other ngram tails
  for (i in 1:length(words_to_groupby)){
    word_to_filter<-names(words_to_groupby)[i]
    x<-ngrams%>% group_by(!!!syms(words_to_groupby[[i]])) %>% filter (!!sym(word_to_filter)!='')
    message(i,': ', word_to_filter, ' ', nrow(x))
  }
  return(ngrams)
}


### MAIN BODY OF THE SCRIPT

#Set and create directories
setwd('~/Documents/data_science_jhu/10_capstone')
target_path='intermediate/pre-processed/'
dir.create(target_path)

#Load data
source_data<-load_source_data('input/final/en_US/')
toxic_words<-data.frame(term=read_lines('input/bad_words.txt'))
kaggle_stopwords<-read_csv('input/kaggle_stopwords.csv')
colnames(kaggle_stopwords)<-c('term')

# Sample rows to reduce the size of data requiring processing
source_data_sample<-sample_rows(source=source_data, target_sample_rate=0.001)

# Tokenise into sentences
source_data_sample<-source_data_sample %>% unnest_tokens(input=value, output=value, token="sentences")

# Split source data into training, validation and test data set
source_data_split<-split_testtrain(source_data_sample, c(0.9,0.05,0.05))

#Pre-clean sampled split data sets
tic('Data cleaning')
train_rows_clean<-preclean_rows(source_data_split[["train"]])
test_rows_clean<-preclean_rows(source_data_split[["test"]])
valid_rows_clean<-preclean_rows(source_data_split[["valid"]])
toc()

# Write cleaned-up rows sample to disk
clean_path='intermediate/split/'
dir.create(clean_path)
write_rds(train_rows_clean, paste0(clean_path,'train_rows.rds'), "gz")
write_rds(test_rows_clean, paste0(clean_path,'test_rows.rds'), "gz")
write_rds(valid_rows_clean, paste0(clean_path, 'validation_rows.rds'), "gz")

# Free up memory
rm(source_data); rm(source_data_split); rm(source_data_sample)
gc()


## Generate test and training ngrams
train_rows_clean<-read_rds('intermediate/split/train_rows.rds') %>% sample_n(1000)
train_ngram_keras<-generate_train_ngrams_keras(source=train_rows_clean, n_min=2, n_max=5, stopwords = toxic_words)
write_rds(train_ngram_keras, paste0(target_path,'train_ngrams_keras.rds'), "gz")
test_rows_clean<-read_rds('intermediate/split/test_rows.rds') %>% sample_n(1000)
test_ngram_keras<-generate_test_ngrams_keras(source=test_rows_clean, n_min=3, n_max=5, stopwords=toxic_words)
write_rds(test_ngram_keras, paste0(target_path,'test_ngrams_keras.rds'), "gz")

