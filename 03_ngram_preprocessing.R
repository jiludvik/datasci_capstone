# Dependencies
library(tidyverse)
library(textclean)
library(tidytext)
library(hunspell)
library(tictoc)

## Supporting Functions

### LOAD_SOURCE_DATA: Function to load source data from a specificed directory
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
                               replace_white() %>%
                               replace_hash() %>%
                               replace_tag() %>%
                               replace_url(replacement = "") %>%
                               str_remove_all(pattern = "(?![.,!])[[:punct:]]") %>% 
                               str_remove_all(pattern = "[0-9]") %>%
                               str_replace_all(pattern = "[.]", replacement = " .") %>% 
                               str_replace_all(pattern = "[!]", replacement = " !") %>% 
                               str_replace_all(pattern = "[,]", replacement = " ,") %>%
                               replace_contraction() %>%
                               replace_word_elongation()) %>%
                filter(value!='' | !is.na(value))
        head(rows_clean)
        return(rows_clean)
}


### GENERATE_UNIGRAMS: Function to generate unigrams and their statistics
generate_unigrams<-function(source, cleanse=TRUE, spellcheck=FALSE, descriptive_stats=FALSE, stopwords=NULL){
  message('Generating unigrams')
  word_sample <- source %>% 
    unnest_tokens(output=term, input=value, # split to words
                  token= "words", 
                  strip_numeric = TRUE, strip_punct = TRUE)
  message('...Tokenisation complete')
  if (cleanse){
    word_sample<- word_sample %>%
      filter (!str_detect(term, '.*\\d.*'))%>% # drop words with digits
      filter (!(str_detect(term, '_'))) %>% # drop words with _
      anti_join(y=stopwords, by="term") %>% # drop profanities
      mutate(term=term %>%
               replace_internet_slang(replacement ="") %>%
               replace_names()) %>%
      filter(term!='')
    message('...Removal of stopwords, words with digits, internet slang and peoples names complete')
    # Spell-check words
    if (spellcheck) {
      word_sample<- word_sample%>% 
        filter(hunspell_check(word_sample$term, dict = dictionary("en_US")))
      message('...Spellcheck complete')
    }
  }
  if (descriptive_stats) {
    # Calculate term frequencies
    word_sample<- word_sample %>%
      group_by(term) %>%
      summarise(term_instances_in_sample=n()) %>%
      ungroup()%>%
      mutate(total=sum(term_instances_in_sample), term_freq=term_instances_in_sample/total) %>% 
      arrange(by=desc(term_instances_in_sample))
    message ('...Term frequencies calculated')
    # Calculate cumulative term frequencies
    
    word_sample<- word_sample %>%
      arrange(by=desc(term_freq)) %>% # arrange by term frequency
      mutate(rank = row_number(), # Add rank
             cum_term_freq =cumsum(term_freq)) # Add cumulative term frequency
    message ('...Cumulative term frequencies calculated')
  }
  return(word_sample)
}

### GENERATE_BIGRAMS: Function generates data set with bigrams and their statistics
generate_bigrams<-function(source, cleanse=TRUE, spellcheck=FALSE, descriptive_stats=FALSE, separate=TRUE, stopwords=NULL){
  message('Generating bigrams')
  bigram_sample<- source %>% 
    unnest_tokens(input=value, output=term, token= "ngrams", n=2) %>%
    separate(term, c("word1", "word2"), sep = " ", remove = FALSE)
  message('...Tokenisation complete')
  if (cleanse){
    bigram_sample<- bigram_sample%>%
      filter(!(word1 %in% stopwords$term) & !str_detect(word1, '.*\\d.*'))%>%  
      filter(!(word2 %in% stopwords$term) & !str_detect(word2, '.*\\d.*')) %>%
      mutate(word1=word1 %>%
               replace_internet_slang(replacement ="") %>%
               replace_names(),
             word2=word2 %>%
               replace_internet_slang(replacement ="") %>%
               replace_names()) %>%
      filter(word1!='' & word2!='')
    message('...Removal of stopwords, words with digits, internet slang, people names complete')
    
    if (spellcheck){
      bigram_sample <- bigram_sample %>%
        filter(hunspell_check(bigram_sample$word1, dict = dictionary("en_US")))
      bigram_sample <- bigram_sample %>%
        filter(hunspell_check(bigram_sample$word2, dict = dictionary("en_US"))) 
      message('...Spellcheck complete')
    }
  }
  
  if (descriptive_stats){
    # Calculate term frequencies
    bigram_sample<- bigram_sample %>%
      group_by(term) %>%
      summarise(term_instances_in_sample=n()) %>%
      ungroup()%>%
      mutate(total=sum(term_instances_in_sample), term_freq=term_instances_in_sample/total) %>% 
      arrange(by=desc(term_instances_in_sample)) %>%
      separate(term, c("word1", "word2"), sep = " ", remove = FALSE)
    message('...Term frequencies calculated')
    bigram_sample<- bigram_sample %>%
      arrange(by=desc(term_freq)) %>% # arrange by term frequency
      mutate(rank = row_number(), # Add rank
             cum_term_freq =cumsum(term_freq)) # Add cumulative term frequency
    message ('...Cumulative term frequencies calculated')
  }
  if (!separate){
    bigram_sample<-select (bigram_sample, -starts_with('word'))
  }
  return(bigram_sample)
}

### GENERATE_TRIGRAMS: Function generates data set with trigrams and their statistics
generate_trigrams<-function(source, cleanse=TRUE, spellcheck=FALSE, descriptive_stats=FALSE, separate=TRUE, stopwords=NULL){
  message('Generating trigrams')
  trigram_sample<- source %>% 
    unnest_tokens(input=value, output=term, token= "ngrams", n=3) %>%
    separate(term, c("word1", "word2", "word3"), sep = " ", remove = FALSE) 
  message('...Tokenisation complete')
  
  if (cleanse){
    trigram_sample<-trigram_sample %>%
      filter(!(word1 %in% stopwords$term) & !str_detect(word1, '.*\\d.*'))%>%  
      filter(!(word2 %in% stopwords$term) & !str_detect(word2, '.*\\d.*')) %>%
      filter(!(word3 %in% stopwords$term) & !str_detect(word3, '.*\\d.*')) 
    message('...Removal of words with digits and stopwords complete')
    
    trigram_sample<-trigram_sample %>%
      mutate(word1=word1 %>%
               replace_internet_slang(replacement ="") %>%
               replace_names(),
             word2=word2 %>%
               replace_internet_slang(replacement ="") %>%
               replace_names(),
             word3=word3 %>%
               replace_internet_slang(replacement ="") %>%
               replace_names()) %>%
      filter(word1!='' & word2!='' & word3!='')
    message('...Removal of internet slang and people names complete')
    if (spellcheck){
      trigram_sample <- trigram_sample %>%
        filter(hunspell_check(trigram_sample$word1, dict = dictionary("en_US")))
      trigram_sample <- trigram_sample %>%
        filter(hunspell_check(trigram_sample$word2, dict = dictionary("en_US")))
      trigram_sample <- trigram_sample %>%
        filter(hunspell_check(trigram_sample$word3, dict = dictionary("en_US")))
      message('...Spellcheck complete')
    }
  }
  
  # Calculate term frequencies
  if (descriptive_stats){
    trigram_sample<- trigram_sample %>%
      group_by(term) %>%
      summarise(term_instances_in_sample=n()) %>%
      ungroup()%>%
      mutate(total=sum(term_instances_in_sample), term_freq=term_instances_in_sample/total) %>% 
      arrange(by=desc(term_instances_in_sample)) %>%
      separate(term, c("word1", "word2", "word3"), sep = " ", remove = FALSE)
    message ('...Term frequencies calculated')
    
    trigram_sample<- trigram_sample %>%
      arrange(by=desc(term_freq)) %>% # arrange by term frequency
      mutate(rank = row_number(), # Add rank
             cum_term_freq =cumsum(term_freq)) # Add cumulative term frequency
    message ('...Cumulative term frequencies calculated')
  }
  if (!separate){
    trigram_sample<-select (trigram_sample, -starts_with('word'))
  }
  return(trigram_sample)
}

### GENERATE_QUADRIGRAMS: Function generates data set with quadrigrams and their statistics
generate_quadrigrams<-function(source, cleanse=TRUE, spellcheck=FALSE, descriptive_stats=FALSE, separate=TRUE, stopwords=NULL){
  message('Generating quadrigrams')
  #Tokenise quadrigrams
  quadrigram_sample<- source %>% 
    unnest_tokens(input=value, output=term, token= "ngrams", n=4) %>%
    separate(term, c("word1", "word2", "word3", "word4"), sep = " ", remove = FALSE)
  message('...Tokenisation complete')
  
  if (cleanse){
    # filter stopwords and words with digits
    quadrigram_sample<-quadrigram_sample %>%
      filter(!(word1 %in% stopwords$term) & !str_detect(word1, '.*\\d.*'))%>%  
      filter(!(word2 %in% stopwords$term) & !str_detect(word2, '.*\\d.*')) %>%
      filter(!(word3 %in% stopwords$term) & !str_detect(word3, '.*\\d.*')) %>%
      filter(!(word4 %in% stopwords$term) & !str_detect(word4, '.*\\d.*')) 
    message('...Removal of words with digits and stopwords complete')
    
    #remove internet slang and names
    quadrigram_sample<-quadrigram_sample %>%
      mutate(word1=word1 %>%
               replace_internet_slang(replacement ="") %>%
               replace_names(),
             word2=word2 %>%
               replace_internet_slang(replacement ="") %>%
               replace_names())
    quadrigram_sample <- quadrigram_sample %>%
      filter(word1!='' & word2!='')
    
    quadrigram_sample <-quadrigram_sample %>%
      mutate(word3=word3 %>%
               replace_internet_slang(replacement ="") %>%
               replace_names(), 
             word4=word4 %>%
               replace_internet_slang(replacement ="") %>%
               replace_names())
    quadrigram_sample <- quadrigram_sample %>%
      filter(word3!='' & word4!='')
    gc()
    message('...Removal of internet slang & people names complete')
    if (spellcheck){
      quadrigram_sample <- quadrigram_sample %>%
        filter(hunspell_check(quadrigram_sample$word1, dict = dictionary("en_US")))
      quadrigram_sample <- quadrigram_sample %>%
        filter(hunspell_check(quadrigram_sample$word2, dict = dictionary("en_US")))
      quadrigram_sample <- quadrigram_sample %>%
        filter(hunspell_check(quadrigram_sample$word3, dict = dictionary("en_US")))
      quadrigram_sample <- quadrigram_sample %>%
        filter(hunspell_check(quadrigram_sample$word3, dict = dictionary("en_US")))
      message('...Spellcheck complete')
    }
  }
  
  if (descriptive_stats){
    # Calculate term frequencies
    quadrigram_sample<- quadrigram_sample %>%
      group_by(term) %>%
      summarise(term_instances_in_sample=n()) %>%
      ungroup()%>%
      mutate(total=sum(term_instances_in_sample), term_freq=term_instances_in_sample/total) %>% 
      arrange(by=desc(term_instances_in_sample)) %>%
      separate(term, c("word1", "word2", "word3", "word4"), sep = " ", remove = FALSE)
    message ('...Term frequencies calculated')
    
    quadrigram_sample<- quadrigram_sample %>%
      arrange(by=desc(term_freq)) %>% # arrange by term frequency
      mutate(rank = row_number(), # Add rank
             cum_term_freq =cumsum(term_freq)) # Add cumulative term frequency
    message ('...Cumulative term frequencies calculated')
  }
  if (!separate){
    quadrigram_sample<-select (quadrigram_sample, -starts_with('word'))
  }
  return(quadrigram_sample)
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

# GENERATE_TRAIN_NGRAMS: Generates training dataset with cleaned blended data from blogs, news and twitter
generate_train_ngrams<-function(source, spellcheck=FALSE, descriptive_stats=TRUE, stopwords=NULL){
  #Generate Ngrams
  ngrams=list()
  ngrams[[1]]<-generate_unigrams(source=source, cleanse=TRUE, spellcheck=spellcheck, descriptive_stats = descriptive_stats, stopwords=stopwords)
  ngrams[[2]]<-generate_bigrams(source=source, cleanse=TRUE, spellcheck=spellcheck, descriptive_stats = descriptive_stats, separate=TRUE, stopwords=stopwords)
  ngrams[[3]]<-generate_trigrams(source=source, cleanse=TRUE, spellcheck=spellcheck, descriptive_stats = descriptive_stats, separate=TRUE, stopwords=stopwords)
  # Print Summary Stats
  range<-c(5, seq(10,100, by=10))
  print(data.frame(range_pct=range,
                   unigrams=get_ndist_summary(ngrams[[1]], range),
                   bigrams=get_ndist_summary(ngrams[[2]], range),
                   trigrams=get_ndist_summary(ngrams[[3]], range)))
  
  #Calculate number of terms required for different language coverages
  print(data.frame(coverage_pct=c(30,40,50,60,70,80,90,95),
                   words=get_unique_term_no(ngrams[[1]]),
                   bigrams=get_unique_term_no(ngrams[[2]]),
                   trigrams=get_unique_term_no(ngrams[[3]])))
  return(ngrams)
}

## GENERATE_TEST_NGRAMS: Generates a df with predictor and response columns from text rows
generate_test_ngrams<-function(source, spellcheck=FALSE, stopwords=NULL){
  #browser()
  #generate ngrams
  bigrams<-generate_bigrams(source=source, 
                            cleanse=TRUE, 
                            spellcheck=spellcheck,
                            descriptive_stats=FALSE, 
                            separate=TRUE, 
                            stopwords=stopwords) %>% 
    mutate(predictor=word1, response=word2, .after=term) %>% 
    # align word colnames with quadrigram
    rename(word4=word2, word3=word1) %>% 
    mutate(word2='', word1='', .after=word3)
  trigrams<-generate_trigrams(source=source, 
                              cleanse=TRUE,
                              spellcheck=spellcheck,
                              descriptive_stats=FALSE, 
                              separate=TRUE, 
                              stopwords=stopwords) %>% 
    mutate(predictor=paste(word1,word2), response=word3, .after=term) %>%
    # align word colnames with quadrigram
    rename(word4=word3, word3=word2, word2=word1) %>% 
    mutate(word1='', .after=word2)
  quadrigrams<-generate_quadrigrams(source=source, 
                                    cleanse=TRUE,
                                    spellcheck=spellcheck,
                                    descriptive_stats=FALSE, 
                                    separate=TRUE, 
                                    stopwords=stopwords)
  quadrigrams <- quadrigrams %>% 
    mutate(predictor=paste(word1,word2,word3), response=word4, .after=term)
  
  # Combine all ngrams to a single data frame and remove duplicates
  ngrams<-bigrams%>% #combine ngrams
    add_row(trigrams) %>% 
    add_row(quadrigrams) %>%
    distinct(term,.keep_all=TRUE) %>% # remove all duplicate terms
    group_by(word4,word3,word2) %>% 
    filter(word1!="")%>%#remove trigrams that exist in the same row quadrigrams
    ungroup() %>%
    group_by(word4,word3)%>%
    filter(word2!="") %>% #remove bigrams that exist in the same row quadrigrams
    ungroup() %>%
    select (predictor, response)
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
source_data_sample<-sample_rows(source=source_data, target_sample_rate=0.1)

# Split source data into training, validation and test data set
source_data_split<-split_testtrain(source_data_sample, c(0.9,0.05,0.05))
test_rows_sample<-source_data_split[["test"]]
train_rows_sample<-source_data_split[["train"]]
valid_rows_sample<-source_data_split[["valid"]]

#Pre-clean sampled rows
train_rows_clean<-preclean_rows(train_rows_sample)
test_rows_clean<-preclean_rows(test_rows_sample)
valid_rows_clean<-preclean_rows(valid_rows_sample)

# Write cleaned-up rows sample to disk
clean_path='intermediate/split/'
write_rds(test_rows_clean, paste0(clean_path,'test_rows_sample.rds'), "gz")
write_rds(train_rows_clean, paste0(clean_path,'train_rows_sample.rds'), "gz")
write_rds(valid_rows_clean, paste0(clean_path, 'validation_rows_sample.rds'), "gz")

# Free up memory
rm(source_data); rm(source_data_split)
rm(train_rows); rm(train_rows_sample)
rm(test_rows); rm(test_rows_sample)
rm(valid_rows); rm(valid_rows_sample)
gc()

## Generate test and training ngrams

# Dataset 1 (Baseline): no spellcheck + stopwords included 
train_ngram_nospell_stop<-generate_train_ngrams(source=train_rows_clean, spellcheck=FALSE, descriptive_stats = TRUE, stopwords=toxic_words)
write_rds(x=train_ngram_nospell_stop, path=paste0(target_path,'train_ngrams_nospell_stop.rds'), compress="gz")
test_ngrams_nospell_stop<-generate_test_ngrams(source=test_rows_clean, spellcheck=FALSE, stopwords=toxic_words)
write_rds(x=test_ngrams_nospell_stop, path=paste0(target_path,'test_ngrams_nospell_stop.rds'), compress="gz")
valid_ngrams_nospell_stop<-generate_test_ngrams(source=valid_rows_clean, spellcheck=FALSE, stopwords=toxic_words)


# Dataset 2: spellcheck + stopwords included
train_ngram_spell_stop<-generate_train_ngrams(source=train_rows_clean, spellcheck=TRUE, descriptive_stats = TRUE, stopwords=toxic_words)
write_rds(x=train_ngram_spell_stop, path=paste0(target_path,'train_ngrams_spell_stop.rds'), compress="gz")
test_ngrams_spell_stop<-generate_test_ngrams(source=test_rows_clean, spellcheck=TRUE, stopwords=toxic_words)
write_rds(x=test_ngrams_spell_stop, path=paste0(target_path,'test_ngrams_spell_stop.rds'), compress="gz")

# Dataset 3: no spellcheck + stopwords excluded
train_ngram_nospell_nostop<-generate_train_ngrams(source=train_rows_clean,spellcheck=FALSE, descriptive_stats = TRUE, stopwords=rbind(toxic_words, kaggle_stopwords))
write_rds(x=train_ngram_nospell_nostop, path=paste0(target_path,'train_ngrams_nospell_nostop.rds'), compress="gz")
test_ngrams_nospell_nostop<-generate_test_ngrams(source=test_rows_clean, spellcheck=FALSE, stopwords=rbind(toxic_words, kaggle_stopwords))        
write_rds(x=test_ngrams_nospell_nostop, path=paste0(target_path,'test_ngrams_nospell_nostop.rds'), compress="gz")

# Dataset 4: spellcheck + stopwords excluded
train_ngram_spell_nostop<-generate_train_ngrams(source=train_rows_clean, spellcheck=TRUE, descriptive_stats = TRUE, stopwords=rbind(toxic_words, kaggle_stopwords))
write_rds(x=train_ngram_spell_nostop, path=paste0(target_path,'train_ngrams_spell_nostop.rds'), compress="gz")
test_ngrams_spell_nostop<-generate_test_ngrams(source=test_rows_clean, spellcheck=TRUE, stopwords=rbind(toxic_words, kaggle_stopwords))        
write_rds(x=test_ngrams_spell_nostop, path=paste0(target_path,'test_ngrams_spell_nostop.rds'), compress="gz")
