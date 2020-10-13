library(readr) # read files and guess encoding
library(dplyr) # data wrangling
library(stringr) # data cleaning (regular expressions)
library(hunspell) # spell checking
library(tidytext) # text analysis
library(tidyr) # bigram separation
library(igraph) # transformation of bigrams to directional graphs
library(ggplot2) # frequency and barplots
library(ggraph) # generation of network graph visuals
library (gridExtra)


# 1. LOAD AND PRE-PROCESS FULL RAW DATA SETS

# Define Function to read raw data
load_data<-function(filepath,filename,n_max=-1){
  df <- as_tibble(read_lines(paste0(filepath,filename), n_max=n_max)) %>%
    mutate(source=filename, row_no=row_number())
  return(df)
}

# 1.1 Load reference data sets
setwd('~/Documents/data_science_jhu/10_capstone')
# Load google's toxic word list https://raw.githubusercontent.com/jiludvik/Google-profanity-words/master/list.txt
google_toxic_words<-read_lines('input/bad_words.txt')
#load kaggle twitter slang https://www.kaggle.com/gogylogy/twitterslang
kaggle_twitter_slang<-read_csv('input/twitterSlang.csv') %>% select(slang) %>% rename(term=slang)
# merge two lists
my_stop_words<-rbind(data.frame(term=google_toxic_words), kaggle_twitter_slang)

# 1.2 Load and pre-process full raw datasets (twitter, blogs, news)
input_path='input/final/en_US/'
rows=list()
sentences=list()
words=list()
i=0
for (filename in dir(input_path)) {
  i=i+1
  message('Loading ',filename,'...')
  rows[[i]]<-load_data(input_path,filename)
#  message('...Splitting rows into words...')
#  words[[i]]<- rows[[i]] %>%
#    unnest_tokens(output=word, input=value, token= "words")
#  message('...Splitting rows into sentences...')
#  sentences[[i]] <- rows[[i]] %>%
#    unnest_tokens(output=sentence, input=value, token= "sentences")
#  message('...Calculating row lengths...')
#  rows[[i]] <- rows[[i]] %>%
#    mutate(row_len=nchar(value))
}

#2. HEADLINE DESCRIPTIVE STATISTICS
#2.1 Generate headline descriptive statistics on the full data sets
summary_df=data.frame()
for (i in 1:3){
  summary_df<-rbind(summary_df,data.frame(
    source=rows[[i]][1,2],
    row_len_min=min(rows[[i]]$row_len, na.rm=TRUE),
    row_len_mean=round(mean(rows[[i]]$row_len, na.rm = TRUE),0),
    row_len_max=max(rows[[i]]$row_len, na.rm = TRUE),
    total_row_no=nrow(rows[[i]]),
    total_sentence_no=nrow(sentences[[i]]),
    total_word_no=nrow(words[[i]]))
  )
}
print ('Raw Data Summary:\n')
print (summary_df)
rm(list=c('words', 'sentences', 'filename', 'i'))

#Observation - twitter row len limited to 140 characters. Mean row len 3 times smaller than the other two data sets. This needs to be taken into account in sampling



# 3. EXPLORATORY DATA ANALYSIS - WORDS

# Key decisions - 
# 1) Which data sources to use 
# 2) filter out stop words or not
# 3) spellcheck or not

#1) Considering different register (formal/informal and complexity of the text across the three sources, we will rely on all three data sources
# 2 - Considering the purpose of the predictive model is to predict next word(s) responding to a word provided by a user (as opposed to classification of or analysis of extensive text), and as stop-words are very common in such a use case, it is probably important to include these in the corpora used to train predictive model
# 3 - Assuming we do not want recommendations to include 'twitterspeak', we will use spell-check using American English dictionary (which will probably help us also with foreign language content.

# 3.1 Sample the three data sets and combine the samples into a single data frame
row_sample=data.frame()
set.seed(0)
sampling_factor=c(0.7,1,5) # Adjusts sample proportions to arrive at comparable number of unique words per data set
for (i in 1:3) {
  row_sample<- rbind(row_sample, sample_n(rows[[i]],sampling_factor[i]*20000))
}

row_sample<-row_sample %>% 
  filter (nchar(value)>5) %>% # filter out rows <5 characters long
  mutate(source=recode(source, # make source names human readable
                       en_US.blogs.txt = "blogs", 
                       en_US.twitter.txt="twitter", 
                       en_US.news.txt="news"), .keep="unused") %>%
  select(-row_len)

#free up memory
#rm(rows)
gc()

# 3.2. Generate and Clean-Up Words Data Set
word_sample <- row_sample %>% 
  select(value, source, row_no)%>% 
  unnest_tokens(output=term, input=value, # split to words
                token= "words", 
                strip_numeric = TRUE, strip_punct = TRUE) %>% 
  filter (!str_detect(term, '.*\\d.*'))%>% # drop words with digits
  filter (!(str_detect(term, '_'))) %>% # drop words with _
  anti_join(y=my_stop_words, by="term") # drop twitter slang & profanities
word_sample<- word_sample%>%
  filter(hunspell_check(word_sample$term, dict = dictionary("en_US"))) %>% # drop misspellings
  count(source, term, sort=TRUE) #count number of instances of each unique word per data set
total_words_no <- word_sample %>%  # count total no of words per source + join back in word_sample
  group_by(source) %>% 
  summarize(total = sum(n))
word_sample <- left_join(word_sample, total_words_no, by="source")

# 3.2. Dictionary Size
dict_summary_df<-word_sample %>% group_by(source) %>% summarise(unique_words=n_distinct(term))
print('Size of the dictionary: ')
print(dict_summary_df)

# Observation: Thanks to varied sampling rates, all three dictionaries have similar number of unique words. All three dictionaries are quite large, considering Shakespeare's works contain 25k unique words. 

#3.3 Most Frequent Words
plot_top10<- function(df){
  df %>% 
    group_by(source) %>% 
    slice_max(order_by=n, n=10, with_ties = FALSE) %>%
    ggplot(aes(x=term,y=n, fill = source)) +
    geom_col(show.legend = FALSE) +
    labs(x = NULL, y = "No of ocurrences") +
    facet_wrap(~source, ncol = 3, scales = "free") +
    coord_flip() +
    scale_x_reordered()
}

word_sample%>% group_by(source) %>% 
  slice_max(order_by=n, n=5, with_ties = FALSE)
#or
plot_top10(df=word_sample)

#Observation - all of these are stop words. Frequency sorted dictionary is not a good idea. 

# Word Frequency Distribution
ggplot(word_sample, aes(n/total, fill = source)) +
  geom_histogram(show.legend = FALSE) +
  xlim(NA, 0.0001) +
  facet_wrap(~source, ncol = 3, scales = "free_y")

# These plots exhibit similar distributions for all the sources, with many words that occur rarely and fewer words that occur frequently. Please note there is a very long tail of very rare words that has been truncated for from visualisation.
# We can see that the tails are quite extreme. This confirms frequency-weighted dictionary is not suitable for prediction purposes.


# 3.4 Words Importance - TF/ IDF
# what is TF/IDF

#Calculate tf-idf
word_sample <- word_sample %>%
  bind_tf_idf(term, source, n)

#plot
word_sample %>% 
  group_by(source) %>% 
  slice_max(order_by=tf_idf, n=10, with_ties = FALSE) %>%
  ggplot(aes(x=term,y=tf_idf, fill = source)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "TF-IDF") +
  facet_wrap(~source, ncol = 3, scales = "free") +
  coord_flip() +
  scale_x_reordered()

# Observation - still some mistypes but much better than using frequency.

#3.5 checking for foreign words
# A measure of word importance is inverse document frequency (idf), which decreases the weight for commonly used words and increases the weight for words that are not used very much in a collection of documents. This could be used for instance to identify foreign words in the text (with high IDF). 

word_sample %>% 
  group_by(source) %>% 
  slice_max(order_by=idf, n=5, with_ties=FALSE)

#As we have applied spellcheck as part of words pre-processing, we would not expect foreign language words in the text. However, should such words ocurr, they would have high IDF. Visual inspection of the above sample as well as other words with high IDF suggests this is not the case


# 3.6 Word coverage
#How many unique words do you need in a frequency sorted dictionary to cover 50% of all word instances in the language? 90%?

word_sample <- word_sample %>% 
  group_by(source) %>% 
  mutate(term_freq = n/total) %>%
  arrange(by=term_freq) %>%
  mutate(rank = row_number(), cum_term_freq =cumsum(term_freq))

p1<- ggplot(word_sample, aes(y=rank, x=cum_term_freq*100, colour = source)) +
  geom_line(size = 1.1, alpha = 0.8) +   
  scale_y_log10() + 
  geom_vline(xintercept = c(50,90), colour="grey", linetype = "longdash") +
  labs(x = 'Target coverage', y = "Unique terms required") +
  ggtitle('Word Coverage')

p2<- ggplot(bigram_sample, aes(y=rank, x=cum_term_freq*100, colour = source)) +
  geom_line(size = 1.1, alpha = 0.8) +   
  scale_y_log10() + 
  geom_vline(xintercept = c(50,90), colour="grey", linetype = "longdash") +
  labs(x = 'Target coverage', y = "Unique terms required") +
  ggtitle('Bi-Gram Coverage')

p3<- ggplot(trigram_sample, aes(y=rank, x=cum_term_freq*100, colour = source)) +
  geom_line(size = 1.1, alpha = 0.8) +   
  scale_y_log10() + 
  geom_vline(xintercept = c(50,90), colour="grey", linetype = "longdash") +
  labs(x = 'Target coverage', y = "Unique terms required") +
  ggtitle('Tri-Gram Coverage')

grid.arrange(p1, p2, p3, nrow = 1)

#How many unique words are required for 50% coverage of all words in the corpus

term_coverage <- function(df){
  df %>% group_by(source) %>%
  summarise (t50=which(cum_term_freq>=0.5)[1], t90=which(cum_term_freq>=0.9)[1])
}

uniquewords_coverage_50pct<-word_sample %>% group_by(source) %>% summarise(unique_words_reqd=which(cum_term_freq>=0.5)[1])
print(uniquewords_coverage_50pct)

#How many unique words are required for 90% coverage of all words in the corpus
uniquewords_coverage_90pct<-word_sample %>% group_by(source) %>% summarise(unique_words_reqd=which(cum_term_freq>=0.9)[1])
print(uniquewords_coverage_90pct)

# Increasing coverage

#Can you think of a way to increase the coverage -- identifying words that may not be in the corpora or using a smaller number of words in the dictionary to cover the same number of phrases?

# - in case user enters word that is not in the corpora, recommend some of the most common words
# - go beyond bag of words
# - lemmatization of user input (plus de-lemmatization of predicted words)
# - combining the custom text corpora with a standardised thesaurus providing synonyms of individual terms

# 4. EXPLORATORY DATA ANALYSIS - BIGRAMS and TRIGRAMS

#4.1 Generate and cleaned-up bi-grams
set.seed(0)
bigram_sample<- row_sample %>% # sample_frac(0.01) %>%
  select(value, source, row_no) %>% 
  unnest_tokens(input=value, output=term, token= "ngrams", n=2) %>%
  separate(term, c("word1", "word2"), sep = " ", remove = FALSE) %>%
  filter(!(word1 %in% my_stop_words$term) & !str_detect(word1, '.*\\d.*'))%>%
  filter(!(word2 %in% my_stop_words$term) & !str_detect(word2, '.*\\d.*')) 

bigram_sample <- bigram_sample %>%
  filter(hunspell_check(bigram_sample$word1, dict = dictionary("en_US")) & hunspell_check(bigram_sample$word2, dict = dictionary("en_US"))) %>%
  count(source, term, sort=TRUE) %>%
  separate(term, c("word1", "word2"), sep = " ", remove = FALSE)

# count total no of bigrams per source + join back in word_sample
total_bigram_no <- bigram_sample %>%  
  group_by(source) %>% 
  summarize(total = sum(n))
bigram_sample <- left_join(bigram_sample, total_bigram_no, by="source")

#Calculate tf-idf
bigram_sample <- bigram_sample %>% 
  bind_tf_idf(term, source, n)

#4.2 Plot most important bigrams - PROBABLY REQ'S SORTING BY TF-IDF

bigram_sample %>% 
  group_by(source) %>% 
  slice_max(order_by=tf_idf, n=10) %>%
  ggplot( aes(term, tf_idf, fill = source)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "TF-IDF") +
  facet_wrap(~source, ncol = 3, scales = "free") +
  coord_flip() +
  scale_x_reordered()

#Observation - blog and news bigrams look OK. Twitter still has a substantial portion of 'twitterspeak' as most important bigrams. This could be addressed in two ways: 
# a) dropping bigrams with high idf  frequency , potentialy
# b) adding twitterspeak to custom stopword list - TO DO

bigram_sample2 <- bigram_sample %>% filter(idf<1)

bigram_sample2 %>% 
  group_by(source) %>% 
  slice_max(order_by=tf_idf, n=10) %>%
  ggplot( aes(term, tf_idf, fill = source)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "TF-IDF") +
  facet_wrap(~source, ncol = 3, scales = "free") +
  coord_flip() +
  scale_x_reordered()


# 4.3 Show bigram relationships
bigram_graph <- bigram_sample %>% select(-term, -source)%>%
  filter(n > 100) %>%
  graph_from_data_frame()
set.seed(0)
ggraph(bigram_graph, layout = "fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)
gc()

#4.4 Generate and clean up tri-grams
set.seed(0)
trigram_sample<- row_sample %>% 
  select(value, source, row_no) %>% 
  unnest_tokens(input=value, output=term, token= "ngrams", n=3) %>%
  separate(term, c("word1", "word2", "word3"), sep = " ", remove = FALSE) 

trigram_sample<-trigram_sample %>%
  filter(!(word1 %in% my_stop_words$term) & !str_detect(word1, '.*\\d.*'))%>%
  filter(!(word2 %in% my_stop_words$term) & !str_detect(word2, '.*\\d.*'))%>% 
  filter(!(word3 %in% my_stop_words$term) & !str_detect(word3, '.*\\d.*'))

trigram_sample <- trigram_sample %>%
  filter(hunspell_check(trigram_sample$word1, dict = dictionary("en_US")) & hunspell_check(trigram_sample$word2, dict = dictionary("en_US")) & hunspell_check(trigram_sample$word3, dict = dictionary("en_US"))) %>%
  count(source, term, sort=TRUE) %>%
  separate(term, c("word1", "word2", "word3"), sep = " ", remove = FALSE)

#4.5 Plot most frequent trigrams
trigram_sample %>% 
  group_by(source) %>% 
  slice_max(order_by=n, n=10) %>%
  ggplot( aes(term, n, fill = source)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "No of occurences") +
  facet_wrap(~source, ncol = 3, scales = "free") +
  coord_flip() +
  scale_x_reordered()

# 4.6 Show trigram relationships
trigram_graph <- bigram_sample %>% select(-term, -source)%>%
  filter(n > 60) %>%
  graph_from_data_frame()
set.seed(0)
ggraph(trigram_graph, layout = "fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)
gc()

#Observation: there are not many popular bigrams that use common words

