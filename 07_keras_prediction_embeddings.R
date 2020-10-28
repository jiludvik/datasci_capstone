# INITIALISE ENVIRONMENT
library(dplyr)
library(keras)
library(tensorflow)
library(reticulate)
library(tictoc)
use_condaenv('r-reticulate')
tf$constant("Hellow Tensorflow")
gc()


# SUPPORTING FUNCTIONS
# Function to download Glove embeddings
download_glove<-function(path="input/glove"){
        # load glove embeddings
        download.file(url="http://nlp.stanford.edu/data/glove.6B.zip", destfile="input/glove.6B.zip")
        unzip(zipfile="input/glove.6B.zip",exdir=path)}

# Function to transform Glove embeddings into a character matrix
transform_glove<-function(file='input/glove/glove.6B.100d.txt'){
        glove_rows<-read_lines(file=file)
        glove<-t(unname(data.frame(strsplit(glove_rows, split=" "))))
        write_rds(x=glove, path="input/glove/glove.6B.rds", compress="gz")
        return(glove)}

# Function to predict sequence of words based on input text
generate_seq<-function(model, tokenizer, max_length, seed_text, n_words){
        in_text<- seed_text
        # generate a fixed number of words
        for (i in (1:n_words)){ # loop through the following times for every word to be predicted
                # generate encoded index for the input word
                encoded <- texts_to_sequences(tokenizer, in_text)
                # pad sequences
                encoded <- unlist(pad_sequences(encoded, maxlen=max_length, padding='pre'))
                # predict an index of the predicted word based on the index of the encoded input word
                yhat = as.numeric(predict_classes(model, encoded, verbose=0))
                # extract word index of the full training data from tokenizer object
                word_index<-t(data.frame(tokenizer$word_index)) 
                # extract a word from word_index corresponding to the predicted index no
                out_word<-rownames(word_index)[yhat==word_index]
                # set input text for the next word prediction to be the last word predicted
                in_text<-paste(in_text, out_word)
                # append the predicted word to the previously generated text}
        return(in_text)
        }
}

# Function to extract last word from the predicted sequence of words
last_word<-function(sentence){
        return(tail(strsplit(sentence, split=" ")[[1]], 1))}

#Function to calculate accuracy of the prediction
get_accuracy<-function(prediction, actual){
        acc<-sum(prediction==actual)/length(prediction)
        return(acc)}



# LOAD DATA
setwd("/Users/jiri/Documents/data_science_jhu/10_capstone")
source<-readRDS('intermediate/pre-processed/train_ngrams_keras.rds')

# GENERAL NGRAM PREPROCESSING
# set minimum threshold for ngrams instances for ngram to be used
count_threshold<-c(10,6,3)
# set number of ngrams to be sampled
ngram_sample_size<-40000
# filter and prepare ngram data sets
ngrams<-list()
for (i in 1:3) {
        source[[i]]<-source[[i]][1:2] # strip fields that are not required
        colnames(source[[i]])<-c('term','n') # rename columns
        source[[i]]<-source[[i]][!is.na(source[[i]]$term),] # remove NAs
        ngram_count<-sum(source[[i]]$n>count_threshold[i]) # count no of rows with count >threshold
        message ('Number of unique ', i+1,'-grams in source with count >',count_threshold[i],': ',ngram_count)
        source[[i]]<-source[[i]][source[[i]]$n>count_threshold[i],]# filter out ngrams < threshold
        set.seed(123)
        source[[i]]$n<-as.integer(round(source[[i]]$n/count_threshold[i]))
        message('Number of ',i+1,'-gram instances before sampling:', sum(source[[i]]$n))
        source[[i]]<-dplyr::sample_n(source[[i]], size=ngram_sample_size) #sample rows
        message('Number of ',i+1,'-gram instances after sampling:', sum(source[[i]]$n))
        ngrams[[i]]<-unname(unlist(lapply(source[[i]], rep, times=source[[i]]$n)[1])) # repeat each ngram n-time
}

# MODEL-SPECIFIC NGRAM PRE-PROCESSING
# Combine trigrams and quadrigrams
data<- append(ngrams[[2]],ngrams[[3]])

# define tokenizer
tokenizer <- text_tokenizer() %>% fit_text_tokenizer(data)
encoded = texts_to_sequences(tokenizer, data)

# extract the vocabulary & determine its size
vocabulary<-as.vector(names(tokenizer$word_index))
vocab_size<-length(vocabulary)+1
cat('Vocabulary Size:', vocab_size)

# pad source data to the length of the longest row
max_length <- max(lengths(encoded))
encoded <- pad_sequences(encoded, maxlen=max_length, padding='pre')
cat('Max Sequence Length:', max_length)

# load embeddings
embeddings_index<-readRDS('input/glove/glove.6B.rds')
# join embeddings with vocabulary
embedding_matrix<-left_join(data.frame(X1=vocabulary),data.frame(embeddings_index), by = "X1")
# set coefficient for words that don't exist in embeddings
embedding_matrix[is.na(embedding_matrix)]<-"0"
# add first row with zero weights to align the weights dimensions
embedding_matrix<-rbind(rep(0,101),embedding_matrix)
# drop the column with word
embedding_matrix[,1]<-NULL
#change data type of all columns to numeric
embedding_matrix<-sapply(embedding_matrix, as.numeric)

# split data into input and output elements & one-hot encode outputs
X= encoded[,1:max_length-1]
y=encoded[,max_length]
y<-to_categorical(y, num_classes=vocab_size)



# DEFINE & TRAIN THE PREDICTIVE MODEL
# define model
model<- keras_model_sequential()
model %>%
        layer_embedding(input_dim=vocab_size, 
                        output_dim=100, 
                        weights=list(embedding_matrix), 
                        input_length = max_length-1, 
                        trainable=FALSE) %>%
        layer_lstm(50) %>%
        layer_dense(vocab_size,activation="softmax")

summary(model)

# compile network
compile(model, loss='categorical_crossentropy', optimizer='adam', metrics=c('accuracy'))

# fit model
tic()
fit(model, X, y, epochs=100, verbose=2)
toc()

# EVALUATE THE TRAINED MODEL
#Functional test
print(generate_seq(model, tokenizer, max_length-1, 'He is such a', 1))
print(generate_seq(model, tokenizer, max_length-1, 'You should feel', 1))
print(generate_seq(model, tokenizer, max_length-1, 'I would love to', 1))
print(generate_seq(model, tokenizer, max_length-1, "The guy in front of me just bought a pound of bacon, a bouquet, and a case of", 1))
print(generate_seq(model, tokenizer, max_length-1, "You're the reason why I smile everyday. Can you follow me please? It would mean the", 1))
print(generate_seq(model, tokenizer, max_length-1, "Hey sunshine, can you follow me and make me the", 1))
print(generate_seq(model, tokenizer, max_length-1, "Very early observations on the Bills game: Offense still struggling but the", 1))
print(generate_seq(model, tokenizer, max_length-1, "Go on a romantic date at the", 1))
print(generate_seq(model, tokenizer, max_length-1, "Well I'm pretty sure my granny has some old bagpipes in her garage I'll dust them off and be on my", 1))
print(generate_seq(model, tokenizer, max_length-1, "Ohhhhh #PointBreak is on tomorrow. Love that film and haven't seen it in quite some", 1))
print(generate_seq(model, tokenizer, max_length-1, "After the ice bucket challenge Louis will push his long wet hair out of his eyes with his little", 1))
print(generate_seq(model, tokenizer, max_length-1, "You're the reason why I smile everyday. Can you follow me please? It would mean the", 1))
print(generate_seq(model, tokenizer, max_length-1, 'Be grateful for the good times and keep the faith during the', 1))
print(generate_seq(model, tokenizer, max_length-1, "If this isn't the cutest thing you've ever seen, then you must be", 1))

#Calculate prediction accuracy
set.seed(158)
valid<-readRDS(paste0('intermediate/pre-processed/valid_ngrams.rds')) %>% dplyr::sample_n(500)
prediction<-character()
for (term in valid$predictor) {
        prediction<-append (prediction, last_word(generate_seq(model,tokenizer, max_length-1, term,1)))
}
get_accuracy(prediction,valid$response)
