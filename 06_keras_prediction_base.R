#INITIALISE THE ENVIRONMENT
library(keras)
library(tensorflow)
library(tictoc)
use_condaenv('r-reticulate')
library(reticulate)
library(dplyr)

tf$constant("Hello Tensorflow")
gc()

# In case of issues with missing python version / tensorflow installation, see
# https://stackoverflow.com/questions/63220597/python-in-r-error-could-not-find-a-python-environment-for-usr-bin-python

generate_seq<-function(model, tokenizer, max_length, seed_text, n_words){
        in_text<- seed_text
        # generate a fixed number of words
        for (i in (1:n_words)){ # loop through the following for every word to be predicted
                # generate encoded index for the input word
                encoded <- texts_to_sequences(tokenizer, in_text)
                # pad sequences
                encoded <- unlist(pad_sequences(encoded, maxlen=max_length, padding='pre'))
                # predict an index of the predicted word based on the index of the encoded input word
                yhat = as.numeric(predict_classes(model, batch_size=batch_size, encoded, verbose=0))
                # extract word index of the full training data from tokenizer object
                word_index<-t(data.frame(tokenizer$word_index)) 
                # extract a word from word_index corresponding to the predicted index no
                out_word<-rownames(word_index)[yhat==word_index]
                # set input text for the next word prediction to be the last word predicted
                in_text<-paste(in_text, out_word)
                # append the predicted word to the previously generated text
        }
        return(in_text)
}

# Function to extract last word from the predicted sequence
last_word<-function(sentence){
        return(tail(strsplit(sentence, split=" ")[[1]], 1))
}

# Function to calculate accuracy of the prediction compared to the actual (held out) response
get_accuracy<-function(prediction, actual){
        acc<-sum(prediction==actual)/length(prediction)
        return(acc)
}

# Helper function to round x down to a number divisble by m
round_down <- function(x,m) m*floor(x / m)

# LOAD DATA
setwd("/Users/jiri/Documents/data_science_jhu/10_capstone")
source<-readRDS('intermediate/pre-processed/train_ngrams_keras.rds')

# GENERAL NGRAM PREPROCESSING
# set minimum threshold for ngrams instances for ngram to be used
count_threshold<-c(5,4,2,1)
# print number of unique ngrams in each data set with count > threshold
for (i in 1:4) {print(sum(source[[i]]$term_instances_in_sample>count_threshold[i]))}

# set number of unique ngrams to be sampled from each data set
ngram_sample_size<-1000
batch_size<-64

ngrams<-list()
for (i in 1:4) {
        source[[i]]<-source[[i]][1:2] # strip fields that are not required
        colnames(source[[i]])<-c('term','n') # rename columns
        source[[i]]<-source[[i]][!is.na(source[[i]]$term),] # remove NAs
        ngram_count<-sum(source[[i]]$n>count_threshold[i]) # count no of rows with count >threshold
        message ('Number of unique ', i+1,'-grams in source with count >',count_threshold[i],': ',ngram_count)
        source[[i]]<-source[[i]][source[[i]]$n>count_threshold[i],]# filter out ngrams < threshold
        source[[i]]$n<-as.integer(round(source[[i]]$n/count_threshold[i])) # reduce no of ngram counts
        message('Number of ',i+1,'-gram instances before sampling:', sum(source[[i]]$n))
        set.seed(123)
        source[[i]]<-dplyr::sample_n(source[[i]], size=ngram_sample_size) #sample rows
 #      message('Number of ',i+1,'-gram instances after sampling:', sum(source[[i]]$n))
        ngrams[[i]]<-unname(unlist(lapply(source[[i]], rep, times=source[[i]]$n)[1])) # repeat each ngram n-times
        message('Number of ',i+1,'-gram instances after sampling:', length(ngrams[[i]]))
        ngrams[[i]]<-sample(x=ngrams[[i]], size=round_down(length(ngrams[[i]]), batch_size)) #make ngrams# divisble by batch_size
        #ngrams[[i]]<-sample_n(source[[i]], size=round_down(nrow(source[[i]]), batch_size)) #make ngrams# divisble by batch_size
        message('Number of ',i+1,'-gram instances after adjusting for batch_size:', length(ngrams[[i]]))
}

# MODEL-SPECIFIC PRE-PROCESSING
# Combine trigrams and quadrigrams into a training data set
train<- append(ngrams[[2]],ngrams[[3]])
message('Total Sequences in Text:', length(train))

# define tokenizer
tokenizer <- text_tokenizer() %>% fit_text_tokenizer(train)
sequences <- texts_to_sequences(tokenizer, train)

# determine vocabulary size
word_index<-tokenizer$word_index
vocab_size<-length(word_index) +1
message('Vocabulary Size:', vocab_size)

# pad input sequences
max_length <- max(lengths(sequences))
sequences <- pad_sequences(sequences, maxlen=max_length, padding='pre')
message('Max Sequence Length:', max_length)

# split into input and output elements
X<-sequences[,1:max_length-1]
y<-sequences[,max_length]

# one-hot encoding of the outputs
message('One-hot encoding outputs')

# using mltools::one_hot encoder
#y<-mltools::one_hot(data.table::data.table(y=as.factor(y)), cols=c('y'))
# does not work with smaller batch sizes

# using keras::to_categorical (max 120k unique ngrams)
y<-to_categorical(y, num_classes=vocab_size)

# DEFINE & TRAIN THE PREDICTIVE MODEL
# define model
model<- keras_model_sequential()
model %>%
        layer_embedding(input_dim=vocab_size, 
                        output_dim=10, 
                        input_length = max_length-1) %>%
        layer_lstm(50) %>%
        layer_dense(vocab_size,activation="softmax")

#model2 %>% # same or worse results than model
#        layer_embedding(input_dim=vocab_size, 
#                        output_dim=10, 
#                        input_length = max_length-1 ) %>%
#        layer_lstm(100, return_sequences=TRUE) %>%
#        layer_lstm(100) %>%
#        layer_dense(100, activation="relu") %>%
#        layer_dense(vocab_size,activation="softmax")

#model<- keras_model_sequential()
#model %>%
#        layer_embedding(input_dim=vocab_size, 
#                        output_dim=10, 
#                        input_length = max_length-1,
#                        batch_size=batch_size) %>%
#        layer_lstm(50, 
#                   batch_size=batch_size,
#                   batch_input_shape=c(batch_size, max_length-1)) %>%
#        layer_dense(vocab_size,
#                    activation="softmax", 
#                    batch_size=batch_size,
#                    batch_input_shape=c(batch_size, max_length-1)
#                    )

cat(summary(model))

# compile model
compile(model, 
        loss='categorical_crossentropy', 
        optimizer='adam', 
        metrics=c('accuracy'))

# fit model
epochs<-10
fit(model, X, y, epochs=epochs, batch_size=batch_size, verbose=2)

#copy weights to a new model used for prediction
batch_size=1
pred_model<- keras_model_sequential()
pred_model %>%
        layer_embedding(input_dim=vocab_size, 
                        output_dim=10, 
                        input_length = max_length-1) %>%
        layer_lstm(50) %>%
        layer_dense(vocab_size,activation="softmax")

old_weights = get_weights(model)
set_weights(pred_model, old_weights)
compile(pred_model, 
        loss='categorical_crossentropy', 
        optimizer='adam', 
        metrics=c('accuracy'))


# EVALUATE THE TRAINED MODEL
# Functional test
print(generate_seq(pred_model, tokenizer, max_length-1, 'He is such a', 1))
print(generate_seq(pred_model, tokenizer, max_length-1, "The guy in front of me just bought a pound of bacon, a bouquet, and a case of", 1))
print(generate_seq(pred_model, tokenizer, max_length-1, "You're the reason why I smile everyday. Can you follow me please? It would mean the", 1))
print(generate_seq(pred_model, tokenizer, max_length-1, "Hey sunshine, can you follow me and make me the", 1))
print(generate_seq(pred_model, tokenizer, max_length-1, "Very early observations on the Bills game: Offense still struggling but the", 1))
print(generate_seq(pred_model, tokenizer, max_length-1, "Go on a romantic date at the", 1))
print(generate_seq(pred_model, tokenizer, max_length-1, "Well I'm pretty sure my granny has some old bagpipes in her garage I'll dust them off and be on my", 1))
print(generate_seq(pred_model, tokenizer, max_length-1, "Ohhhhh #PointBreak is on tomorrow. Love that film and haven't seen it in quite some", 1))
print(generate_seq(pred_model, tokenizer, max_length-1, "After the ice bucket challenge Louis will push his long wet hair out of his eyes with his little", 1))
print(generate_seq(pred_model, tokenizer, max_length-1, "You're the reason why I smile everyday. Can you follow me please? It would mean the", 1))
print(generate_seq(pred_model, tokenizer, max_length-1, 'Be grateful for the good times and keep the faith during the', 1))
print(generate_seq(pred_model, tokenizer, max_length-1, "If this isn't the cutest thing you've ever seen, then you must be", 1))

# Calculate prediction accuracy
set.seed(100)
test<-readRDS(paste0('intermediate/pre-processed/test_ngrams_keras.rds')) %>% dplyr::sample_n(1000)
prediction<-character()
tic()
for (term in test$predictor) {
        prediction<-append (prediction, last_word(generate_seq(pred_model,tokenizer, max_length-1, term,1)))
}
toc()
get_accuracy(prediction,test$response)

#model 1 - non batch
# 80k unique trigrams + quadrigrams  / vocab size 6000 / 100 epochs: 0.08-0.1
# 20k unique trigrams +quadrigrams / vocab size 3500 / 100 epochs : 0.04-0.06
# 20k unique bi, tri, quadri and pentagrams / vocab size 4500 / 50 epochs: 0.012
# 15k unique tri, quadri and pentagrams / vocab size 4000 / 50 epochs : 0.011
# 10k unique quadri and pentagrams / vocab size 3500 / 50 epochs : 0.015
# 15k unique pentagrams / vocab size 3000 / 100 epochs: 0.01
# 20k unique quadri & pentagrams / vocab size 6500 / 50 epochs: 0.01
# 20k unique tri and quadrigrams / vocab size 4500 / 50 epochs: 0.013
# 150k unique trigrams + quadrigrams / vocab size 6900 / 100 epochs: 0.096

#model2
# 0.04-0.055 for 20000 unique trigrams + quadrigrams (vocab size 3500) - same or worse than model 1

# model 2 - batch
# 40k unique tri+quadrigrams / vocab size 5.3k / 150 epochs / batch size 1024: 0.076


dir.create('intermediate/models')

#save_model_hdf5(model, "intermediate/models/model_3_4grams_150kseqs.h5")
save_model_tf(model, "intermediate/models/model_3_4grams_150kseqs.tf")
save_text_tokenizer(tokenizer, "intermediate/models/tokenizer_3_4grams_150kseqs")
