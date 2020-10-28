library(keras)
library(tensorflow)
library(reticulate)
library(tictoc)


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

#INITIALISE THE ENVIRONMENT
use_condaenv('r-reticulate')
tf$constant("Hellow Tensorflow")
gc()

# LOAD DATA
setwd("/Users/jiri/Documents/data_science_jhu/10_capstone")
source<-readRDS('intermediate/pre-processed/train_ngrams_keras.rds')

# GENERAL NGRAM PREPROCESSING
# set minimum threshold for ngrams instances for ngram to be used
count_threshold<-c(10,6,3)
# set number of unique ngrams to be sampled
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
#       ngrams[[i]]<-unname(source[[i]]$term)
        ngrams[[i]]<-unname(unlist(lapply(source[[i]], rep, times=source[[i]]$n)[1])) # repeat each ngram n-time
}

# MODEL-SPECIFIC PRE-PROCESSING
# Combine trigrams and quadrigrams
data<- append(ngrams[[2]],ngrams[[3]])
message('Total Sequences in Text:', length(data))

# define tokenizer
tokenizer <- text_tokenizer() %>% fit_text_tokenizer(data)
sequences <- texts_to_sequences(tokenizer, data)

# determine vocabulary size
word_index<-tokenizer$word_index
vocab_size<-length(word_index) +1
message('Vocabulary Size:', vocab_size)

# pad input sequences
max_length <- max(lengths(sequences))
sequences <- pad_sequences(sequences, maxlen=max_length, padding='pre')
message('Max Sequence Length:', max_length)

# split into input and output elements; one-hot encode outputs
X= sequences[,1:max_length-1]
y=sequences[,max_length]
message('One-hot encoding outputs')
tic()
y<-to_categorical(y, num_classes=vocab_size)
toc()



# DEFINE & TRAIN THE PREDICTIVE MODEL
# define model
model<- keras_model_sequential()
model %>%
        layer_embedding(input_dim=vocab_size, 
                        output_dim=10, 
                        input_length = max_length-1 ) %>%
        layer_lstm(50) %>%
        layer_dense(vocab_size,activation="softmax")
cat(summary(model))

# compile model
compile(model, 
        loss='categorical_crossentropy', 
        optimizer='adam', 
        metrics=c('accuracy'))

# fit model
epochs<-100
fit(model, X, y, epochs=epochs, verbose=2)


# EVALUATE THE TRAINED MODEL
# Functional test
print(generate_seq(model, tokenizer, max_length-1, 'He is such a', 1))
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

# Calculate prediction accuracy
set.seed(101)
valid<-readRDS(paste0('intermediate/pre-processed/valid_ngrams.rds')) %>% dplyr::sample_n(500)
prediction<-character()
for (term in valid$predictor) {
        prediction<-append (prediction, last_word(generate_seq(model,tokenizer, max_length-1, term,1)))
}
#View(data.frame(prediction, valid$response))
get_accuracy(prediction,valid$response)

# 0.8-0.1 for 80000 unique ngrams (trigrams + quadrigrams)

save(ngrams, tokenizer, model, valid, file="keras_wordsin_onewordout.RData")
