#INITIALISE THE ENVIRONMENT
library(shiny)
library(textclean)
library(tokenizers)
library(keras)
library(reticulate)

#PYTHON_DEPENDENCIES=c('keras','tensorflow')
PYTHON_DEPENDENCIES=c('tensorflow', 'h5py')
#PYTHON_DEPENDENCIES=c('h5py')

if (Sys.info()[['user']]!='shiny') {
    use_condaenv('r-reticulate')} else {
    virtualenv_dir = Sys.getenv('VIRTUALENV_NAME')
    python_path = Sys.getenv('PYTHON_PATH')
    # Create virtual env and install dependencies
    reticulate::virtualenv_create(envname = virtualenv_dir, python = python_path)
    reticulate::virtualenv_install(virtualenv_dir, packages = PYTHON_DEPENDENCIES)#, ignore_installed=TRUE)
    reticulate::use_virtualenv(virtualenv_dir, required = T)
    }
library(tensorflow)

# SUPPORTING FUNCTIONS

# Function to predict next word
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
    return(out_word)
}

## Function to pre-process user input and request next word prediction
next_word <- function(phrase, model=model, tokenizer=tokenizer, stopwords=toxic_words) {
    prephrase<-phrase %>%
        replace_contraction() %>%
        replace_word_elongation() %>%
        replace_internet_slang( replacement ="")
    prephrase2<-tokenize_words(x=prephrase, strip_punct=TRUE, strip_numeric=TRUE, stopwords = stopwords)
    prephrase3<-lapply(prephrase2, function(x) {paste(tail(x,2), collapse=" ")})
    result <- generate_seq(model, tokenizer, 3, prephrase3, 1)
    return(result)
}


## BODY OF THE PROGRAMME

# Load model and stopwords
#model<-load_model_tf("model")
model<-load_model_hdf5("model_3_4grams_150k.h5")
tokenizer<-load_text_tokenizer("tokenizer_3_4grams_150kseqs")
toxic_words<-readRDS('bad_words.rds')

# Define server
shinyServer(function(input, output) {
    
    # Reactive statement for prediction function when user input changes
    prediction =  reactive( {
        
        # Obtain user input
        inputText = input$text
        
        # Predict
#        prediction = generate_seq(model, tokenizer, 3, prephrase, 1)
        prediction = next_word(inputText, model, tokenizer, toxic_words)
    })
    
    # Output data table - needs rewriting
    output$predicted_word = renderText({ 
        prediction()
    })
    
    
    # Output word cloud
    #wordcloud_rep = repeatable(wordcloud)
    #output$wordcloud = renderPlot(
    #    wordcloud_rep(
    #        prediction()$term,
    #        prediction()$Prob,
    #        colors = brewer.pal(8, 'Dark2'),
    #        scale=c(4, 0.5),
    #        max.words = 10
    #    )
    #)
})