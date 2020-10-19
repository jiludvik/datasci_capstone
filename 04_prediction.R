#Dependencies

require(data.table)
require(stringi)
require(stringr)
library(tokenizers)
library(textclean)
library(readr)


# FUNCTIONS A. Good-Turing Pre-Processing

## COUNTNC: Function to calculate  "frequency of frequency r" (N_r)
CountNC <- function(FreqVec) {
        CountTbl <- table(FreqVec[,.(c)])
        return(data.table(cbind(c=as.integer(names(CountTbl)), Nr=as.integer(CountTbl))))
}

# AVG.ZR: Average non-zero count, replace N_r with Z_r
avg.zr <- function(Bins) {
        max <- dim(Bins)[1]
        r<-2:(max-1)
        Bins[1, Zr:=2*Nr/Bins[2,c]]  # r=1, q=0, Zr=Nr/(0.5t)
        Bins[r, Zr:=2*Nr/(Bins[r+1,c]-Bins[r-1,c])]  # else, Zr=Nr/(0.5(t-q))
        Bins[max, Zr:=Nr/(c-Bins[(max-1),c])]  # r=max, t=2r-q, Zr=Nr/(r-q)
        return(Bins)
}

#FITLM: Function to fits a linear regression model log(Z_r)=a+b*log(r)

## Replace Z_r with value computed from a linear regression that is fit to map Z_r to c in log space
## log(Z_r) = a + b*log(c)
FitLM <- function(CountTbl) {
        return(lm(log(Zr) ~ log(c), data = CountTbl))
}

#CAL_GTDISCOUNT: Functions to calculate GT discount for small counts
#count (c) n-grams, where c <= k, using Katz's formula
# invoked by UpdateCount

Cal_GTDiscount <- function(cnt, model, k) {
        Z1 <- exp(predict(model, newdata=data.frame(c=1)))
        Zr <- exp(predict(model, newdata=data.frame(c=cnt)))
        Zrp1 <- exp(predict(model, newdata=data.frame(c=(cnt+1))))
        Zkp1 <- exp(predict(model, newdata=data.frame(c=(k+1))))
        
        sub <- ((k+1)*Zkp1)/(Z1)
        new_r <- ((cnt+1)*(Zrp1)/(Zr)-cnt*sub)/(1-sub)
        return(new_r)
}

#UPDATECOUNT: Function to update GT discount to the table 
UpdateCount <- function(FreqTbl, model, k=5) {
        FreqTbl[c>k ,cDis:=as.numeric(c)]
        FreqTbl[c<=k, cDis:=Cal_GTDiscount(c, model, k)]
        return(FreqTbl)
}

## GOOD_TURING_PREPROC: Function orchestrates Good-Turing preprocessing
good_turing_preproc<-function(corpus, min_count=4, k=5){
        
        NgramBins<-list()
        NgramLM<-list()
        
        for (i in 1:3){
                # Tidy up corpus data
                corpus[[i]]<-corpus[[i]] %>% 
                        rename(c=term_instances_in_sample) %>% 
                        select(term, c) %>% 
                        filter(c>min_count) %>% 
                        mutate (term=gsub(" ", "_", term)) %>% 
                        data.table()
                # Calculate the frequency of frequency
                NgramBins[[i]] <- CountNC(corpus[[i]])
                # Average non-zero count, replace N_r with Z_r
                NgramBins[[i]]<-avg.zr(NgramBins[[i]])
                #Fit a linear regression model log(Z_r)=a+b*log(r)
                NgramLM[[i]] <- FitLM(NgramBins[[i]])
                #Perform the discounting to small count (c) n-grams, where c <= k, using Katz's formula
                corpus[[i]]<- UpdateCount(corpus[[i]], NgramLM[[i]], k=k)
                #corpus[[i]]<-data.table(corpus[[i]])
                # set term as the key
                setkey(corpus[[i]], term)
        }
        return(corpus)
}

## PART 2: WORD PREDICTION

# GET.OBS.NGRAMS.BY.PRE: Function to retrieve observed N-grams from (N−1)-gram
## Return all the observed N-grams given the previous (N-1)-gram
## - wordseq: character vector of (N-1)-gram separated by underscore, e.g. "x1_x2_..._x(N-1)"
## - NgramFreq: datatable of N-grams
get.obs.NGrams.by.pre <- function(wordseq, FreqTbl) {
        #browser()
        PreTxt <- sprintf("%s%s%s", "^", wordseq, "_")
        FreqTbl[grep(PreTxt, FreqTbl[,term], perl=T, useBytes=T),]
}

# GET.UNOBS.NGRAM.TAILS: Function to retrieve all the unigrams that end unobserved N-grams
get.unobs.Ngram.tails <- function(ObsNgrams, N, UniFreq) {
        ObsTails <- str_split_fixed(ObsNgrams[,term], "_", N)[,N]
        return(data.table(term=UniFreq[!ObsTails,term,on="term"]))
}


#CAL.OBS.PROB: Function to compute the probabilities of observed N-grams
## Compute the probabilities of observed N-gram.
## We need the counts from (N-1)-gram table since corpus doesn't include <EOS> explicitly,
## therefore the denominator will be smaller if only summing up all the terms
## from N-gram table
cal.obs.prob <- function(ObsNgrams, Nm1Grams, wordseq) {
        PreCount <- Nm1Grams[wordseq, c, on=.(term)]
        ObsNgrams[,Prob:=ObsNgrams[,cDis]/PreCount]  # c_dis/c
}


##CAL.ALPHA: Function to compute Alpha
## Return the normalization factor Alpha
##
## - ObsNgrams: datatable contains all observed ngrams starting with wordseq
## - Nm1Grams: datatable of (N-1)-grams containing count of wordseq
## - wordseq: an observed history: w_{i-N+1}^{i-1}
cal.alpha <- function(ObsNGrams, Nm1Grams, wordseq) {
        if (dim(ObsNGrams)[1] != 0) {
                # return(1-sum(ObsNGrams[,.(Qbo)]))  # We don't use this formular because End Of Sentence is not counted
                return(sum(ObsNGrams[,c-cDis]/Nm1Grams[wordseq, c, on=.(term)]))
        } else {
                return(1)
        }
}

# FIND_NEXT_WORD: Function to find next word

## Return a list of predicted next words according to previous 2 user input words
##
## - xy: character vector containing user-input bigram, separated by a space
## - words_num: number of candidates of next words returned
Find_Next_word <- function(xy, words_num, corpus) {
        xy <- gsub(" ", "_", xy)
        #browser()
        if (length(which(corpus[[2]]$term == xy)) > 0) {  # C(x,y) > 0
                ## N-grams preparation
                # Retrieve all observed trigrams beginning with xy: OT
                ObsTriG <- get.obs.NGrams.by.pre(xy, corpus[[3]])
                y <- str_split_fixed(xy,"_", 2)[,2]
                # Retrieve all observed bigrams beginning with y: OB
                ObsBiG <- get.obs.NGrams.by.pre(y, corpus[[2]])
                # Retrieve all unigrams end the unobserved bigrams UOBT: z where C(y,z) = 0, UOB in UOT
                UnObsBiTails <- get.unobs.Ngram.tails(ObsBiG, 2, corpus[[1]])
                # Exclude observed bigrams that also appear in observed trigrams: OB in UOT
                ObsBiG <- ObsBiG[!str_split_fixed(ObsTriG[,term], "_", 2)[,2], on="term"]
                
                ## Calculation part
                # Calculate probabilities of all observed trigrams: P^*(z|x,y)
                ObsTriG <- cal.obs.prob(ObsTriG, corpus[[2]], xy)
                # Calculate Alpha(x,y)
                Alpha_xy <- cal.alpha(ObsTriG, corpus[[2]], xy)
                # Calculate probabilities of all observed bigrams: P^*(z|y), (y,z) in UOT
                ObsBiG <- cal.obs.prob(ObsBiG, corpus[[1]], y)
                # Calculate Alpha(y)
                Alpha_y <- cal.alpha(ObsBiG, corpus[[1]], y)
                # Calculate P_{ML}(z), where c(y,z) in UOB: Alpha_y * P_{ML}(z)
                UnObsBiTails[, Prob:=corpus[[1]][UnObsBiTails, c, on=.(term)]/corpus[[1]][UnObsBiTails, sum(c), on=.(term)]]
                UnObsBiTails[, Prob:=Alpha_xy*Alpha_y*Prob]
                # Remove unused column in ObsTriG and ObsBiG
                ObsTriG[, c("c", "cDis"):=NULL]
                ObsTriG[, term:=str_remove(ObsTriG[, term], "([^_]+_)+")]
                ObsBiG[, c("c", "cDis"):=NULL]
                ObsBiG[, term:=str_remove(ObsBiG[, term], "([^_]+_)+")]
                # Compare OT, Alpha_xy * P_{Katz}(z|y)
                # P_{Katz}(z|y) = 1. P^*(z|y), 2. Alpha_y * P_{ML}(z)
                ObsBiG[,Prob:=Alpha_xy*Prob]
                AllTriG <- setorder(rbind(ObsTriG, ObsBiG, UnObsBiTails), -Prob)
                return(AllTriG[Prob!=0][1:min(dim(AllTriG[Prob!=0])[1], words_num)])
        } else {  # C(x,y) = 0
                y <- str_split_fixed(xy,"_", 2)[,2]
                # c(y>0)
                if (length(which(corpus[[1]]$term == y)) > 0) {
                        # Retrieve all observed bigrams beginning with y: OB
                        ObsBiG <- get.obs.NGrams.by.pre(y, corpus[[2]])
                        # Calculate probabilities of all observed bigrams: P^*(z|y)
                        ObsBiG <- cal.obs.prob(ObsBiG, corpus[[1]], y)
                        # Calculate Alpha(y)
                        Alpha_y <- cal.alpha(ObsBiG, corpus[[1]], y)
                        # Retrieve all unigrams end the unobserved bigrams UOBT: z where C(y,z) = 0
                        UnObsBiTails <- get.unobs.Ngram.tails(ObsBiG, 2, corpus[[1]])
                        # Calculate P_{ML}(z), where c(y,z) in UOB: Alpha_y * P_{ML}(z)
                        UnObsBiTails[, Prob:=corpus[[1]][UnObsBiTails, c, on=.(term)]/corpus[[1]][UnObsBiTails, sum(c), on=.(term)]]
                        UnObsBiTails[, Prob:=Alpha_y*Prob]
                        # Remove unused column in ObsBiG
                        ObsBiG[, c("c", "cDis"):=NULL]
                        ObsBiG[, term:=str_remove(ObsBiG[, term], "([^_]+_)+")]
                        AllBiG <- setorder(rbind(ObsBiG, UnObsBiTails), -Prob)
                        return(AllBiG[Prob!=0][1:words_num])
                } else {  # c(y=0)
                        # P^*z
                        return(setorder(corpus[[1]], -cDis)[1:words_num,.(term, Prob=cDis/corpus[[1]][,sum(c)])])  
                }
        }
}

# PART C: Prediction user interface
## USER_INPUT_PREPROC: Real-time pre-processing of user input
# cleans up user input and splits it into words, returns last two words

user_input_preproc <- function(phrase, stopwords=toxic_words) {
        prephrase<-phrase %>%
                replace_hash() %>%
                replace_tag() %>%
                replace_url(replacement = "") %>% 
                replace_contraction() %>%
                replace_word_elongation() %>%
                replace_internet_slang( replacement ="") %>%
                replace_names()
        prephrase<-tokenize_words(x=prephrase, strip_punct=TRUE, strip_numeric=TRUE, stopwords = stopwords)
        ret<-lapply(prephrase, function(x) {paste(tail(x,2), collapse=" ")})
        return(ret)
}


## NEXT_WORD: Function to trigger real time pre-processing and next word prediction
next_word <- function(phrase, words_num=5, corpus, stopwords=toxic_words) {
        #browser()
        bigram <- user_input_preproc(phrase, stopwords)
        result <- Find_Next_word(bigram, words_num, corpus)
        if (dim(result)[1] == 0) {
                rbind(result, list("<Enter more text>", 1))
        }
        return(result)
}


next_word_batch <- function(phrase, corpus, stopwords) {
        #browser()
        #phrase<-tokenize_words(x=bigram, strip_punct=TRUE, strip_numeric=TRUE, stopwords = stopwords)
        #phrase<-lapply(phrase, function(x) {paste(tail(x,2), collapse=" ")})
        bigram <- user_input_preproc(phrase, stopwords)
        result=character()
        for (x in bigram) {
                result<-append(result,Find_Next_word(x, 1, corpus)$term)
        }
        return(result)
}

# GET_ACCURACY: Function to calculate accuracy of the prediction compared to the actual (held out) response
get_accuracy<-function(prediction, actual){
        acc<-sum(prediction==actual)/length(prediction)
        return(acc)
}


### BODY OF THE SCRIPT

#Global Options
input_path='intermediate/pre-processed/'
toxic_words<-read_lines('input/bad_words.txt')
kaggle_stopwords<-read_lines('input/kaggle_stopwords.csv')

## Functional Test - Quiz 1
# Load corpora
i=0
ngram=list()
for (filename in dir(input_path, pattern="train*")){
        i=i+1
        message('ngram[[',i,']]: ', filename)
        ngram[[i]]<-read_rds(paste0 (input_path, filename))
        ngram[[i]]<-good_turing_preproc(corpus=ngram[[i]], min_count = 4, k=5)
}

# Select the preferred corpus
NgramFreq<-ngram[[2]] #train_ngrams_nospell_stop.rds
# Functional test - Quiz 1
next_word("The guy in front of me just bought a pound of bacon, a bouquet, and a case of", corpus=NgramFreq)
# options: soda, beer, cheese, pretzels answer: 4.beer
next_word ("You're the reason why I smile everyday. Can you follow me please? It would mean the", corpus=NgramFreq)
# options: world, universe, most, best answer: 1.world
next_word ("Hey sunshine, can you follow me and make me the", corpus=NgramFreq)
#options: smelliest, happiest, saddest, bluest answer: NA - guess happiest
next_word ("Very early observations on the Bills game: Offense still struggling but the", corpus=NgramFreq)
#options: players, defense, crowd, referees answer: NA - guess crowd
next_word ("Go on a romantic date at the", corpus=NgramFreq)
#options: movies, grocery, beach, mall answer: NA - guess beach
next_word ("Well I'm pretty sure my granny has some old bagpipes in her garage I'll dust them off and be on my", corpus=NgramFreq)
# options: way, phone, horse, motorcycle answer: 1. way
next_word ("Ohhhhh #PointBreak is on tomorrow. Love that film and haven't seen it in quite some", corpus=NgramFreq)
# options: weeks, time, thing, years answer: 1. time
next_word ("After the ice bucket challenge Louis will push his long wet hair out of his eyes with his little", corpus=NgramFreq)
#options: ears, fingers, toes, eyes answer: NA guess: fingers
next_word ("Be grateful for the good times and keep the faith during the", corpus=NgramFreq)
#options: worse, hard, sad, bad answer: NA guess-  bad
next_word ("If this isn't the cutest thing you've ever seen, then you must be", corpus=NgramFreq)
#options: callous, insensitive, asleep, insane answer: NA guess insane

# likely to be 4 out of 10 correctly - stopwords are the problem

# Select the preferred corpus and stopword list
#NgramFreq<-ngram[[1]]
# Functional test
next_word("The guy in front of me just bought a pound of bacon, a bouquet, and a case of", corpus=NgramFreq)
next_word ("You're the reason why I smile everyday. Can you follow me please? It would mean the", corpus=NgramFreq)
next_word ("Hey sunshine, can you follow me and make me the", corpus=NgramFreq)
next_word ("Very early observations on the Bills game: Offense still struggling but the", corpus=NgramFreq)
next_word ("Go on a romantic date at the", corpus=NgramFreq)
next_word ("Well I'm pretty sure my granny has some old bagpipes in her garage I'll dust them off and be on my", corpus=NgramFreq)
next_word ("Ohhhhh #PointBreak is on tomorrow. Love that film and haven't seen it in quite some", corpus=NgramFreq)
next_word ("After the ice bucket challenge Louis will push his long wet hair out of his eyes with his little", corpus=NgramFreq)
next_word ("Be grateful for the good times and keep the faith during the", corpus=NgramFreq)
next_word ("If this isn't the cutest thing you've ever seen, then you must be", corpus=NgramFreq)

# corpus train_ngrams_nospell_stop.rds


## Calculate Prediction Accuracy for 1000 validation ngrams using four different corpora 
set.seed(158)
valid<-read_rds(paste0(input_path,'valid_ngrams.rds')) %>% sample_n(1000)
prediction=list()
accuracy=numeric()
for (i in 1:4) {
        tic()
        if (i %in% c(1, 3)) {
                prediction[[i]]<-next_word_batch (phrase=valid$predictor,
                                                  corpus=ngram[[i]], 
                                                  stopwords=rbind(toxic_words,kaggle_stopwords))} 
        else {
                prediction[[i]]<-next_word_batch (phrase=valid$predictor,
                                                  corpus=ngram[[i]], 
                                                  stopwords=kaggle_stopwords)}
                
        toc()
        #accuracy<-append(accuracy, sum(prediction[[i]]==valid$response)/length(prediction[[i]]))
        accuracy<-append(accuracy, get_accuracy(prediction[[i]],valid$response))
}

print(accuracy)
# 0.042 0.125 0.040 0.123
# Corpora 2 & 4 (train_ngrams_nospell_stop.rds and train_ngrams_spell_stop.rds) generate highest accuracy and will be taken into the next round of optimisations


##Tune K/Min_Count Hyperparameters
# For the best performing corporus (train_ngrams_nospell_stop.rds)

valid_full<-read_rds('intermediate/pre-processed/valid_ngrams.rds')
ngram<-read_rds('intermediate/pre-processed/train_ngrams_nospell_stop.rds')
pred_metrics<-data.frame()
for (k_const in seq(1,6, by=1)){
        message('k/min_count=', k_const)
        start <- Sys.time ()
        message('...Preprocessing ngrams')
        ngram_gt<-good_turing_preproc(corpus=ngram, min_count = k_const, k=k_const)
        for (fold in 1:5){
                message('...Processing fold ',fold)
                set.seed(fold)
                valid_sample<-sample_n(valid_full,200)
                prediction<-next_word_batch (phrase=valid_sample$predictor, corpus=ngram_gt, stopwords=toxic_words)
                pred_metrics<-rbind(pred_metrics, data.frame(
                        "min_count"=k_const,
                        "k"=k_const,
                        "fold"=fold,
                        "accuracy_pct"=get_accuracy(prediction,valid_sample$response)*100))
        }
        act<-pred_metrics %>% filter(pred_metrics$k==1) %>% summarise(accuracy=mean(accuracy_pct))
        message('Accuracy=', act[1,1],'%')
        elapsed<-Sys.time () - start
        message('Elapsed time=', round(elapsed,1))
        gc()
}
pred_metrics%>% group_by(k) %>% summarise(accuracy_pct=mean(accuracy_pct))
