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
                        ObsBiG <- cal.obs.prob(ObsBiG, corus[[1]], y)
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


## USER_INPUT_PREPROC: Real-time preprocessing of user input
# cleans up user input and splits it into words, returns last two words
user_input_preproc <- function(wordseq) {
        names(wordseq) <- NULL
        quest <- replace_hash(wordseq)
        quest<-replace_tag(quest)
        quest<-replace_url(quest, replacement = "")
        quest<-replace_contraction(quest)
        quest<-replace_word_elongation(quest)
        quest<-replace_internet_slang(quest, replacement ="")
        quest<-replace_names(quest)
        quest <- tokenize_words(x=quest, strip_punct=TRUE, strip_numeric=TRUE, stopwords = toxic_words)[[1]]
        quest <- replace_white(quest)
        return(paste(tail(quest, 2), collapse = " "))
}

## Interface to preprocess text and request prediction
next_word <- function(prephrase, words_num=5, corpus) {
        #browser()
        bigr <- user_input_preproc(prephrase)
        result <- Find_Next_word(bigr, words_num, corpus)
        if (dim(result)[1] == 0) {
                rbind(result, list("<Enter more text>", 1))
        }
        return(result)
}


### BODY OF THE SCRIPT

#Global Options
input_path='intermediate/pre-processed/'
toxic_words<-read_lines('input/bad_words.txt')


## Load  Training Data
NgramFreq<-read_rds(paste0 (input_path, 'train_ngrams_nospell_stop.rds'))

# Calculate discounted term count, using Good-Turing
NgramFreq<-good_turing_preproc(corpus=NgramFreq, min_count = 4, k=5)

# Functional Test
next_word("He likes to eat ice", corpus=NgramFreq)
next_word("On Mondays he likes to play a game of", corpus=NgramFreq)
next_word ("He went the other", corpus=NgramFreq)
next_word ("Where are you", corpus=NgramFreq)
next_word ("At what time are you going to", corpus=NgramFreq)
next_word ("Laughing out", corpus=NgramFreq)
next_word ("New car", corpus=NgramFreq)

# Calculate accuracy
test<-read_rds(paste0(input_path,'test_ngrams_nospell_stop.rds'))

test_sample<-test%>%sample_n(5000)
tic()
y=character()
for (x in test_sample$predictor) {
        y<-append(y,Next_word(x)[1]$term)
}
toc()
accuracy<-sum(y==test_sample$response)/length(y)
