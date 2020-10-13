#Dependencies

#require(readtext)
#require(quanteda)
require(data.table)
require(stringi)
require(stringr)
library(tokenizers)
library(textclean)
library(readr)

#Global Options
input_path='intermediate/pre-processed/'
min_count=4 # minimum count
sample_reduction_rate=1
toxic_words<-read_lines('input/bad_words.txt')

## Load  Training Data

train_uni<-read_rds(paste0 (input_path, 'train_unigrams_nostopwords.rds')) %>% rename(c=term_instances_in_sample) %>% select(term, c) %>% filter(c>min_count) %>% sample_frac(sample_reduction_rate)
train_bi<-read_rds(paste0 (input_path, 'train_bigrams_nostopwords.rds')) %>% rename(c=term_instances_in_sample) %>% select(term, c) %>% filter(c>min_count) %>% sample_frac(sample_reduction_rate)
train_tri<-read_rds(paste0 (input_path, 'train_trigrams_nostopwords.rds')) %>% rename(c=term_instances_in_sample) %>% select(term, c) %>% filter(c>min_count) %>% sample_frac(sample_reduction_rate)
# transform spaces in terms to underscore (req'd by prediction algo)
train_bi <- train_bi %>% mutate (term=gsub(" ", "_", term))
train_tri <- train_tri %>% mutate (term=gsub(" ", "_", term))

UniFreq<-data.table(train_uni)
BiFreq<-data.table(train_bi)
TriFreq<-data.table(train_tri)


# Good-Turing

## Calculates the "frequency of frequency r" (N_r)
CountNC <- function(FreqVec) {
        CountTbl <- table(FreqVec[,.(c)])
        return(data.table(cbind(c=as.integer(names(CountTbl)), Nr=as.integer(CountTbl))))
}

UniBins <- CountNC(UniFreq)
BiBins <- CountNC(BiFreq)
TriBins <- CountNC(TriFreq)



# Average non-zero count, replace N_r with Z_r
avg.zr <- function(Bins) {
        max <- dim(Bins)[1]
        r<-2:(max-1)
        Bins[1, Zr:=2*Nr/Bins[2,c]]  # r=1, q=0, Zr=Nr/(0.5t)
        Bins[r, Zr:=2*Nr/(Bins[r+1,c]-Bins[r-1,c])]  # else, Zr=Nr/(0.5(t-q))
        Bins[max, Zr:=Nr/(c-Bins[(max-1),c])]  # r=max, t=2r-q, Zr=Nr/(r-q)
}

avg.zr(UniBins)
avg.zr(BiBins)
avg.zr(TriBins)


#Fit a linear regression model log(Z_r)=a+b*log(r)

## Replace Z_r with value computed from a linear regression that is fit to map Z_r to c in log space
## log(Z_r) = a + b*log(c)
FitLM <- function(CountTbl) {
        return(lm(log(Zr) ~ log(c), data = CountTbl))
}
UniLM <- FitLM(UniBins)
BiLM <- FitLM(BiBins)
TriLM <- FitLM(TriBins)


#Perform the discounting to small count (c) n-grams, where c <= k, using Katz's formula

k=5
Cal_GTDiscount <- function(cnt, N) {
        if (N==1) {
                model <- UniLM
        } else if (N==2) {
                model <- BiLM
        } else if (N==3) {
                model <- TriLM
        }
        # Common parts
        Z1 <- exp(predict(model, newdata=data.frame(c=1)))
        Zr <- exp(predict(model, newdata=data.frame(c=cnt)))
        Zrp1 <- exp(predict(model, newdata=data.frame(c=(cnt+1))))
        Zkp1 <- exp(predict(model, newdata=data.frame(c=(k+1))))
        
        sub <- ((k+1)*Zkp1)/(Z1)
        new_r <- ((cnt+1)*(Zrp1)/(Zr)-cnt*sub)/(1-sub)
        return(new_r)
}

UpdateCount <- function(FreqTbl, N) {
        FreqTbl[c>k ,cDis:=as.numeric(c)]
        FreqTbl[c<=k, cDis:=Cal_GTDiscount(c, N)]
}
UpdateCount(UniFreq, 1)
UpdateCount(BiFreq, 2)
UpdateCount(TriFreq, 3)
setkey(UniFreq, term)
setkey(BiFreq, term)
setkey(TriFreq, term)


## N-Grams Preparation Functions

# Function to retrieve observed N-grams from (N−1)-gram

## Return all the observed N-grams given the previous (N-1)-gram
##
## - wordseq: character vector of (N-1)-gram separated by underscore, e.g. "x1_x2_..._x(N-1)"
## - NgramFreq: datatable of N-grams
get.obs.NGrams.by.pre <- function(wordseq, NgramFreq) {
        PreTxt <- sprintf("%s%s%s", "^", wordseq, "_")
        NgramFreq[grep(PreTxt, NgramFreq[,term], perl=T, useBytes=T),]
}

# Function to retrieve all the unigrams that end unobserved N-grams

## Return all the unigrams that end unobserved Ngrams
get.unobs.Ngram.tails <- function(ObsNgrams, N) {
        ObsTails <- str_split_fixed(ObsNgrams[,term], "_", N)[,N]
        return(data.table(term=UniFreq[!ObsTails,term,on="term"]))
}


# Function to compute the probabilities of observed N-grams

## Compute the probabilities of observed N-gram.
## We need the counts from (N-1)-gram table since corpus doesn't include <EOS> explicitly,
## therefore the denominator will be smaller if only summing up all the terms
## from N-gram table
cal.obs.prob <- function(ObsNgrams, Nm1Grams, wordseq) {
        PreCount <- Nm1Grams[wordseq, c, on=.(term)]
        ObsNgrams[,Prob:=ObsNgrams[,cDis]/PreCount]  # c_dis/c
}


## Function to compute Alpha
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

# Function to find next word

## Return a list of predicted next words according to previous 2 user input words
##
## - xy: character vector containing user-input bigram, separated by a space
## - words_num: number of candidates of next words returned
Find_Next_word <- function(xy, words_num) {
        xy <- gsub(" ", "_", xy)
        debug<-which(BiFreq$term == xy)
        if (length(which(BiFreq$term == xy)) > 0) {  # C(x,y) > 0
                ## N-grams preparation
                # Retrieve all observed trigrams beginning with xy: OT
                ObsTriG <- get.obs.NGrams.by.pre(xy, TriFreq)
                y <- str_split_fixed(xy,"_", 2)[,2]
                # Retrieve all observed bigrams beginning with y: OB
                ObsBiG <- get.obs.NGrams.by.pre(y, BiFreq)
                # Retrieve all unigrams end the unobserved bigrams UOBT: z where C(y,z) = 0, UOB in UOT
                UnObsBiTails <- get.unobs.Ngram.tails(ObsBiG, 2)
                # Exclude observed bigrams that also appear in observed trigrams: OB in UOT
                ObsBiG <- ObsBiG[!str_split_fixed(ObsTriG[,term], "_", 2)[,2], on="term"]
                
                ## Calculation part
                # Calculate probabilities of all observed trigrams: P^*(z|x,y)
                ObsTriG <- cal.obs.prob(ObsTriG, BiFreq, xy)
                # Calculate Alpha(x,y)
                Alpha_xy <- cal.alpha(ObsTriG, BiFreq, xy)
                # Calculate probabilities of all observed bigrams: P^*(z|y), (y,z) in UOT
                ObsBiG <- cal.obs.prob(ObsBiG, UniFreq, y)
                # Calculate Alpha(y)
                Alpha_y <- cal.alpha(ObsBiG, UniFreq, y)
                # Calculate P_{ML}(z), where c(y,z) in UOB: Alpha_y * P_{ML}(z)
                UnObsBiTails[, Prob:=UniFreq[UnObsBiTails, c, on=.(term)]/UniFreq[UnObsBiTails, sum(c), on=.(term)]]
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
                debug<-which(UniFreq$term == y)
                if (length(which(UniFreq$term == y)) > 0) {
                        # Retrieve all observed bigrams beginning with y: OB
                        ObsBiG <- get.obs.NGrams.by.pre(y, BiFreq)
                        # Calculate probabilities of all observed bigrams: P^*(z|y)
                        ObsBiG <- cal.obs.prob(ObsBiG, UniFreq, y)
                        # Calculate Alpha(y)
                        Alpha_y <- cal.alpha(ObsBiG, UniFreq, y)
                        # Retrieve all unigrams end the unobserved bigrams UOBT: z where C(y,z) = 0
                        UnObsBiTails <- get.unobs.Ngram.tails(ObsBiG, 2)
                        # Calculate P_{ML}(z), where c(y,z) in UOB: Alpha_y * P_{ML}(z)
                        UnObsBiTails[, Prob:=UniFreq[UnObsBiTails, c, on=.(term)]/UniFreq[UnObsBiTails, sum(c), on=.(term)]]
                        UnObsBiTails[, Prob:=Alpha_y*Prob]
                        # Remove unused column in ObsBiG
                        ObsBiG[, c("c", "cDis"):=NULL]
                        ObsBiG[, term:=str_remove(ObsBiG[, term], "([^_]+_)+")]
                        AllBiG <- setorder(rbind(ObsBiG, UnObsBiTails), -Prob)
                        return(AllBiG[Prob!=0][1:words_num])
                } else {  # c(y=0)
                        # P^*z
                        return(setorder(UniFreq, -cDis)[1:words_num,.(term, Prob=cDis/UniFreq[,sum(c)])])  
                }
        }
}

##The Result

## Remove elements not being used by prediction model
Preprocess <- function(wordseq) {
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
Next_word <- function(prephrase, words_num=5) {
        bigr <- Preprocess(prephrase)
        result <- Find_Next_word(bigr, words_num)
        if (dim(result)[1] == 0) {
                rbind(result, list("<Please input more text>", 1))
        }
        return(result)
}

# Predict next word
Next_word("He likes to eat ice")
Next_word("On Mondays he likes to play a game of")
Next_word ("He went the other")
Next_word ("Where are you")
Next_word ("At what time are you going to")
Next_word ("Laughing out")
Next_word ("New car")

## Issue, if a single word is not recognised system returns stopwords. Probably not an issue for accuracy of the prediction but probably for usability.

# 
test<-read_rds(paste0(input_path,'test_ngrams_nostopwords.rds'))

test_sample<-test%>%sample_n(5000)
tic()
y=character()
for (x in test_sample$predictor) {
        y<-append(y,Next_word(x)[1]$term)
}
toc()
accuracy<-sum(y==test_sample$response)/length(y)

