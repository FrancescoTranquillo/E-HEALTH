rm(list = ls())

library(rvest)
library(tidyverse)
library(tm)

training_test_4<-read.csv2("Across_Specialties3.csv",header = T)

# make corpus for text mining (data comes from package, for reproducibility) 
corpus <- Corpus(VectorSource(training_test_4))



skipWords <- function(x) removeWords(x, stopwords("english"))
funcs <- list(tolower, removePunctuation, removeNumbers, stripWhitespace, skipWords)
a <- tm_map(corpus, FUN = tm_reduce, tmFuns = funcs)
a.dtm1 <- TermDocumentMatrix(a, control = list(wordLengths = c(3,10))) 

findFreqTerms(a.dtm1,2)

m <- as.matrix(a.dtm1)
v <- sort(rowSums(m), decreasing=TRUE)
head(v, N)





t4_train<-training_test_4[which(training_test_4$X=="Test"),c(4)]

N<-matrix(t4_train);


descr_freq_words<-findFreqTerms(N,lowfreq = 0, highfreq = Inf)


