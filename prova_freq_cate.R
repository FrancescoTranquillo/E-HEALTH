#PRIMO METODO
rm(list = ls())

library(rvest)
library(tidyverse)
library(pbapply)
library(reshape2)
library(ggplot2)
library(data.table)


#leggo l'output di metamap
invalue <- read_html("Across_Specialties3.txt")

prova <- invalue %>%
  html_nodes("candidatepreferred") %>%
  html_text(trim = F)

df <- tibble(
  "cate" = prova
)

df1 <- count(df, "cate")

df1_sub<-df1 %>% filter(freq >= 10)




#SECONDO METODO
rm(list = ls())

library(rvest)
library(tidyverse)
library(tm)

training_test_4<-read.csv2("Across_Specialties3.csv",header = T)

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
