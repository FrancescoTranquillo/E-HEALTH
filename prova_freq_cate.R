#PRIMO METODO
rm(list = ls())

library(rvest)
library(tidyverse)
library(pbapply)
library(reshape2)
library(ggplot2)
library(data.table)
library(tm)
library(SnowballC)


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

training_test_4<-read.csv2("Across_Specialties.csv",header = T)
training_test_4$Description <-
  as.character(training_test_4$Description)

corpus <- Corpus(VectorSource(training_test_4$Description[1:429]))

stopwords <- c(stopwords("en"), "app", "can", "use","will","may")

clean_corpus <- function(corpus) {
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removeWords, stopwords)
  #corpus <- tm_map(corpus, removeNumbers)
  
}

corpus_clean<-clean_corpus(corpus)

corpus_tdm<-TermDocumentMatrix(corpus_clean)
m <- as.matrix(corpus_tdm)
v <- sort(rowSums(m), decreasing=T)
v[1:50]

barplot(v[1:30], col = "tomato", las = 2,horiz = T)
t4_train<-training_test_4[which(training_test_4$X=="Test"),c(4)]

N<-matrix(t4_train);

descr_freq_words<-findFreqTerms(N,lowfreq = 0, highfreq = Inf)

