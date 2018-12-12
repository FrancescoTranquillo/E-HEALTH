
library(rvest)
library(tidyverse)
library(pbapply)
library(reshape2)
library(ggplot2)
library(data.table)



df <-read.csv2("Database_preprocessed_english.csv", stringsAsFactors = FALSE)
x <- NULL


len<- nrow(df)
len 


i<-0

for (i in 1:len){

  y <- c(df[i,5])
  char <- nchar(y)
  x <- c(x, char)
}

df$DescriptionLength <- x
