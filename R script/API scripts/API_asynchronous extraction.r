rm(list = ls()) #pulisce l'enviroment di tutte le variabili create

library(plyr)
library(dplyr)
library(Rcrawler)
library(tidyverse)
library(rvest)
library(stringr)
library(rebus)
library(urltools)
library(RCurl)
library(gdata)
library(anytime)
library(dplyr)
library(lubridate)
library(progress)
library(httr)
library(jsonlite)
library(purrrlyr)
library(parallel)
library(microbenchmark)
#id<-1178589519

df <- read.csv2("M.csv", header = TRUE, stringsAsFactors = FALSE)
df<-df[!duplicated(df),]
df<-df[1:50,]

nr <- nrow(df)
cl <- makeCluster(detectCores() - 1)
n<-length(cl)
a<-split(df, rep(1:ceiling(nr/n), each=n, length.out=nr))

source("API_extractor_(Aynchronous).r")

