rm(list = ls())

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

n<-100

df <- read.csv2("Merged_db.csv", stringsAsFactors = FALSE)%>%
  .[!duplicated(.),]%>%
  .[sample(nrow(.), n),]

list_url<-as.list(df[,2])

tictoc::tic()
source("f_addattributes.r")
closeAllConnections()
tictoc::toc()
rbind

attrs<-g%>%do.call("rbind",.)

final_db<-merge(df, attrs, all = TRUE)

filename<-paste("Sample_", n, ".csv", sep = "")
write.csv2(final_db, filename,row.names = FALSE)
