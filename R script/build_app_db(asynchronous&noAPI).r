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

n<-10

df <- read.csv2("Merged_db.csv", stringsAsFactors = FALSE)%>%
  .[!duplicated(.),]%>%
  .[sample(nrow(.), n),]

list_url<-as.list(df[,2])

tictoc::tic()
source("f_addattributes.r",echo=TRUE)
closeAllConnections()
tictoc::toc()

g<-g%>%do.call("rbind",.)

final_db<-merge(df, g, all = TRUE)
