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


unlink(x = "~/GitHub/E-HEALTH/R script/HF_splitted",recursive = T)

df <- read.csv2("HF.csv", stringsAsFactors = FALSE)%>%
  .[!duplicated(.),]
  

n<-as.numeric(readline("In quante parti vuoi dividere il dataframe?   "))

df.list<-split(df, rep(1:n, length.out = nrow(df), each = ceiling(nrow(df)/n)))

dir.create(file.path("~/GitHub/E-HEALTH/R script","HF_splitted"))

for (i in 1:n){
  print(i)

  dir.create(file.path("~/GitHub/E-HEALTH/R script/HF_splitted",i))
  dfs <- df.list[[i]]
  
  n<-2
  
  dfs.list<-split(dfs, rep(1:n, length.out = nrow(dfs), each = ceiling(nrow(dfs)/n)))
  
  for (j in 1:n){
  filename2<-paste0("~/GitHub/E-HEALTH/R script/HF_splitted/",i,"/","HF_db_piece_", i,"_part_",j, ".csv", sep="")
  write.csv2(dfs.list[[j]], filename2, row.names = FALSE)
  }
}


