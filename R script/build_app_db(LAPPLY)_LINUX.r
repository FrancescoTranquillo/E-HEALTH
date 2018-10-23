rm(list = ls())
#Sys.sleep(60)
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
library(pbapply)

delay<-2.85
piece<-readline("Inserisci il numero della cartella: ")
part<-readline("Di quale parte vuoi estrarre gli attributi? ")
filename<-paste0("~/Documents/GitHub/E-HEALTH/R script/HF_splitted/",piece,"/HF_db_piece_",piece,"_part_",part,".csv")
df <- read.csv2(filename, stringsAsFactors = FALSE)%>%
  .[!duplicated(.),]
  # %>%
  # .[sample(nrow(.), n,),]

list_url<-as.list(df[,2])


source("f_addattributes.r")

closeAllConnections()


attrs<-g%>%do.call("rbind",.)

final_db<-merge(df, attrs, all = TRUE)


filename<-paste0("~/Documents/GitHub/E-HEALTH/R script/HF_splitted/",piece,"/HF_db_piece_",piece,"_part_",part,"_completed.csv", sep = "")
write.csv2(final_db, filename,row.names = FALSE)
