
#secondo me non servono le librerie, ma le metto lo stesso, mi danno sicurezza :)
library(rvest)
library(tidyverse)
library(pbapply)
library(reshape2)
library(ggplot2)
library(data.table)
library(plyr)


#da mettere il file csv corretto 
df <-read.csv2("test_set_results.csv" , stringsAsFactors = FALSE)


#Prendo solo app che sono state categorizzate "dermatology" 
df1 <- df %>% filter(grepl("dermatology", V1, ignore.case = TRUE) | grepl("dermatology", V2, ignore.case = TRUE) | grepl("dermatology", V3, ignore.case = TRUE))


write.csv2(df1, "Dermatology.csv", row.names = FALSE)


#prendo il db con tutti gli attributi
df2 <- read.csv2("Database_preprocessed_english.csv")


#unisco i due db solo per le app cardiology
d<-merge(df1, df2, by="ID")


write.csv2(d, "Dermatology_with_attributes.csv", row.names = FALSE)