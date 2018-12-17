
#secondo me non servono le librerie, ma le metto lo stesso, mi danno sicurezza :)
library(rvest)
library(tidyverse)
library(pbapply)
library(reshape2)
library(ggplot2)
library(data.table)
library(plyr)


df1 <-read.csv2("75K_Part1_results.csv" , stringsAsFactors = FALSE)
df2 <-read.csv2("75K_Part2_results.csv" , stringsAsFactors = FALSE)
df3 <-read.csv2("75K_Part3_results.csv" , stringsAsFactors = FALSE)
df4 <-read.csv2("75K_Part4_results.csv" , stringsAsFactors = FALSE)
df5 <-read.csv2("75K_Part5_results.csv" , stringsAsFactors = FALSE)
df6 <-read.csv2("75K_Part6_results.csv" , stringsAsFactors = FALSE)

df <- rbind(df1, df2, df3, df4, df5, df6)


#da mettere il file csv corretto 
#df <-read.csv2("test_set_results.csv" , stringsAsFactors = FALSE)


#Prendo solo app che sono state categorizzate "dermatology" 
df1 <- df %>% filter(grepl("dermatology", V1, ignore.case = TRUE) | grepl("dermatology", V2, ignore.case = TRUE) | grepl("dermatology", V3, ignore.case = TRUE))


#prendo il db con tutti gli attributi
df2 <- read.csv2("Database_preprocessed_english.csv")


#unisco i due db solo per le app cardiology
d<-merge(df1, df2, by="ID")


write.csv2(d, "Dermatology_with_attributes.csv", row.names = FALSE)