# 1: Caricamento librerie ####
# General-purpose data wrangling
library(tidyverse)

# Parsing of HTML/XML files
library(rvest)

# String manipulation
library(stringr)

# Verbose regular expressions
library(rebus)

#URL parser
library(urltools)

#1: Inizializzazione del dataset
d = NULL


#avvio lo script che mi da il max numero
#di pagine per ogni lettera
source("maxpagenumber.r")
maxpagenumber

#2: Cicli for ####

#creazione del vettore delle lettere
letters<-c(LETTERS,"*")
lenletters<-length(letters)

#primo for: questo cicla tra tutte le lettere
for(i in 1:lenletters) {

letter<-letters[i]
#secondo for: questo cicla da 1 al massimo numero
#di pagine proprio di ogni lettera
for(j in 1:maxpagenumber[i]){
  url<-paste("https://itunes.apple.com/us/genre/ios-health-fitness/id6013?mt=8&letter=",letter,"&page=", j,"#page",sep="")


page<-read_html(url)

#3: Estrazione url delle app ####
urls<- page%>%
  rvest::html_nodes("#selectedcontent li")%>%
  rvest::html_nodes("a")%>%
  rvest::html_attr("href")

#4: Estrazione nomi delle app ####
nomi<- page%>%
  rvest::html_nodes("#selectedcontent li")%>%
  rvest::html_text()

#5 Estrazione ID delle app ####
IDs1<-gsub("(?<=)(.*)(id)", "", urls, perl = TRUE)
IDs2<-gsub("(?<=[id])(\\d+)", "", IDs1, perl = TRUE)
id<-gsub("\\?mt=8", "", IDs2)


#6: Costruzione del dataset ####
d = rbind(d, data.frame(Name= nomi, URL= urls, ID=id,category="Health&Fitness")) 
}
}

#7: Salvataggio del dataset in formato csv ####
write.csv2(d, "AppH&FCat.csv", row.names = FALSE)
