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

d = NULL 



source("maxpagenumber.r")
maxpagenumber

#2: Lettura URL ####
letters<-c(LETTERS,"*")
lenletters<-length(letters)

for(i in 1:lenletters) {

letter<-letters[i]

for(j in 1:maxpagenumber[i]){
  url<-paste("https://itunes.apple.com/us/genre/ios-medical/id6020?mt=8&letter=",letter,"&page=", j,"#page",sep="")


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


d = rbind(d, data.frame(Name= nomi, URL= urls, ID=id)) 
}
}

#7: Salvataggio del dataset in formato csv ####
write.csv2(d, "appinfo2.csv")
