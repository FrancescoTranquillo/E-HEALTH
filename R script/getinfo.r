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

url<-"https://itunes.apple.com/us/genre/ios-medical/id6020?mt=8&letter=N&page=5#page"

page<-read_html(url)

urls<- page%>%
  rvest::html_nodes("#selectedcontent li")%>%
  rvest::html_nodes("a")%>%
  rvest::html_attr("href")


nomi<- page%>%
  rvest::html_nodes("#selectedcontent li")%>%
  rvest::html_text()


pattern<-


ids<-gsub("(?<=[id])(\d+)(?=[?mt=8])", "", urls)

str_sub(urls, -50, -60)

df<-data_frame(Name= nomi, URL= urls, ID=ids)



write.csv(df, "appinfo.csv")

