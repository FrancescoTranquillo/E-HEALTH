library(tidyverse)
library(rvest)
library(stringr)
library(rebus)
library(urltools)
library(gdata)
library(anytime)
library(dplyr)
library(lubridate)

url<-"https://itunes.apple.com/us/app/human-anatomy-atlas-2019/id1117998129?mt=8"

page<-read_html(url)

#punteggio medio (DOUBLE)####
avgrating<-page%>%
  html_nodes(".we-customer-ratings__averages__display")%>%
  html_text(trim = TRUE)%>%
  as.numeric(.)

#punteggio totale (DOUBLE)####
ratings<-page%>%
  html_node(".we-customer-ratings__count")%>%
  html_text(trim = TRUE)%>%
  gsub(" Ratings", "", .)

ratings<-as.numeric(sub("K", "e3", ratings))

#PEGI (DOUBLE)####

pegipattern<-"\\d+"

pegi<-page%>%
  html_node(".l-row:nth-child(6) .large-6")%>%
  html_text(trim=TRUE)%>%
  str_extract(., pattern = pegipattern)%>%
  as.numeric()

#Categoria (STRINGA)####

category<-page%>%
  html_node(".large-6 .link")%>%
  html_text(trim=TRUE)

#Descrizione (STRINGA)####

description<-page%>%
  html_node(".section__description .we-clamp__contents")%>%
  html_text(trim=TRUE)

#Data ultimo update (DATA)####



lastupdatedate<-page%>%
  html_node(".version-history__item__release-date")%>%
  html_text(trim=TRUE)%>%
gsub(",","",.)%>%
parse_date_time(., orders = "b d Y", locale="us")


#Ultima versione app (STRINGA) ####

version<-page%>%
  html_node(".version-history__item__version-number")%>%
  html_text()

#Developer (STRINGA)####

devname<-page%>%
  html_node(".information-list__item:nth-child(1) .large-6")%>%
  html_text(trim=TRUE)

#Prezzo (DOUBLE) e valuta (STRINGA)

currencypattern<-"^\\W"
price<-page%>%
  html_node(".inline-list__item--bulleted:nth-child(1)")%>%
  html_text(trim=TRUE)

if(price=="Free"){
  price<-0
  currency<-0
} else {
  currency<-str_extract(price, currencypattern)
  price<-gsub(currencypattern, "", price)
  price<-as.numeric(price)
}


#creazione dataset
combined_data <- tibble(average.rating=avgrating, ratings=ratings, category=category,
                        pegi=pegi, description=description, devname=devname, price=price, currency=currency,
                        version=version, lastupdate=lastupdatedate) 

combined_data
