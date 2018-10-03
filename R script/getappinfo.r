library(tidyverse)
library(rvest)
library(stringr)
library(rebus)
library(urltools)
library(gdata)
library(anytime)
library(dplyr)
library(lubridate)

url<-'https://itunes.apple.com/us/app/sprout-period-fertility-ovulation-tracker/id1003306557?mt=8'

page<-read_html(url)

#punteggio medio (DOUBLE)####
avgrating<-page%>%
  html_nodes(".we-customer-ratings__averages__display")%>%   ##Prende la classe
  html_text(trim = TRUE)%>%                                  ##sulla cosa che hai preso fammi diventare testo, trim=pulisci html
  as.numeric(.)                                              ##diventa numerico il . serve per prendere la riga prima

#punteggio totale (DOUBLE)####
ratings<-page%>%
  html_node(".we-customer-ratings__count")%>%
  html_text(trim = TRUE)%>%
  gsub(" Ratings", "", .)                                   ##gsub per togliere spazio e ratings

ratings<-as.numeric(sub("K", "e3", ratings))                ##Sostituisci a K(che sta per 1000) a 10elevato3

#PEGI (DOUBLE)####

pegipattern<-"\\d+"

pegi<-page%>%
  html_node(".l-row:nth-child(6) .large-6")%>%            ##prendi classe dove trovi età
  html_text(trim=TRUE)%>%                                 ##pulisco
  str_extract(., pattern = pegipattern)%>%                ##estrae in . (page)       corrisponde il pattern che ho selezionato sopra, prende cifre
  as.numeric()

#Categoria (STRINGA)####

category<-page%>%
  html_node(".large-6 .link")%>%                         ##prende la categoria, la classe in r gli devi dire .link
  html_text(trim=TRUE)

#Descrizione (STRINGA)####

description<-page%>%
  html_node(".section__description .we-clamp__contents")%>%      ##METAMAP?????
  html_text(trim=TRUE)



#Data ultimo update (DATA)####



lastupdatedate<-page%>%
  html_node(".version-history__item__release-date")%>%
  html_text(trim=TRUE)%>%
gsub(",","",.)%>%
parse_date_time(., orders = "b d Y", locale="us")        ##b=mese d=giorno Y=anno   locale us = parse date utilizza il sistema di orologio del cumputer sul computer in cui lo stai mandando 


#Ultima versione app (STRINGA) ####

version<-page%>%
  html_node(".version-history__item__version-number")%>%         ##non è un numero è una stringa, va beneeeeeee???
  html_text()

#Developer (STRINGA)####

devname<-page%>%
  html_node(".information-list__item:nth-child(1) .large-6")%>%       
  html_text(trim=TRUE)

#Prezzo (DOUBLE) e valuta (STRINGA)

currencypattern<-"^\\W"                                               ##creo pattern da riconoscere simbolo e toglierlo // per non prendere quello 
price<-page%>%        
  html_node(".inline-list__item--bulleted:nth-child(1)")%>%
  html_text(trim=TRUE)

if(price=="Free"){
  price<-0
  currency<-0                        ##currency=valuta
} else {
  currency<-str_extract(price, currencypattern)
  price<-gsub(currencypattern, "", price)     ##gsub(guardatutto, sostituisci con, dove metterlo)
  price<-as.numeric(price)
}


#creazione dataset
combined_data <- tibble(average.rating=avgrating, ratings=ratings, category=category,                         ##tibble crea la tabella (nome colonna=vettore etc)
                        pegi=pegi, description=description, devname=devname, price=price, currency=currency,  ##se il vettore non è un valore ma è un vettore 
                        version=version, lastupdate=lastupdatedate) 

combined_data
