library(tidyverse)
library(rvest)
library(stringr)
library(rebus)
library(urltools)        ##XXX PROBLEMA ENORME
library(gdata)           ##con queste colonne di getappinfo Error: Columns `average.rating`, `ratings`, `currency` 
library(anytime)         ##must be length 1 or 4, not 0, 0, 0
library(dplyr)
library(lubridate)

avgrating=NULL          ######inizializzo per forza tutto se no il codice mi infama
ratings=NULL
pegi=NULL
category=NULL
description=NULL
lastupdate<-NULL
version<-NULL
devname<-NULL
price2<-NULL              ######price2 non chiedermi perch? 2, mi piaceva di pi?


df<-read.csv2("app_M_H&F_ENGLISH_ONLY.csv", stringsAsFactors = FALSE)
dc<- df[,2]                                                              ##prendo seconda colonna dove ci sono gli url
len<-length(dc)                                                          ##prendo lunghezza in len sar? la fine del ciclo quando ci decidiamo con
                                                                         ##coraggio a mandarlo ed aspettare 6 ore che finisca

for (i in 1:4){                           ###### METTO SOLO 4 PERCH? SE METTESSI "len" (DI RIGA 23) CIAONE, CI METTEREBBE 6 GIORNI E NONCCCCI?VOGLIA
  url<- dc[i]                             ######url prende l'iesima riga di dc che sarebbe la seconda colonna fatta da getinfo.r dove ci sono gli url e la apro in page
  page<-read_html(url)
  
  #punteggio medio (DOUBLE)####
  avgrating1<-page%>%
    html_nodes(".we-customer-ratings__averages__display")%>%   
    html_text(trim = TRUE)%>%                                  
    as.numeric(.)                                              
  if(length(avgrating1)=="0"){
    avgrating1<-NA
  }
  
  avgrating<-c(avgrating,avgrating1)                           ######allora qui per non sovrascrivere come avevamo fatto in matlab lo richiamo cos?
  
  #punteggio totale (DOUBLE)####
  ratings1<-page%>%
    html_node(".we-customer-ratings__count")%>%
    html_text(trim = TRUE)%>%
    gsub(" Ratings", "", .)                                  
  
  ratings1<-as.numeric(sub("K", "e3", ratings))
  if(length(ratings1)=="0"){
    ratings1<-NA
  }
  
  ratings<-c(ratings,ratings1)                                ##qui mi infama mi sa il codice dopo da problemi nella costruzione della tabella
  
  #PEGI (DOUBLE)####
  
  pegipattern<-"\\d+"
  
  pegi1<-page%>%
    html_node(".l-row:nth-child(6) .large-6")%>%            
    html_text(trim=TRUE)%>%                                 
    str_extract(., pattern = pegipattern)%>%               
    as.numeric()
pegi<-c(pegi,pegi1)


  #Categoria (STRINGA)####
  
  category1<-page%>%
    html_node(".large-6 .link")%>%                         
    html_text(trim=TRUE)
category<-c(category,category1)
  
  #Descrizione (STRINGA)####
  
  description1<-page%>%
    html_node(".section__description .we-clamp__contents")%>%      
    html_text(trim=TRUE)
  description<-c(description,description1)
  
  
  #Data ultimo update (DATA)####
  
    lastupdate1<-page%>%
    html_node(".version-history__item__release-date")%>%
    html_text(trim=TRUE)%>%
    gsub(",","",.)%>%
    parse_date_time(., orders = "b d Y", locale="us")        
    lastupdate<-c(lastupdate,lastupdate1)
  
  #Ultima versione app (STRINGA) ####
  
  version1<-page%>%
    html_node(".version-history__item__version-number")%>%         ##non ? un numero ? una stringa, va beneeeeeee??? non 
    html_text()
  version<-c(version,version1)
  
  #Developer (STRINGA)####
  
  devname1<-page%>%
    html_node(".information-list__item:nth-child(1) .large-6")%>%       
    html_text(trim=TRUE)
  devname<-c(devname,devname1)
  #Prezzo (DOUBLE) e valuta (STRINGA)
  
  currencypattern<-"^\\W"                                                
  price<-page%>%        
    html_node(".inline-list__item--bulleted:nth-child(1)")%>%
    html_text(trim=TRUE)
  
  if(price=="Free"){
    price<-0
    currency<-0                        ##currency=valuta
  } else {
    currency<-str_extract(price, currencypattern)
    price<-gsub(currencypattern, "", price)     
    price<-as.numeric(price)
  }
  price2<-c(price2,price)
  
  
}
data <- tibble(ratings=ratings1, average.rating=avgrating, category=category, pegi=pegi, description=description, devname=devname, price=price2, version=version, lastupdate=lastupdate)  ## problemi con queste colonne di getappinfo Error: Columns `average.rating`, `ratings`, `currency` must be length 1 or 4, not 0, 0, 0
write.csv2(data, "appcategory3.csv")
