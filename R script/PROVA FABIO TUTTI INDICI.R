library(tidyverse)
library(rvest)
library(stringr)
library(rebus)
library(urltools)       
library(gdata)            
library(anytime)      
library(dplyr)
library(lubridate)

avgrating=NULL       ##1 
ratings=NULL         #2
pegi=NULL            #3
category=NULL        #4
description=NULL     #5
lastupdate<-NULL     #6
version<-NULL        #7
devname<-NULL        #8
price2<-NULL         #9
appid<-NULL          #10
appname<-NULL        #11
appurl<-NULL         #12
lang<-NULL           #13
urldev<-NULL         #14
size<-NULL           #15
keywords<-NULL                    #16keywords
                     #17DEV ID
                     #18RELEASEDATE
                     #19Averageuser rat current + ALL????
                     #20NUm of user rating current+ALL????
                     #21 Num user rating
                     #22 % 5star
                     #23 % 4star
                     #24 % 3star (integer)
                     #25 % 2star
                     #26 % 1star
                     #27 Date RETRIEVED



df<-read.csv2("app_M_H&F_ENGLISH_ONLY.csv", stringsAsFactors = FALSE)
dc<- df[,2]  ##prendo seconda colonna dove ci sono gli url                                                             ##prendo seconda colonna dove ci sono gli url
d2<- df[,3]
d3<-df[,1]
len<-length(dc)                                                          ##prendo lunghezza in len sar? la fine del ciclo quando ci decidiamo con
##coraggio a mandarlo ed aspettare 6 ore che finisca

for (i in 228:230){                           ###### METTO SOLO 4 PERCH? SE METTESSI "len" (DI RIGA 23) CIAONE, CI METTEREBBE 6 GIORNI E NONCCCCI?VOGLIA
  url<- dc[i]                             ######url prende l'iesima riga di dc che sarebbe la seconda colonna fatta da getinfo.r dove ci sono gli url e la apro in page
  page<-read_html(url)
  
  
  ##KEYWORDS(HO BESTEMMIATO MALE)
  keywords1<-page%>%
    html_nodes('meta')%>%
    html_attr('content')
  keywords1<-keywords1[8]
  keywords<-c(keywords,keywords1)
  
##LINGUAGGIO ####
lang1<-page%>%
html_nodes(".l-row:nth-child(8) .large-6 , .l-row:nth-child(7) .large-6 , .l-row:nth-child(6) .large-6 , .large-6 .we-clamp__contents , .l-row:nth-child(2) .large-6 , .large-6 .link , .information-list__item:nth-child(1) .large-6")%>%
html_text(trim=TRUE)
lang1<-lang1[5]
lang<-c(lang,lang1)
  
  
  ##size
  size1<-page%>%
    html_nodes(".l-row:nth-child(8) .large-6 , .l-row:nth-child(7) .large-6 , .l-row:nth-child(6) .large-6 , .large-6 .we-clamp__contents , .l-row:nth-child(2) .large-6 , .large-6 .link , .information-list__item:nth-child(1) .large-6")%>%
    html_text(trim=TRUE)
  size1<-size1[2]
  size<-c(size,size1)
  
##PROVA PER GB MB KB CHE NON FUNGE
 #  patternnum<-"\\d+"
 # size1<-page%>%
 # html_nodes(".l-row:nth-child(8) .large-6 , .l-row:nth-child(7) .large-6 , .l-row:nth-child(6) .large-6 , .large-6 .we-clamp__contents , .l-row:nth-child(2) .large-6 , .large-6 .link , .information-list__item:nth-child(1) .large-6")%>%
 # html_text(trim=TRUE)
 # size1<-size1[2]
 # if(grepl("GB",size1)==TRUE){
 #   num<-str_extract(size1, pattern = patternnum)
 #   size1<-as.numeric(num)
 #   size1<-size1[1]*1000
 # }
 # if(grepl("KB",size1)==TRUE){
 #   num<-str_extract(size1, pattern = patternnum)
 #   size1<-as.numeric(num)
 #   size1<-size1/1000
 # }
 # m<-str_extract(size1, pattern = patternnum)
 # size1<-as.numeric(num)
  
  
  
  
  
  ##URLDEV
  urldev1<-page%>%
    html_node(".app-header__identity")%>%      ##non ? un numero ? una stringa, va beneeeeeee???
    html_nodes(".link")%>%
    html_attr("href")
  urldev<-c(urldev,urldev1)
  
  
  
  ##appid da tabella easy ####
  appid1<-d2[i]
  appid<-c(appid,appid1)
  
  ##appname ####
  appname1<-d3[i]
  appname<-c(appname,appname1)
  
  #appurl ####
  appurl1<-dc[i]
  appurl<-c(appurl,appurl1)
  
  #punteggio medio (DOUBLE)####
  avgrating1<-page%>%
    html_nodes("we-clamp__contents")%>%   
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
  ratings1<-as.numeric(sub("K", "e3", ratings1))
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
data <- tibble(keywords=keywords, languages=lang, Size=size, AppID=appid, Name=appname, AppURL=appurl, description=description, average.rating=avgrating, category=category, pegi=pegi, devname=devname, price=price2, version=version, lastupdate=lastupdate)  ## problemi con queste colonne di getappinfo Error: Columns `average.rating`, `ratings`, `currency` must be length 1 or 4, not 0, 0, 0
write.csv2(data, "appcategoryratingsPROVAFINALE.csv")