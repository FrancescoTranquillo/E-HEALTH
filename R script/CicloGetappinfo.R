library(tidyverse)
library(rvest)
library(stringr)
library(rebus)
library(urltools)
library(gdata)
library(anytime)
library(dplyr)
library(lubridate)
d = NULL
df<-read.csv2("app_M_H&F_ENGLISH_ONLY.csv", stringsAsFactors = FALSE)
dc<- df[,2]

for (i in 100){
url<- dc[i]
page<-read_html(url)

##diventa numerico il . serve per prendere la riga prima
pegipattern<-"\\d+"

pegi<-page%>%
  html_node(".l-row:nth-child(6) .large-6")%>%            ##prendi classe dove trovi età
  html_text(trim=TRUE)%>%                                 ##pulisco
  str_extract(., pattern = pegipattern)%>%                ##estrae in . (page)       corrisponde il pattern che ho selezionato sopra, prende cifre
  as.numeric()

d=rbind(d, data.frame(pdb=pegi))
}


