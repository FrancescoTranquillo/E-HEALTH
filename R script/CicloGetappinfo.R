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
len<-length(dc)
cate<-NULL
 for (i in 1:len){
url<- dc[i]
page<-read_html(url)
category<-page%>%
  html_node(".large-6 .link")%>%                         
  html_text(trim=TRUE)
cate<-c(cate,category)
}

