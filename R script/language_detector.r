library(rvest)
library(textcat)
library(dplyr)
library(tictoc)
library(curl)
library(tidyverse)
library(progress)

df<-read.csv2("app_M_H&F.csv", stringsAsFactors=FALSE)
anyDuplicated(df)
df_subset<-select(df[90:110,], URL)

df_subset$URL


len<-length(df_subset$URL)
len



df_subset_lang <- df_subset %>%
  rowwise() %>%
  mutate(languages = possibly(~.x %>% 
                                read_html() %>%
                                html_nodes(".section__description .we-clamp__contents") %>%
                                html_text(trim=TRUE) %>%
                                textcat(),
                                
                          NA)(URL))



df_all_languages<-df%>%
  mutate(language=language)

df_english_only<-df[which(language=="english"),]

last(language)
