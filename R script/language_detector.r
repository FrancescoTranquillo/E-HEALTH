library(rvest)
library(textcat)
library(dplyr)
library(tictoc)
library(curl)
library(tidyverse)
library(progress)

df<-read.csv2("app_M_H&F.csv", stringsAsFactors=FALSE)
anyDuplicated(df)
df_subset<-select(df, URL)

df_subset_lang <- df_subset %>%
  rowwise() %>%
  mutate(languages = possibly(~.x %>% 
                                read_html() %>%
                                html_nodes(".section__description .we-clamp__contents") %>%
                                html_text(trim=TRUE) %>%
                                textcat(),
                                
                          NA)(URL))


df_english_only<-df[which(language=="english"),]


