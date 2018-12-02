rm(list = ls())

library(rvest)
library(tidyverse)
library(pbapply)
library(reshape2)
library(ggplot2)
library(data.table)
library(plyr) 

#leggo l'output di metamap
metaout <- read_html("Across_out.xml")

candidate_preferred <- metaout %>%
  html_nodes("candidatepreferred") %>%
  html_text(trim = F)

df <- tibble(
  "Candidate_Preferred" = candidate_preferred
)

df1 <- count(df, "Candidate_Preferred")

df1_sub<-df1 %>% filter(freq >= 10)

write.csv2(df1_sub) #da sistemare per prelevare solo colonna con i preferred o meno
