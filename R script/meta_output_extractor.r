library(rvest)
library(tidyverse)


metaout<-read_html("metamap_output_test.xml")

candidate_score<-metaout%>%
  html_nodes("candidatescore")%>%
  html_text(trim=F)

candidate_cui<-metaout%>%
  html_nodes("candidatecui")%>%
  html_text(trim=F)

candidate_matched<-metaout%>%
  html_nodes("candidatematched")%>%
  html_text(trim=F)

candidate_preferred<-metaout%>%
  html_nodes("candidatepreferred")%>%
  html_text(trim=F)

semantic_type<-metaout%>%
  html_nodes("semtype")%>%
  html_text(trim=F)

mapping_score<-metaout%>%
  html_nodes("mappingscore")%>%
  html_text(trim=T)

df<-tibble("Mapping Score"=mapping_score,
           "Candidate Score" =candidate_score,
           "Candidate CUI" = candidate_cui,
           "Candidate Matched" = candidate_matched,
           "Candidate Preferred" = candidate_preferred,
           "Semantic Type" = semantic_type)
