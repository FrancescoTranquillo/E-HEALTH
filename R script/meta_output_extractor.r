rm(list = ls())

library(rvest)
library(tidyverse)
library(pbapply)
library(reshape2)
library(ggplot2)
library(data.table)
#leggo l'output di metamap
metaout <- read_html("25_FRA_out.xml")

#lo suddivido per le singole app
mmos <- metaout %>%
  html_nodes("mmo")

mappings <- mmos[2] %>%
  html_nodes("mapping")

#scrivo la funzione che estrae i dati di interesse per la singola app
extract_output <- function(x) {
  candidate_score <- x %>%
    html_nodes("candidatescore") %>%
    html_text(trim = F)
  
  candidate_cui <- x %>%
    html_nodes("candidatecui") %>%
    html_text(trim = F)
  
  candidate_matched <- x %>%
    html_nodes("candidatematched") %>%
    html_text(trim = F)
  
  candidate_preferred <- x %>%
    html_nodes("candidatepreferred") %>%
    html_text(trim = F)
  
  matched_word <- x %>%
    html_nodes("matchedword") %>%
    html_text(trim = F)
  
  df <- tibble(
    "Candidate Score" = candidate_score,
    "Candidate CUI" = candidate_cui,
    "Candidate Matched" = candidate_matched,
    "Candidate Preferred" = candidate_preferred
  )
  
  
}

#applico la funzione a tutte le app
metamap_output <- pblapply(mmos, extract_output)
head(metamap_output)

#carico il file mesh e lo preparo togliendo duplicati e colonne inutili
mesh <- read.csv2("mesh.csv", header = T, stringsAsFactors = F)
names(mesh)[1] <- "specialty"
mesh <- mesh[, 1:2]
mesh <- mesh[!duplicated(mesh),]

#scrivo la funzione che prende in ingresso un singolo MMO di metamap
# (ogni MMO è il risultato dell'analisi di una descrizione) e in uscita riporta il
#numero di parole che sono presenti nel file mesh, includendo la corrispondente specialità
#medica
add_specialty <- function(df) {
  terms_list <- as.list(df[[4]])
  
  result <-
    lapply(terms_list, function(x)
      as.tibble(filter(mesh, terms == x)[1])) %>%
    lapply(., function(df)
      if (dim(df)[1] == 0)
        df[1, 1] <- NA
      else
        df)
  
  
  result <-
    mapply(cbind,
           result,
           "Candidate Preferred" = terms_list,
           SIMPLIFY = F)
  
  result <- lapply(result, as.tibble) %>%
    lapply(., setNames, c("Specialty", "Candidate Preferred"))
  
  
  result <- result %>% do.call("rbind", .)
  #matching<-(nrow(result)/length(terms_list))*100
  
  t_specialty <- cbind(df, result) %>%
    .[, 1:5]
  
  
  
  # a<-as.list(t_specialty)
  return(t_specialty)
}

#applico la funzione all'output di metamap
a <- pblapply(metamap_output, add_specialty)







#  TEST
# mydf <- count(a[[18]], specialty)
# head(mydf)
#
# mydf.molten <-
#   melt(
#     mydf,
#     id.vars = "specialty",
#     measure.vars = "n"
#     )
# head(mydf.molten)
#
# g<-ggplot(mydf.molten,aes(specialty,value))+
#   geom_col()+
#
# g
