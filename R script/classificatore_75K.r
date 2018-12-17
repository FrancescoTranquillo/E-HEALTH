rm(list = ls())

library(rvest)
library(tidyverse)
library(pbapply)
library(reshape2)
library(ggplot2)
library(data.table)


#leggo l'output di metamap
metaout <- read.table("./Tabelle to be consegnate/Results/75KDescription_ID_Part2_results.txt",sep = "|",fill = T,
                      row.names=NULL,header = F)

metaout_ID_Candidate<-metaout[,c(1,4)]
metaout_ID_Candidate$V4<-as.character(metaout_ID_Candidate$V4)

metamap_output<-split(tibble("Candidate Preferred"=metaout_ID_Candidate$V4),metaout_ID_Candidate$V1)

head(metamap_output)


#carico il file mesh e lo preparo togliendo duplicati e colonne inutili
mesh <- read.csv2("mesh.csv", header = T, stringsAsFactors = F)
names(mesh)[1] <- "specialty"
mesh <- mesh[, 1:2]
mesh <- mesh[!duplicated(mesh),]
mesh <- mesh[!apply(is.na(mesh) | mesh == "", 1, all),]
mesh_specialty <- subset(mesh, specialty != "Across")
#mesh_across <- subset(mesh, specialty == "Across")


#scrivo la funzione che prende in ingresso un singolo MMO di metamap
# (ogni MMO è il risultato dell'analisi di una descrizione) e in uscita riporta il
#numero di parole che sono presenti nel file mesh, includendo la corrispondente specialità
#medica

add_specialty <- function(df) {
  #crea lista dei preferred
  terms_list <- as.list(df$`Candidate Preferred`) 

  
  result <-lapply(terms_list, function(x)
      as.tibble(filter(mesh_specialty, terms == x)[1])) %>%
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
    lapply(., setNames, c("specialty", "Candidate Preferred"))
  
  
  result <- result %>%
    do.call("rbind", .)
  

  return(result)
}

#applico la funzione all'output di metamap
a <- pblapply(metamap_output, add_specialty)


#estrazione delle (max 2) specialità mediche
#funzione da applicare ad ogni elemento della lista a, ovvero una funzione
#da applicare ad un dataframe

classifier <- function(a.df) {
  count(a.df, specialty, sort = T)
}


classified<-lapply(a,classifier)
  
head(classified)
classified_na<-sapply(classified, function(x)
    if(!all(is.na(x$specialty))){
      x<-drop_na(x)
    }
    )
head(classified_na)
top3 <- classified_na %>%
  lapply(., function(x)
    if (length(x$specialty)==0){
      as.tibble(x$specialty<-c(NA,NA,NA))
    }
    else
      x)

top3 <- top3 %>%
    lapply(., function(x)
    
      if (dim(x)[1] >= 3)
        x[1:3, 1]
      else
        x[, 1])%>%
lapply(., transpose)



top3_tab <- top3 %>% rbindlist(., fill = T)


table<-read.csv2("./Tabelle to be consegnate/File_Meta_75K/75K_Part2.csv",header = T, stringsAsFactors = F)
table<-table[1:420,]


result<-cbind(table, top3_tab)

write.csv2(result, "./Tabelle to be consegnate/75K6part2_results.csv", row.names = F)
#  TEST (da ignorare)

#  mydf <- count(a[[18]], Specialty)
# head(mydf)
#
# mydf.molten <-
#   melt(
#     mydf,
#     id.vars = "Specialty",
#     measure.vars = "n"
#     )
# head(mydf.molten)
#
# g<-ggplot(mydf.molten,aes(Specialty,value))+
#   geom_col()
#

# g


