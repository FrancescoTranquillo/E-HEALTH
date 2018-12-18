rm(list = ls())


library(tidyverse)
library(pbapply)
library(reshape2)
library(ggplot2)
library(data.table)

#leggo l'output di metamap
metaout1 <-fread("./Tabelle to be consegnate/File_Meta_75K/75KDescription_ID_Part4_results.txt",
                 data.table = F,
                 fill=T,
                 sep = "|")

metaout2 <-fread("./Tabelle to be consegnate/File_Meta_75K/75KDescription_ID_Part4_b_results.txt",
                 data.table = F,
                 fill=T,
                 sep = "|")
# metaout3 <-fread("./Tabelle to be consegnate/File_Meta_75K/75KDescription_ID_Part5_C.txt.out",
#                  data.table = F,
#                  fill=T,
#                  sep = "|")

metaout2$V1<-factor(metaout2$V1)
metaout1$V1<-factor(metaout1$V1)
metaout3$V1<-factor(metaout3$V1)
# metaout1 <- read.table("./Tabelle to be consegnate/File_Meta_75K/75KDescription_ID_Part1_A.txt",sep = "|",fill = T,
#                       row.names=NULL,header = F)
# metaout2 <- read.table("./Tabelle to be consegnate/File_Meta_75K/75KDescription_ID_Part1_B.txt",sep = "|",fill = T,
#                        row.names=NULL,header = F)

metaout<- rbind(metaout1, metaout2)
metaout_ID_Candidate<-metaout[,c(1,4)]
metaout_ID_Candidate$V4<-as.character(metaout_ID_Candidate$V4)

dt <- data.table(metaout_ID_Candidate)
dt[, grp := .GRP, by = V1]
setkey(dt, grp)
metamap_output <- dt[, list(list(.SD)), by = grp]$V1

#metamap_output<-split(tibble("Candidate Preferred"=metaout_ID_Candidate$V4),metaout_ID_Candidate$V1, )




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
  terms_list <- as.list(df$`V4`) 

  
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


classified<-pblapply(a,classifier)
  
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


table<-read.csv2("./Tabelle to be consegnate/File_Meta_75K/75K_Part4.csv",header = T, stringsAsFactors = F)
table$ID<-as.character(table$ID)

f1 <- first(metaout1$V1)
l1 <- last(metaout1$V1)
f2 <- first(metaout2$V1)
l2 <- last(metaout2$V1)

rf1<- which(table$ID==f1)
rl1<- which(table$ID==l1)
rf2<- which(table$ID==f2)
rl2<- which(table$ID==l2)

table1<-table[rf1:rl1,]
table2<-table[rf2:rl2,]

table<-rbind(table1,table2)

x<-table$ID
y<-as.character(as.numeric(levels(metaout$V1))[metaout$V1])
z<-setdiff(x,y)

table<-table[-(which(table$ID==z)),]

result<-cbind(table, top3_tab)

write.csv2(result, "./Tabelle to be consegnate/Results/75K_Part4_results.csv", row.names = F)


