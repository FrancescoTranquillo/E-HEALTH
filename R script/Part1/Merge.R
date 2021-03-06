

#Librerie

library(tidyverse)
library(rvest)
library(stringr)
library(rebus)
library(urltools)
library(gdata)
library(anytime)
library(dplyr)
library(lubridate)

#Inizializzo Variabili

df1 <- NULL
df2 <- NULL
df3 <- NULL
SoloDuplicati <- NULL
SoloUnici <- NULL
Mertab <- NULL

#Creo 2 DataFrame dalle 2 tabelle Medical e Health&Fitness

df1 <- read.csv2("HF.csv", stringsAsFactors = F)
df2 <- read.csv2("M.csv", stringsAsFactors = F)

#elimino duplicati iniziali

df1 <- unique(df1)
df2 <- unique(df2)

#Creo DataFrame unito

df3 <- rbind(df1, df2)

#Cerco duplicati usando l'ID

SoloDuplicati <- df3[duplicated(df3$ID), ]
SoloDuplicati[, 4] <-
  "Both"               #Sostituisco categoria con "Both", il numero della colonna considera le tabelle senza prima colonna con numero

SoloUnici <-
  df3[!(duplicated(df3$ID) | duplicated(df3$ID, fromLast = TRUE)),]

Mertab <- rbind(SoloUnici, SoloDuplicati)

#Ordino alfabeticamente le righe del DF (Mette prima simboli e numeri)

Mertab = Mertab[order(Mertab$Name), ]
Mertab <- Mertab[!duplicated(Mertab), ]
#Creo file CSV
sum(which(duplicated(Mertab) == T))
write.csv2(Mertab, "Merged_db.csv", row.names = FALSE)
