rm(list = ls())
#1 eliminazione caratteri non ASCII
library(dplyr)
library(pbapply)
library(parallel)
library(tictoc)
library(textcat)

if (!file.exists("Database_preprocessing.csv")){
  message("Creazione del database in corso...")
  source("db_25_attributes.r")
  message("Database correttamente creato")
} 
message("Inizializzazione preprocessing...")
Sys.sleep(2)
df<- read.csv2("Database_preprocessing.csv",stringsAsFactors = FALSE)#Apri tabella finale prendi quinta colonna con descrizioni ed elimina non-ASCII
descr<- as.list(df[,5])

message("Fase 1:")
message("Rimozione caratteri non ASCII dalle descrizioni delle applicazioni...")
descrAscii<-pblapply(descr, function(x) gsub('([^\x20-\x7E])', '', x))%>%
  do.call("rbind",.)
message("Riconversione descrizioni...")
df[,5]<- descrAscii
message("Eliminazione completata")

language.detector<-function(description){
  textcat(description)
}

message("Fase 2:")
message("identificazione delle lingue")
Sys.sleep(2)
message("identificazione numero di cores logici...")
cl <- makeCluster(detectCores() - 1)
message(length(cl)," cores trovati, avvio parallelizzazione...")
clusterEvalQ(cl, {
  library(dplyr)
  library(pbapply)
  library(textcat)
  library(parallel)
  library(tictoc)
})

clusterExport(cl,
              c("descr", "language.detector"))
message("riconoscimento lingue avviato...")

lingue_identificate<-pblapply(descr, language.detector,cl = cl)

stopCluster(cl)

message("Lingue identificate, ricostruzione database in corso...")

lingue_identificate%>%
  do.call("rbind",.)

df$lang<-lingue_identificate

message("Estrazione App con descrizione in inglese...")
df_english_only<-df[which(df$lang=="english"),1:25]

message("Scrittura database finale")
write.csv2(df_english_only,"Database_preprocessed_english.csv", row.names = FALSE)

message("Preprocessing completato")
