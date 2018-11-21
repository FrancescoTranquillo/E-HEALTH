library(rvest)
library(textcat)
library(dplyr)
library(tictoc)
library(curl)
library(tidyverse)
library(progress)

# 1: Lettura tabella creata da getinfo.r ####
df<-read.csv2("app_M_H&F.csv", stringsAsFactors=FALSE)

# 2: Creazione subset #####
# di 3000 app prese casualmente dal data frame appena creato,per comodità prendo solo la colonna URL. 
df_subset<-select(df, URL)

df_subset<-df_subset[sample(nrow(df_subset), "3000"), ]

# 3: Detezione lingua ####

# prima di far partire queste righe, assicurarsi di cambiare il "3000" scritto a riga 16!!!!!
# L'algoritmo mutate (riga 26) aggiunge una colonna al dataset che prende in ingresso (df_subset),
# seguendo le istruzioni scritte al suo interno. Nello specifico, da riga 28 a 31 è chiesto di estrarre
# ogni url (cioè ogni riga del subset), leggerlo (riga 29), estrarre il testo corrispondente alla descrizione
# dell'app (riga 31-32) e identificare la lingua della descrizione (33).

# i comandi "possibly" e riga 37 sono stati aggiunti perchè alcuni url, quando letti, chiedevano di
# aprire itunes, cosa che bloccava l'algoritmo. In corrispondenza di questi url "difettosi", l'algoritmo
# "mutate" aggiunge il valore NA.

# i comandi tic e toc (32, 42) servono a stampare a video quanti secondi passano per eseguire le righe che
# li separano. Aggiunti in fase di scrittura preliminare, possono essere cancellati.

# vista la natura di mutate (un for mascherato), il numero di righe (definite a riga 16) influenza notevolmente
# il tempo impiegato per completare questo algoritmo. Con 3000 righe, si impiegano circa 45 minuti (!). 


#       Chiedere a Caiani se 3000 app bastano.


  df_subset<-tibble(URL=df_subset)
  tic()
  df_subset_lang <- df_subset %>%
    rowwise() %>%
    mutate(languages = possibly(~.x %>% 
                                read_html() %>%
                                html_nodes(".section__description .we-clamp__contents") %>%
                                html_text(trim=TRUE) %>%
                                textcat(),
                                
                          NA)(URL))
  toc()
  
  
#4: Eliminazione duplicati ####

# L'inclusione del df iniziale genera (non ho capito il motivo ma non ho indagato) un numero
# variabile di duplicati. Le righe successive ricostruiscono il dataset eliminando i duplicati

if(anyDuplicated(df_subset_lang)>0){
  df_subset_lang <- df_subset_lang[!duplicated(paste(df_subset_lang$URL, df_subset_lang$languages)),]
}

#5: Selezione app inglesi ####
# I comandi "attach" e "detach" servono ad "incollare" in memoria il dataset
# e poter accedere alle sue variabili senza chiamarle con il comando $ (una chiamata normale sarebbe scritta così:
#         
#         dataset$variabile
#  )

# la riga 75 seleziona (come un select in sql) solo le righe la cui variabile "languages" ha valore
# "english" e crea un nuovo dataset chiamato df_english_only
attach(df_subset_lang)
df_english_only<-df_subset_lang[which(languages=="english"),]
detach()

#6: Creazione dataset inglese ####

# Unione dataset iniziale con quello di app inglesi tramite inner join
#(creo un nuovo dataset (df_app) mantenendo tutte le variabili ma unisco in base al valore dal parametro "by")

df_app<-inner_join(df, df_english_only, by="URL")

# uguale a riga 61, per sicurezza ho eliminato gli eventuali duplicati (ma a questo punto non dovrebbero esserci
# quindi volendo le righe 85-87 possono essere cancellate)
if(anyDuplicated(df_app)>0){
  df_app <- df_app[!duplicated(paste(df_app$Name,df_app$URL, df_app$ID, df_app$languages)),]
}



#7: Scrittura nuove tabelle ####

# Le prime due sono solo create per evitare di dover rilanciare la fase 3 (riga 18),
# quella importante è quella di riga 99.
write.csv2(df_subset_lang, "subset_M_H&F_ALL_LANGUAGES.csv",row.names=FALSE)

write.csv2(df_english_only, "subset_M_H&F_ENGLISH_ONLY.csv",row.names=FALSE)

write.csv2(df_app, "app_M_H&F_ENGLISH_ONLY.csv",row.names=FALSE)

