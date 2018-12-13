library(rvest)
library(tidyverse)
library(pbapply)
library(reshape2)
library(ggplot2)
library(data.table)
library(plyr)
library(vegan)


# carico csv delle app dermatology con tutti gli attributi
df <-read.csv2("Database_preprocessed_english.csv" , stringsAsFactors = FALSE)
df_10 <- df[1:10,]

# normalizzo colonna Average.user.ratings
df$Average.user.rating <- decostand(df$Average.user.rating, "range", MARGIN=2, logbase = 2, na.rm=TRUE)

# normalizzo colonna Number.of.ratings
df$Number.of.user.ratings <- decostand(df$Number.of.user.ratings, "range", MARGIN=2, logbase = 2, na.rm=TRUE)

# cerco nella Description Medical Device, FDA ecc.. e mette TRUE se lo trova o FALSE se non lo trova
df$Medical_Device <- grepl("Medical Device|medical device|FDA", df$Description)
df$Medical_Device[df$Medical_Device == TRUE] <- 1
df$Medical_Device[df$Medical_Device == FALSE] <- 0

# cerco lunghezza descrizione, creo nuova colonna con lunghezza descrizione e normalizzo
x <- NULL
y <- NULL

len<- nrow(df)       #calcolo lunghezza

i<-0                 #conto caratteri descrizione 
for (i in 1:len){
    y <- c(df[i,5])
    char <- nchar(y)
    x <- c(x, char)
}

df$Description <- x #sostituisco alla descrizione il numero di caratteri
df$Description <- decostand(df$Description, "range", MARGIN=2, logbase = 2, na.rm=TRUE)  #normalizzo valori


#Calcolo actuality cioè differenza tra data di estrazione e "Release Date"
df_10$diff_in_days<- difftime(df_10$Date ,df_10$Last.Update.Date , units = c("days"))

#normalizzazione
df_10$diff_in_days <- decostand(df_10$diff_in_days, "range", MARGIN=2, logbase = 2, na.rm=TRUE)
df_10$diff_in_days <- 1-df_10$diff_in_days

#verificare se URL del developer ancora attivo 
library(rvest)
library(pbapply)

#definisci la funzione 
check_url<-function(url){
  
  possibleError <- tryCatch(
    
    read_html(url),
    error = function(e)
      e
  )
  
  if (inherits(possibleError, "error")) {
    check<-0
  }else
    check<-1
  return(check)
}

#esegui la funzione sugli url
df_10$urlcheck<-pbsapply(df_10$URL, check_url) %>%
mutate(urlcheck=check_url())


#differenza tra date
