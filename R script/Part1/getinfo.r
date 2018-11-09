rm(list = ls())
# 1: Caricamento librerie ####

library(tidyverse)

library(rvest)

library(stringr)

library(rebus)

library(urltools)

library(progress)
#1: Inizializzazione del dataset



#2: Cicli for ####

#creazione del vettore delle lettere
letters <- c(LETTERS, "*")
lenletters <- length(letters)

filenames <- c("M.csv", "HF.csv")

medical <-
  "https://itunes.apple.com/us/genre/ios-medical/id6020?mt=8&letter="
hf <-
  "https://itunes.apple.com/us/genre/ios-health-fitness/id6013?mt=8&letter="

pagine<- c(medical, hf)
categories <- c("Medical", "Health&Fitness")



for (k in 1:2) {
  d = NULL
  print(k)
  urlin <- pagine[k]
  cat <- categories[k]
  source("maxpagenumber.r")
  print(maxpagenumber)
  
  #primo for: questo cicla tra tutte le lettere
  for (i in 1:lenletters) {
    
    letter <- letters[i]
    
    #secondo for: questo cicla da 1 al massimo numero
    #di pagine proprio di ogni lettera
    for (j in 1:maxpagenumber[i]) {
      
      urlr <- paste(urlin, letter, "&page=", j, "#page", sep = "")
      
      print(paste("lettera:", letter, "pagina:", j))
      
      page <- read_html(urlr)
      
      #3: Estrazione url delle app ####
      urls <- page %>%
        rvest::html_nodes("#selectedcontent li") %>%
        rvest::html_nodes("a") %>%
        rvest::html_attr("href")
      
      if(length(urls)=="0"){
        urls<-NA
        nomi<-NA
        id<-NA
      } else{
      #4: Estrazione nomi delle app ####
      nomi <- page %>%
        rvest::html_nodes("#selectedcontent a") %>%
        rvest::html_text()
      
      #5 Estrazione ID delle app ####
      IDs1 <- gsub("(?<=)(.*)(id)", "", urls, perl = TRUE)
      IDs2 <- gsub("(?<=[id])(\\d+)", "", IDs1, perl = TRUE)
      id <- gsub("\\?mt=8", "", IDs2)
      
      }
      
      #6: Costruzione del dataset ####
      d <- rbind(d, data.frame(Name= nomi, URL= urls, ID=id, Category= cat))
   
      
      
      

    } 
    
  }
  

  
  
  write.csv2(d, filenames[k], row.names = FALSE)
  
}
