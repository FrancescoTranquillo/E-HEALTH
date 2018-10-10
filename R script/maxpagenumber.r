#1: Caricamento librerie####

# General-purpose data wrangling
library(tidyverse)

# Parsing of HTML/XML files
library(rvest)

# String manipulation
library(stringr)

# Verbose regular expressions
library(rebus)

#URL parser
library(urltools)

#2: Inizializzazione#####

#creazione vettore delle lettere (da A a *, per le app che nel nome iniziano con un simbolo )
letters <- c(LETTERS, "*")
lenletters <- length(letters)



#si inizializza a 0 il vettore che "guarda" il numero massimo di pagine e a 0 il numero massimo
#di pagine per tutte le lettere
nmaxpage <- 0
maxpagenumber <- 0

for (i in 1:lenletters) {
  maxpage<-NULL
  nmaxpage <- 0
  
  #in questa prima parte si imposta la lettera di cui si vuole sapere il massimo numero di pagine
  #presenti
  letter <- letters[i]
  

  
  
  
  #viene incollato l'url che considera come lettera quella impostata a riga 22
  url <-
    paste(
      urlin,
      #"https://itunes.apple.com/us/genre/ios-medical/id6020?mt=8&letter=",
      letter,
      "&page=1#page",
      sep = ""
    )
  
  #si utilizza il pacchetto "rvest" per far leggere l'html a R
  page <- read_html(url)
  
  #In queste righe viene detto a R di creare un vettore chiamato lista1 che contiene
  #(in formato testo, quindi carattere) la lista dei numeri che si
  #visualizzano nella prima pagina di una lettera
  lista1 <- page %>%
    rvest::html_nodes(".alpha+ .paginate") %>%
    html_text()
  
  if(length(lista1)=="0"){
    maxpagenumber[i]<-1
    next
  }else{
    
  #Con la parte precedente, in formato carattere si è presa anche la stringa "Next"
  #il comando serve a verificare che questa stringa sia effettivamente
  #presente nel vettore lista1
  
  #il pezzo successivo fixa il problema delle lettere con 1 o 2
  #pagine, nelle quali è subito assente la stringa "Next"
  if (grepl("Next", lista1) == FALSE) {
    maxpage <- gsub("[A-z]", "", lista1, perl = TRUE)
    nchar <- as.numeric(nchar(maxpage))
    if (nchar >= 10) {
      maxpage <- substr(maxpage, nchar(maxpage) - 1, nchar(maxpage))
      
    } else {
      maxpage <- substr(maxpage, nchar, nchar)
    }
    nmaxpage <- c(nmaxpage, as.numeric(maxpage))
    nmaxpage
    if (last(nmaxpage) == nmaxpage[length(nmaxpage) - 1]) {
      print(" ultimi due numeri uguali")
      nmaxpage <- last(nmaxpage) + 1
    } else {
      nmaxpage <- last(nmaxpage)
    }
    
  }
  
  }  
  # 3: Ciclo while per identificazione del numero massimo delle pagine ####
  
  #la presenza della stringa "Next", indica che il numero massimo di pagine non è quello
  #visualizzato, ma, essendoci "Next", è possibile visualizzare una pagina successiva
  #in cui sono presenti altre app. Questa pagina, se non è più presente la
  #stringa "Next" è quindi l'ultima in cui è possibile trovare app.
  
  #il while verifica ogni volta la presenza/assenza della stringa "Next"
  #nel vettore lista1, che ad ogni ciclo viene aggiornato a seconda del numero massimo
  #identificato nella parte chiamata "Identificazione", spiegata sotto
  
  while (grepl("Next", lista1))
  {
    #controlli di ciclo, da ignorare
    print(paste("inizio ciclo: Next?", grepl("Next", lista1), sep = " "))
    
    ### Identificazione ###
    
    #nel vettore lista1, che è una stringa, tolgo tutte le lettere possibili,
    # così da avere solo una stringa fatta di soli numeri (che rimane comunque una stringa
    #di caratteri)
    maxpage <- gsub("[A-z]", "", lista1, perl = TRUE)
    
    #conto quanto è lunga questa stringa per capire se ci sono pagine da due cifre
    #aggiungo as.numeric per convertire da stringa a numero
    nchar <- as.numeric(nchar(maxpage))
    
    #se la lunghezza è maggiore di 10, allora vuol dire che le ultime due cifre sono il
    #massimo numero numero di pagine visualizzabili
    if (nchar >= 10) {
      #substr dice ad R di far diventare la variabile maxpage un numero composto dall'ultima
      #e dalla penultima cifra del vettore
      maxpage <- substr(maxpage, nchar(maxpage) - 1, nchar(maxpage))
    } else {
      #altrimenti, il massimo numero di pagine visualizzabili è composto da una sola cifra
      #tra 1 e 9
      maxpage <- substr(maxpage, nchar, nchar)
    }
    
    #ora quindi aggiorno il vettore inizializzato alla riga 27 con il numero maxpage trovato
    #nell'if precedente, AGGIUNGENDOLO in coda al vettore
    nmaxpage <- c(nmaxpage, as.numeric(maxpage))
    nmaxpage
    
    #controllo, da ignorare
    length(nmaxpage)
    last(nmaxpage)
    
    
    #questo if serve per capire se gli ultimi due numeri trovati nel vettore sono identici
    #se sono identici, vuol dire che l'utente, cliccando su "Next", visualizzerà la pagina
    #nmaxpage+1. Se non lo sono, vuol dire che cliccando sul massimo numero, vedrò una nuova
    #lista con altri possibili numeri
    if (last(nmaxpage) == nmaxpage[length(nmaxpage) - 1]) {
      print(" ultimi due numeri uguali")
      nmaxpage <- last(nmaxpage) + 1
    } else {
      nmaxpage <- last(nmaxpage)
    }
    
    #a questo punto ricostruisco l'url con nmaxpage, visto che era presente Next"
    
    #tratto l'url come un puzzle, separandolo in 2 parti:
    urlpart1 <-
      paste(
        urlin,
        #"https://itunes.apple.com/us/genre/ios-medical/id6020?mt=8&letter=",
        letter,
        "&page=",
        sep = ""
      )
    
    urlpart2 <- "#page"
    
    #che poi incollo usando paste in una sola stringa
    urlnew <- paste(urlpart1, nmaxpage, urlpart2, sep = "")
    
    #questo è l'url nuovo
    urlnew
    
    #a questo punto ripeto la lettura dell'url per vedere se in questa nuova pagina c'è ancora
    #la scritta "Next"
    page <- read_html(urlnew)
    
    lista1 <- page %>%
      rvest::html_nodes(".alpha+ .paginate") %>%
      html_text()
    
    #Ora controllo che in questo nuovo url ci sia ancora Next
    print(paste("fine ciclo: Next?", grepl("Next", lista1), sep = " "))
    
    #se è ancora presente il ciclo si ripete essendo verificata la condizione del while iniziale
    #se non è presente, ho trovato la massima pagina
  }
  
  
  #4: Risultato finale ####
  print(paste("Letter", letter, "contains:", nmaxpage, "pages", sep = " "))
  
  #5: Salvataggio valore nel vettore maxpagenumber
  maxpagenumber[i] <- c(nmaxpage)
  maxpagenumber
}
