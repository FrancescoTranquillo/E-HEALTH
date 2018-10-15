rm(list = ls()) #pulisce l'enviroment di tutte le variabili create

# 1 Librerie ####
library(tidyverse)
library(rvest)
library(stringr)
library(rebus)
library(urltools)
library(gdata)
library(anytime)
library(dplyr)
library(lubridate)
library(progress)
library(httr)
library(jsonlite)
# 2 Inizializzazione attributi ####
avgrating = NULL       #1
ratings = NULL         #2
pegi = NULL            #3
category = NULL        #4
description = NULL     #5
lastupdate <- NULL     #6
version <- NULL        #7
devname <- NULL        #8
price <- NULL          #9
currency <- NULL
appid <- NULL          #10

appname <- NULL        #11
appurl <- NULL         #12
lang <- NULL           #13
urldev <- NULL         #14
size <- NULL           #15
keywords <- NULL       #16
iddev <- NULL          #17
releasedate <- NULL    #18
today <- NULL
avg_ratings_currentversion <- NULL
number_ratings_currentversion <- NULL
avg_ratings_all <- NULL
number_ratings_all <- NULL

fivestar <- NULL       #22 % 5star
fourstar <- NULL       #23 % 4star
threestar <- NULL      #24 % 3star (integer)
twostar <- NULL        #25 % 2star
onestar <- NULL        #26 % 1star
#27 Date RETRIEVED  ----->gi? fatto?

#3 Lettura app inglesi ####
df <- read.csv2("M.csv", stringsAsFactors = FALSE)
dc <-
  df[, 2]  ##prendo seconda colonna dove ci sono gli url                                                             ##prendo seconda colonna dove ci sono gli url
d2 <- df[, 3]
d3 <- df[, 1]
d44 <- df[, 4]
len <-
  length(dc)                                                          ##prendo lunghezza in len sar? la fine del ciclo quando ci decidiamo con

#4 Estrazione dei 27 attributi ####
rows <- 1:2000
# p <- progress_estimated(rows)
tictoc::tic()
for (i in rows) {
  ###### METTO SOLO 4 PERCH? SE METTESSI "len" (DI RIGA 23) CIAONE, CI METTEREBBE 6 GIORNI E NONCCCCI?VOGLIA
  url <-
    dc[i]                             ######url prende l'iesima riga di dc che sarebbe la seconda colonna fatta da getinfo.r dove ci sono gli url e la apro in page
  
  possibleError <- tryCatch(
    read_html(url),
    error = function(e)
      e
  )
  
  if (inherits(possibleError, "error")) {
    next
  } else {
    page <- possibleError
  }
  
  ## CATEGORY
  category1 <- d44[i]
  category <- c(category, category1)
  
  ## Keywords (HO BESTEMMIATO MALE) ####
  keywords1 <- page %>%
    html_nodes('meta') %>%
    html_attr('content')
  keywords1 <- keywords1[8]
  keywords <- c(keywords, keywords1)
  
  ## Lingue ####
  lang1 <- page %>%
    html_nodes(
      ".l-row:nth-child(8) .large-6 , .l-row:nth-child(7) .large-6 , .l-row:nth-child(6) .large-6 , .large-6 .we-clamp__contents , .l-row:nth-child(2) .large-6 , .large-6 .link , .information-list__item:nth-child(1) .large-6"
    ) %>%
    html_text(trim = TRUE)
  lang1 <- lang1[5]
  lang <- c(lang, lang1)
  
  
  ## Size ####
  patternnum <- "\\sGB|KB|MB"
  size1 <- page %>%
    html_nodes(
      ".l-row:nth-child(8) .large-6 , .l-row:nth-child(7) .large-6 , .l-row:nth-child(6) .large-6 , .large-6 .we-clamp__contents , .l-row:nth-child(2) .large-6 , .large-6 .link , .information-list__item:nth-child(1) .large-6"
    ) %>%
    html_text(trim = TRUE)
  size1 <- size1[2]
  
  num <- gsub(pattern = patternnum, "", size1)
  num <- as.double(num)
  if (grepl("GB", size1) == TRUE) {
    size1 <- num * 1024
  } else if (grepl("KB|kB", size1) == TRUE) {
    size1 <- num / 1024
  } else if (grepl("MB|mB", size1) == TRUE) {
    size1 <- num
  }
  size <- c(size, size1)
  
  
  ## Ratings per star (INTEGER)####
  ratingsxstar <- page %>%
    html_nodes(".we-star-bar-graph .we-star-bar-graph__bar__foreground-bar") %>%
    html_attr("style") %>%
    str_extract_all(., "\\d+") %>%
    as.numeric(.)
  
  if (length(ratingsxstar) == "0") {
    ratingsxstar <- NA
  }
  
  onestar <- c(onestar, last(ratingsxstar))
  twostar <- c(twostar, ratingsxstar[2])
  threestar <- c(threestar, ratingsxstar[3])
  fourstar <- c(fourstar, ratingsxstar[4])
  fivestar <- c(fivestar, first(ratingsxstar))
  
  
  
  ##Url dev ####
  urldev1 <- page %>%
    html_node(".app-header__identity") %>%      ##non ? un numero ? una stringa, va beneeeeeee???
    html_nodes(".link") %>%
    html_attr("href")
  urldev <- c(urldev, urldev1)
  
  
  ##ID dev ####
  IDs1 <- gsub("(?<=)(.*)(id)", "", urldev1, perl = TRUE)
  IDs2 <- gsub("(?<=[id])(\\d+)", "", IDs1, perl = TRUE)
  iddev1 <- gsub("\\?mt=8", "", IDs2)
  iddev <- c(iddev, iddev1)
  
  
  ##Appid da tabella easy ####
  appid1 <- d2[i]
  appid <- c(appid, appid1)
  
  ##Appname ####
  appname1 <- d3[i]
  appname <- c(appname, appname1)
  
  #Appurl ####
  appurl1 <- dc[i]
  appurl <- c(appurl, appurl1)
  
  #Ratings e punteggi, versione corrente e totale ####
  urlID <-
    paste("https://itunes.apple.com/lookup?id=",
          as.character(d2[i]),
          sep = "")
  
  urlapp <- GET(url = urlID)
  
  cont <- fromJSON(txt = content(urlapp))
  
  cont_results <- cont[["results"]]
  
  #current version
  avg_ratings_currentversion1 <-
    cont_results$averageUserRatingForCurrentVersion
  l1 <- length(avg_ratings_currentversion1)
  if (l1 == 0)
    avg_ratings_currentversion1 <- NA
  
  avg_ratings_currentversion <-
    c(avg_ratings_currentversion, avg_ratings_currentversion1)
  
  number_ratings_currentversion1 <-
    cont_results$userRatingCountForCurrentVersion
  l2 <- length(number_ratings_currentversion1)
  if (l2 == 0)
    number_ratings_currentversion1 <- NA
  
  number_ratings_currentversion <-
    c(number_ratings_currentversion,
      number_ratings_currentversion1)
  
  #all versions
  avg_ratings_all1 <- cont_results$averageUserRating
  l3 <- length(avg_ratings_all1)
  if (l3 == 0)
    avg_ratings_all1 <- NA
  
  avg_ratings_all <- c(avg_ratings_all, avg_ratings_all1)
  
  number_ratings_all1 <- cont_results$userRatingCount
  l4 <- length(number_ratings_all1)
  if (l4 == 0)
    number_ratings_all1 <- NA
  
  number_ratings_all <- c(number_ratings_all, number_ratings_all1)
  
  
  # #Punteggio medio (DOUBLE)####
  # avgrating1<-page%>%
  #   html_nodes(".we-customer-ratings__averages__display")%>%
  #   html_text(trim = TRUE)%>%
  #   as.numeric(.)
  # if(length(avgrating1)=="0"){
  #   avgrating1<-NA
  # }
  #
  # avgrating<-c(avgrating,avgrating1)                           ######allora qui per non sovrascrivere come avevamo fatto in matlab lo richiamo cos?
  #
  # #Punteggio totale (DOUBLE)####
  # ratings1<-page%>%
  #   html_node(".we-customer-ratings__count")%>%
  #   html_text(trim = TRUE)%>%
  #   gsub(" Ratings", "", .)
  # ratings1<-as.numeric(sub("K", "e3", ratings1))
  # if(length(ratings1)=="0"){
  #   ratings1<-NA
  # }
  # ratings<-c(ratings,ratings1)                                ##qui mi infama mi sa il codice dopo da problemi nella costruzione della tabella
  
  #PEGI (DOUBLE)####
  
  pegipattern <- "\\d+"
  
  pegi1 <- page %>%
    html_node(".l-row:nth-child(6) .large-6") %>%
    html_text(trim = TRUE) %>%
    str_extract(., pattern = pegipattern) %>%
    as.numeric()
  pegi <- c(pegi, pegi1)
  
  
  # TOLTO QUINDI LASCIO IN COMMENTO Categoria (STRINGA)####
  
  #category1<-page%>%
  # html_node(".large-6 .link")%>%
  #html_text(trim=TRUE)
  #category<-c(category,category1)
  
  #Descrizione (STRINGA)####
  
  description1 <- page %>%
    html_node(".section__description .we-clamp__contents") %>%
    html_text(trim = TRUE)
  description <- c(description, description1)
  
  
  #Data ultimo update e release date (DATA)####
  
  lastupdate1 <- page %>%
    html_nodes(".version-history__item__release-date") %>%
    html_text(trim = TRUE) %>%
    gsub(",", "", .)
  
  if (length(lastupdate1) == "0") {
    lastupdate1 <- NA
  }
  
  lastupdate <- c(lastupdate, first(lastupdate1))
  releasedate <- c(releasedate, last(lastupdate1))
  
  #Ultima versione app (STRINGA) ####
  
  version1 <- page %>%
    html_node(".version-history__item__version-number") %>%         ##non ? un numero ? una stringa, va beneeeeeee??? non
    html_text()
  version <- c(version, version1)
  
  #Developer (STRINGA)####
  
  devname1 <- page %>%
    html_node(".information-list__item:nth-child(1) .large-6") %>%
    html_text(trim = TRUE)
  devname <- c(devname, devname1)
  
  #Prezzo (DOUBLE) e valuta (STRINGA) ####
  
  currencypattern <- "^\\W"
  price1 <- page %>%
    html_node(".inline-list__item--bulleted:nth-child(1)") %>%
    html_text(trim = TRUE)
  if (length(price1) == "0") {
    price1 <- NA
  }
  
  if (price1 == "Free") {
    price1 <- 0
    currency1 <- 0                        ##currency=valuta
  } else {
    currency1 <- str_extract(price1, currencypattern)
    price1 <- gsub(currencypattern, "", price1)
    price1 <- as.numeric(price1)
  }
  price <- c(price, price1)
  currency <- c(currency, currency1)
  
  ##DATA
  today1 <- Sys.Date()
  today <- c(today, today1)
  
  # print(p$tick())
}
#5 Creazione database ####
data <- tibble(
  "App ID" = appid,
  "App Name" = appname,
  "App URL" = appurl,
  "App description" = description,
  "Keywords" = keywords,
  "Version" = version,
  "Age rating" = pegi,
  "Language(s)" = lang,
  "Developer ID" = iddev,
  "Developer Name" = devname,
  "Developer Website" = urldev,
  "Price" = price,
  "Currency" = currency,
  "Size" = size,
  "Last update date" = anydate(lastupdate),
  "category" = category,
  "Release Date" = anydate(releasedate),
  "Average user ratings-current" = avg_ratings_currentversion,
  "Number of user ratings-current" = number_ratings_currentversion,
  "Average user ratings-all" = avg_ratings_all,
  "Number of user ratings-all" = number_ratings_all,
  "% of user ratings with 5 stars" = fivestar,
  "% of user ratings with 4 stars" = fourstar,
  "% of user ratings with 3 stars" = threestar,
  "% of user ratings with 2 stars" = twostar,
  "% of user ratings with 1 star" = onestar,
  "Date" = anydate(today)
)
tictoc::toc()
#6 Scrittura file ####
#write.csv2(data, "App_DatabaseProva.csv", row.names = FALSE)
