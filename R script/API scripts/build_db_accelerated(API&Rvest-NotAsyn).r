rm(list = ls()) #pulisce l'enviroment di tutte le variabili create

# 1 Librerie ####
library(tidyverse)
library(rvest)
library(stringr)
library(rebus)
library(urltools)
library(RCurl)
library(gdata)
library(anytime)
library(dplyr)
library(lubridate)
library(progress)
library(httr)
library(jsonlite)
library(purrrlyr)
library(parallel)
library(microbenchmark)
# ID <- 382952264

df <- read.csv2("M.csv", header = TRUE, stringsAsFactors = FALSE)


ID <- df[1, 3]
ID <- 520319166
firstgroup <- function(ID) {
  #print(ID)
  urlID <-
    paste("https://itunes.apple.com/lookup?id=", ID, sep = "")
  urlapp <- GET(url = urlID)
  
  
  cont <- fromJSON(txt = content(urlapp))
  appdata <- cont[["results"]]
  
  if (length(appdata) == 0) {
    attrs <- tibble(
      "Description" = NA,
      "Version" = NA,
      "Kwords" = NA,
      "Age rating" = NA,
      "Language(s)" = NA,
      "Developer ID" = NA,
      "Developer Name" = NA,
      "Developer URL" = NA,
      "Price" = NA,
      "Currency" = NA,
      "Size" = NA,
      "Last update date" =  NA,
      "Release Date" = NA,
      "Average user ratings-current" = NA,
      "Number of user ratings-current" = NA,
      "Average user ratings-all" = NA,
      "Number of user ratings-all" = NA,
      "% of user ratings with 5 stars" = NA,
      "% of user ratings with 4 stars" = NA,
      "% of user ratings with 3 stars" = NA,
      "% of user ratings with 2 stars" = NA,
      "% of user ratings with 1 star" = NA,
      "Date" = NA
      
    )
  } else {
    possibleError <- tryCatch(
      read_html(appdata$trackViewUrl),
      error = function(e)
        e
    )
    
    if (inherits(possibleError, "error")) {
      appkeywords <- NA
      onestar <- NA
      twostar <- NA
      threestar <- NA
      fourstar <- NA
      fivestar <- NA
    } else {
      page <- possibleError
      appkeywords <- page %>%
        html_nodes('meta') %>%
        html_attr('content') %>%
        .[8]
      
      #ratings per star #####
      appratingsxstar <- page %>%
        html_nodes(".we-star-bar-graph .we-star-bar-graph__bar__foreground-bar") %>%
        html_attr("style") %>%
        str_extract_all(., "\\d+") %>%
        as.numeric(.)
      
      if (length(appratingsxstar) == "0") {
        onestar <- NA
        twostar <- NA
        threestar <- NA
        fourstar <- NA
        fivestar <- NA
      } else {
        onestar <- last(appratingsxstar)
        twostar <- appratingsxstar[2]
        threestar <-  appratingsxstar[3]
        fourstar <- appratingsxstar[4]
        fivestar <- first(appratingsxstar)
      }
    }
    
    
    
    
    #
    appdesc <- appdata$description
    appversion <- appdata$version
    appagerating <- appdata$contentAdvisoryRating
    # 8. Language(s) ####
    applanguages <-
      paste(unlist(appdata$languageCodesISO2A), collapse = ",")
    
    # 9. Developer ID ####
    devid <- appdata$artistId
    
    # 10. Developer Name ####
    
    devname <- appdata$sellerName
    
    # 11. Developer URL ####
    devurl <- appdata$sellerUrl
    if (length(devurl) == 0)
      devurl <- NA
    # 12. Price ####
    appprice <- appdata$price
    
    #13. Currency ####
    appcurr <- appdata$currency
    
    #14. Size ####
    appsize <- as.numeric(appdata$fileSizeBytes) / 1e6
    
    #15. Last update date ####
    applastupdatedate <- appdata$currentVersionReleaseDate
    
    #17. Release date ####
    appreleasedate <- appdata$releaseDate
    
    #Ratings e punteggi, versione corrente e totale ####
    
    #current version
    appcurravgr <- appdata$averageUserRatingForCurrentVersion
    l1 <- length(appcurravgr)
    if (l1 == 0)
      appcurravgr <- NA
    
    appcurrnorating <- appdata$userRatingCountForCurrentVersion
    l2 <- length(appcurrnorating)
    if (l2 == 0)
      appcurrnorating <- NA
    
    #all versions
    appallavgr <- appdata$averageUserRating
    l3 <- length(appallavgr)
    if (l3 == 0)
      appallavgr <- NA
    
    appallnorating <- appdata$userRatingCount
    l4 <- length(appallnorating)
    if (l4 == 0)
      appallnorating <- NA
    
    
    
    
    
    attrs <- tibble(
      "Description" = appdesc,
      "Version" = appversion,
      "Kwords" = appkeywords,
      "Age rating" = appagerating,
      "Language(s)" = applanguages,
      "Developer ID" = devid,
      "Developer Name" = devname,
      "Developer URL" = devurl,
      "Price" = appprice,
      "Currency" = appcurr,
      "Size" = appsize,
      "Last update date" =  anytime::anydate(applastupdatedate),
      "Release Date" = anytime::anydate(appreleasedate),
      "Average user ratings-current" =  appcurravgr,
      "Number of user ratings-current" = appcurrnorating,
      "Average user ratings-all" = appallavgr,
      "Number of user ratings-all" = appallnorating,
      "% of user ratings with 5 stars" = fivestar,
      "% of user ratings with 4 stars" = fourstar,
      "% of user ratings with 3 stars" = threestar,
      "% of user ratings with 2 stars" = twostar,
      "% of user ratings with 1 star" = onestar,
      "Date" = anydate(Sys.Date())
    )
  }
  #cbind(df[row,],attrs)
}



# m1 <- microbenchmark(out1 <- df[1:50, ] %>%
#                        rowwise() %>%
#                        do(firstgroup(.$ID)) %>%
#                        cbind(df[1:50, ], .))

#tdf <- cbind(df[1:150, ], tdf)


# tictoc::tic()
# 
createdb<-function(df,rows){
  out1 <- df[rows, ]
  out.list = mclapply(df[rows, 3], firstgroup, mc.preschedule = TRUE)
  out2 = do.call("rbind", out.list)
  out = cbind(out1, out2)
}

m<-microbenchmark(
  tictoc::tic()
  createdb(df, 1:150)
  tictoc::toc()
)
# 
autoplot.microbenchmark(m)
# tictoc::toc()

