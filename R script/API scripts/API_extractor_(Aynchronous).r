api.single <- function(id) {
  urlID <-
    paste("https://itunes.apple.com/lookup?id=", id , sep = "") #id
  urlapp <- GET(url = urlID)
  cont <- fromJSON(txt = content(urlapp))
  appdata <- cont[["results"]]
  
  
  if (length(appdata) == 0) {
    attrs <- data.frame(matrix(NA, nrow = 1, ncol = 23)) %>%
      cbind(., id)
  } else{
    possibleError <- tryCatch(
      read_html(appdata$trackViewUrl),
      error = function(e)
        e
    )
    
    if (inherits(possibleError, "error")) {
      attrs <- data.frame(matrix(NA, nrow = 1, ncol = 23)) %>%
        cbind(., id)
    } else {
      page <- possibleError
      appkeywords <- page %>%
        html_nodes('meta') %>%
        html_attr('content') %>%
        .[8]
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
      
      appurl <- appdata$trackViewUrl
      #
      print(id)
      
      attrs <- data.frame(
        "Description" = appdesc,
        "Keywords" = appkeywords,
        "Version" = appversion,
        "Age rating" = appagerating,
        "Language(s)" = applanguages,
        "Developer ID" = devid,
        "Developer Name" = devname,
        "Developer URL" = devurl,
        "Price" = appprice,
        "Currency" = appcurr,
        "Size" = appsize,
        "Last update date" =  applastupdatedate,
        #anytime::anydate(applastupdatedate),
        "Release Date" = appreleasedate,
        #anytime::anydate(appreleasedate),
        "Average user ratings-current" =  appcurravgr,
        "Number of user ratings-current" = appcurrnorating,
        "Average user ratings-all" = appallavgr,
        "Number of user ratings-all" = appallnorating,
        "% of user ratings with 5 stars" = fivestar,
        "% of user ratings with 4 stars" = fourstar,
        "% of user ratings with 3 stars" = threestar,
        "% of user ratings with 2 stars" = twostar,
        "% of user ratings with 1 star" = onestar,
        "Date" = anydate(Sys.Date()),
        "ID" = id,
        stringsAsFactors = FALSE
      )
    }
  }
  names(attrs) <-
    c(
      "Description",
      "Keywords" ,
      "Version",
      "Age Rating",
      "Language(s)",
      "Developer ID",
      "Developer Name",
      "Developer URL",
      "Price",
      "Currency",
      "Size",
      "Last Update Date",
      "Release Date",
      "Average user ratings-current",
      "Number of user ratings-current",
      "Average user ratings-all",
      "Number of user ratings-all",
      "% of user ratings with 5 stars",
      "% of user ratings with 4 stars",
      "% of user ratings with 3 stars",
      "% of user ratings with 2 stars",
      "% of user ratings with 1 star",
      "Date",
      "ID"
    )
  return(attrs)
  
}




api.list.id <- function(id.list) {
  t <- lapply(X = id.list, api.single) %>%
    do.call("rbind", .)
  
}

id.extractor <- function(df) {
  id <- df[3]
}


api.subset <- function(subset.list) {
  id.list <- lapply(X  = subset.list, id.extractor) %>%
    do.call("rbind", .)
}







cl <- makeCluster(detectCores() - 1)
clusterEvalQ(cl, {
  library(Rcrawler)
  library(plyr)
  library(dplyr)
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
  library(tictoc)
})

clusterExport(cl,
              c("api.list.id", "api.single", "api.subset", "id.extractor"))

id.list <- parLapply(cl, a, id.extractor) %>%
  unlist(.)

apidf <- parLapply(cl, id.list, api.list.id) %>%
  do.call("rbind", .)
final_db_multithreaded <- merge(df, apidf, all = TRUE)
