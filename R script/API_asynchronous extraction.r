rm(list = ls()) #pulisce l'enviroment di tutte le variabili create

library(plyr)
library(dplyr)
library(Rcrawler)
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


df <- read.csv2("M.csv", header = TRUE, stringsAsFactors = FALSE)
df<-df[!duplicated(df),]
df<-df[1:150,]

nr <- nrow(df)
n<-7
a<-split(df, rep(1:ceiling(nr/n), each=n, length.out=nr))

api.bridge<-function(x){
  listid<-as.list(x$ID)
  lapply(listid, api.single)
  
}

api.single<-function(id){
  
  urlID <-
    paste("https://itunes.apple.com/lookup?id=", id, sep = "")
  urlapp <- GET(url = urlID)
  cont <- fromJSON(txt = content(urlapp))
  appdata <- cont[["results"]]
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
  
  appurl<-appdata$trackViewUrl
  
  attrs <- data.frame(
    "Description" = appdesc,
    "Version" = appversion,
    "Age rating" = appagerating,
    "Language(s)" = applanguages,
    "Developer ID" = devid,
    "Developer Name" = devname,
    "Developer URL" = devurl,
    "Price" = appprice,
    "Currency" = appcurr,
    "Size" = appsize,
    "Last update date" =  applastupdatedate,#anytime::anydate(applastupdatedate),
    "Release Date" = appreleasedate,#anytime::anydate(appreleasedate),
    "Average user ratings-current" =  appcurravgr,
    "Number of user ratings-current" = appcurrnorating,
    "Average user ratings-all" = appallavgr,
    "Number of user ratings-all" = appallnorating,
    "Date" = anydate(Sys.Date()),
    "URL" = appurl
  )
  # attrs<-c(appdesc,appversion,appagerating,applanguages,devid,devname,devurl,appprice,appcurr,appsize,applastupdatedate,
  #          appreleasedate,appcurr,appcurrnorating,appallavgr,appallnorating,anydate(Sys.Date()),appurl)
  # data.frame(attrs,stringsAsFactors = FALSE)
}


t<-api.single(382952264)

api.list<-function(id.list){
  t<-lapply(X = id.list, api.single)

}

z<-api.list(as.list(df[,3]))

names(z)<-c("Description", "Version", "Age Rating","Language(s)",
            "Developer ID", "Developer Name", "Developer URL",
            "Price", "Currency", "Size", "Last Update Date", 
            "Release Date", "Average user ratings-current",
            "Number of user ratings-current", "Average user ratings-all",
            "Number of user ratings-all", "Date","URL")

# cl <- makeCluster(detectCores() - 1)
# clusterEvalQ(cl, { library(Rcrawler)
#   library(plyr)
#   library(dplyr)
#   library(Rcrawler)
#   library(tidyverse)
#   library(rvest)
#   library(stringr)
#   library(rebus)
#   library(urltools)
#   library(RCurl)
#   library(gdata)
#   library(anytime)
#   library(dplyr)
#   library(lubridate)
#   library(progress)
#   library(httr)
#   library(jsonlite)
#   library(purrrlyr)
#   library(parallel)
#   library(microbenchmark)
#   library(tictoc)
# })
# 
# clusterExport(cl, c( "api.bridge", "api.single"))
# tictoc::tic()
# 
# 
# apidf<-parLapply(cl, a, api.bridge)%>%
#   do.call("rbind",.)
# db_final<-merge(df, apidf, all = TRUE)
# tictoc::toc()
