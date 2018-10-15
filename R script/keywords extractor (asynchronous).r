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




pattern<-'//*[@name="keywords"]//@content'


# "https://itunes.apple.com/us/app/mychart/id382952264?mt=8"
crawl_vector <- function(x){
  Rcrawler(x$URL  ,ExtractXpathPat = pattern, MaxDepth = 0)

  DATAdf<-do.call("rbind.data.frame",DATA)
  risultatoMAP<-cbind( INDEX$Url, DATAdf, stringsAsFactors = FALSE)
  names(risultatoMAP)<-c( "URL","KEYWORDS")
  
  return(risultatoMAP)
  
#XpathPatterns = pattern,PatternsName = "Keywords")%>% #
  
}


extract_keywords<-function(a){

  db_keywords<-lapply(X = a, FUN = crawl_vector)%>%
  do.call("rbind",.)
  db_final<-merge(df, db_keywords, all = TRUE)
  return(db_final)
}
# tictoc::tic()
# db<-extract_keywords(a)
# 
# tictoc::toc()

# 
# # Rcrawler(df[,2],  no_cores = nrow(df))
# crawl_vector<-function(x){
#   t<-ContentScraper(Url = x , XpathPatterns = pattern,PatternsName = "Keywords" )%>%
#     do.call("rbind",.)
#   out= cbind(df,t)
# }
# 
# crawl_keywords<-function(x){
#   t<-ContentScraper(Url = x , XpathPatterns = pattern,PatternsName = "Keywords" )
# }
# keywords_extractor<-function(x){
#   keywords<-mapply(crawl_keywords, x)%>%
#   do.call("rbind",.)
#   out<-cbind(df,keywords)
# }
#   
# 
#   t<-keywords_extractor(df[,2])
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
  applanguages <- paste(unlist(appdata$languageCodesISO2A), collapse = ",")
  devid <- appdata$artistId
  api.df<-data.frame(appdesc, appversion, appagerating, applanguages, devid, appurl<-appdata$trackViewUrl)
}
cl <- makeCluster(detectCores() - 1)
clusterEvalQ(cl, { library(Rcrawler)
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
  library(tictoc)
})

clusterExport(cl, c("extract_keywords", "crawl_vector", "pattern", "api.bridge", "api.single"))
tictoc::tic()

# newdf<-parLapply(cl, a, crawl_vector)%>%
#   do.call("rbind",.)

apidf<-parLapply(cl, a, api.bridge)%>%
  do.call("rbind",.)
db_final<-merge(df, apidf, all = TRUE)
tictoc::toc()
