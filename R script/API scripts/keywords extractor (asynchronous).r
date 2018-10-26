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

#t<-extract_keywords(a)


cl <- makeCluster(detectCores() - 1)
clusterEvalQ(cl, { library(Rcrawler)
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

clusterExport(cl, c( "crawl_vector","extract_keywords","pattern"))


db_keywords<-parLapply(cl, a,crawl_vector)%>%
  do.call("rbind",.)
keywords_db<-merge(df, db_keywords, all = TRUE)

