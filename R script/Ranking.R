library(rvest)
library(tidyverse)
library(pbapply)
library(reshape2)
library(ggplot2)
library(data.table)
library(plyr)
library(vegan)
library(RCurl)
library(parallel)
# carico csv delle app dermatology con tutti gli attributi
df <-read.csv2("./Tabelle to be consegnate/Results/Dermatology.csv" , stringsAsFactors = FALSE)
#df <- df[1:10,]

# normalizzo colonna Average.user.ratings
df$Norma.average.user.rating <- decostand(df$Average.user.rating, "range", MARGIN=2, logbase = 2, na.rm=TRUE)

# normalizzo colonna Number.of.ratings
df$Norma.number.of.user.ratings <- decostand(df$Number.of.user.ratings, "range", MARGIN=2, logbase = 2, na.rm=TRUE)

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

df$Norma.length.description <- x #metto in una nuova colonna il numero di caratteri
df$Norma.length.description <- decostand(df$Norma.length.description, "range", MARGIN=2, logbase = 2, na.rm=TRUE)  #normalizzo valori


#Calcolo actuality: differenza tra data di estrazione e "Release Date" e normalizzo 
df$diff_in_days<- difftime(df$Date ,df$Last.Update.Date , units = c("days"))

df$diff_in_days <- decostand(df$diff_in_days, "range", MARGIN=2, logbase = 2, na.rm=TRUE)
df$diff_in_days <- 1-df$diff_in_days

#verificare se URL del developer ancora attivo 
#definisci la funzione 
check_url<-function(url){
  
  check<-ifelse(url.exists(url),yes = 1,no=0)
  # possibleError <- tryCatch(
  #   
  #   read_html(url),
  #   error = function(e)
  #     e
  # )
  # 
  # if (inherits(possibleError, "error")) {
  #   check<-0
  # }else
  #   check<-1
  # return(check)
}
cl <- makeCluster(detectCores() - 1)

clusterEvalQ(cl, {
  library(rvest)
  library(tidyverse)
  library(pbapply)
  library(reshape2)
  library(ggplot2)
  library(data.table)
  library(plyr)
  library(vegan)
  library(RCurl)
})

clusterExport(cl,
              c("df", "check_url"))

#esegui la funzione sugli url
df$urlcheck<-pbsapply(df$URL, check_url,cl = cl) 

stopCluster(cl)


#calcolo ranking
df[is.na(df)] <- 0  #metto 0 dove c'era NA altrimenti non calcola il punteggio finale
df$Ranking <- ((df$Norma.average.user.rating*0.17)+(df$Norma.number.of.user.ratings*0.17)+(df$Medical_Device*0.2)+(df$Norma.length.description*0.22)+(df$diff_in_days*0.12)+(df$urlcheck*0.12))

df_top60<-df[order(-df$Ranking),]%>%.[1:60,]

write.csv2(df, paste0(path, "Dermatology.csv"), row.names = FALSE)
write.csv2(df_top60, paste0(path, "Dermatology_top60.csv"), row.names = FALSE)
# install.packages('microbenchmark')  
# library(microbenchmark) 
# library(viridis)
# results <- microbenchmark(  
#   check_url.test = pbsapply(df$URL[2:20], check_url),
#   check_url_parallelized.test  = pbsapply(df$URL[2:20], check_url,cl=cl),
#   times = 10)
# 
# 
# autoplot(results)+
#   aes(fill = expr)+
#   the
# 
# 
#   qplot(y=time,data=results, colour=expr)+geom_smooth(alpha=.5)+ labs(title = "Parallel vs Unparallel url checking")+
#   aes(fill = expr)
# 
# print(results)
# time2<-results$time/1e9
# qplot(y=time2,data=results, colour=expr)+geom_smooth(size = 1.5)+
#   geom_point(size = 3, alpha=1, colour="black", aes(fill=expr),shape = 21, stroke = 1.5)+
#   geom_jitter(width = 1,height = 0.5, size=3, alpha=1, colour="black", aes(fill=expr),shape = 21, stroke = 1.5)+
#   aes(fill=expr)+
#   theme_classic(base_size = 18)+
#   labs(x = "urls checked")+
#   labs(y = "time [s]")+
#   ggtitle("URLs validation") +
#   theme(plot.title = element_text(hjust = 0.5))+
#   labs(subtitle= "Parallelization versus Serial approach", hjust=0.5)
