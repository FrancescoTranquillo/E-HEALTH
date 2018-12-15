##Mi apro le derma
df<-read.csv2("Prova1.csv")   #è un file di prova che ho nel desktop non riesco a caricarlo perchè troppo pesante per github
#df<-na.omit(df)    #Non so se omettere gli NA
StatisticsDerma<-summary(df)
StatisticsDerma

##Mi apro tutte
df1<-read.csv2("Prova2.csv")
StaticsTot<- summary (df1)
StaticsTot 


##Esempio confronto di due istogramma + boxplot su media recensioni utenti

par(mfcol=c(2,2))
hist(df[,4], xlab = "Stars", main = "Average User Ratings", col = "green")
boxplot(df[,4], xlab = "Stars", main = "Average User Ratings", col = "green")

hist(df1[,4], xlab = "Stars", main = "Average User Ratings", col = "red")
boxplot(df1[,4], xlab = "Stars", main = "Average User Ratings", col = "red")


##Questo crea invece il grafico sulla matrice di correlazione
library(PerformanceAnalytics)
chart.Correlation(df[,2:7],col="red")




#cor(df[,2], df[,3])        #CORRELAZIONI DA FARE SULLE COLONNE, DECIDERE QUALI FARE
##Questo è quello vecchio senza summary non l'ho cancellato potrebbe servire
#i<-0
#col<-ncol(df)
#media<-0
#media3<-0
#mediana<-0
#varianza<-0
#sd<-0
#max<-0
#min<-0
#range<-0
#quantinf<-0
#quantsup<-0
#
#
#for (i in 1:col){
#  
# media1<-mean(df[,i])
# media<-c(media,media1)
#
# media2<- mean(df[,i], trim = 0.10) #rimuovo i top 10 percento e i bottom 10 percento di osservazioni
# media3<- c(media3, media2)
# 
# mediana1<-median(df[,i])
# mediana<- c(mediana,mediana1)
#  
# varianza1<- var(df[,i])
# varianza<- c(varianza,varianza1)
#  
# sd1<- sd(df[,i])
# sd<-c(sd,sd1)
#  
# max1<-max(df[,i])
# max<-c(max,max1)
# 
# min1<-min(df[,i])
# min<-c(min,min1)
# 
# range1<-range(df[,i])
# range<-c(range,range1)
#  
####quantinf1<-quantile((df[,i]), probs = 0.25) #Quantili quanti ne volete? io direi 0.25/0.75 (se metto 0.5 esce la media)
# #quantinf<-c(quantinf,quantinf1)
# #
# #quantsup1<-quantile((df[,i]), probs = 0.75)
# #quantsup<-c(quantsup,quantsup1)
# 
#}
#
#media
#media3
#varianza
#sd
#max
#min
#range
##quantinf
##quantsup
#
#hist(media)
#