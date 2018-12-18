library(reshape)
###Apro il primo csv con le top ranking derma

df<-read.csv2("Database_preprocessing.csv")   #è un file di prova che ho nel desktop non riesco a caricarlo perchè troppo pesante per github
df<-na.omit(df)    #Non so se omettere gli NA
df1<-df[,19:22]
gsub(',', '.', df1)
# df1<-as.numeric(df1)
df<- melt(df1, a.rm = FALSE, value.name = "mediche")

df
# ###Apro il primo csv con le derma
# gsub(',', '.', df[,13])
# age<-as.numeric(df[,13])
# gsub(',', '.', df[,18])
# rating<-as.numeric(df[,18])
# rating
# library(ggplot2)
# df1<-cbind(rating, age)
# df1
# boxplot(df1)
