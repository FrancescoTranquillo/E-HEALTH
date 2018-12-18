
###Apro il primo csv con le top ranking derma
df<-read.csv2(".csv")  
df<-na.omit(df)    #Non so se omettere gli NA

###Apro il primo csv con le dermatolog
df1<-read.csv2(".csv")   
df1<-na.omit(df) 

###Apro il primo csv con le mediche totali
df2<-read.csv2(".csv")   
df2<-na.omit(df)



gsub(',', '.', df[,2])    ##Prendo la colonna che mi interessa del primo csv
x<-as.numeric(df[,2])     ##la trasformo prima . con , e poi salvo in numerico
gsub(',', '.', df[,2])   ##Prendo la colonna che mi interessa del secondo csv
y<-as.numeric(df[,2])
gsub(',', '.', df[,2])    ##Prendo la colonna che mi interessa del terzo csv
z<-as.numeric(df[,2])
df1<-cbind(x, y, z)
df1
boxplot(df1)




# gsub(',', '.', df[,18])
# rating<-as.numeric(df[,18])
# rating
# library(ggplot2)
# df1<-cbind(rating, age)
# df1
# boxplot(df1)
