df <-read.csv2("test_set_results.csv" , stringsAsFactors = FALSE)
#df[is.na(df)] <- ""
count<-0
df1 <- read.csv2("Acc_prova.csv", stringsAsFactors = FALSE, header = FALSE)
df <- subset(df, NC.1.0 == "1")
df[,6] <- df1[,6] 
df[,7] <- df1[,7]
df[,8] <- df1[,8]


df <- subset(df, Tipo == "Test")
df <- subset (df, SA1 != "Across" )



len<- nrow(df)
len

i<-0

for (i in 1:len){
  y <- c(df[i,15], df[i,16], df[i,17])
  x <- c(df[i,6], df[i,7], df[i,8])
  k<- intersect (y,x)
 
  if (length(k) != 0) {
   count<- count + 1
  } 
  #else if(length(k)==2){
   # count <- count +2
  #} else if(length(k)==3){
    #count <- count + 3
  #}
}
count
#Accuracy = count/(len*3)
Accuracy = count/len
Accuracy

