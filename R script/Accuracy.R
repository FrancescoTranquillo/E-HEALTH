df <-read.csv2("Acc_prova.csv" , stringsAsFactors = FALSE, header = FALSE)
df[is.na(df)] <- ""
count<-0
len<- nrow(df)
len

i<-7

for (i in 1:len){
  y <- c(df[i,15], df[i,16], df[i,17])
  x <- c(df[i,6], df[i,7], df[i,8])
  k <- intersect(x,y)
  if (length(k) == 1) {
   count<- count + 1
  } else if(length(k)==2){
    count <- count +2
  } else if(length(k)==3){
    count <- count + 3
  }
}
count
Accuracy = count/(len*3)
Accuracy
