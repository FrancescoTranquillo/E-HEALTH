df <-read.csv2("Acc_prova.csv"  )
count<-0
df2<- df[,6]
len<- length(df2)
df1<- df[,15]
i<-0
for (i in 1:len){
  if (df2[i]==df1[i]) {
   count<- count + 1
  }
}
count
Accuracy = count/len
Accuracy
