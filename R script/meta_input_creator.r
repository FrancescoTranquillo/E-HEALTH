df <-
  read.csv2(
    "wewe.csv",
    stringsAsFactors = F,
    header = T
  )
count<-0
df2<- df[,2]
df1<- df[,1]
if (df2==df1) {
  count<-count+1
}

