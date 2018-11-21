filenames <-
  list.files(path = ".",
             pattern = "_completed.csv$",
             recursive = T)

df.list <- lapply(filenames,
                  read.csv2,
                  header = TRUE,
                  stringsAsFactors = FALSE)%>%
  do.call("rbind",.)

df.list[,16]<-anytime::anydate(df.list[,16])
df.list[,17]<-anytime::anydate(df.list[,17])
df.list[,25]<-anytime::anydate(df.list[,25])



write.csv2(df.list, "Database_preprocessing.csv", row.names = FALSE)
