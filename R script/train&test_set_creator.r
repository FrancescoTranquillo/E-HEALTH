library(tidyverse)


df<-read.csv2("~/Documents/GitHub/E-HEALTH/R script/Database_preprocessed_english.csv", stringsAsFactors = FALSE)


summary(df)

dfsample_m<-df[which(df$Category=='Medical'),]%>%
  .[sample(nrow(.), 120),]

dfsample_hf<-df[which(df$Category=='Health&Fitness'),]%>%
  .[sample(nrow(.), 30),]

dfsample<-rbind(dfsample_m, dfsample_hf)%>%
  .[sample(nrow(.), nrow(.)), ]

dfs.list <-
  split(dfsample, rep(1:6, length.out = nrow(dfsample), each = ceiling(nrow(dfsample) / 6)))


for (j in 1:6) {
  filename<-
    paste0(
      "~/Documents/GitHub/E-HEALTH/R script/sample_",
      j,
      "_.csv",
      sep = ""
    )
  write.csv2(dfs.list[[j]], filename, row.names = FALSE)
}