rm(list = ls())

library(plyr)
library(dplyr)
library(Rcrawler)
library(tidyverse)
library(rvest)
library(stringr)
library(rebus)
library(urltools)
library(RCurl)
library(gdata)
library(anytime)
library(dplyr)
library(lubridate)
library(progress)
library(httr)
library(jsonlite)
library(purrrlyr)


unlink(x = "~/GitHub/E-HEALTH/R script/Merged_splitted", recursive = T)

df <- read.csv2("Merged_db.csv", stringsAsFactors = FALSE) %>%
  .[!duplicated(.), ]


n <-
  as.numeric(readline("In quante cartelle vuoi dividere il dataframe?   "))

df.list <-
  split(df, rep(1:n, length.out = nrow(df), each = ceiling(nrow(df) / n)))


dir.create(file.path("~/GitHub/E-HEALTH/R script", "Merged_splitted"))

m <-
  as.numeric(readline("Quante parti vuoi in ogni cartella?   "))

for (i in 1:n) {
  print(i)
  
  dir.create(file.path("~/GitHub/E-HEALTH/R script/Merged_splitted", i))
  dfs <- df.list[[i]]
  
 # n <- 4
  
  dfs.list <-
    split(dfs, rep(1:m, length.out = nrow(dfs), each = ceiling(nrow(dfs) / m)))
  
  for (j in 1:m) {
    filename2 <-
      paste0(
        "~/GitHub/E-HEALTH/R script/Merged_splitted/",
        i,
        "/",
        "Merged_db_piece_",
        i,
        "_part_",
        j,
        ".csv",
        sep = ""
      )
    write.csv2(dfs.list[[j]], filename2, row.names = FALSE)
  }
}
