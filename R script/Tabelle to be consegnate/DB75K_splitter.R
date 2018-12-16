
rm(list = ls())

library(tidyverse)

df <- read.csv2("database_english_nc1_across.csv", stringsAsFactors = FALSE) %>%
  .[!duplicated(.), ]

df <- subset(df, Across == "0")
n <- as.numeric("6")

df.list <-
  split(df, rep(1:n, length.out = nrow(df), each = ceiling(nrow(df) / n)))

for (i in 1:n) {
  filename <- paste0(
    "./File_Meta_75K/75K_Part",
    i,
    ".csv"
  )
  write.csv2(df.list[[i]], filename, row.names = FALSE)
  df1 <- read.csv2(filename,stringsAsFactors = F, header = T)
  df1_ID_description <- df1[, c(3,5)]
  filename2 <- paste0(
    "./File_Meta_75K/75KDescription_ID_Part",
    i,
    ".txt"
  )
  write.table(
    df1_ID_description,
    filename2,
    append = F,
    dec = ".",
    col.names = F,
    row.names = F,
    sep= "|",
    eol = "\n"
  )
}
  

