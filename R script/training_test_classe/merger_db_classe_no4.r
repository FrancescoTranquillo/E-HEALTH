rm(list = ls())

library(readxl)
library(rlist)
library(tidyverse)
filenames <-
  list.files(path = "./training_test_classe/", pattern = ".xls")

setwd("~/GitHub/E-HEALTH/R script/training_test_classe")
my.df_list <- lapply(filenames, read_excel)

setwd("~/GitHub/E-HEALTH/R script")
df_g4 <- read.csv2("training_test_set_group_4.csv", header = T)

my.df_list <- list.append(my.df_list, as.tibble(df_g4))

# correzione nomi
lapply(my.df_list, names)

names(my.df_list[[5]])[6] <- "NC 1/0"
names(my.df_list[[11]])[5] <- "NC 1/0"

names(my.df_list[[7]])[5] <- "Description"

#estrazione descrizioni e NC1/0

train_test_merged <- lapply(my.df_list, function(df)
  df[, which(names(df) == "Description" | names(df) == "NC 1/0")])

train_test_merged<-train_test_merged%>% 
  do.call("rbind",.)

write.csv2(train_test_merged,file = "class_db_no 4.csv",row.names = F)
