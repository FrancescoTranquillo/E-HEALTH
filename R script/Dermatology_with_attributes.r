

#secondo me non servono le librerie, ma le metto lo stesso, mi danno sicurezza :)
library(rvest)
library(tidyverse)
library(pbapply)
library(reshape2)
library(ggplot2)
library(data.table)
library(plyr)


# df1 <-read.csv2("75K_Part1_results.csv" , stringsAsFactors = FALSE)
# df2 <-read.csv2("75K_Part2_results.csv" , stringsAsFactors = FALSE)
# df3 <-read.csv2("75K_Part3_results.csv" , stringsAsFactors = FALSE)
# df4 <-read.csv2("75K_Part4_results.csv" , stringsAsFactors = FALSE)
# df5 <-read.csv2("75K_Part5_results.csv" , stringsAsFactors = FALSE)
# df6 <-read.csv2("75K_Part6_results.csv" , stringsAsFactors = FALSE)
#
# df <- rbind(df1, df2, df3, df4, df5, df6)

path <- "./Tabelle to be consegnate/Results/"

filenames <-
  as.list(list.files(pattern = "results\\.csv$", path = path))

my.df <-
  do.call(rbind,
          lapply(
            paste0(path, filenames),
            read.csv2,
            header = TRUE,
            stringsAsFactors = F
          ))
anyDuplicated(my.df)

#conversione dei tipi di dato
my.df <- within(my.df, {
  URL <- as.character(URL)
  Name <- as.character(Name)
  ID <- as.factor(ID)
  Category <- as.factor(Category)
  Description <- as.character(Description)
  Keywords <- as.character(Keywords)
  Version <- as.character(Version)
  Age.Rating <- as.integer(Age.Rating)
  Language.s. <- as.character(Language.s.)
  Developer.ID <- as.factor(Developer.ID)
  Developer.Name <- as.character(Developer.Name)
  Developer.URL <- as.character(Developer.URL)
  Price <- as.numeric(Price)
  Currency <- as.factor(Currency)
  Size <- as.numeric(Size)
  Last.Update.Date <- as.Date(Last.Update.Date)
  Release.Date <- as.Date(Release.Date)
  Average.user.rating <- as.numeric(Average.user.rating)
  Number.of.user.ratings <- as.integer(Number.of.user.ratings)
  X..of.user.ratings.with.5.stars <-
    as.integer(X..of.user.ratings.with.5.stars)
  X..of.user.ratings.with.4.stars <-
    as.integer(X..of.user.ratings.with.4.stars)
  X..of.user.ratings.with.3.stars <-
    as.integer(X..of.user.ratings.with.3.stars)
  X..of.user.ratings.with.2.stars <-
    as.integer(X..of.user.ratings.with.2.stars)
  X..of.user.ratings.with.1.star <-
    as.integer(X..of.user.ratings.with.1.star)
  Date <- as.Date(Date)
  NC <- as.factor(NC)
  Across <- as.factor(Across)
  V1 <- as.factor(V1)
  V2 <- as.factor(V2)
  V3 <- as.factor(V3)
})

write.csv2(my.df, paste0(path, "db_totale.csv"), row.names = FALSE)

#Prendo solo app che sono state categorizzate "dermatology"
df_dermatology <- my.df[which(my.df$V1=="Dermatology"),]


#
#
# #prendo il db con tutti gli attributi
# df2 <- read.csv2("Database_preprocessed_english.csv")
#
#
# #unisco i due db solo per le app cardiology
# d<-merge(df1, df2, by="ID")


write.csv2(df_dermatology, paste0(path, "Dermatology.csv"), row.names = FALSE)
