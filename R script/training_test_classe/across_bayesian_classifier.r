rm(list = ls())

library(readxl)
library(rlist)
library(tidyverse)
library(tm)
library(caret)
library(klaR)
library(MASS)

clean_corpus<-function(corpus){
  corpus%>%
    tm_map(content_transformer(tolower)) %>% 
    tm_map(removeNumbers) %>%
    tm_map(removeWords, stopwords()) %>%
    tm_map(removePunctuation) %>%
    tm_map(stripWhitespace)
  
}

filenames <-
  list.files(path = "./training_test_classe/", pattern = ".xls")

setwd("~/GitHub/E-HEALTH/R script/training_test_classe")
my.df_list <- lapply(filenames, read_excel)

setwd("~/GitHub/E-HEALTH/R script")
df_g4 <- read.csv2("training_test_set_group_4.csv", header = T)%>%
  subset(., .$X=="Training")

my.df_list <- list.append(my.df_list, as.tibble(df_g4))

# correzione nomi
lapply(my.df_list, names)

names(my.df_list[[5]])[6] <- "NC 1/0"
names(my.df_list[[11]])[5] <- "NC 1/0"

names(my.df_list[[7]])[5] <- "Description"
names(my.df_list[[2]])[6] <- "SA1"
names(my.df_list[[2]])[7] <- "SA2"

#estrazione descrizioni e NC1/0


train_test_merged <- lapply(my.df_list, function(df)
  df[, which(names(df) == "Description" | names(df) == "NC 1/0")])

train_test_merged<-train_test_merged%>% 
  do.call("rbind",.)

#write.csv2(train_test_merged,file = "class_db_no 4.csv",row.names = F)

descr_list<-lapply(my.df_list, function(df)
  cbind(df["Description"], df[,"SA1"]))%>%
  do.call("rbind",.)%>%
  mutate(across = ifelse(grepl(
        pattern = "across", .$SA1, ignore.case = T
      ), 1, 0))

across1<-subset(descr_list, descr_list$across==1)%>%
  .[,-2]
across0<-subset(descr_list,descr_list$across==0)%>%
  .[,-2]%>%
  drop_na()

across0_50<-across0[sample(354),]

across_df<-rbind(across1,across0)%>%
  .[sample(nrow(.)), ]

across_df_50<-rbind(across1, across0_50)%>%
  .[sample(nrow(.)),]

across_df$across<-factor(across_df$across)

across_df_50$across<-factor(across_df_50$across)





descriptions <- across_df$Description

#trasformo il vettore in un corpus di documenti
descr_corpus <- VCorpus(VectorSource(descriptions))

#pulisco tutti i documenti
descr_corpus_clean <- descr_corpus %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removeNumbers) %>%
  tm_map(removeWords, stopwords(kind = "en")) %>%
  tm_map(removePunctuation) %>%
  tm_map(stripWhitespace)

#trasformo il corpus pulito in una DTM
descr_dtm <- DocumentTermMatrix(descr_corpus_clean)

#separo in training e test indicando la percentuale con p, creando gli indici delle
#righe indiate come train, e quelle indicate come test
train_index <- createDataPartition(across_df$across, p = 0.75, list = F)

#separo in train e test usando gli indici trovati al passaggio precendente
descr_raw_train <- across_df[train_index, ]
descr_raw_test <- across_df[-train_index, ]
descr_corpus_clean_train <- descr_corpus_clean[train_index]
descr_corpus_clean_test <- descr_corpus_clean[-train_index]
descr_dtm_train <- descr_dtm[train_index,]
descr_dtm_test <- descr_dtm[-train_index,]

#creo il dizionario di termini sul quale allenare il classificatore
descr_dict <- findFreqTerms(descr_dtm_train, lowfreq = 100)

#filtro le descrizioni utilizzando il dizionario
descr_train <- DocumentTermMatrix(descr_corpus_clean_train, list(dictionary=descr_dict))
descr_test <- DocumentTermMatrix(descr_corpus_clean_test, list(dictionary=descr_dict)) 

#funzione per convertire la presenza/assenza di una parola in una descrizione
# in una variabile categorica "si", "no"
convert_counts <- function(x) {
  x <- ifelse(x > 0, 1,0)
  x <- factor(x, levels = c(0, 1), labels = c("No", "Yes"))
}

descr_train <- descr_train%>%apply(MARGIN = 2, convert_counts)
descr_test <- descr_test%>%apply(MARGIN = 2, convert_counts)

#creo il modello

#validazione 10-fold
ctrl<-trainControl(method="cv", 10)

set.seed(1223)

descr_model1 <- train(descr_train, descr_raw_train$across, method="nb",trControl = ctrl)



descr_predict <- predict(descr_model1, descr_test)
cm <- confusionMatrix(descr_predict, descr_raw_test$across, positive = "0",mode="everything")
cm

setwd("~/GitHub/E-HEALTH/R script")

#carico tutto il db di app con NC=1
dbnc1<-read.csv2("database_preprocessed_english_nc_1.csv",stringsAsFactors = F, header = T)

df_totale_corpus <-  Corpus(VectorSource(dbnc1$Description))

df_totale_corpus_clean <- clean_corpus(df_totale_corpus)

df_totale_dtm <- DocumentTermMatrix(df_totale_corpus_clean, list(dictionary=descr_dict))

#converto la presenza/assenza in variabile categorica
df_totale_test <- df_totale_dtm %>% apply(MARGIN=2, convert_counts)

#predico  utilizzando il modello
df_totale_predict <- predict(descr_model1, df_totale_test)

#aggiungo la classificazione across al db totale
df_totale_classified<-cbind(dbnc1, "Across"=df_totale_predict)

write.csv2(df_totale_classified, "database_preprocessed_english_nc_1_across.csv", row.names = F)
