rm(list = ls())

library(tidyverse)
library(tm)
library(caret)
library(klaR)
library(MASS)
#carico il dataset della classe (comprende anche le nostre)
df_class <-
  read.csv2("class_db_no 4.csv",
            header = T,
            stringsAsFactors = F)
df_class <- df_class[!apply(is.na(df_class) | df_class == "", 1, all),]


df_class$NC.1.0 <- factor(df_class$NC.1.0)

table(df_class$NC.1.0)

# # carico il nostro dataset e lo separo in training e test
# df_nostro <-
#   read.csv2("test_set_results.csv",
#             header = T,
#             stringsAsFactors = F)%>%
#   drop_na()
# df_nostro$Tipo <- factor(df_nostro$Tipo)
# 
# df_nostro$NC.1.0 <- factor(df_nostro$NC.1.0)
# 
# df_nostro_test <-
#   df_nostro[which(df_nostro$Tipo == "Test"), c(4, 5)]
# df_nostro_training <-
#   df_nostro[which(df_nostro$Tipo == "Training"), c(4, 5)]
# 
# #unisco il nostro training a quello della classe
# df_train <- rbind(df_class, df_nostro_training) %>%
#   .[sample(nrow(.)), ]

#raccolgo tutte le descrizioni del training in un vettore
descriptions <- df_class$Description

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
train_index <- createDataPartition(df_class$NC.1.0, p = 1, list = F)

#separo in train e test usando gli indici trovati al passaggio precendente
descr_raw_train <- df_class[train_index, ]
descr_raw_test <- df_class[-train_index, ]
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

descr_model1 <- train(descr_train, descr_raw_train$NC.1.0, method="nb",trControl = ctrl)


descr_predict <- predict(descr_model1, descr_test)
cm <- confusionMatrix(descr_predict, descr_raw_test$NC.1.0, positive = "0")
cm

saveRDS(descr_model1, "NC_bayesian_classifier.rds")
NC_B <- readRDS("NC_bayesian_classifier.rds")
#uso il modello per classificare le nostre app

#carico il nostro db
df_nostro <-
  read.csv2("test_set_results.csv",
            header = T,
            stringsAsFactors = F)
df_nostro$NC.1.0 <- factor(df_nostro$NC.1.0)
df_nostro$Tipo <- factor(df_nostro$Tipo)

#seleziono le nostre indicate come Test 
df_nostro <- df_nostro[which(df_nostro$Tipo=="Test"),]

#Trasformo in corpus
df_nostro_corpus <- Corpus(VectorSource(df_nostro$Description))

clean_corpus<-function(corpus){
  corpus%>%
    tm_map(content_transformer(tolower)) %>% 
    tm_map(removeNumbers) %>%
    tm_map(removeWords, stopwords()) %>%
    tm_map(removePunctuation) %>%
    tm_map(stripWhitespace)
    
}
df_nostro_corpus_clean <- clean_corpus(df_nostro_corpus)

#filtro il corpus secondo il dizionario definito prima
df_nostro_dtm <- DocumentTermMatrix(df_nostro_corpus_clean, list(dictionary=descr_dict))

#converto la presenza/assenza in variabile categorica
df_nostro_test <- df_nostro_dtm %>% apply(MARGIN=2, convert_counts)

#predico  utilizzando il modello
df_nostro_predict1 <- predict(descr_model1, df_nostro_test)

#creo e richiamo la matrice di confusione
df_nostro_cm1 <- confusionMatrix(df_nostro_predict1, df_nostro$NC.1.0,  positive="1", mode = "everything")
df_nostro_cm1

df_nostro$NC_predicted<-as.numeric(levels(df_nostro_predict1))[df_nostro_predict1]

write.csv2(df_nostro, "./Tabelle to be consegnate/75_NC.csv",row.names = F)

#carico l'intero db
df_totale <-
  read.csv2("Database_preprocessed_english.csv",
            header = T,
            stringsAsFactors = F)

df_totale_corpus <-  Corpus(VectorSource(df_totale$Description))

df_totale_corpus_clean <- clean_corpus(df_totale_corpus)

df_totale_dtm <- DocumentTermMatrix(df_totale_corpus_clean, list(dictionary=descr_dict))

#converto la presenza/assenza in variabile categorica
df_totale_test <- df_totale_dtm %>% apply(MARGIN=2, convert_counts)

#predico  utilizzando il modello
df_totale_predict <- predict(descr_model1, df_totale_test)

#aggiungo la classificazione NC al db totale
df_totale_classified<-cbind(df_totale, "NC"=df_totale_predict)

write.csv2(df_totale_classified, "./Tabelle to be consegnate/database_preprocessed_english_nc.csv", row.names = F)

#salvo solo quelle mediche
df_totale_classified_mediche<-df_totale_classified[which(df_totale_classified$NC==1),]

write.csv2(df_totale_classified_mediche, "database_preprocessed_english_nc_1.csv", row.names = F)
