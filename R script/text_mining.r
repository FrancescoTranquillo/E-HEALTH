rm(list = ls())


library(tidyverse)
library(tm)
library(wordcloud)
library(e1071)
library(caret)
library(ROCR)
library(SnowballC)
library(plotrix)
library(qdap)
library(gmodels)
library(viridis)


training_test_4 <- read.csv2("train_test_groups.csv", header = T) %>%
  drop_na()

training_test_4$Description <-
  as.character(training_test_4$Description)
training_test_4$NC.1.0 <- factor(training_test_4$NC.1.0)

table(training_test_4$NC.1.0)

stopwords <- c(stopwords("en"), "app", "can", "use","will","may")

clean_corpus <- function(corpus) {
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removeWords, stopwords)
  #corpus <- tm_map(corpus, removeNumbers)
  
}

#Data preparation – cleaning and standardizing text data####

descr_corpus <- VCorpus(VectorSource(training_test_4$Description))

descr_corpus_clean<-clean_corpus(descr_corpus)
# descr_corpus_clean<-
#   tm_map(descr_corpus, content_transformer(tolower))
# 
# descr_corpus_clean <-
#   descr_corpus_clean %>% tm_map(., removeNumbers)
# 
# 
# descr_corpus_clean <-
#   tm_map(descr_corpus_clean, removeWords, stopwords) %>%
#   tm_map(., removePunctuation) #This line remove punctuation
# 
# descr_corpus_clean <- tm_map(descr_corpus_clean, stemDocument)
# 
# descr_corpus_clean <- tm_map(descr_corpus_clean, stripWhitespace)

#Data preparation – splitting text documents into words####

descr_dtm <- DocumentTermMatrix(descr_corpus_clean)

# Data preparation – creating training and test datasets ####

#50% train, 50% test
perf_v<-NULL
splices<-seq(0.5, 0.95, by=0.05)
for(i in splices){
a <- nrow(descr_dtm) * i
b <- a + 1
descr_train <- descr_dtm[1:a,]
descr_test <- descr_dtm[b:nrow(descr_dtm),]

descr_train_labels <- training_test_4[1:a,]$NC.1.0
descr_test_labels <-
  training_test_4[b:nrow(descr_dtm),]$NC.1.0


# # Visualizing text data – word clouds ####
# wordcloud(
#   descr_corpus_clean,
#   max.words = 100,
#   random.order = F,
#   colors = c("grey80", "darkgoldenrod1", "tomato")
# )
# 
# #subset of NC=0 and NC=1 and respective corpora
# NC <- subset(training_test_4 , NC.1.0 == 0)
# med <- subset(training_test_4, NC.1.0 == 1)
# 
# nc_corpus <- VCorpus(VectorSource(NC$Description))
# nc_corpus_clean <- clean_corpus(nc_corpus)
# 
# med_corpus <- VCorpus(VectorSource(med$Description))
# med_corpus_clean <- clean_corpus(med_corpus)
# 
# # Combine both corpora
# all_nc <- paste(NC$Description, collapse = "")
# all_med <- paste(med$Description, collapse = "")
# all_corpus_clean <- VCorpus(VectorSource(c(all_nc, all_med))) %>%
#   clean_corpus(.)
# 
# all_tdm <- TermDocumentMatrix(all_corpus_clean)
# all_m <- as.matrix(all_tdm)
# 
# commonality.cloud(all_m,
#                   colors = "steelblue1",
#                   max.words = 100)
# 
# colnames(all_tdm) <- c("Not medical", "Medical")
# all_m <- as.matrix(all_tdm)
# 
# comparison.cloud(
#   all_m,
#   colors = c("grey80", "tomato"),
#   max.words = 25,random.order = T,
#   scale = c(0.5, 5)
# )
# 
# 
# 
# # Identify terms shared by both documents
# common_words <- subset(all_m,
#                        all_m[, 1] > 0 & all_m[, 2] > 0)
# 
# head(common_words)
# 
# # calc common words and difference
# difference <- abs(common_words[, 1] - common_words[, 2])
# common_words <- cbind(common_words, difference)
# common_words <- common_words[order(common_words[, 3],
#                                    decreasing = T),]
# head(common_words)
# 
# top25_df <- data.frame(x = common_words[1:25, 1],
#                        y = common_words[1:25, 2],
#                        labels = rownames(common_words[1:25,]))
# 
# # The plotrix package has been loaded
# 
# # Make pyramid plot
# pyramid.plot(
#   top25_df$y,
#   top25_df$x,
#   labels = top25_df$labels,
#   main = "Words in Common",
#   gap = 85,
#   laxlab = NULL,
#   raxlab = NULL,
#   unit = NULL,
#   top.labels = c("Medical",
#                  "Words",
#                  "Not medical"),show.values = T,ndig = 0
# )
# 
# # This time, we'll use the max.words parameter to look at the 15 most common words in
# # each of the two sets. The scale parameter allows us to adjust the maximum and
# # minimum font size for words in the cloud
# # par(mfrow = c(1, 2))
# #
# #
# #
# # w1 <- wordcloud(
# #   NC$Description,
# #   max.words = 100,
# #
# #   scale = c(3, 0.5),
# #   colors = brewer.pal(4, "Dark2")
# # )
# #
# # w2 <- wordcloud(
# #   med$Description,
# #   max.words = 100,
# #   #min.freq = 15,
# #   scale = c(3, 0.5),
# #   colors = brewer.pal(4, "Dark2")
# # )
# # dev.off()
# 
# med_tdm <-
#   descr_tdm <- TermDocumentMatrix(descr_corpus_clean)
# descr_m <- as.matrix(descr_tdm)
# 
# # Calculate the rowSums: term_frequency
# term_frequency <- rowSums(descr_m)
# 
# # Sort term_frequency in descending order
# term_frequency <- sort(term_frequency, decreasing = T)
# 
# # View the top 10 most common words
# term_frequency[1:20]
# 
# barplot(term_frequency[1:30], col = "tomato", las = 2,horiz = F)
# 
# # Data preparation – creating indicator features for
# frequent words ####

descr_freq_words <- findFreqTerms(descr_train,lowfreq = 68)

descr_dtm_freq_train <- descr_train[, descr_freq_words]
descr_dtm_freq_test <- descr_test[, descr_freq_words]

convert_counts <- function(x) {
  x <- ifelse(x > 0, "Yes", "No")
}

descr_train <-
  apply(descr_dtm_freq_train, MARGIN = 2, convert_counts)
descr_test <- apply(descr_dtm_freq_test, MARGIN = 2, convert_counts)

descr_classifier2 <-
  naiveBayes(descr_train, descr_train_labels, laplace = 1)

descr_test_pred2 <- predict(descr_classifier2, descr_test)

confusionMatrix(descr_test_pred2, descr_test_labels,
                positive = "0")


#obtain predicted probabilities
descr_test_prob <-
  predict(descr_classifier2, descr_test, type = "raw")

#combine the results in a df
descr_results <- data.frame(
  actual_type = descr_test_labels,
  predict_type = descr_test_pred2,
  prob_1 = round(descr_test_prob[, 2], 5),
  prob_0 = round(descr_test_prob[, 1], 5)
)

#write.csv(descr_results, "descr_results.csv", row.names = F)

#Confusion matrix #2
confusionMatrix(descr_results$predict_type,
                descr_results$actual_type,
                positive = "0")

#ROC
pred <- prediction(descr_results$prob_1, descr_results$actual_type)

perf <- performance(pred, measure = "tpr", x.measure = "fpr")
perf_v<-c(perf_v, perf)
#Area Under the Curve AUC
# calculate AUC
perf.auc <- performance(pred, measure = "auc")
str(perf.auc)
unlist(perf.auc@y.values)


plot(perf,
     main = paste("ROC curve for medical content","\n",i*100,"% train set", "\n AUC: ", round(unlist(perf.auc@y.values),digits = 2)),
     col = rgb(red = 0, green = 0, blue = 1, alpha = i),
     
     lwd = 2)

abline(a = 0,
       b = 1,
       lwd = 2,
       lty = 2)

}




cols<-viridis_pal(option = "D")(11)
for(i in 1:length(perf_v)){
  par(new=TRUE)
 
  plot(perf_v[[i]],
       main = "ROC for different percentages of train test size",
       col =  cols[11-i] ,
       lwd = 2
         )
  
}

# container = create_container(descr_dtm, training_test_4$NC.1.0, 
#                              trainSize = 1:1005, testSize = 1006:1341, 
#                              virgin = FALSE)
# 
# 
# descr_model <- train_model(container, algorithm ="SVM")
# 
# 
# descr_model_result <- classify_model(container, descr_model)
# table( training_test_4$NC.1.0[1006:1341], descr_model_result[,"SVM_label"])
# 
# analytics = create_analytics(container, descr_model_result)
# summary(analytics)

