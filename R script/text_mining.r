rm(list = ls())


library(tidyverse)
library(tm)

training_test_4 <- read.csv2("train_test_groups.csv", header = T)

t4_train <- training_test_4 %>%
  drop_na()

t4_train$Description <- as.character(t4_train$Description)
t4_train$NC.1.0 <- factor(t4_train$NC.1.0)

table(t4_train$NC.1.0)

#Data preparation – cleaning and standardizing text data####

#The first step in processing text data involves creating a corpus, which is a collection
# of text documents.

descr_corpus <- VCorpus(VectorSource(t4_train$Description))

#Our first order of business will be to standardize the descriptions to use only lowercase
# characters

descr_corpus_clean <-
  tm_map(descr_corpus, content_transformer(tolower))

# Let's continue our cleanup by removing numbers

descr_corpus_clean <- descr_corpus_clean %>% tm_map(., removeNumbers)


# next task is to remove filler words such as to, and, but, and or from our descriptions.
# These terms are known as stop words and are typically removed prior to
# text mining. This is due to the fact that although they appear very frequently, they do
# not provide much useful information for machine learning.

descr_corpus_clean <-
  tm_map(descr_corpus_clean, removeWords, stopwords()) %>%
  tm_map(., removePunctuation) #This line remove punctuation


# Another common standardization for text data involves reducing words to their root
# form in a process called stemming. The stemming process takes words like learned,
# learning, and learns, and strips the suffix in order to transform them into the base
# form, learn. This allows machine learning algorithms to treat the related terms as a
# single concept rather than attempting to learn a pattern for each variant.

#library(SnowballC)

descr_corpus_clean<-tm_map(descr_corpus_clean,stemDocument)


# The final step in our text cleanup process is to remove
# additional whitespace, using the built-in stripWhitespace()

descr_corpus_clean <- tm_map(descr_corpus_clean, stripWhitespace)

#Data preparation – splitting text documents into words####

# Now that the data are processed to our liking, the final step is to split the messages
# into individual components through a process called tokenization. A token is a
# single element of a text string; in this case, the tokens are words.

descr_dtm <- DocumentTermMatrix(descr_corpus_clean)

# Data preparation – creating training and test datasets ####

#75% train, 25% test
n75 <- nrow(descr_dtm) * 0.95
n75next <- n75 + 1
descr_train <- descr_dtm[1:n75, ]
descr_test <- descr_dtm[n75next:nrow(descr_dtm), ]

descr_train_labels <- t4_train[1:n75, ]$NC.1.0
descr_test_labels <- t4_train[n75next:nrow(descr_dtm), ]$NC.1.0


# Visualizing text data – word clouds ####
library(wordcloud)
wordcloud(
  descr_corpus_clean,
  max.words = 20,
  min.freq = 1,
  random.order = F,
  colors = brewer.pal(8, "Dark2")
)

#subset of NC=0 and NC=1
NC <- subset(t4_train , NC.1.0 == 0)
med <- subset(t4_train, NC.1.0 == 1)

# This time, we'll use the max.words parameter to look at the 15 most common words in
# each of the two sets. The scale parameter allows us to adjust the maximum and
# minimum font size for words in the cloud

wordcloud(
  NC$Description,
  max.words = 20,
  min.freq = 1,
  scale = c(3, 0.5),
  colors = brewer.pal(3, "Dark2")
)

wordcloud(
  med$Description,
  max.words = 20,
  min.freq = 1,
  scale = c(3, 0.5),
  colors = brewer.pal(3, "Dark2")
)

# Data preparation – creating indicator features for
# frequent words ####

# The final step in the data preparation process is to transform the sparse matrix into a
# data structure that can be used to train a Naive Bayes classifier

# Currently, the sparse matrix includes over 2,500 features; this is a feature for every word that appears in at
# least one description

# It's unlikely that all of these are useful for classification.

#To reduce the number of features, we will eliminate any word that appear in less than 0.1 percent of the records in the training data.

# Finding frequent words requires use of the findFreqTerms() function in the
# tm package. This function takes a DTM and returns a character vector containing
# the words that appear for at least the specified number of times.

descr_freq_words <- findFreqTerms(descr_train, 2)

# We now need to filter our DTM to include only the terms appearing in a specified
# vector
descr_dtm_freq_train <- descr_train[, descr_freq_words]
descr_dtm_freq_test <- descr_test[, descr_freq_words]


#HEATMAP
# library(reshape2)
# library(ggplot2)
# tdm <- descr_dtm_freq_test
# df <- melt(as.matrix(tdm))
# df <- df[df$Terms %in% findFreqTerms(tdm, lowfreq = 10),]
# ggplot(df, aes(as.factor(Docs), Terms, fill = log(value))) + geom_tile() + xlab("Docs") + scale_fill_continuous(low =
#                                                                                                                   "#FEE6CE", high = "#E6550D")




# The Naive Bayes classifier is typically trained on data with categorical features.
# This poses a problem, since the cells in the sparse matrix are numeric and measure
# the number of times a word appears in a message. We need to change this to a
# categorical variable that simply indicates yes or no depending on whether the
# word appears at all.

convert_counts <- function(x) {
  x <- ifelse(x > 0, "Yes", "No")
}

# We now need to apply convert_counts() to each of the columns in our sparse
# matrix.

descr_train <- apply(descr_dtm_freq_train, MARGIN = 2, convert_counts)
descr_test <- apply(descr_dtm_freq_test, MARGIN = 2, convert_counts)

# The result will be two character type matrixes, each with cells indicating "Yes" or
# "No" for whether the word represented by the column appears at any point in the
# message represented by the row.

# Training a model on the data ####

# Now that we have transformed the raw SMS messages into a format that can be
# represented by a statistical model, it is time to apply the Naive Bayes algorithm. The
# algorithm will use the presence or absence of words to estimate the probability that a
# given app's description is not medical

library(e1071)

descr_classifier <- naiveBayes(descr_train, descr_train_labels)

# Evaluating model performance

# To evaluate, we need to test its predictions on
# unseen descriptions

# The predict() function is used to make the predictions

descr_test_pred <- predict(descr_classifier, descr_test)

# To compare the predictions to the true values, we'll use the CrossTable() function
# in the gmodels package

library(gmodels)

CrossTable(
  descr_test_pred,
  descr_test_labels,
  prop.chisq = F,
  prop.t = F,
  dnn = c('predicted', 'actual')
)

# This time we'll build a Naive Bayes model as done
# earlier, but this time set laplace=1

descr_classifier2 <-
  naiveBayes(descr_train, descr_train_labels, laplace = 1)

descr_test_pred2 <- predict(descr_classifier2, descr_test)

CrossTable(
  descr_test_pred2,
  descr_test_labels,
  prop.chisq = F,
  prop.t = F,
  dnn = c('predicted', 'actual')
)

library(caret)

confusionMatrix(descr_test_pred2, descr_test_labels,
                positive = "0")


#obtain predicted probabilities
descr_test_prob <- predict(descr_classifier2, descr_test, type = "raw")
head(descr_test_prob)

#combine the results in a df
descr_results <- data.frame(
  actual_type = descr_test_labels,
  predict_type = descr_test_pred2,
  prob_1 = round(descr_test_prob[, 2], 5),
  prob_0 = round(descr_test_prob[, 1], 5)
)

write.csv(descr_results, "descr_results.csv", row.names = F)

#Confusion matrix #1
CrossTable(descr_results$actual_type, descr_results$predict_type)

#Confusion matrix #2
confusionMatrix(descr_results$predict_type,
                descr_results$actual_type,
                positive = "1")

#ROC
library(ROCR)

pred <- prediction(descr_results$prob_1, descr_results$actual_type)

perf <- performance(pred, measure = "tpr", x.measure = "fpr")
plot(perf,
     main = "ROC curve for medical content",
     col = "blue",
     lwd = 2)

abline(a = 0,
       b = 1,
       lwd = 2,
       lty = 2)


#Area Under the Curve AUC
# calculate AUC
perf.auc <- performance(pred, measure = "auc")
str(perf.auc)
unlist(perf.auc@y.values)














#Prediction with new descriptions

#1 load 10 random description from 75k rows db

df<-read.csv2("Database_preprocessed_english.csv", header = T)%>%
  .[,c(1:3,5)]

df<-df[sample(nrow(df), 10000), ]
df$NC.1.0<-as.factor(NA)
df$Description<-as.character(df$Description)

df<-df[,c(4,5)]

#data prep
df_corpus <- VCorpus(VectorSource(df$Description))

df_corpus <- tm_map(df_corpus, removePunctuation)

df_corpus <- tm_map(df_corpus, function(x)removeWords(x,stopwords()))
wordcloud(df_corpus)

df_dtm<-DocumentTermMatrix(df_corpus, weightSMART)

df_dtm_labels <- df$NC.1.0

descr_freq_words_new <- findFreqTerms(df_dtm, 2)

findAssocs(df_dtm,"health",0.8)

df_dtm_freq<-df_dtm[, descr_freq_words]

df_test <- apply(df_dtm, MARGIN = 2, convert_counts)




tnew<-rbind(t4_train, df)
descr_corpus <- VCorpus(VectorSource(tnew$Description))
descr_dtm<-DocumentTermMatrix(descr_corpus,control = list(
  tolower=T,
  removeNumbers=T,
  stopwords=T,
  removePunctuation=T,
  stemming=T
))

n75 <- nrow(descr_dtm) * 0.95
n75next <- n75 + 1
descr_train <- descr_dtm[1:1341, ]
descr_test <- descr_dtm[1342:11341, ]

descr_train_labels <- t4_train[1:1341, ]$NC.1.0
descr_test_labels <- t4_train[1342:11341, ]$NC.1.0

descr_freq_words <- findFreqTerms(descr_train, 2)
descr_dtm_freq_train <- descr_train[, descr_freq_words]
descr_dtm_freq_test <- descr_test[, descr_freq_words]

convert_counts <- function(x) {
  x <- ifelse(x > 0, "Yes", "No")
}
descr_train <- apply(descr_dtm_freq_train, MARGIN = 2, convert_counts)
descr_test <- apply(descr_dtm_freq_test, MARGIN = 2, convert_counts)

descr_classifier <- naiveBayes(descr_train, descr_train_labels,laplace = 1)
descr_test_pred <- predict(descr_classifier, descr_test)

library(caret)
confusionMatrix(descr_test_pred, descr_test_labels,
                positive = "0")
