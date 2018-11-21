install.packages("tidytext", dependencies = T)

library(RTextTools)
library(e1071)

pos_tweets =  rbind(
  c('I love this car', 'positive'),
  c('This view is amazing', 'positive'),
  c('I feel great this morning', 'positive'),
  c('I am so excited about the concert', 'positive'),
  c('He is my best friend', 'positive')
)

neg_tweets = rbind(
  c('I do not like this car', 'negative'),
  c('This view is horrible', 'negative'),
  c('I feel tired this morning', 'negative'),
  c('I am not looking forward to the concert', 'negative'),
  c('He is my enemy', 'negative')
)

test_tweets = rbind(
  c('feel happy this morning', 'positive'),
  c('larry friend', 'positive'),
  c('not like that man', 'negative'),
  c('house not great', 'negative'),
  c('your song annoying', 'negative')
)

tweets = rbind(pos_tweets, neg_tweets, test_tweets)

# build dtm
matrix= create_matrix(tweets[,1], language="english", 
                      removeStopwords=FALSE, removeNumbers=TRUE, 
                      stemWords=FALSE) 

# train the model
mat = as.matrix(matrix)
classifier = naiveBayes(mat[1:10,], as.factor(tweets[1:10,2]) )

# test the validity
predicted = predict(classifier, mat[11:15,]); predicted
table(tweets[11:15, 2], predicted)
recall_accuracy(tweets[11:15, 2], predicted)

##################################################################

# build the data to specify response variable, training set, testing set.
container = create_container(matrix, as.numeric(as.factor(tweets[,2])),
                             trainSize=1:10, testSize=11:15,virgin=FALSE)

models = train_models(container, algorithms=c("MAXENT" , "SVM", "RF", "BAGGING", "TREE"))

results = classify_models(container, models)
View(results)

# accuracy table
table(as.numeric(as.factor(tweets[11:15, 2])), results[,"FORESTS_LABEL"])
table(as.numeric(as.factor(tweets[11:15, 2])), results[,"MAXENTROPY_LABEL"])

# recall accuracy
recall_accuracy(as.numeric(as.factor(tweets[11:15, 2])), results[,"FORESTS_LABEL"])
recall_accuracy(as.numeric(as.factor(tweets[11:15, 2])), results[,"MAXENTROPY_LABEL"])
recall_accuracy(as.numeric(as.factor(tweets[11:15, 2])), results[,"TREE_LABEL"])
recall_accuracy(as.numeric(as.factor(tweets[11:15, 2])), results[,"BAGGING_LABEL"])
recall_accuracy(as.numeric(as.factor(tweets[11:15, 2])), results[,"SVM_LABEL"])

# model summary
analytics = create_analytics(container, results)
summary(analytics)
head(analytics@document_summary)
analytics@ensemble_summar

N=4
set.seed(2014)
cross_validate(container,N,"MAXENT")
cross_validate(container,N,"TREE")
cross_validate(container,N,"SVM")
cross_validate(container,N,"RF")

##################################################################
rm(list = ls())
###################
"load data"
###################
setwd("D:/NOTES/Analytics Notes/Practice/Text Mining & Web Scraping/Twitter-Sentimental-Analysis-master")
happy = readLines("D:/NOTES/Analytics Notes/Practice/Text Mining & Web Scraping/Twitter-Sentimental-Analysis-master/happy.txt")
sad = readLines("D:/NOTES/Analytics Notes/Practice/Text Mining & Web Scraping/Twitter-Sentimental-Analysis-master/sad.txt")
happy_test = readLines("D:/NOTES/Analytics Notes/Practice/Text Mining & Web Scraping/Twitter-Sentimental-Analysis-master/happy_test.txt")
sad_test = readLines("D:/NOTES/Analytics Notes/Practice/Text Mining & Web Scraping/Twitter-Sentimental-Analysis-master/sad_test.txt")

tweet = c(happy, sad)
tweet_test= c(happy_test, sad_test)
tweet_all = c(tweet, tweet_test)
sentiment = c(rep("happy", length(happy) ), 
              rep("sad", length(sad)))
sentiment_test = c(rep("happy", length(happy_test) ), 
                   rep("sad", length(sad_test)))
sentiment_all = as.factor(c(sentiment, sentiment_test))

library(RTextTools)

# naive bayes
mat= create_matrix(tweet_all, language="english", 
                   removeStopwords=FALSE, removeNumbers=TRUE, 
                   stemWords=FALSE, tm::weightTfIdf)

mat = as.matrix(mat)

classifier = naiveBayes(mat[1:160,], as.factor(sentiment_all[1:160]))
predicted = predict(classifier, mat[161:180,]); predicted

table(sentiment_test, predicted)
recall_accuracy(sentiment_test, predicted)

# the other methods
mat= create_matrix(tweet_all, language="english", 
                   removeStopwords=FALSE, removeNumbers=TRUE, 
                   stemWords=FALSE, tm::weightTfIdf)

container = create_container(mat, as.numeric(sentiment_all),
                             trainSize=1:160, testSize=161:180,virgin=FALSE) #????????????removeSparseTerms

models = train_models(container, algorithms=c("MAXENT",
                                              "SVM",
                                              #"GLMNET", "BOOSTING", 
                                              "SLDA","BAGGING", 
                                              "RF", # "NNET", 
                                              "TREE" 
))

# test the model
results = classify_models(container, models)
table(as.numeric(as.numeric(sentiment_all[161:180])), results[,"FORESTS_LABEL"])
recall_accuracy(as.numeric(as.numeric(sentiment_all[161:180])), results[,"FORESTS_LABEL"])

# formal tests
analytics = create_analytics(container, results)
summary(analytics)

head(analytics@algorithm_summary)
head(analytics@label_summary)
head(analytics@document_summary)
analytics@ensemble_summary # Ensemble Agreement

# Cross Validation
N=3
cross_SVM = cross_validate(container,N,"SVM")
cross_GLMNET = cross_validate(container,N,"GLMNET")
cross_MAXENT = cross_validate(container,N,"MAXENT")

##################################################################
rm(list = ls())
install.packages("plyr")
install.packages("ggplot2")
install.packages("wordcloud")
install.packages("RColorBrewer")
install.packages("tm")
install.packages("SnowballC")
install.packages("devtools")
require(devtools)
library(plyr)
library(ggplot2)
library(wordcloud)
library(RColorBrewer)
library(tm)
library(SnowballC)

data <- read_data ("D:/NOTES/Analytics Notes/Practice/Text Mining & Web Scraping/RDataMining-Tweets-20160203.rdata")
install_url("http://www.omegahat.org/Rstem/Rstem_0.4-1.tar.gz")
install_url("http://cran.r-project.org/src/contrib/Archive/sentiment/sentiment_0.1.tar.gz")
install_url("http://cran.r-project.org/src/contrib/Archive/sentiment/sentiment_0.2.tar.gz")

dfb <- do.call("rbind", lapply(rdmTweets, as.data.frame))
dim(df)


library(tm)
# build a corpus, which is a collection of text documents
# VectorSource specifies that the source is character vectors.
myCorpus <- Corpus(VectorSource(df$text))

##################################################################

library(dplyr)
library(tidyr)
library(purrr)
library(readr)

training_folder <- "D:/NOTES/Analytics Notes/Practice/Text Mining & Web Scraping/20news-bydate/20news-bydate-train/"

# Define a function to read all files from a folder into a data frame
read_folder <- function(infolder) {
  data_frame(file = dir(infolder, full.names = TRUE)) %>%
    mutate(text = map(file, read_lines)) %>%
    transmute(id = basename(file), text) %>%
    unnest(text)
}

# Use unnest() and map() to apply read_folder to each subfolder
raw_text <- data_frame(folder = dir(training_folder, full.names = TRUE)) %>%
  unnest(map(folder, read_folder)) %>%
  transmute(newsgroup = basename(folder), id, text)

raw_text

library(ggplot2)

raw_text %>%
  group_by(newsgroup) %>%
  summarize(messages = n_distinct(id)) %>%
  ggplot(aes(newsgroup, messages)) +
  geom_col() +
  coord_flip()

library(stringr)

cleaned_text <- raw_text %>%
  group_by(newsgroup, id) %>%
  filter(cumsum(text == "") > 0,
         cumsum(str_detect(text, "^--")) == 0) %>%
  ungroup()

cleaned_text <- cleaned_text %>%
  filter(str_detect(text, "^[^>]+[A-Za-z\\d]") | text == "",
         !str_detect(text, "writes(:|\\.\\.\\.)$"),
         !str_detect(text, "^In article <"),
         !id %in% c(9704, 9985))

library(tidytext)

usenet_words <- cleaned_text %>%
  unnest_tokens(word, text) %>%
  filter(str_detect(word, "[a-z']$"),
         !word %in% stop_words$word)

usenet_words %>%
  count(word, sort = TRUE)

