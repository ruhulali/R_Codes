rm(list = ls())
install.packages("e1071", dependencies = TRUE)
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

####################################################################

# build dtm
matrix= create_matrix(tweets[,1], language="english", 
                      removeStopwords=FALSE, removeNumbers=TRUE, 
                      stemWords=FALSE) 

####################################################################

# train the model
mat = as.matrix(matrix)
classifier = naiveBayes(mat[1:10,], as.factor(tweets[1:10,2]) )
View(mat)
####################################################################

# test the validity
predicted = predict(classifier, mat[11:15,]); predicted
table(tweets[11:15, 2], predicted)
recall_accuracy(tweets[11:15, 2], predicted)

####################################################################

#build the data to specify response variable, training set, testing set.
container = create_container(matrix, as.numeric(as.factor(tweets[,2])),
                             trainSize=1:10, testSize=11:15,virgin=FALSE)

####################################################################

models = train_models(container, algorithms=c("MAXENT" , "SVM", "RF", "BAGGING", "TREE"))

####################################################################

results = classify_models(container, models)

####################################################################

# accuracy table
table(as.numeric(as.factor(tweets[11:15, 2])), results[,"FORESTS_LABEL"])
table(as.numeric(as.factor(tweets[11:15, 2])), results[,"MAXENTROPY_LABEL"])

# recall accuracy
recall_accuracy(as.numeric(as.factor(tweets[11:15, 2])), results[,"FORESTS_LABEL"])
recall_accuracy(as.numeric(as.factor(tweets[11:15, 2])), results[,"MAXENTROPY_LABEL"])
recall_accuracy(as.numeric(as.factor(tweets[11:15, 2])), results[,"TREE_LABEL"])
recall_accuracy(as.numeric(as.factor(tweets[11:15, 2])), results[,"BAGGING_LABEL"])
recall_accuracy(as.numeric(as.factor(tweets[11:15, 2])), results[,"SVM_LABEL"])

####################################################################

# model summary
analytics = create_analytics(container, results)
summary(analytics)
head(analytics@document_summary)
analytics@ensemble_summary

####################################################################

N=4
set.seed(2014)
cross_validate(container,N,"MAXENT")
cross_validate(container,N,"TREE")
cross_validate(container,N,"SVM")
cross_validate(container,N,"RF")

###################
#Sentiment analysis for tweets
"load data"
###################
setwd("D:/Sentiment Analysis/practise/Twitter-Sentimental-Analysis-master/")
happy = readLines("./happy.txt")
sad = readLines("./sad.txt")
happy_test = readLines("./happy_test.txt")
sad_test = readLines("./sad_test.txt")

tweet = c(happy, sad)
tweet_test= c(happy_test, sad_test)
tweet_all = c(tweet, tweet_test)
sentiment = c(rep("happy", length(happy) ), 
              rep("sad", length(sad)))
sentiment_test = c(rep("happy", length(happy_test) ), 
                   rep("sad", length(sad_test)))
sentiment_all = as.factor(c(sentiment, sentiment_test))
View(sentiment_test)
library(RTextTools)

####################################################################

# naive bayes
mat= create_matrix(tweet_all, language="english", 
                   removeStopwords=FALSE, removeNumbers=TRUE, 
                   stemWords=FALSE, tm::weightTfIdf)

mat = as.matrix(mat)
View(mat)
classifier = naiveBayes(mat[1:160,], as.factor(sentiment_all[1:160]))
predicted = predict(classifier, mat[161:180,]); predicted

table(sentiment_test, predicted)
recall_accuracy(sentiment_test, predicted)

####################################################################

mat= create_matrix(tweet_all, language="english", 
                   removeStopwords=TRUE, removeNumbers=TRUE, 
                   stemWords=TRUE, tm::weightTfIdf)

####################################################################

setwd('D:/Sentiment Analysis')

# Importing the dataset
library(readr)
dataset_original = read.delim('Ebay_Comments_Revised.txt', quote = '', stringsAsFactors = FALSE)
View(as.matrix.data.frame(dataset_original))
tweets=dataset_original
