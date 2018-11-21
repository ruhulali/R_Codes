rm(list = ls())
getwd()
setwd('D:/Sentiment Analysis/new')

# Natural Language Processing

# Importing the dataset
dataset_original = read.delim('verbatim_cleaned.txt', quote = '', stringsAsFactors = FALSE)
View(dataset_original)

dataset_original <- NULL
library(readr)
write.csv(dataset_original, "D:/Sentiment Analysis/new/verbatim_cleaned_rexport.csv", row.names = T)

# Cleaning the texts
install.packages('tm')
install.packages('SnowballC')

library(NLP)
library(tm)
library(SnowballC)

corpus = VCorpus(VectorSource(dataset_original$Comments))
as.character(corpus[[1]])
corpus = tm_map(corpus, content_transformer(tolower))
as.character(corpus[[2]])
corpus = tm_map(corpus, removeNumbers)
as.character(corpus[[92]])
corpus = tm_map(corpus, removePunctuation)
as.character(corpus[[92]])
corpus = tm_map(corpus, removeWords, stopwords())
as.character(corpus[[92]])
corpus = tm_map(corpus, stemDocument)
as.character(corpus[[5]])
corpus = tm_map(corpus, stripWhitespace)
as.character(corpus[[92]])

# Creating the Bag of Words model
dtm = DocumentTermMatrix(corpus)
dtm = removeSparseTerms(dtm, 0.999)
dataset = as.data.frame(as.matrix(dtm))
dataset$Sentiment = dataset_original$Sentiment


# Encoding the target feature as factor
dataset[,550] = factor(dataset[,550],levels = c('Negative','Neutral','Positive'),labels = c(1,2,3))
table(dataset[,550])

# Splitting the dataset into the Training set and Test set

install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(dataset$Sentiment, SplitRatio = 0.8)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

# Fitting Random Forest Classification to the Training set

install.packages('randomForest')
library(randomForest)
classifier = randomForest(x = training_set[-550],
                          y = training_set$Sentiment,
                          ntree = 10)

# Predicting the Test set results
y_pred = predict(classifier, newdata = test_set[-550])
View(as.matrix(y_pred))

# Making the Confusion Matrix
cm = table(test_set[, 550], y_pred)
recall_accuracy(test_set[, 550], y_pred)

#cm
#cm1 <- as.data.frame(cm)
#x <- sum(cm1$Freq)
#y <- sum(c(cm1[1,3]+cm1[5,3]+cm1[9,3]))
#z <- y/x*100
#View(z)


#Other Classification Models


# Naive Bayes

library(e1071)
library(RTextTools)

classifier = naiveBayes(training_set[1:54270,-550], training_set[1:54270,550])
predicted = predict(classifier, test_set[1:13567,-550]); predicted

table(test_set$Sentiment, predicted)
recall_accuracy(test_set$Sentiment, predicted)


#Trying different models

#container = create_container(dtm, as.numeric(dataset_original$Sentiment),
            #trainSize=1:54270, testSize=1:13567,virgin=FALSE)

#models = train_models(container, algorithms=c("MAXENT", "BAGGING", "TREE"))

################################### seed test ###################################

#New Seed

set.seed(5343)
split = sample.split(dataset$Sentiment, SplitRatio = 0.8)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

# Fitting Random Forest Classification to the Training set
#install.packages('randomForest')
library(randomForest)
classifier = randomForest(x = training_set[-547],
                          y = training_set$Sentiment,
                          ntree = 10)

# Predicting the Test set results
y_pred = predict(classifier, newdata = test_set[-547])
View(as.matrix(y_pred))


# Making the Confusion Matrix

cm = as.matrix(table(test_set[, 547], y_pred))
cm


#New Seed

set.seed(35357)
split = sample.split(dataset$Sentiment, SplitRatio = 0.8)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

# Fitting Random Forest Classification to the Training set
#install.packages('randomForest')
library(randomForest)
classifier = randomForest(x = training_set[-547],
                          y = training_set$Sentiment,
                          ntree = 10)

# Predicting the Test set results
y_pred = predict(classifier, newdata = test_set[-547])
View(as.matrix(y_pred))

# Making the Confusion Matrix

cm = as.matrix(table(test_set[, 547], y_pred))
cm


#New Seed

set.seed(8462)
split = sample.split(dataset$Sentiment, SplitRatio = 0.8)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

# Fitting Random Forest Classification to the Training set
#install.packages('randomForest')
library(randomForest)
classifier = randomForest(x = training_set[-547],
                          y = training_set$Sentiment,
                          ntree = 10)

# Predicting the Test set results
y_pred = predict(classifier, newdata = test_set[-547])
View(as.matrix(y_pred))

# Making the Confusion Matrix

cm = as.matrix(table(test_set[, 547], y_pred))

cm1 <- as.data.frame(cm)
x <- sum(cm1$Freq)
y <- sum(c(cm1[1,3]+cm1[5,3]+cm1[9,3]))
z <- y/x*100
View(z)


#New Seed

set.seed(75315)
split = sample.split(dataset$Sentiment, SplitRatio = 0.8)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

# Fitting Random Forest Classification to the Training set
#install.packages('randomForest')
library(randomForest)
classifier = randomForest(x = training_set[-547],
                          y = training_set$Sentiment,
                          ntree = 10)

# Predicting the Test set results
y_pred = predict(classifier, newdata = test_set[-547])
View(as.matrix(y_pred))

# Making the Confusion Matrix

cm = as.matrix(table(test_set[, 547], y_pred))
cm

cm1 <- as.data.frame(cm)
x <- sum(cm1$Freq)
y <- sum(c(cm1[1,3]+cm1[5,3]+cm1[9,3]))
z <- y/x*100
View(z)
