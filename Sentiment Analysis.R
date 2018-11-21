find_funs("create_matrix") 
source("https://sebastiansauer.github.io/Rcode/find_funs.R")
install.packages("tibble")
library(tibble)

rm(list = ls())
getwd()
setwd('E:/NOTES/Analytics Notes/Sentiment Analysis')

# Natural Language Processing

# Importing the dataset
library(readr)
dataset_original = read.delim('Ebay_Comments_Revised.txt', quote = '', stringsAsFactors = FALSE)
View(as.matrix.data.frame(dataset_original))
dataset=as.matrix.data.frame(dataset_original)
View(dataset)

matrix= create_matrix(dataset[,2], language="english", 
                      removeStopwords=TRUE, removeNumbers=TRUE, 
                      stemWords=TRUE) 
View(as.matrix(matrix))

library(caTools)
set.seed(123)
split = sample.split(dataset$Review, SplitRatio = 0.9)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

# Cleaning the texts

install.packages("tm", dependencies = T)
install.packages("SnowballC", dependencies = T)
install.packages("NLP", dependencies = T)

library(tm)
library(SnowballC)

corpus = VCorpus(VectorSource(test_set$Comments))
as.character(corpus[[1]])
corpus = tolower(corpus)
as.character(corpus[[2]])
#corpus = tm_map(corpus, removeNumbers)
corpus = removeNumbers(corpus)
as.character(corpus[[1]])
#corpus = tm_map(corpus, removePunctuation)
corpus = removePunctuation(corpus)
as.character(corpus[[1]])
#corpus = tm_map(corpus, removeWords, stopwords())

corpus = tm_map(corpus, stemDocument)
corpus = tm_map(corpus, stripWhitespace)

# Creating the Bag of Words model
dtm = DocumentTermMatrix(corpus)
dtm = removeSparseTerms(dtm, 0.999)
dataset = as.data.frame(as.matrix(dtm))
dataset$Liked = dataset_original$Liked

# Importing the dataset
dataset = read.csv('Social_Network_Ads.csv')
dataset = dataset[3:5]

# Encoding the target feature as factor
dataset$Liked = factor(dataset$Liked, levels = c(0, 1))

# Splitting the dataset into the Training set and Test set
# install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(dataset$Liked, SplitRatio = 0.8)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

# Fitting Random Forest Classification to the Training set
# install.packages('randomForest')
library(randomForest)
classifier = randomForest(x = training_set[-692],
                          y = training_set$Liked,
                          ntree = 10)

# Predicting the Test set results
y_pred = predict(classifier, newdata = test_set[-692])

# Making the Confusion Matrix
cm = table(test_set[, 692], y_pred)