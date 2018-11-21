########################################################
rm(list = ls())                       
path <- "D:/NOTES/Analytics Notes/Practice/Hackathons/TITANIC - R/working"
setwd(path)
load(file = "titanic.RData")
save.image("D:/NOTES/Analytics Notes/Practice/Hackathons/TITANIC - R/working/titanic")
install.packages("aod", dependencies = T)
install.packages("Amelia", dependencies = T)
library(aod)
library(ggplot2)
library(Rcpp)
library(Amelia)
########################################################

train <- read.csv("D:/NOTES/Analytics Notes/Practice/Hackathons/TITANIC - R/working/train.csv", header = T, na.strings = c(""))

head(train)
summary(train)
sapply(train, function(x) sum(is.na(x)))
sapply(train, function(x) length(unique(x)))

missmap(train, main = "Missing values vs observed")

data <- subset(train, select = c(2,3,5,6,7,8,10,12))

data$Age[is.na(data$Age)] <- mean(data$Age,na.rm = T)
str(data)

is.factor(data$Sex)
is.factor(data$Embarked)

data <- data[!is.na(data$Embarked),]
rownames(data) <- NULL

traindata <- data[1:800,]
testdata <- data[801:889,]

model <- glm(as.factor(Survived) ~ ., family="binomial"(link='logit'),data=traindata)

traindata$fitted.results <- predict(model,newdata=traindata,type='response')
traindata$newfit <- ifelse(traindata$fitted.results > 0.5,1,0)

misClasificError <- mean(traindata$newfit != traindata$Survived)
print(paste('Accuracy',1-misClasificError))

