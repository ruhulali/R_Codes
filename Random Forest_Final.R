setwd("Z:/CT-Mum/Cello Health/170223 Segmentation/Working/CHAID and Random Forest")

# --------------------------------Data Preprocessing ------------------------------

data3 <- read.csv("UC+CD for JAK.csv", header=T, sep = ",")

data3$Q2 = as.factor(data3$Q2)
data3$p0 = as.factor(data3$p0)
data3$p1 = as.factor(data3$p1)
data3$p2 = as.factor(data3$p2)
data3$p21a = as.factor(data3$p21a)

str(data3)

data3 <- data3[,-c(2:18)]

# --------------------------------- Random Forest-------------------------

# Installing Required Packages
# install.packages("party")
# install.packages("randomForest")

# Load the party package. It will automatically load other required packages.

library(party)
library(randomForest)

#Find the optimal mtry value          #Select mtry value with minimum out of bag(OOB) error.
  
mtry <- tuneRF(data3[-1],data3$Q2, ntreeTry=1000,stepFactor=1.5,improve=0.01, trace=TRUE, plot=TRUE)
best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]
print(mtry)
print(best.m)

# Plotting both Test Error and Out of Bag Error
  
#matplot(1:mtry , cbind(oob.err,test.err), pch=19 , col=c("red","blue"),type="b",ylab="Mean Squared Error",xlab="Number of Predictors Considered at each Split")
#legend("topright",legend=c("Out of Bag Error","Test Error"),pch=19, col=c("red","blue"))
  
# Creating the forest
    
output.forest <- randomForest(Q2 ~ ., ntree = 1000,importance=TRUE, data = data3, mtry=15)
#getTree(output.forest, 1)
  
# Plot 
plot(output.forest)  
  
# View the forest results.
print(output.forest) 

# Importance of each predictor.
print(importance(output.forest,type = 2))
print(importance(output.forest,type = 1))

# Variable Importance Plot
varImpPlot(output.forest,sort = T,main="Variable Importance", n.var=15)

# Variable Importance Table
  
# MeanGini
var.imp1 <- data.frame(importance(output.forest,type=2))
var.imp1$Variables <- row.names(var.imp1)
Mean_Gini = var.imp1[order(var.imp1$MeanDecreaseGini,decreasing = T),]
capture.output(Mean_Gini, file = "Mean_Gini_excluding_q3.csv", append = FALSE)
  
# MeanAccuracy
var.imp2 <- data.frame(importance(output.forest,type=1))
var.imp2$Variables <- row.names(var.imp2)
Mean_Accuracy = var.imp2[order(var.imp2$MeanDecreaseAccuracy,decreasing = T),]
capture.output(Mean_Accuracy, file = "Mean_Accuracy_excluding_q3.csv", append = FALSE)
  
# ------------------ CHAID nahi yeh CART hai ------------------

library(rpart)
library(rpart.plot)
library(RColorBrewer)
library(party)
library(partykit)
library(caret)
library(grid)
set.seed(123)
ctrl<- ctree_control(mincriterion = 0.05, minsplit = 50, minbucket = 25)
fit <- ctree(Q2~ ., data=data3, control=ctrl)
print(fit)
plot(fit,main="Conditional Inference Tree")
  
  
#Tree using rpart
  
tree.1 <- rpart(Q2~ .,data=data3,control=rpart.control(minsplit=50, minbucket = 25,cp=0))
plot(tree.1)
text(tree.1)
prp(tree.1)

  
  