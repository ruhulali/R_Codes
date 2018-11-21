setwd("Z:/CT-Mum/Cello Health/170223 Segmentation/Working/CHAID_Random Forest/Wotking_9thApril/Data for Random Forest")
data3 <- read.csv("UC+CD for Entyvio SC.csv", header=T, sep = ",")

str(data3)

data3$Q2 = as.factor(data3$Q2)
data3$p0 = as.factor(data3$p0)
data3$p1 = as.factor(data3$p1)
data3$p2 = as.factor(data3$p2)
data3$p21a = as.factor(data3$p21a)

library(rpart)
library(caret)
library(lattice)
library(ggplot2)

modfit <- train(Q2~.,method="rpart",data=data3) 
library(rattle)
fancyRpartPlot(modfit$finalModel)

detach(package:rattle)
install.packages("rattle")

options(repos = "http://cran.stat.auckland.ac.nz")
install.packages(c("gWidgets", "RGtk2", "gWidgetsRGtk2", "devtools"))

-----------------------------------------
# CHAID #  
  
install.packages("CHAID", dependencies = T)
install.packages("CHAID", repos="http://R-Forge.R-project.org")

library("CHAID")

### fit tree to subsample
set.seed(123456)
USvoteS <- USvote[sample(1:nrow(USvote), 1000),]

ctrl <- chaid_control(minsplit=20, minprob=0.05)
chaidUS <- chaid(Q2 ~ ., data=data3, control=ctrl)

print(chaidUS)
plot(chaidUS)

----------------------------------# This does mekes some tree just check 

# CHAID 
library(partykit)
library(grid)
set.seed(777)
ctrl<- ctree_control(mincriterion = 0.05, minsplit = 100, minbucket = 25)
fit <- ctree(Q2~ ., data=data3, control=ctrl)
print(fit)
plot(fit,main="Conditional Inference Tree")

-----------------------------------------
# Decision Tree #
    
library(rpart)
x <- cbind(x_train,y_train)

# grow tree 
fit <- rpart(Q2 ~ ., data = data3,method="class")
summary(fit)

#Predict Output 
predicted= predict(fit,x_test)

----------------------------------------
# Random Forest #
  
library(randomForest)
x <- cbind(x_train,y_train)

# Fitting model
fit <- randomForest(Q2 ~ ., data3,ntree=1000)
summary(fit)

#Predict Output 
predicted= predict(fit,x_test)

----------------

# Load the party package. It will automatically load other required packages.
library(party)
library(randomForest)

# Create the forest.
output.forest <- randomForest(Q2 ~ ., ntree = 1000,importance=TRUE, data = data3)

# View the forest results.
print(output.forest) 

# Importance of each predictor.
print(importance(fit,type = 2)) 

#or

# Variable Importance Plot
varImpPlot(output.forest, sort = T, main="Variable Importance", n.var=10)
# or

# Variable Importance Table
var.imp <- data.frame(importance(output.forest,type=2))
var.imp$Variables <- row.names(var.imp)
var.imp[order(var.imp$MeanDecreaseGini,decreasing = T),]

# Plot 
plot(output.forest)

