rm(list = ls())
data(cars)
View(cars)

install.packages("DAAG", dependencies = T)

dim(cars)
names(cars)
psych::describe(cars)
mlr::summarizeColumns(cars)
colSums(is.na(cars))

hist(cars$dist, breaks=100, main="dist Chart", xlab="cars")
boxplot(cars$speed, main="Speed")
boxplot(cars$dist, main="Distance")
par(mfrow=c(1, 2))  # divide graph area in 2 columns
par(mfrow=c())  # divide graph area in 2 columns

hist(cars$dist, border=F , col=rgb(0.2,0.2,0.8,0.7) , main="dist")
boxplot(cars$dist , col="grey" , xlab="dist")

scatter.smooth(x=cars$speed, y=cars$dist, main="Dist ~ Speed")

cor(cars$speed, cars$dist)

linearMod <- lm(dist ~ speed, data=cars)
print(linearMod)
summary(linearMod)
plot(linearMod)
AIC(linearMod)
BIC(linearMod)

# Create Training and Test data -
set.seed(100)  # setting seed to reproduce results of random sampling
trainingRowIndex <- sample(1:nrow(cars), 0.8*nrow(cars))  # row indices for training data
trainingData <- cars[trainingRowIndex, ]  # model training data
testData  <- cars[-trainingRowIndex, ]   # test data

# Build the model on training data
lmMod <- lm(dist ~ speed, data=trainingData)  # build the model
distPred <- predict(lmMod, testData)  # predict distance
summary(lmMod)
AIC(lmMod)
BIC(lmMod)

actuals_preds <- data.frame(cbind(actuals=testData$dist, predicteds=distPred))  # make actuals_predicteds dataframe.
correlation_accuracy <- cor(actuals_preds)  # 82.7%
correlation_accuracy
head(actuals_preds)

# Min-Max Accuracy Calculation
min_max_accuracy <- mean(apply(actuals_preds, 1, min) / apply(actuals_preds, 1, max))  
min_max_accuracy

# MAPE (mean absolute percentage deviation) Calculation
mape <- mean(abs((actuals_preds$predicteds - actuals_preds$actuals))/actuals_preds$actuals)  
# => 69.95%, mean absolute percentage deviation
mape


DMwR::regr.eval(actuals_preds$actuals, actuals_preds$predicteds)


library(DAAG)
cvResults <- suppressWarnings(CVlm(data=cars, form.lm=dist ~ speed, m=5, dots=FALSE, seed=29, legend.pos="topleft",  printit=FALSE, main="Small symbols are predicted values while bigger ones are actuals."))  # performs the CV
attr(cvResults, 'ms')  
# => 251.2783 mean squared error
