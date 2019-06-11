library(dplyr)
library(hflights)
View(hflights)

############################################## data check, structure and summary of data ####
class(hflights)
names(hflights)                         
dim(hflights)                           
str(hflights)                           
summary(hflights)
psych::describe(hflights)               
mlr::summarizeColumns(hflights)

############################################## finding missing value ####
sort(colSums(is.na(hflights)), decreasing = T)
table(is.na(hflights))                  

# or

NAcol <- which(colSums(is.na(hflights)) > 0)
sort(colSums(sapply(hflights[NAcol], is.na)), decreasing = TRUE)
cat('There are', length(NAcol), 'columns with missing values')

#get percentage of missing value of the attributes - Approach 2 (Function)
sapply(hflights, function(df)
{
  round(sum(is.na(df)==T)/length(df)*100,2)
})

#Approach - Amelia Package
# install.packages("Amelia")
library("Amelia")
missmap(hflights, main = "Missing Map")

############################################## EDA ####
library(corrplot)
library(ggplot2)
library(scales) # for percentage scales                    

############################################## Correlations ####
numericVars <- which(sapply(hflights, is.numeric)) #index vector numeric variables
numericVarNames <- names(numericVars) #saving names vector for use later on
cat('There are', length(numericVars), 'numeric variables')
all_numVar <- hflights[, numericVars]
cor_numVar <- cor(all_numVar, use="pairwise.complete.obs") #correlations of all numeric variables
cor_sorted <- as.matrix(sort(cor_numVar[,'Month'], decreasing = TRUE)) #sort on decreasing correlations with target variable
CorHigh <- names(which(apply(cor_sorted, 1, function(x) abs(x)>0.5))) #select only high corelations
cor_numVar <- cor_numVar[CorHigh, CorHigh]
corrplot.mixed(cor_numVar, tl.col="black", tl.pos = "lt")

### simple correlation ###
round(cor(hflights[, sapply(hflights, is.numeric)])*100,2)

### corrplot ###
corrplot(cor(hflights[, sapply(hflights, is.numeric)]))

t <- cor(hflights[, sapply(hflights, is.numeric)])
corrplot(t, method = "number")
corrplot.mixed(t)

### plot correlation ###
library(GGally)
pairs(hflights)

# or

ggpairs(data=hflights, title="hflights data",
        mapping=ggplot2::aes(colour = sex),
        lower=list(combo=wrap("facethist",binwidth=1)))


##Data Manipulation
##-----------------

## Select
select(hflights, FlightNum, ArrTime, DepTime) -> flight1
select(hflights, contains("Time")) -> flight1
select(hflights, starts_with("Day"), ends_with("Time")) -> flight1

## Mutate
mutate(hflights, ActualGroundTime = ActualElapsedTime - AirTime) -> flight1
mutate(flight1, AverageSpeed = Distance / AirTime * 60) -> flight1
mutate(flight1, TotalTaxii = TaxiIn + TaxiOut) -> flight1
mutate(flight1, TimeLoss = ArrDelay + DepDelay) -> flight1

## Filter
filter(hflights, Distance > 3000) -> flight1
filter(hflights, TaxiIn + TaxiOut > AirTime) -> flight1

## Arrange
arrange(hflights, DepDelay) -> flight1
arrange(hflights, DepDelay + ArrDelay) -> flight1

## Summarise
summarise(hflights, min_dist = min(Distance), max_dist = max(Distance))

## Pipe
hflights %>% select(contains("Time")) %>% filter(AirTime > 60) -> flight1


## Data Visualization
##-------------------

house <- read.csv(file.choose())
library(ggplot2)

## Histogram
ggplot(data= house, aes(x= price)) + geom_histogram()

## Bar Plot
ggplot(data= house, aes(x= centralAir)) + geom_bar(fill="orange")

## Scatter Plot
ggplot(data= house, aes(y= price, x= livingArea, col= factor(rooms))) + geom_point()

## Box Plot
ggplot(data= house, aes(y= price, x= factor(rooms), fill= factor(rooms))) + geom_boxplot()


## Linear Regression
##------------------

View(mtcars)
library(caTools)

sample.split(mtcars$mpg, SplitRatio = 0.65) -> mysplit
train <- subset(mtcars, mysplit == T)
test <- subset(mtcars, mysplit == F)
nrow(train)
nrow(test)

##---------
lm(mpg~., data= train) -> mod1
predict(mod1, test) -> result
View(result)

cbind(actual= test$mpg, predicted= result) -> final
View(final)

as.data.frame(final) -> final
cbind(final, error= final$actual - final$predicted) -> final
View(final)

rmse1 <- sqrt(mean((final$error)^2))
rmse1

##---------
lm(mpg~.-gear,-carb, data= train) -> mod2
predict(mod2, test) -> result
View(result)

cbind(actual= test$mpg, predicted= result) -> final
View(final)

as.data.frame(final) -> final
cbind(final, error= final$actual - final$predicted) -> final
View(final)

rmse2 <- sqrt(mean((final$error)^2))
rmse2


## Classification
##---------------

census_data <- read.csv(file.choose())
library(rpart) # recursive partioning
library(caTools)

sample.split(census_data$salary, SplitRatio = 0.65) -> mysplit
train <- subset(census_data, mysplit == T)
test <- subset(census_data, mysplit == F)
nrow(train)
nrow(test)

rpart(salary~., data= train, method= "class") -> mod_classify
predict(mod_classify, test, type= "class") -> result

library(caret)
library(e1071)
confusionMatrix(test$salary, result)


## Clustering
##-----------

data("iris")
View(iris)
iris[,-5] -> iris4
kmeans(iris4,3) ->k1
cbind(iris4, k1$cluster) -> iris4
cbind(iris4, iris$Species) -> iris4
table(iris4$`k1$cluster`)

