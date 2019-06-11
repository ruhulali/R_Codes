rm(list = ls())

#### measure execution time of a program ####
start.time <- Sys.time()           
runif(5555,1,1000)
end.time <- Sys.time()
end.time - start.time


#### finding functions in a package ####
find_funs("varImp") 
source("https://sebastiansauer.github.io/Rcode/find_funs.R")

install.packages("sos")
findFn("plotFilterValuesGGVIS")

#### upload / sample data ####
churn <- read.csv(file.choose())
View(churn)

#### EDA ####
library(DataExplorer)

plot_str(churn)
plot_missing(churn)
plot_histogram(churn)
plot_density(churn)
plot_correlation(churn, type = 'continuous','Exited')
plot_bar(churn)
create_report(churn)

#### dropping variable ####
dfa$dfb <- NULL                     
dfa <- NULL                         

# or 

library(dplyr)                      
df = select(mydata, -c(x,y,z))

#### data check, structure and summary of data ####
class(churn)
names(churn)                         
dim(churn)                           
str(churn)                           
summary(churn)
psych::describe(churn)               
mlr::summarizeColumns(churn)

#### finding missing value ####
colSums(is.na(churn))                
table(is.na(churn))                  

# or

NAcol <- which(colSums(is.na(churn)) > 0)
sort(colSums(sapply(churn[NAcol], is.na)), decreasing = TRUE)
cat('There are', length(NAcol), 'columns with missing values')

#### variable conversion ####
library(dplyr)

churn$CreditScore <- as.numeric(churn$CreditScore)
churn$Surname <- as.character(churn$Surname)
churn$NumOfProducts <- as.factor(churn$NumOfProducts)
churn$HasCrCard <- as.factor(churn$HasCrCard)
churn$IsActiveMember <- as.factor(churn$IsActiveMember)
churn$Exited <- as.factor(churn$Exited)

#### renaming ####
df= rename(df, variable1=var1)      
trimws()                            

#### EDA ####
library(corrplot)
library(ggplot2)
library(scales) # for percentage scales                    

#### Correlations ####
numericVars <- which(sapply(churn, is.numeric)) #index vector numeric variables
numericVarNames <- names(numericVars) #saving names vector for use later on
cat('There are', length(numericVars), 'numeric variables')
all_numVar <- churn[, numericVars]
cor_numVar <- cor(all_numVar, use="pairwise.complete.obs") #correlations of all numeric variables
cor_sorted <- as.matrix(sort(cor_numVar[,'tip'], decreasing = TRUE)) #sort on decreasing correlations with SalePrice
CorHigh <- names(which(apply(cor_sorted, 1, function(x) abs(x)>0.5))) #select only high corelations
cor_numVar <- cor_numVar[CorHigh, CorHigh]
corrplot.mixed(cor_numVar, tl.col="black", tl.pos = "lt")

### simple correlation ###
round(cor(churn[, sapply(churn, is.numeric)])*100,2)

### corrplot ###
corrplot(cor(churn[, sapply(churn, is.numeric)]))

t <- cor(churn[, sapply(churn, is.numeric)])
corrplot(t, method = "number")
corrplot.mixed(t)

### plot correlation ###
library(GGally)

churn1 <- select(churn,-c(Surname))
pairs(churn)

# or

ggpairs(churn1)
ggpairs(data=churn2, title="churn data",
        mapping=ggplot2::aes(colour = Gender),
        lower=list(combo=wrap("facethist",binwidth=1)))

#### small function to display plots only if it's interactive ####
p_ <- GGally::print_if_interactive

pm <- ggpairs(data= churn2, mapping=ggplot2::aes(colour = Gender))
p_(pm)

pm <- ggpairs(data= churn, 1:3, mapping=ggplot2::aes(colour = sex), columnLabels = c("Total Bill", "Tip", "Sex"))
p_(pm)

pm <- ggpairs(data= churn, mapping=ggplot2::aes(colour = sex), upper = "blank")
p_(pm)

### Plot Types
### Change default plot behavior
pm <- ggpairs(
  churn[, c(1, 3, 4, 2)],
  upper = list(continuous = "density", combo = "box_no_facet"),
  lower = list(continuous = "points", combo = "dot_no_facet")
)
p_(pm)

### Supply Raw Functions (may be user defined functions!)
pm <- ggpairs(
  churn[, c(1, 3, 4, 2)],
  upper = list(continuous = ggally_density, combo = ggally_box_no_facet),
  lower = list(continuous = ggally_points, combo = ggally_dot_no_facet)
)
p_(pm)


#### Frequencies and Crosstabs ####

### frequency distribution of a categorical variable
table(churn$Gender)                     

t = data.frame(table(churn$Geography))
t$percent= round(t$Freq / sum(t$Freq)*100,2)
t

### 2-Way Frequency Table
library(data.table)
dcast(churn, churn$Geography ~ churn$Gender)

tab1<-table(churn$Geography, churn$Gender) #Create a table of counts (object of class table)
barplot(tab1, legend.text = T) #Stacked barchart
barplot(tab1, legend.text = T, beside=T) #Barchart (not stacked)
barplot(tab1, legend.text = T, horiz=T) #Bars are shown horizontally
dotchart(tab1) #Cleveland's dot chart
mosaicplot(tab1) #from the {graphics} package

mytable <- table(churn$Geography,churn$Gender) # A will be rows, B will be columns 
mytable

margin.table(mytable,1) # A frequencies (summed over B) 
margin.table(mytable,2) # B frequencies (summed over A)

round(prop.table(mytable),4)*100  # cell percentages
round(prop.table(mytable,1),4)*100 # row percentages 
round(prop.table(mytable,2),4)*100 # column percentages

### 3-Way Frequency Table
mytable <- xtabs(~churn$Geography + churn$Gender + churn$NumOfProducts)
ftable(mytable) # print table 


#### Data Visualization ####

### for one categorical variable
library(gmodels)
library(ggplot2)

qplot(Gender, data=churn)
ggplot(churn,aes(x=Gender,fill=Geography)) + geom_bar() + CrossTable(churn$Gender, format="SPSS")
ggplot(na.omit(churn),aes(x=Gender,fill=Geography)) + geom_bar(position="dodge") + CrossTable(churn$Gender, format="SPSS")

# or

library(dplyr)
churn2 <- churn %>% count(Geography) %>% mutate(perc = n / nrow(churn))
churn2
ggplot(churn2, aes(x = reorder(Geography,-perc), y = perc, fill= Geography)) + geom_bar(stat = "identity")+
  geom_text(aes(label = round(perc, digits = 4)*100), size = 5, hjust = 0.5, vjust = 2)


ggplot(churn2, aes(x = reorder(Geography,-perc), y = perc, fill=Geography)) + geom_bar(stat = "identity")+
  geom_text(aes(label = round(perc, digits = 4)*100), size = 5, hjust = 0.5, vjust = 2)+
  CrossTable(churn$Geography, format="SPSS")


### for single continuos variable
summary(churn$Balance)

ggplot(data=churn[!is.na(churn$Balance),], aes(x=Balance)) +
  geom_histogram(fill="blue", binwidth = 10000) +
  scale_x_continuous(breaks= seq(0, 500000, by=100000), labels = comma)

hist(churn$Balance, breaks=100, main="Balance Chart", xlab="Balance")
h <- hist(churn$Age) + text(h$mids,h$counts,labels=h$counts, adj=c(0.5, -0.5))
boxplot(churn$Balance, main="churn")
scatter.smooth(x=churn$tip, y=churn$smoker, main="tip ~ smoker")


### for multiple categorical variable
ggplot(churn, aes(x= Gender,  group=Geography)) + geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
  geom_text(aes( label = scales::percent(..prop..),y= ..prop.. ), stat= "count", vjust = 2) +
  labs(y = "Percent", fill="Geography") + facet_grid(~Geography) + scale_y_continuous(labels = scales::percent) + 
  CrossTable(churn$Gender,churn$Geography, format="SPSS")

# or

tab1<- table(churn$Exited, churn$Gender, churn$HasCrCard)
round(ftable(prop.table(tab1)*100),2)

# or

ggplot(churn, aes(x= Gender,  group=Geography)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
  geom_text(aes( label = scales::percent(..prop..),y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(y = "Percent", fill="Gender") + facet_grid(~Geography) +
  scale_y_continuous(labels = scales::percent) + 
  CrossTable(churn$Gender,churn$Geography, prop.chisq=F, format="SPSS")

CrossTable(churn$Gender, churn$Geography, prop.chisq=F, format="SPSS")

#### Machine Learning Models ####

#### 1. Linear Regression ####

### Splitting the dataset into the Training set and Test set
# install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(churn$total_bill, SplitRatio = 2/3)
training_set = subset(churn, split == TRUE)
test_set = subset(churn, split == FALSE)

str(training_set)
str(test_set)
psych::describe(training_set)
mlr::summarizeColumns(training_set)

### Feature Scaling
### training_set = scale(training_set)
### test_set = scale(test_set)

### Fitting the Regression Model to the dataset
mod1 <-  lm(total_bill ~ ., data = training_set)
summary(mod1)
anova(mod1)
plot(mod1)


library(QuantPsyc)
z3 <- lm.beta(mod1)*100
z3

### RMSE Accuracy Calculation
result1 <- predict(mod1, test_set)
final1 <- cbind(actual= test_set$total_bill, predicted= result1)
final1 <- as.data.frame(final1)
final1 <- cbind(final1, error= final1$actual - final1$predicted)
rmse1 <- sqrt(mean((final1$error)^2))
rmse1

### AIC & BIC Calculation
y_pred1 = predict(mod1, test_set)
AIC(mod1)
BIC(mod1)

### Make actuals_predicteds Dataframe.
actuals_preds1 <- data.frame(cbind(actuals=test_set$total_bill, predicteds=y_pred1))  
correlation_accuracy1 <- cor(actuals_preds1)
correlation_accuracy1
head(actuals_preds1)

### Min-Max Accuracy Calculation
min_max_accuracy1 <- mean(apply(actuals_preds1, 1, min) / apply(actuals_preds1, 1, max))  
min_max_accuracy1

### MAPE (mean absolute percentage deviation) Calculation
mape1 <- mean(abs((actuals_preds1$predicteds - actuals_preds1$actuals))/actuals_preds1$actuals)  
mape1

reg.eval1 <- DMwR::regr.eval(actuals_preds1$actuals, actuals_preds1$predicteds)
reg.eval1

### performs the CV
library(DAAG)
cvResults1 <- suppressWarnings(CVlm(data=churn, form.lm=total_bill ~ ., m=5, dots=FALSE, seed=29, 
                                    legend.pos="topleft",  printit=TRUE, main="Small symbols are predicted values while bigger ones are actuals."))  
attr(cvResults1, 'ms')  


### Model 2 Regression ###
mod2 <-  lm(total_bill ~ tip, day, size, data = training_set)
summary(mod2)
anova(mod2)
plot(mod2)

library(QuantPsyc)
z4 <- lm.beta(mod2)*100
z4

### RMSE Accuracy Calculation
result2 <- predict(mod2, test_set)
final2 <- cbind(actual= test_set$total_bill, predicted= result2)
final2 <- as.data.frame(final2)
final2 <- cbind(final2, error= final2$actual - final2$predicted)
rmse2 <- sqrt(mean((final2$error)^2))
rmse2

### AIC & BIC Calculation
y_pred2 = predict(mod2, test_set)
AIC(mod2)
BIC(mod2)

### Make actuals_predicteds Dataframe.
actuals_preds2 <- data.frame(cbind(actuals=test_set$total_bill, predicteds=y_pred2))  
correlation_accuracy2 <- cor(actuals_preds2)
correlation_accuracy2
head(actuals_preds2)

### Min-Max Accuracy Calculation
min_max_accuracy2 <- mean(apply(actuals_preds2, 1, min) / apply(actuals_preds2, 1, max))  
min_max_accuracy2

### MAPE (mean absolute percentage deviation) Calculation
mape2 <- mean(abs((actuals_preds2$predicteds - actuals_preds2$actuals))/actuals_preds2$actuals)  
mape2

reg.eval2 <- DMwR::regr.eval(actuals_preds2$actuals, actuals_preds2$predicteds)
reg.eval2

### performs the CV
library(DAAG)
cvResults2 <- suppressWarnings(CVlm(data=churn, form.lm=total_bill ~ ., m=5, dots=FALSE, seed=29, 
                                    legend.pos="topleft",  printit=TRUE, main="Small symbols are predicted values while bigger ones are actuals."))  
attr(cvResults, 'ms')  


### comparing both models
rmse1
AIC(mod1)
BIC(mod1)
correlation_accuracy1
min_max_accuracy1
mape1
reg.eval1
cvResults1

tab1<-table(churn$sex, churn$day) #Create a table of counts (object of class table)
barplot(rmse, legend.text = T) #Stacked barchart
barplot(tab1, legend.text = T, beside=T) #Barchart (not stacked)
barplot(tab1, legend.text = T, horiz=T) #Bars are shown horizontally
dotchart(tab1) #Cleveland's dot chart
mosaicplot(tab1) #from the {graphics} package


#### 2. Logistic Regression ####

#### One way ####

data(churn, package = "reshape2")
View(churn)

### Encoding the target feature as factor
str(churn1)
psych::describe(churn1)
mlr::summarizeColumns(churn1)
colSums(is.na(churn1))
table(is.na(churn1))
levels(churn1$Exited)

library('plyr')
churn$sex <- revalue(churn$sex, c("Male"="1", "Female"="0"))
churn$smoker <- revalue(churn$smoker, c("Yes"="1", "No"="0"))
churn$day <- revalue(churn$day, c("Thur"="1", "Fri"="2", "Sat"="3", "Sun"="4"))
churn$time <- revalue(churn$time, c("Lunch"="1", "Dinner"="2"))
as.numeric(as.character(churn$size))
table(churn$size)

### Splitting the dataset into the Training set and Test set
# install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(churn1$Exited, SplitRatio = 0.75)
training_set = subset(churn1, split == TRUE)
test_set = subset(churn1, split == FALSE)

### Build Logistic Model
logitmod <- glm(Exited ~ ., family = "binomial", data=training_set)
summary(logitmod)

### Predicting the Test set results
pred <- predict(logitmod, newdata = test_set, type = "response")
pred
y_pred = ifelse(pred > 0.5, 1, 0)

library(caret)
fitted.results <- as.factor(pred)
test_set$Exited <- as.factor(test_set$Exited)
confusionMatrix(data = pred, test_set$Exited)


library(InformationValue)
optCutOff <- optimalCutoff(test_set$Exited, pred)[1] 
optCutOff

### Misclassification error is the percentage mismatch of predcited vs actuals, 
# irrespective of 1's or 0's. The lower the misclassification error, the better is your model.
misClassError(test_set$Exited, pred, threshold = optCutOff) 

###check for multicollinearity in the model.
DAAG::vif(logitmod)  

### Receiver Operating Characteristics Curve traces the percentage of true positives accurately predicted by a given logit model as the 
# prediction probability cutoff is lowered from 1 to 0. For a good model, as the cutoff is lowered, it should mark more of actual 1's 
# as positives and lesser of actual 0's as 1's. So for a good model, the curve should rise steeply, indicating that the TPR (Y-Axis) 
# increases faster than the FPR (X-Axis) as the cutoff score decreases. Greater the area under the ROC curve, better the predictive 
# ability of the model.
library(ROCR)
plotROC(test_set$Exited, pred)

### In simpler words, of all combinations of 1-0 pairs (actuals), Concordance is the percentage of pairs, whose scores of actual 
# positive's are greater than the scores of actual negative's. For a perfect model, this will be 100%. So, the higher the concordance, 
# the better is the quality of model.
Concordance(test_set$Exited, pred)

### Sensitivity (or True Positive Rate) is the percentage of 1's (actuals) correctly predicted by the model, 
sensitivity(test_set$Exited, pred, threshold = optCutOff)

### Specificity is the percentage of 0's (actuals) correctly predicted
specificity(test_set$Exited, pred, threshold = optCutOff)
confusionMatrix(test_set$Exited, pred, threshold = optCutOff)

### Making the Confusion Matrix
cm = table(test_set[, 13], y_pred > 0.5)
cm

### Recode factors
y_pred_num <- ifelse(pred > 0.5, 1, 0)
y_pred_num
y_pred <- factor(y_pred_num, levels=c(0, 1))
y_pred
y_act <- test_set$Exited
y_act

### Accuracy
mean(y_pred == y_act)


#### Another way ####

### load mtcars data
data(mtcars)
str(mtcars)
mtcars$vs <- as.factor(mtcars$vs)

library(corrplot)
library(ggplot2)

### simple correlation ###
round(cor(mtcars[, sapply(mtcars, is.numeric)])*100,2)

### corrplot ###
corrplot(cor(mtcars[, sapply(mtcars, is.numeric)]))

t <- cor(mtcars[, sapply(mtcars, is.numeric)])
corrplot(t, method = "number")
corrplot.mixed(t)

### plot correlation ###
library(GGally)
pairs(mtcars)
ggpairs(data=mtcars, title="churn data",
        mapping=ggplot2::aes(colour = vs),
        lower=list(combo=wrap("facethist",binwidth=1)))

### Step 1:Split data in train and test data
library(caTools)
set.seed(279)
split <- sample.split(mtcars, SplitRatio = 0.8)
split

train <- subset(mtcars, split== "TRUE")
test <- subset(mtcars, split== "FALSE")
train
test

### Step 2:Train model with logistics regression using glm function
logit_model <- glm(vs~wt+disp, data = train, family = "binomial")
logit_model
summary(logit_model)

### Interpretation
# wt influences dependent variables positively and for 1 unit increase in wt increases the log of odds for vs=1 by 1.44
# disp influences dependent variable negatively and for 1 unit increase in disp decreases the log of odds for vs=1 by 0.0344


# Null Deviance = 30.78 (fit dependent variable only with intercept)
# Residual Deviance = 15.01 (fit dependent variable with all the independent variable)
# AIC (lesser the better, used for comparing different models)


### Step 3:Predict test data based on trained model -logit_model
fitted.results <- predict(logit_model, test, type = "response")

fitted.results # Predicted Result
test$vs    # Actual Result


### Step 4: Change probabilities to class (0 or 1/Yes or No)
# If prob > 0.5 then 1, else 0. Threshold can be set for better results
fitted.results <- ifelse(fitted.results > 0.5,1,0)

fitted.results # Predicted Result
test$vs    # Actual Result


### Step 5: Evauate Model Accuracy using Confusion matrix
# accuracy = (true positive + true negatve) / all (100 times this is the same as percentCorrect)
table(test$vs, fitted.results)
misClassError <- mean(fitted.results != test$vs)
print(paste('Accuracy =',1-misClassError))

# sensitivity = true pasitive rate = true positive / all positive (sensitivity is also called recall)
# specificity = true negative rate = true negative / all negative
# precision = positive predictive velue = true positive rate
fitted.results <- as.factor(fitted.results)
test$vs <- as.factor(test$vs)
confusionMatrix(data = fitted.results, test$vs)

### ROC-AUC Curve
#install.packages("ROCR")

library(ROCR)
plotROC(test$vs, fitted.results)

#### 3. Decision Tree ####

# structure and summary of data #
names(churn)                         
dim(churn)                           
str(churn)                           
summary(churn)
psych::describe(churn)               
mlr::summarizeColumns(churn)


