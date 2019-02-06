##cars data ML##

## sample data ##
data(tips, package = "reshape2")
View(tips)

#measure execution time of a program
start.time <- Sys.time()           
runif(5555,1,1000)
end.time <- Sys.time()
end.time - start.time

library(tictoc)
tic()
runif(5555,1,1000)
toc()

######################### Data Checks ######################################
rm(list = ls())
class(df$trigger_dt)                 

# dropping variable #
dfa$dfb <- NULL                     
dfa <- NULL                         

library(dplyr)                      
df = select(mydata, -c(x,y,z))

# structure and summary of data #
names(tips)                         
dim(tips)                           
str(tips)                           
summary(tips)
psych::describe(tips)               
mlr::summarizeColumns(tips)

# finding missing value #
colSums(is.na(tips))                
table(is.na(tips))                  

NAcol <- which(colSums(is.na(all)) > 0)
sort(colSums(sapply(all[NAcol], is.na)), decreasing = TRUE)
cat('There are', length(NAcol), 'columns with missing values')

# levels and filtering variable #
levels(tips$sex)                    
filter(deliveries, batsman_runs>4)  

# renaming #
df= rename(df, variable1=var1)      
trimws()                            

# finding functions in a package #

find_funs("varImp") 
source("https://sebastiansauer.github.io/Rcode/find_funs.R")

install.packages("sos")
findFn("plotFilterValuesGGVIS")

########
# for percentage scales
library(scales)                    

################################ Data Check ################################
library(corrplot)
library(ggplot2)

###################################
#Correlations
numericVars <- which(sapply(all, is.numeric)) #index vector numeric variables
numericVarNames <- names(numericVars) #saving names vector for use later on
cat('There are', length(numericVars), 'numeric variables')
all_numVar <- all[, numericVars]
cor_numVar <- cor(all_numVar, use="pairwise.complete.obs") #correlations of all numeric variables
cor_sorted <- as.matrix(sort(cor_numVar[,'SalePrice'], decreasing = TRUE)) #sort on decreasing correlations with SalePrice
CorHigh <- names(which(apply(cor_sorted, 1, function(x) abs(x)>0.5))) #select only high corelations
cor_numVar <- cor_numVar[CorHigh, CorHigh]
corrplot.mixed(cor_numVar, tl.col="black", tl.pos = "lt")

### simple correlation ###
round(cor(tips[, sapply(tips, is.numeric)])*100,2)

### corrplot ###
corrplot(cor(tips[, sapply(tips, is.numeric)]))

t <- cor(tips[, sapply(tips, is.numeric)])
corrplot(t, method = "number")
corrplot.mixed(t)

### plot correlation ###
library(GGally)
pairs(tips)
ggpairs(data=tips, title="tips data",
        mapping=ggplot2::aes(colour = sex),
        lower=list(combo=wrap("facethist",binwidth=1)))

# small function to display plots only if it's interactive
p_ <- GGally::print_if_interactive

pm <- ggpairs(data= tips[, 1:3], mapping=ggplot2::aes(colour = sex))
p_(pm)

pm <- ggpairs(data= tips, 1:3, mapping=ggplot2::aes(colour = sex), columnLabels = c("Total Bill", "Tip", "Sex"))
p_(pm)

pm <- ggpairs(data= tips, mapping=ggplot2::aes(colour = sex), upper = "blank")
p_(pm)

## Plot Types
# Change default plot behavior
pm <- ggpairs(
  tips[, c(1, 3, 4, 2)],
  upper = list(continuous = "density", combo = "box_no_facet"),
  lower = list(continuous = "points", combo = "dot_no_facet")
)
p_(pm)

# Supply Raw Functions (may be user defined functions!)
pm <- ggpairs(
  tips[, c(1, 3, 4, 2)],
  upper = list(continuous = ggally_density, combo = ggally_box_no_facet),
  lower = list(continuous = ggally_points, combo = ggally_dot_no_facet)
)
p_(pm)


######################### Frequencies and Crosstabs #########################

#frequency distribution of a categorical variable
table(tips$sex)                     

t = data.frame(table(tips$size))
t$percent= round(t$Freq / sum(t$Freq)*100,2)
t

# 2-Way Frequency Table
library(data.table)
dcast(tips, tips$sex ~ tips$smoker)

tab1<-table(tips$sex, tips$day) #Create a table of counts (object of class table)
barplot(tab1, legend.text = T) #Stacked barchart
barplot(tab1, legend.text = T, beside=T) #Barchart (not stacked)
barplot(tab1, legend.text = T, horiz=T) #Bars are shown horizontally
dotchart(tab1) #Cleveland's dot chart
mosaicplot(tab1) #from the {graphics} package

mytable <- table(tips$sex,tips$time) # A will be rows, B will be columns 
mytable

margin.table(mytable,1) # A frequencies (summed over B) 
margin.table(mytable,2) # B frequencies (summed over A)

round(prop.table(mytable),4)*100  # cell percentages
round(prop.table(mytable,1),4)*100 # row percentages 
round(prop.table(mytable,2),4)*100 # column percentages

# 3-Way Frequency Table
mytable <- xtabs(~tips$sex + tips$day + tips$time, data=tips)
ftable(mytable) # print table 


######################### Data Visualization ######################################

#for one categorical variable
library(gmodels)
library(ggplot2)

qplot(c(sex,time), data=tips)
ggplot(tips,aes(x=sex,fill=day)) + geom_bar() + CrossTable(tips$sex, format="SPSS")
ggplot(na.omit(tips),aes(x=sex,fill=day)) + geom_bar(position="dodge") + CrossTable(tips$sex, format="SPSS")

########

library(dplyr)
tips2 <- tips %>% count(day) %>% mutate(perc = n / nrow(tips))
tips2
ggplot(tips2, aes(x = reorder(day,-perc), y = perc, fill=day)) + geom_bar(stat = "identity")+
  geom_text(aes(label = round(perc, digits = 4)*100), size = 5, hjust = 0.5, vjust = 2)


ggplot(tips2, aes(x = reorder(day,-perc), y = perc, fill=day)) + geom_bar(stat = "identity")+
  geom_text(aes(label = round(perc, digits = 4)*100), size = 5, hjust = 0.5, vjust = 2)+
  CrossTable(tips$day, format="SPSS")


#for single continuos variable
summary(tips$tip)

ggplot(data=all[!is.na(all$SalePrice),], aes(x=SalePrice)) +
  geom_histogram(fill="blue", binwidth = 10000) +
  scale_x_continuous(breaks= seq(0, 800000, by=100000), labels = comma)

hist(tips$tip, breaks=100, main="Tip Chart", xlab="Tip")
h <- hist(train$SalePrice) + text(h$mids,h$counts,labels=h$counts, adj=c(0.5, -0.5))
boxplot(tips$tip, main="Tips")
scatter.smooth(x=cars$speed, y=cars$dist, main="Dist ~ Speed")


#for multiple categorical variable
ggplot(tips, aes(x= day,  group=sex)) + geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
  geom_text(aes( label = scales::percent(..prop..),y= ..prop.. ), stat= "count", vjust = 2) +
  labs(y = "Percent", fill="Sex") + facet_grid(~sex) + scale_y_continuous(labels = scales::percent) + 
  CrossTable(tips$day,tips$sex, format="SPSS")

########

tab1<- table(tips$sex, tips$day, tips$time)
prop.table(tab1)
round(ftable(prop.table(tab1)*100),2)

########

ggplot(tips, aes(x= sex,  group=time)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
  geom_text(aes( label = scales::percent(..prop..),y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(y = "Percent", fill="sex") + facet_grid(~time) +
  scale_y_continuous(labels = scales::percent) + 
  CrossTable(tips$sex,tips$time, prop.chisq=F, format="SPSS")

CrossTable(tips$sex, tips$smoker, prop.chisq=F, format="SPSS")

#################################################################################################

############################## 1. Linear Regression ##############################

# Splitting the dataset into the Training set and Test set
# # install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(tips$total_bill, SplitRatio = 2/3)
training_set = subset(tips, split == TRUE)
test_set = subset(tips, split == FALSE)

str(training_set)
str(test_set)
psych::describe(training_set)
mlr::summarizeColumns(training_set)

# Feature Scaling
# training_set = scale(training_set)
# test_set = scale(test_set)

# Fitting the Regression Model to the dataset
mod1 <-  lm(total_bill ~ ., data = training_set)
summary(mod1)
anova(mod1)
plot(mod1)

library(QuantPsyc)
z3 <- lm.beta(mod1)*100
z3

# RMSE Accuracy Calculation
result1 <- predict(mod1, test_set)
final1 <- cbind(actual= test_set$total_bill, predicted= result1)
final1 <- as.data.frame(final1)
final1 <- cbind(final1, error= final1$actual - final1$predicted)
rmse1 <- sqrt(mean((final1$error)^2))
rmse1

# AIC & BIC Calculation
y_pred1 = predict(mod1, test_set)
AIC(mod1)
BIC(mod1)

# Make actuals_predicteds Dataframe.
actuals_preds1 <- data.frame(cbind(actuals=test_set$total_bill, predicteds=y_pred1))  
correlation_accuracy1 <- cor(actuals_preds1)
correlation_accuracy1
head(actuals_preds1)

# Min-Max Accuracy Calculation
min_max_accuracy1 <- mean(apply(actuals_preds1, 1, min) / apply(actuals_preds1, 1, max))  
min_max_accuracy1

# MAPE (mean absolute percentage deviation) Calculation
mape1 <- mean(abs((actuals_preds1$predicteds - actuals_preds1$actuals))/actuals_preds1$actuals)  
mape1

reg.eval1 <- DMwR::regr.eval(actuals_preds1$actuals, actuals_preds1$predicteds)
reg.eval1

# performs the CV
library(DAAG)
cvResults1 <- suppressWarnings(CVlm(data=tips, form.lm=total_bill ~ ., m=5, dots=FALSE, seed=29, 
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

# RMSE Accuracy Calculation
result2 <- predict(mod2, test_set)
final2 <- cbind(actual= test_set$total_bill, predicted= result2)
final2 <- as.data.frame(final2)
final2 <- cbind(final2, error= final2$actual - final2$predicted)
rmse2 <- sqrt(mean((final2$error)^2))
rmse2

# AIC & BIC Calculation
y_pred2 = predict(mod2, test_set)
AIC(mod2)
BIC(mod2)

# Make actuals_predicteds Dataframe.
actuals_preds2 <- data.frame(cbind(actuals=test_set$total_bill, predicteds=y_pred2))  
correlation_accuracy2 <- cor(actuals_preds2)
correlation_accuracy2
head(actuals_preds2)

# Min-Max Accuracy Calculation
min_max_accuracy2 <- mean(apply(actuals_preds2, 1, min) / apply(actuals_preds2, 1, max))  
min_max_accuracy2

# MAPE (mean absolute percentage deviation) Calculation
mape2 <- mean(abs((actuals_preds2$predicteds - actuals_preds2$actuals))/actuals_preds2$actuals)  
mape2

reg.eval2 <- DMwR::regr.eval(actuals_preds2$actuals, actuals_preds2$predicteds)
reg.eval2

# performs the CV
library(DAAG)
cvResults2 <- suppressWarnings(CVlm(data=tips, form.lm=total_bill ~ ., m=5, dots=FALSE, seed=29, 
                                    legend.pos="topleft",  printit=TRUE, main="Small symbols are predicted values while bigger ones are actuals."))  
attr(cvResults, 'ms')  


rmse1
AIC(mod1)
BIC(mod1)
correlation_accuracy1
min_max_accuracy1
mape1
reg.eval1
cvResults1

tab1<-table(tips$sex, tips$day) #Create a table of counts (object of class table)
barplot(rmse, legend.text = T) #Stacked barchart
barplot(tab1, legend.text = T, beside=T) #Barchart (not stacked)
barplot(tab1, legend.text = T, horiz=T) #Bars are shown horizontally
dotchart(tab1) #Cleveland's dot chart
mosaicplot(tab1) #from the {graphics} package


################################### 2. Logistic Regression ###################################

######### One way #########

data(tips, package = "reshape2")
View(tips)

# Encoding the target feature as factor
str(tips)
psych::describe(tips)
mlr::summarizeColumns(tips)
colSums(is.na(tips))
table(is.na(tips))
levels(tips$sex)

library('plyr')
tips$sex <- revalue(tips$sex, c("Male"="1", "Female"="0"))
tips$smoker <- revalue(tips$smoker, c("Yes"="1", "No"="0"))
tips$day <- revalue(tips$day, c("Thur"="1", "Fri"="2", "Sat"="3", "Sun"="4"))
tips$time <- revalue(tips$time, c("Lunch"="1", "Dinner"="2"))
as.numeric(as.character(tips$size))

# Splitting the dataset into the Training set and Test set
# install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(tips$sex, SplitRatio = 0.75)
training_set = subset(tips, split == TRUE)
test_set = subset(tips, split == FALSE)

# Build Logistic Model
logitmod <- glm(sex ~ ., family = "binomial", data=training_set)

# Predicting the Test set results
pred <- predict(logitmod, newdata = test_set, type = "response")
pred
y_pred = ifelse(pred > 0.5, 1, 0)


fitted.results <- as.factor(fitted.results)
test$vs <- as.factor(test$vs)
confusionMatrix(data = fitted.results, test$vs)


library(InformationValue)
optCutOff <- optimalCutoff(test_set$sex, pred)[1] 
optCutOff
#Misclassification error is the percentage mismatch of predcited vs actuals, 
#irrespective of 1's or 0's. The lower the misclassification error, the better is your model.
misClassError(test_set$sex, pred, threshold = optCutOff) 
summary(logitmod)
#check for multicollinearity in the model.
DAAG::vif(logitmod)  
#Receiver Operating Characteristics Curve traces the percentage of true positives accurately predicted by a given logit model as the 
#prediction probability cutoff is lowered from 1 to 0. For a good model, as the cutoff is lowered, it should mark more of actual 1's 
#as positives and lesser of actual 0's as 1's. So for a good model, the curve should rise steeply, indicating that the TPR (Y-Axis) 
#increases faster than the FPR (X-Axis) as the cutoff score decreases. Greater the area under the ROC curve, better the predictive 
#ability of the model.
library(ROCR)
plotROC(test_set$sex, pred)
#In simpler words, of all combinations of 1-0 pairs (actuals), Concordance is the percentage of pairs, whose scores of actual 
#positive's are greater than the scores of actual negative's. For a perfect model, this will be 100%. So, the higher the concordance, 
#the better is the quality of model.
Concordance(test_set$sex, pred)
#Sensitivity (or True Positive Rate) is the percentage of 1's (actuals) correctly predicted by the model, 
sensitivity(test_set$sex, pred, threshold = optCutOff)
#specificity is the percentage of 0's (actuals) correctly predicted
specificity(test_set$sex, pred, threshold = optCutOff)
confusionMatrix(test_set$sex, pred, threshold = optCutOff)

# Making the Confusion Matrix
cm = table(test_set[, 3], y_pred > 0.5)
cm

# Recode factors
y_pred_num <- ifelse(pred > 0.5, 1, 0)
y_pred_num
y_pred <- factor(y_pred_num, levels=c(0, 1))
y_pred
y_act <- test_set$sex
y_act

# Accuracy
mean(y_pred == y_act)  # 94%


######### Another way #########

# load mtcars data
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
ggpairs(data=mtcars, title="tips data",
        mapping=ggplot2::aes(colour = vs),
        lower=list(combo=wrap("facethist",binwidth=1)))

# Step 1:Split data in train and test data
library(caTools)
set.seed(279)
split <- sample.split(mtcars, SplitRatio = 0.8)
split

train <- subset(mtcars, split== "TRUE")
test <- subset(mtcars, split== "FALSE")
train
test

# Step 2:Train model with logistics regression using glm function
logit_model <- glm(vs~wt+disp, data = train, family = "binomial")
logit_model
summary(logit_model)

#Interpretation
# wt influences dependent variables positively and for 1 unit increase in wt increases the log of odds for vs=1 by 1.44
# disp influences dependent variable negatively and for 1 unit increase in disp decreases the log of odds for vs=1 by 0.0344


# Null Deviance = 30.78 (fit dependent variable only with intercept)
# Residual Deviance = 15.01 (fit dependent variable with all the independent variable)
# AIC (lesser the better, used for comparing different models)


# Step 3:Predict test data based on trained model -logit_model
fitted.results <- predict(logit_model, test, type = "response")

fitted.results # Predicted Result
test$vs    # Actual Result


# Step 4: Change probabilities to class (0 or 1/Yes or No)
# If prob > 0.5 then 1, else 0. Threshold can be set for better results
fitted.results <- ifelse(fitted.results > 0.5,1,0)

fitted.results # Predicted Result
test$vs    # Actual Result


# Step 5: Evauate Model Accuracy using Confusion matrix
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



# ROC-AUC Curve
#install.packages("ROCR")

library(ROCR)
plotROC(test$vs, fitted.results)

#################################################################################################