##cars data ML##

## sample data ##

data(tips, package = "reshape2")
View(tips)

######################### Data Checks ######################################

class(df$trigger_dt)                #finding class 
dfa$dfb <- NULL                     #remove data
dfa <- NULL                         #remove data
names(tips)                           #variable names
dim(tips)                           #nrow & ncol
str(tips)                           #data structure
psych::describe(tips)               #frequency distribution
mlr::summarizeColumns(tips)
colSums(is.na(tips))                #column with NA count
table(is.na(tips))                  #is.na
levels(tips$sex)                    #values in a variable
filter(deliveries, batsman_runs>4)  #filter
df= rename(df, variable1=var1)      #rename variable
trimws()                            #remove leading and trailing spaces
library(dplyr)                      #Drop Multiple Variables
df = select(mydata, -c(x,y,z))

########

table(tips$sex)                     #frequency distribution of a categorical variable

#OR#

t = data.frame(table(tips$size))
t$percent= round(t$Freq / sum(t$Freq)*100,2)
t

########

find_funs("table.extended") 
source("https://sebastiansauer.github.io/Rcode/find_funs.R")

# OR #

install.packages("sos")
findFn("plotFilterValuesGGVIS")

########
library(scales)                    # for percentage scales

#############################################################################
start.time <- Sys.time()           #measure execution time of a program
runif(5555,1,1000)
end.time <- Sys.time()
end.time - start.time
# OR #
library(tictoc)
tic()
runif(5555,1,1000)
toc()

#############################################################################

######################### Frequencies and Crosstabs #########################

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

### OR #####

ggplot(tips2, aes(x = reorder(day,-perc), y = perc, fill=day)) + geom_bar(stat = "identity")+
  geom_text(aes(label = round(perc, digits = 4)*100), size = 5, hjust = 0.5, vjust = 2)+
  CrossTable(tips$day, format="SPSS")


#for one continuos variable

hist(tips$tip, breaks=100, main="Tip Chart", xlab="Tip")
boxplot(tips$tip, main="Tips")
par(mfrow=c(1, 2))  # divide graph area in 2 columns
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


#for multiple variable

ggplot(tips, aes(x= sex,  group=time)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
  geom_text(aes( label = scales::percent(..prop..),y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(y = "Percent", fill="sex") + facet_grid(~time) +
  scale_y_continuous(labels = scales::percent) + 
  CrossTable(tips$sex,tips$time, prop.chisq=F, format="SPSS")

CrossTable(tips$sex, tips$smoker, prop.chisq=F, format="SPSS")



###### Multilinear Regression ###########


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



### Model 2 Regression

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


name <- c("rmse1", "rmse2", "AIC(mod1)", "AIC(mod2)", "BIC(mod1)", "BIC(mod2)", "correlation_accuracy1", "correlation_accuracy2", "min_max_accuracy1", "min_max_accuracy2", "mape1", "mape2", "reg.eval1", "reg.eval2")
number <- c(rmse1, rmse2, AIC(mod1), AIC(mod2), BIC(mod1), BIC(mod2), correlation_accuracy1, correlation_accuracy2, min_max_accuracy1, min_max_accuracy2, mape1, mape2, reg.eval1, reg.eval2)
final_results <- data.frame(name, number)

name
number

# Visualising the Regression Model results

# install.packages('ggplot2')
library(ggplot2)
ggplot() +
  geom_point(aes(x = dataset$Level, y = dataset$Salary),
             colour = 'red') +
  geom_line(aes(x = dataset$Level, y = predict(regressor, newdata = dataset)),
            colour = 'blue') +
  ggtitle('Truth or Bluff (Regression Model)') +
  xlab('Level') +
  ylab('Salary')

# Visualising the Regression Model results (for higher resolution and smoother curve)

# install.packages('ggplot2')
library(ggplot2)
x_grid = seq(min(dataset$Level), max(dataset$Level), 0.1)
ggplot() +
  geom_point(aes(x = dataset$Level, y = dataset$Salary),
             colour = 'red') +
  geom_line(aes(x = x_grid, y = predict(regressor, newdata = data.frame(Level = x_grid))),
            colour = 'blue') +
  ggtitle('Truth or Bluff (Regression Model)') +
  xlab('Level') +
  ylab('Salary')

