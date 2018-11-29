path <- "E:/NOTES/Analytics Notes/Practice/Kaggle & Hackathons/Loan Prediction III - R/"
setwd(path)

--------------------------------------------------------------------------------

### Load Libraries and Data ###
install.packages("mlr", dependencies = T)
library(mlr)
train <- read.csv(file.choose())
test <- read.csv(file.choose())
train <- read.csv("train_u6lujuX.csv", na.strings = c(""," ",NA))
test <- read.csv("test_Y3wMUE5.csv", na.strings = c(""," ",NA))
View(test)

--------------------------------------------------------------------------------

### Exploring Data ###
source("https://sebastiansauer.github.io/Rcode/find_funs.R")
find_funs("var.imp")
install.packages("sos")
findFn("plotFilterValuesGGVIS")

names(train)
dim(train)
table(is.na(train))
colSums(is.na(train))
summarizeColumns(train)
summarizeLevels(train)

hist(train$ApplicantIncome, breaks=300, main="Applicant Income Chart", xlab="Applicant Income")
hist(train$CoapplicantIncome, breaks=100, main="Co-Applicant Income Chart", xlab="Co-Applicant Income")
boxplot(train$ApplicantIncome)
boxplot(train$CoapplicantIncome)
boxplot(train$LoanAmount)

library(ggplot2)
library(gmodels)
ggplot(train,aes(x=Gender,fill=Gender)) + geom_bar() + CrossTable(train$Gender, format="SPSS")
ggplot(na.omit(train),aes(x=Gender,fill=Gender)) + geom_bar(position="dodge") + CrossTable(train$Gender, format="SPSS")


train$Credit_History <- as.factor(train$Credit_History)
test$Credit_History <- as.factor(test$Credit_History)

class(train$Credit_History)

summary(train)
summary(test)

# Rename Level of Dependents #
levels(train$Dependents) [4] <- "3"
levels(test$Dependents) [4] <- "3"

--------------------------------------------------------------------------------

### Missing Value Imputation ###
# impute missing value by mean and mode #
imp <- impute(train, classes = list(factor = imputeMode(), integer = imputeMean()), 
              dummy.classes = c("integer","factor"), dummy.type = "numeric")

imp1 <- impute(test, classes = list(factor = imputeMode(), integer = imputeMean()), 
               dummy.classes = c("integer","factor"), dummy.type = "numeric")

imp_train <- imp$data
imp_test <- imp1$data

summarizeColumns(imp_train)
summarizeColumns(imp_test)

#Optional Missing Value Imputation Methods#
listLearners("classif", check.packages = TRUE, properties = "missings")[c("class","package")]

#class                           package
#1 classif.bartMachine          bartMachine
#2 classif.boosting            adabag,rpart
# 3 classif.cforest                party
# 4 classif.ctree                  party
# 5 classif.gbm                     gbm
# 6 classif.naiveBayes             e1071
# 7 classif.randomForestSRC   randomForestSRC
# 8 classif.rpart                  rpart

rpart_imp <- impute(train, target = "Loan_Status",
                    classes = list(numeric = imputeLearner(makeLearner("regr.rpart")),
                    factor = imputeLearner(makeLearner("classif.rpart"))),
                    dummy.classes = c("numeric","factor"),
                    dummy.type = "numeric") 

--------------------------------------------------------------------------------

### Feature Engineering ###

#for train data set
cd <- capLargeValues(imp_train, target = "Loan_Status",cols = c("ApplicantIncome"),threshold = 40000)
cd <- capLargeValues(cd, target = "Loan_Status",cols = c("CoapplicantIncome"),threshold = 21000)
cd <- capLargeValues(cd, target = "Loan_Status",cols = c("LoanAmount"),threshold = 520)


#rename the train data as cd_train
cd_train <- cd

#add a dummy Loan_Status column in test data
imp_test$Loan_Status <- sample(0:1,size = 367,replace = T)

cde <- capLargeValues(imp_test, target = "Loan_Status",cols = c("ApplicantIncome"),threshold = 33000)
cde <- capLargeValues(cde, target = "Loan_Status",cols = c("CoapplicantIncome"),threshold = 16000)
cde <- capLargeValues(cde, target = "Loan_Status",cols = c("LoanAmount"),threshold = 470)

#renaming test data
cd_test <- cde

summary(cd_train$ApplicantIncome)
summary(cd_test$ApplicantIncome)


#convert numeric to factor - train
for (f in names(cd_train[, c(14:20)])) {
  if( class(cd_train[, c(14:20)] [[f]]) == "numeric"){
    levels <- unique(cd_train[, c(14:20)][[f]])
    cd_train[, c(14:20)][[f]] <- as.factor(factor(cd_train[, c(14:20)][[f]], levels = levels))
  }
}

#convert numeric to factor - test
for (f in names(cd_test[, c(13:18)])) {
  if( class(cd_test[, c(13:18)] [[f]]) == "numeric"){
    levels <- unique(cd_test[, c(13:18)][[f]])
    cd_test[, c(13:18)][[f]] <- as.factor(factor(cd_test[, c(13:18)][[f]], levels = levels))
  }
}


#Total_Income
cd_train$Total_Income <- cd_train$ApplicantIncome + cd_train$CoapplicantIncome
cd_test$Total_Income <- cd_test$ApplicantIncome + cd_test$CoapplicantIncome

#Income by loan
cd_train$Income_by_loan <- cd_train$Total_Income/cd_train$LoanAmount
cd_test$Income_by_loan <- cd_test$Total_Income/cd_test$LoanAmount

#change variable class
cd_train$Loan_Amount_Term <- as.numeric(cd_train$Loan_Amount_Term)
cd_test$Loan_Amount_Term <- as.numeric(cd_test$Loan_Amount_Term)

#Loan amount by term
cd_train$Loan_amount_by_term <- cd_train$LoanAmount/cd_train$Loan_Amount_Term
cd_test$Loan_amount_by_term <- cd_test$LoanAmount/cd_test$Loan_Amount_Term


#splitting the data based on class
az <- split(names(cd_train), sapply(cd_train, function(x){ class(x)}))

#creating a data frame of numeric variables
xs <- cd_train[az$numeric]

#check correlation
cor(xs)


cd_train$Total_Income <- NULL
cd_test$Total_Income <- NULL

summarizeColumns(cd_train)
summarizeColumns(cd_test)

--------------------------------------------------------------------------------

### Machine Learning ###

#create a task
trainTask <- makeClassifTask(data = cd_train,target = "Loan_Status")
testTask <- makeClassifTask(data = cd_test, target = "Loan_Status")


trainTask
testTask

trainTask <- makeClassifTask(data = cd_train,target = "Loan_Status", positive = "Y")

str(getTaskData(trainTask))


#normalize the variables
trainTask <- normalizeFeatures(trainTask,method = "standardize")
testTask <- normalizeFeatures(testTask,method = "standardize")

trainTask <- dropFeatures(task = trainTask,features = c("Loan_ID","Married.dummy"))

#Feature importance
im_feat <- generateFilterValuesData(trainTask, method = c("information.gain","chi.squared"))
plotFilterValues(im_feat,n.show = 20)

#to launch its shiny application
plotFilterValuesGGVIS(im_feat)

------------------------------------

### Quadratic Discriminant Analysis (QDA) ###
#load qda 
qda.learner <- makeLearner("classif.qda", predict.type = "response")

#train model
qmodel <- train(qda.learner, trainTask)

#predict on test data
qpredict <- predict(qmodel, testTask)

#create submission file
submit <- data.frame(Loan_ID = test$Loan_ID, Loan_Status = qpredict$data$response)
write.csv(submit, "submit1.csv",row.names = F)

------------------------------------

### Logistic Regression ###
  
#logistic regression
logistic.learner <- makeLearner("classif.logreg",predict.type = "response")

#cross validation (cv) accuracy
cv.logistic <- crossval(learner = logistic.learner,task = trainTask,iters = 3,stratify = TRUE,measures = acc,show.info = F)  

#cross validation accuracy
cv.logistic$aggr
cv.logistic$measures.test

#train model
fmodel <- train(logistic.learner,trainTask)
getLearnerModel(fmodel)

#predict on test data
fpmodel <- predict(fmodel, testTask)

#create submission file
submit <- data.frame(Loan_ID = test$Loan_ID, Loan_Status = fpmodel$data$response)
write.csv(submit, "submit_LogitReg_R.csv",row.names = F)

------------------------------------
  
### Decision Tree ###

getParamSet("classif.rpart")

#make tree learner
makeatree <- makeLearner("classif.rpart", predict.type = "response")

#set 3 fold cross validation
set_cv <- makeResampleDesc("CV",iters = 3L)

#Search for hyperparameters
gs <- makeParamSet(
  makeIntegerParam("minsplit",lower = 10, upper = 50),
  makeIntegerParam("minbucket", lower = 5, upper = 50),
  makeNumericParam("cp", lower = 0.001, upper = 0.2)
)

#do a grid search
gscontrol <- makeTuneControlGrid()

#hypertune the parameters
stune <- tuneParams(learner = makeatree, resampling = set_cv, task = trainTask, par.set = gs, control = gscontrol, measures = acc)

#check best parameter
stune$x

#cross validation result
stune$y

#using hyperparameters for modeling
t.tree <- setHyperPars(makeatree, par.vals = stune$x)

#train the model
t.rpart <- train(t.tree, trainTask)
getLearnerModel(t.rpart)

#make predictions
tpmodel <- predict(t.rpart, testTask)

#create a submission file
submit <- data.frame(Loan_ID = test$Loan_ID, Loan_Status = tpmodel$data$response)
write.csv(submit, "submit_DecTree_R.csv",row.names = F)

------------------------------------
  
### Random Forrest ###

getParamSet("classif.randomForest")

#create a learner
rf <- makeLearner("classif.randomForest", predict.type = "response", par.vals = list(ntree = 200, mtry = 3))
rf$par.vals <- list(
  importance = TRUE
)

#set tunable parameters
#grid search to find hyperparameters
rf_param <- makeParamSet(
  makeIntegerParam("ntree",lower = 50, upper = 500),
  makeIntegerParam("mtry", lower = 3, upper = 10),
  makeIntegerParam("nodesize", lower = 10, upper = 50)
)

#let's do random search for 50 iterations
rancontrol <- makeTuneControlRandom(maxit = 50L)

#set 3 fold cross validation
set_cv <- makeResampleDesc("CV",iters = 3L)

#hypertuning
rf_tune <- tuneParams(learner = rf, resampling = set_cv, task = trainTask, par.set = rf_param, control = rancontrol, measures = acc)  


#cv accuracy
rf_tune$y

#best parameters
rf_tune$x

#using hyperparameters for modeling
rf.tree <- setHyperPars(rf, par.vals = rf_tune$x)

#train a model
rforest <- train(rf.tree, trainTask)
getLearnerModel(t.rpart)

#make predictions
rfmodel <- predict(rforest, testTask)

#submission file
submit <- data.frame(Loan_ID = test$Loan_ID, Loan_Status = rfmodel$data$response)
write.csv(submit, "submit_RandomFrst_R.csv",row.names = F)

------------------------------------
  
### SVM ###

#load svm
getParamSet("classif.ksvm") #do install kernlab package 
ksvm <- makeLearner("classif.ksvm", predict.type = "response")

#Set parameters
pssvm <- makeParamSet(
  makeDiscreteParam("C", values = 2^c(-8,-4,-2,0)), #cost parameters
  makeDiscreteParam("sigma", values = 2^c(-8,-4,0,4)) #RBF Kernel Parameter
)

#specify search function
ctrl <- makeTuneControlGrid()

#tune model
res <- tuneParams(ksvm, task = trainTask, resampling = set_cv, par.set = pssvm, control = ctrl,measures = acc)

#CV accuracy
res$y

#set the model with best params
t.svm <- setHyperPars(ksvm, par.vals = res$x)

#train
par.svm <- train(ksvm, trainTask)

#test
predict.svm <- predict(par.svm, testTask)

#submission file
submit <- data.frame(Loan_ID = test$Loan_ID, Loan_Status = predict.svm$data$response)
write.csv(submit, "submit_SVM_R.csv",row.names = F)

------------------------------------
  
### GBM ###

#load GBM
getParamSet("classif.gbm")
g.gbm <- makeLearner("classif.gbm", predict.type = "response")

#specify tuning method
rancontrol <- makeTuneControlRandom(maxit = 50L)

#3 fold cross validation
set_cv <- makeResampleDesc("CV",iters = 3L)

#parameters
gbm_par<- makeParamSet(
  makeDiscreteParam("distribution", values = "bernoulli"),
  makeIntegerParam("n.trees", lower = 100, upper = 1000), #number of trees
  makeIntegerParam("interaction.depth", lower = 2, upper = 10), #depth of tree
  makeIntegerParam("n.minobsinnode", lower = 10, upper = 80),
  makeNumericParam("shrinkage",lower = 0.01, upper = 1)
)


#tune parameters
tune_gbm <- tuneParams(learner = g.gbm, task = trainTask,resampling = set_cv,measures = acc,par.set = gbm_par,control = rancontrol)

#check CV accuracy
tune_gbm$y

#set parameters
final_gbm <- setHyperPars(learner = g.gbm, par.vals = tune_gbm$x)

#train
to.gbm <- train(final_gbm, trainTask)

#test 
pr.gbm <- predict(to.gbm, testTask)

#submission file
submit <- data.frame(Loan_ID = test$Loan_ID, Loan_Status = pr.gbm$data$response)
write.csv(submit, "submit_GBM_R.csv",row.names = F)

------------------------------------

### Xgboost ###

#load xgboost
set.seed(1001)
getParamSet("classif.xgboost")

#make learner with inital parameters
xg_set <- makeLearner("classif.xgboost", predict.type = "response")
xg_set$par.vals <- list(
  objective = "binary:logistic",
  eval_metric = "error",
  nrounds = 250
)


#define parameters for tuning
xg_ps <- makeParamSet(
  makeIntegerParam("nrounds",lower=200,upper=600),
  makeIntegerParam("max_depth",lower=3,upper=20),
  makeNumericParam("lambda",lower=0.55,upper=0.60),
  makeNumericParam("eta", lower = 0.001, upper = 0.5),
  makeNumericParam("subsample", lower = 0.10, upper = 0.80),
  makeNumericParam("min_child_weight",lower=1,upper=5),
  makeNumericParam("colsample_bytree",lower = 0.2,upper = 0.8)
)

#define search function
rancontrol <- makeTuneControlRandom(maxit = 100L) #do 100 iterations

#3 fold cross validation
set_cv <- makeResampleDesc("CV",iters = 3L)

#tune parameters
xg_tune <- tuneParams(learner = xg_set, task = trainTask, resampling = set_cv,measures = acc,par.set = xg_ps, control = rancontrol)

#set parameters
xg_new <- setHyperPars(learner = xg_set, par.vals = xg_tune$x)

#train model
xgmodel <- train(xg_new, trainTask)

#test model
predict.xg <- predict(xgmodel, testTask)

#submission file
submit <- data.frame(Loan_ID = test$Loan_ID, Loan_Status = predict.xg$data$response)
write.csv(submit, "submit7.csv",row.names = F)

--------------------------------------------------------------------------------

#selecting top 6 important features
top_task <- filterFeatures(trainTask, method = "rf.importance", abs = 6)

