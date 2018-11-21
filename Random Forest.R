setwd("Z:/CT-Mum/Cello Health/170223 Segmentation/Working/CHAID_Random Forest/Wotking_9thApril/Data for Random Forest")
data1 <- read.csv("UC for JAK.csv", header=T, sep = ",")
data2 <- data.frame(data1)

data2$Q2 = as.factor(data2$Q2)
data2$p0 = as.factor(data2$p0)
data2$p1 = as.factor(data2$p1)
data2$p2 = as.factor(data2$p2)
data2$p21a = as.factor(data2$p21a)

data2 <- data2[,-c(10,13,33,32,28,31,34,30)]

str(data2)

#To Deal with Missing Data:
  #library(Amelia)
  #missmap(data2)
  #sapply(data2, function(x) sum(is.na(x)))
  #data2.complete <- na.omit(data2)

#install following libraries using install.package before using them
  library(party)
  library(randomForest)
  #library(rattle)
  library(rpart.plot)
  library(RColorBrewer)

#Model Building
  rf <- randomForest( x = data2, y = data2$Q2, ntree = 1000, mtry=11, keep.forest = TRUE)
  print(rf)
  importance(rf)
  varImpPlot(rf)
  fancyRpartPlot(rf)

  capture.output(importance(rf), file = "Result1.csv", append = FALSE)
  