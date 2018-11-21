install.packages("psych", dependencies = T )
install.packages("dplyr", dependencies = T )
install.packages("polycor", dependencies = T )
install.packages("xlsx", dependencies = T )


library(psych)
library(dplyr)
library(polycor)
library(xlsx)

############### Clear environment #################

rm(list = ls())

############### Loading data #####################

data <- read.csv("Z:/CT-Mum/Cello Health/170223 Segmentation/Working/Factor_Analysis_Data_PRF_P13.csv")

##load("Z:/CT-Mum/DB5/Smartphone Switcher/Working/Factor on Screeners/P13_C/P13_C.results.RData")

############### Chosing Variables #####################

P13_C <- data[,2:27]

############### Getting complete cases #####################

P13_C.complete <- na.omit(P13_C)

############### Compute Hetcor matrix #####################

P13_C.complete1 <- data.matrix(P13_C.complete) 

P13_C.hect <- polycor::hetcor(sapply(P13_C.complete1, as.factor, ML=TRUE))$cor 

############### KMO and Eigen value #####################

kmo <- KMO(P13_C.hect)
ev <- eigen(P13_C.hect)$value

############### Running PCA #####################

fit.P13_C.def3<- psych::principal(P13_C.complete, nfactors = 3, rotate = "varimax")
load.P13_C.def3 <- xtable::xtable(unclass(fit.P13_C.def3$loadings))
fit.P13_C.def3$loadings

plot(fit.P13_C.def3$values, type="b", ylab="Eigenvalues", 
     xlab="Component", lab=c(10,10,10))

fit.P13_C.for4 <- psych::principal(P13_C.complete, nfactors = 4, rotate = "varimax")
load.P13_C.for4 <- xtable::xtable(unclass(fit.P13_C.for4$loadings))
fit.P13_C.for4$loadings

fit.P13_C.for5 <- psych::principal(P13_C.complete, nfactors = 5, rotate = "varimax")
load.P13_C.for5 <- xtable::xtable(unclass(fit.P13_C.for5$loadings))
fit.P13_C.for5$loadings

fit.P13_C.for6 <- psych::principal(P13_C.complete, nfactors = 6, rotate = "varimax")
load.P13_C.for6 <- xtable::xtable(unclass(fit.P13_C.for6$loadings))
fit.P13_C.for6$loadings

fit.P13_C.for7 <- psych::principal(P13_C.complete, nfactors = 7, rotate = "varimax")
load.P13_C.for7 <- xtable::xtable(unclass(fit.P13_C.for7$loadings))
fit.P13_C.for7$loadings

fit.P13_C.for8 <- psych::principal(P13_C.complete, nfactors = 8, rotate = "varimax")
load.P13_C.for8 <- xtable::xtable(unclass(fit.P13_C.for8$loadings))
fit.P13_C.for8$loadings

fit.P13_C.for9 <- psych::principal(P13_C.complete, nfactors = 9, rotate = "varimax")
load.P13_C.for9 <- xtable::xtable(unclass(fit.P13_C.for9$loadings))
fit.P13_C.for9$loadings

fit.P13_C.for10 <- psych::principal(P13_C.complete, nfactors = 10, rotate = "varimax")
load.P13_C.for10 <- xtable::xtable(unclass(fit.P13_C.for10$loadings))
fit.P13_C.for10$loadings

fit.P13_C.for11 <- psych::principal(P13_C.complete, nfactors = 11, rotate = "varimax")
load.P13_C.for11 <- xtable::xtable(unclass(fit.P13_C.for11$loadings))
fit.P13_C.for11$loadings

fit.P13_C.for12 <- psych::principal(P13_C.complete, nfactors = 12, rotate = "varimax")
load.P13_C.for12 <- xtable::xtable(unclass(fit.P13_C.for12$loadings))
fit.P13_C.for12$loadings


setwd("Z:/CT-Mum/Cello Health/170223 Segmentation/Working")

##write.csv(load.P13_C.def, file = "load.P13_C.final.csv")
##write.csv(load.P13_C.for4, file = "load.P13_C.final.csv")

write.xlsx(load.P13_C.def3, file = "load.P13_C.final.xlsx", sheetName = "load.P13_C.for3",append = T)
write.xlsx(load.P13_C.for4, file = "load.P13_C.final.xlsx", sheetName = "load.P13_C.for4",append = T)
write.xlsx(load.P13_C.for5, file = "load.P13_C.final.xlsx", sheetName = "load.P13_C.for5",append = T)
write.xlsx(load.P13_C.for6, file = "load.P13_C.final.xlsx", sheetName = "load.P13_C.for6",append = T)
write.xlsx(load.P13_C.for7, file = "load.P13_C.final.xlsx", sheetName = "load.P13_C.for7",append = T)
write.xlsx(load.P13_C.for8, file = "load.P13_C.final.xlsx", sheetName = "load.P13_C.for8",append = T)
write.xlsx(load.P13_C.for9, file = "load.P13_C.final.xlsx", sheetName = "load.P13_C.for9",append = T)
write.xlsx(load.P13_C.for10, file = "load.P13_C.final.xlsx", sheetName = "load.P13_C.for10",append = T)
write.xlsx(load.P13_C.for11, file = "load.P13_C.final.xlsx", sheetName = "load.P13_C.for11",append = T)
write.xlsx(load.P13_C.for12, file = "load.P13_C.final.xlsx", sheetName = "load.P13_C.for12",append = T)

