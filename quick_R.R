################################################################
rm(list = ls())                       
install.packages("pastecs", dependencies = T)

library(readr)
library(data.table)
library(dplyr)
library(Hmisc)
library(pastecs)
library(psych)
library(gmodels)

path <- "Z:/eBay/Item Delivery tpNPS/Coder/Reporting/WIP/Apr'17_Report"
setwd(path)
df <- read_csv("Z:/eBay/Item Delivery tpNPS/US/Reporting/WIP/12_Dec'17/dec17_itm_del_restructured.csv")

data("airquality")
data("mtcars")
data("iris")

myair <- airquality
mycar <- mtcars
myiris <- iris
View(df)

##### descriptive statistics #####
summary(df)
psych::describe(df)
stat.desc(df)

##### Frequencies and Crosstabs #####

# 2-Way Frequency Table
mytable <- table(df$NPS2,df$PROM_ACT_DELIV_2) # A will be rows, B will be columns 
mytable

margin.table(mytable,1) # A frequencies (summed over B) 
margin.table(mytable,2) # B frequencies (summed over A)

round(prop.table(mytable),4)*100  # cell percentages
round(prop.table(mytable,1),4)*100 # row percentages 
round(prop.table(mytable,2),4)*100 # column percentages

# 3-Way Frequency Table
mytable <- table(df$CBT_YN_IND_2, df$PROM_ACT_DELIV_2, df$NPS2)
mytable
ftable(mytable)

# 3-Way Frequency Table
mytable <- xtabs(~df$CBT_YN_IND_2 + df$PROM_ACT_DELIV_2 + df$NPS2, data=df)
ftable(mytable) # print table 
summary(mytable) # chi-square test of indepedence

library(MASS)
mytable <- xtabs(~df$NPS2 + df$PROM_ACT_DELIV_2 + df$CBT_YN_IND_2, data=df)
mytable

# 2-Way Cross Tabulation
library(gmodels)
CrossTable(df$CBT_YN_IND_2, df$PROM_ACT_DELIV_2)

loglm(~df$NPS2 + df$PROM_ACT_DELIV_2 + df$CBT_YN_IND_2, mytable)

# rename programmatically 
library(reshape)
mydata <- rename(mydata, c(oldname="newname"))

# Recoding 1 through 4 to 0 and 5 and 6 to 1
mydata$Q1 <- recode(mydata$Q1, "1:4=0; 5:6=1")

