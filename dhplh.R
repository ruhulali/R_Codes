######################### Path & Saving Data ######################################
rm(list = ls())
path <- "E:/NOTES/Analytics Notes/Practice/Kaggle & Hackathons/DataHack Premier League Hackathon/train/train"
setwd(path)
save.image("Z:/eBay/Item Delivery tpNPS/Coder/Reporting/WIP/Apr'17_Report/itmdel_apr'17.RData")
load(file = "itmdel_mar'17")
install.packages("psych", dependencies = T)
library(psych)

df <- read.csv("E:/NOTES/Analytics Notes/Practice/Kaggle & Hackathons/DataHack Premier League Hackathon/train/train/match_data.csv")

## command used to store R objects in a file ##
save(x, file = "x.Rdata")

######################### Data Checks ######################################
class(df) - #finding class 
dfa$dfb <- NULL - #remove data
dfa <- NULL - #remove data
names(df) - #variable names
dim(df) #nrow & ncol
psych::describe(df) - #frequency distribution
mlr::summarizeColumns(df)
cumsum(df$Rcvd_when_prmsd) - #cumulative frequency distribution
colSums(is.na(df)) #column with NA count
levels(df) #values in a variable
filter(deliveries, batsman_runs>4) #filter
table(is.na(df)) #is.na
df= rename(df, variable1=var1) - #rename variable
trimws() - #remove leading and trailing spaces
library(dplyr) - #Drop Multiple Variables
df = select(mydata, -c(x,y,z))
-------
  
table(df$winner) - #frequency distribution of a categorical variable
#OR#
t = data.frame(table(df$winner))
t$percent= round(t$Freq / sum(t$Freq)*100,2)
t
-------
  
find_funs("table.extended") 
source("https://sebastiansauer.github.io/Rcode/find_funs.R")
# OR #
install.packages("sos")
findFn("plotFilterValuesGGVIS")
-------
library(scales)  # for percentage scales
-------------------------------------------------------------------
start.time <- Sys.time() - #measure execution time of a program
runif(5555,1,1000)
end.time <- Sys.time()
end.time - start.time
# OR #
library(tictoc)
tic()
runif(5555,1,1000)
toc()
-------------------------------------------------------------------
  
######################### Frequencies and Crosstabs #########################

# 2-Way Frequency Table
mytable <- table(df$season,df$winner) # A will be rows, B will be columns 
mytable

margin.table(mytable,1) # A frequencies (summed over B) 
margin.table(mytable,2) # B frequencies (summed over A)

round(prop.table(mytable),4)*100  # cell percentages
round(prop.table(mytable,1),4)*100 # row percentages 
round(prop.table(mytable,2),4)*100 # column percentages

# 3-Way Frequency Table
mytable <- xtabs(~df$season + df$winner, data=df)
ftable(mytable) # print table 

######################### Data Visualization ######################################

## sample data ##
data(tips, package = "reshape2")
View(tips)

#for one categorical variable
library(gmodels)
library(ggplot2)

ggplot(tips,aes(x=sex,fill=day)) + geom_bar() + CrossTable(tips$sex, format="SPSS")
ggplot(na.omit(tips),aes(x=sex,fill=day)) + geom_bar(position="dodge") + CrossTable(tips$sex, format="SPSS")
-------
tips2 <- tips %>% count(day) %>% mutate(perc = n / nrow(tips))
tips2
ggplot(tips2, aes(x = reorder(day,-perc), y = perc, fill=day)) + geom_bar(stat = "identity")+
geom_text(aes(label = round(perc, digits = 4)*100), size = 5, hjust = 0.5, vjust = 2)
-------
ggplot(tips2, aes(x = reorder(day,-perc), y = perc, fill=day)) + geom_bar(stat = "identity")+
geom_text(aes(label = round(perc, digits = 4)*100), size = 5, hjust = 0.5, vjust = 2)+
CrossTable(tips$day, format="SPSS")

#for one continuos variable
hist(tips$tip, breaks=100, main="tip Chart", xlab="tip")
boxplot(tips$tip)

#for multiple categorical variable
ggplot(tips, aes(x= day,  group=sex)) + geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
  geom_text(aes( label = scales::percent(..prop..),y= ..prop.. ), stat= "count", vjust = 2) +
  labs(y = "Percent", fill="Sex") + facet_grid(~sex) + scale_y_continuous(labels = scales::percent) + 
  CrossTable(tips$day,tips$sex, format="SPSS")
-------
  tab1<- table(tips$sex, tips$day, tips$time)
prop.table(tab1)
round(ftable(prop.table(tab1)*100),2)
-------
  
  ######################### Data Manipulation ######################################

##upload##
library(readr)
df <- read_csv("Z:/eBay/Item Delivery tpNPS/Reporting/WIP/10_Oct'17/Blueocean_Verbatim_Oct'17_Delivery_TP_dataset.csv")

##create month & year##
library(tidyverse)
Month <- strptime(df$trigger_dt, "%m/%d/%Y")
df$Months <- format(Month, "%b'%y")
#or
df$Months <- format(Month, "%y-%b")

##create quarter & year##
library(zoo)
df$Quarter=as.yearqtr(df$trigger_dt,"%m/%d/%Y")
df$Quarter=as.yearqtr(as.Date( df$trigger_dt, "%m/%d/%Y" ))

# Recoding 1 through 4 to 0 and 5 and 6 to 1
mydata$Q1 <- recode(mydata$Q1, "1:4=0; 5:6=1")

##recode SAP_GLBL_NAME_2##
df$SAP_GLBL_NAME_2[df$SAP_GLBL_NAME == 'Auto - Parts'] <- 'Automotive Parts & Accessories'
df$SAP_GLBL_NAME_2[df$SAP_GLBL_NAME == 'Travel'] <- 'Automotive Parts & Accessories'

df$LABEL_TRANS_2[df$LABEL_TRANS == '1'] <- 'Labeled'
df$LABEL_TRANS_2[df$LABEL_TRANS == '0'] <- 'Non-Labeled'

df$TRKING_SRC_CRE_DT_2 <- ifelse(df$TRKING_SRC_CRE_DT == '?', "Non-Tracked",  "Tracked")

df$ITEM_PRICE_AMT_2 <- ifelse(df$ITEM_PRICE_AMT >= 0.01 & df$ITEM_PRICE_AMT <= 49.99, "$0.01-49.99", 
                              ifelse(df$ITEM_PRICE_AMT >= 50 & df$ITEM_PRICE_AMT <= 449.99, "$50-449.99",
                                     "$450+"))

##recode NSAT##
df$NPS <- ifelse(df$tp_sat <= 6,-1,
                 ifelse((df$tp_sat == 7 | df$tp_sat == 8),0,
                        ifelse(df$tp_sat > 8,1,NA)))

##save csv file##
write.csv(df, "Z:/eBay/Item Delivery tpNPS/Coder/Allocation/9_Sep'17/sep'17_itm_del_restructured.csv", row.names = F)

### NPS Calc slide#3 ###
round(mean(df$NPS)*100,2) 

### PPD_% Calc slide#3 ###
y = xtabs(~ NPS2 + NPS2, df)
y
z <- data.frame(y)
z[1,"nps"] <- round(z[1,2]/sum(z$Freq)*100,2)
z[2,"nps"] <- round(z[2,2]/sum(z$Freq)*100,2)
z[3,"nps"] <- round(z[3,2]/sum(z$Freq)*100,2)
z

### regression - overall ###
reg <-  lm(tp_sat ~ Rcvd_when_prmsd+Shp_fast+Shp_charges_rsnbl+Pckgd_well, data = df)
summary(reg)
library(QuantPsyc)
regression <- lm.beta(reg)
View(regression)

#### pivot with count and nsat ####
library(data.table)
library(reshape2)
dfa = dcast(df, CBT_YN_IND_2 + FM_BYR_SGMNTN_DESC_2 + SHPNG_FEE_AMT_2 ~ NPS2, subset=.(CBT_YN_IND_2 == "Yes"))
dfa
dfa$total = with(dfa, Det+Pass+Prom)
dfa$detpct = (dfa$Det / dfa$total)*100
dfa$passpct = (dfa$Pass / dfa$total)*100
dfa$prompct = (dfa$Prom / dfa$total)*100
dfa$nsat <- round((dfa$prompct - dfa$detpct),2)
dfa
View(dfa)

## To create a table in R language without using external files? ##
## The below code will open an Excel Spreadsheet for entering data into MyTable.
MyTable= data.frame ()
edit (MyTable)

##  The significance of transpose in R language, it is the easiest method for reshaping the data before analysis.
#Transpose 
t() 

## debug and test R programming code?
## R code can be tested using Hadley's testthat package.

## function in R language to replace the missing value in a vector with the mean of that vector.
mean_impute <- function(x) {x [is.na(x)] <- mean(x, na.rm = TRUE); x}

## sorting in R language ##
Order()

## Combine multiple different string like "Data", "Science", "in" ,"R", "Programming" as a single string "Data_Science_in_R_Programmming" ##
paste("Data", "Science", "in" ,"R", "Programming", sep = "-")
      
      