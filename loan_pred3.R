
rm(list= ls())
path <- "E:/NOTES/Analytics Notes/Practice/Kaggle & Hackathons/Loan Prediction III"
setwd(path)

##### loading data #####
library(readr)
aaa <- read_csv("E:/NOTES/Analytics Notes/Practice/Kaggle & Hackathons/Loan Prediction III/train_u6lujuX_CVtuZ9i.csv")

##### data check #####
source("https://sebastiansauer.github.io/Rcode/find_funs.R")
find_funs("recode") #finding functions (googling) 

names(aaa)
dim(aaa)
table(is.na(aaa))
colSums(is.na(aaa)) #counts blanks in colums
mlr::summarizeColumns(aaa)
psych::describe(aaa)
table(aaa$Gender)
## or ##
t = data.frame(table(aaa$Gender))
t$percent= round(t$Freq / sum(t$Freq)*100,2)
t

##### counts, percentage with graph #####

#for single variable
library(gmodels)
library(ggplot2)

ggplot(aaa,aes(x=Gender,fill=Married)) + geom_bar() + CrossTable(aaa$Gender, format="SPSS")
ggplot(na.omit(aaa),aes(x=Gender,fill=Gender)) + geom_bar(position="dodge") + CrossTable(aaa$Gender, format="SPSS")
-------
  
tips2 <- tips %>% count(day) %>% mutate(perc = n / nrow(tips))
tips2
# OR #
aaa1 <- aaa %>% count(Dependents) %>% mutate(perc = n / nrow(aaa))
aaa1
-------
ggplot(tips2, aes(x = reorder(day,-perc), y = perc, fill=day)) + geom_bar(stat = "identity")+
geom_text(aes(label = round(perc, digits = 4)*100), size = 5, hjust = 0.5, vjust = 2)
# OR #
ggplot(aaa1, aes(x = reorder(Dependents,-perc), y = perc, fill=Dependents)) + geom_bar(stat = "identity")+
geom_text(aes(label = round(perc, digits = 4)*100), size = 5, hjust = 0.5, vjust = 2)
-------
  
ggplot(tips2, aes(x = reorder(day,-perc), y = perc, fill=day)) + geom_bar(stat = "identity")+
geom_text(aes(label = round(perc, digits = 4)*100), size = 5, hjust = 0.5, vjust = 2)+
CrossTable(tips$day, format="SPSS")
# OR #
ggplot(aaa1, aes(x = reorder(Dependents,-perc), y = perc, fill=Dependents)) + geom_bar(stat = "identity")+
geom_text(aes(label = round(perc, digits = 4)*100), size = 5, hjust = 0.5, vjust = 2)+
CrossTable(aaa$Dependents, format="SPSS")
-------
  
hist(tips$tip, breaks=100, main="tip Chart", xlab="tip")
boxplot(tips$tip)


#for multiple variable
ggplot(aaa, aes(x= Gender,  group=Married)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
  geom_text(aes( label = scales::percent(..prop..),y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(y = "Percent", fill="Gender") + facet_grid(~Married) +
  scale_y_continuous(labels = scales::percent) + 
CrossTable(aaa$Married,aaa$Gender, prop.chisq=F, format="SPSS")

ggplot(tips, aes(x= sex,  group=day)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
  geom_text(aes( label = scales::percent(..prop..),y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(y = "Percent", fill="sex") + facet_grid(~day) +
  scale_y_continuous(labels = scales::percent) + 
  CrossTable(tips$sex,tips$day, prop.chisq=F, format="SPSS")


tab1<- table(aaa$Gender, aaa$Married, aaa$Dependents)
prop.table(tab1)
round(ftable(prop.table(tab1)*100),2)

CrossTable(aaa$Gender, aaa$Married, prop.chisq=F, format="SPSS")
##########


aaa$Gender_1 <- ifelse(aaa$Gender == 'Male', "1",  "2")
aaa$Married_1 <- ifelse(aaa$Married == 'Yes', "1",  "2")
aaa$Dependents_1[aaa$Dependents == '0'] <- '0'
aaa$Dependents_1[aaa$Dependents == '1'] <- '1'
aaa$Dependents_1[aaa$Dependents == '2'] <- '2'
aaa$Dependents_1[aaa$Dependents == '3+'] <- '3'


tab1<-table(aaa$Gender_1, aaa$Married_1, aaa$Dependents_1) #Create a table of counts (object of class table)
barplot(tab1) #Stacked barchart
barplot(tab1,beside=T) #Barchart (not stacked)
barplot(tab1,horiz=T) #Bars are shown horizontally
dotchart(tab1) #Cleveland's dot chart
mosaicplot(tab1) #from the {graphics} package

g + geom_bar(aes(x=Dependents, fill = Gender))
g +
  geom_bar(aes(fill = Dependents), position = position_stack(reverse = TRUE)) +
  coord_flip() +
  theme(legend.position = "top")

qplot(Gender, data=aaa)

ggplot(data = aaa, aes( x= Dependents , y= Married, fill= Gender ) ) + geom_bar( stat = 'identity')    # print bar chart
    

ggplot(data=aaa, aes(x=Dependents, y=Married, fill=Gender)) + geom_bar(stat="identity", position=position_dodge())

ggplot(aaa, aes(x=Gender, y=Dependents, fill=Married)) + geom_bar(position='dodge', stat='identity')

library(data.table)
a <- dcast(aaa, aaa$Married ~ aaa$Gender)
a

################################

install.packages("DMwR")
library(DMwR)
a <- aaa
a$Gender <- as.numeric("ApplicantIncome")
knnOutput <- knnImputation(a[, !names(a) %in% "ApplicantIncome"])  # perform knn imputation.
anyNA(knnOutput)

data <- NULL
data <- airquality
View(data)
data[4:10,3] <- rep(NA,7)
data[1:5,4] <- NA

library(psych)
summary(aaa$ApplicantIncome)

sd(aaa$ApplicantIncome)
