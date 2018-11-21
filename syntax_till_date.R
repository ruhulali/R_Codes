#####################################################
rm(list = ls())                       
path <- "D:/NOTES/Analytics Notes/Practice/Hackathons/Bigmart Sales Prediction - R"
setwd(path)
load(file = "IMG_till_date")
load(file = "Bigmart_Sale_Prediction")
install.packages("data.frame", dependencies = T)
library(ggplot2)
library(dplyr)
library(dummies)
library(data.frame)
save.image("D:/NOTES/Analytics Notes/Practice/R/practice/IMG_till_date.RData")
save.image("D:/NOTES/Analytics Notes/Practice/Hackathons/Bigmart Sales Prediction - R/Bigmart_Sale_Prediction")
#####################################################

######################### open excel file ############################
Openxlsx::read.xlsx("Dataframe.xlsx",sheet=3,colNames=FALSE)
Xlsx::read.xlsx("Dataframe.xlsx",sheetIndex=3,header=FALSE)
XLConnect::readWorksheetFromFile("Dataframe.xlsx",sheet=3,header=FALSE)
######################### open excel file ############################

######################### data_handler ######################################
print(ls())
class(df$trigger_dt) - #finding class 
dfa$dfb <- NULL - #remove data
dfb <- NULL - #remove data
View(df) - #viewing data
names(df) - #variable names
describe(df$FAST_N_FREE_YN_IND) - #frequency distribution
library(pastecs) - stat.desc(mycar) #frequency distribution
library(psych) - describe(mycar) - #frequency distribution
cumsum(df$Rcvd_when_prmsd) - #cumulative frequency distribution
table(df$FAST_N_FREE_YN_IND) - #frequency distribution of a categorical variable
df= rename(df, variable1=var1) - #rename variable
trimws() - #remove leading and trailing spaces
dim(train) - #number of columns and rows 
table(is.na(train)) - #count of NA
colSums(is.na(train)) - #column with NA count
find_funs("filter") #source("https://sebastiansauer.github.io/Rcode/find_funs.R")
library(scales)  # for percentage scales  
######################### data_handler ######################################  

var.1 = c(0,1,2,3)
var.2 <- c("learn", "R")
c(TRUE,1) -> var.3
print(var.1)
cat("var.1 is", var.1, "\n")
cat("var.2 is", var.2, "\n")
cat("var.3 is", var.3, "\n")

2+3
x <- 7+8
x

attributes(x)

a <- c(1.8, 4.5)
b <- c(1+2i, 3-6i)
d <- c(23, 44)
e <- vector("logical", length = 5)

qt <- c("Time", 24, "October", TRUE, 3.33)
ab <- c(TRUE, 24)
cd <- c(2.5, "May")

class(cd)

bar <- 0:5
class(bar)
as.numeric(bar)
class(bar)
as.character(bar)
class(bar)

my_list <- list(22, "bc", TRUE, 1+2i)
my_list
my_list[[3]]
my_list[3]

my_matrix <- matrix(1:6, nrow = 3, ncol = 2)
my_matrix
dim(my_matrix)
attributes(my_matrix)
my_matrix[,2]
my_matrix[2,]
class(my_matrix)

age <- c(23, 35, 46, 58, 61, 79)
age
dim(age) <- c(2,3)
age
class(age)

x <- c(1,2,3,4,5,6)
y <- c(10,20,30,40,50,60)

cbind(x,y)
class(cbind(x,y))

df <- data.frame(name = c("king","george","letti","henry"), score = c(68,79,83,94))
df
class(df)
dim(df)
str(df)
nrow(df)
ncol(df)

df[1:2,2] <- NA
df
is.na(df)
table(is.na(df))
mean(df$score)
mean(df$score, na.rm = TRUE)
df
new_df <- na.omit(df)
new_df

N <- 5
#check if N*5 is greater than 40 or not
if (N*5>40){print("This is FA!!!")} else {print("It is not FA, It is FU@#@")}

age <- 12
while(age<19){print(age)
  age <- age+1}

h <- c(151, 174, 138, 186, 128, 136, 179, 163, 152, 131)
w <- c(63, 81, 56, 91, 47, 57, 76, 72, 62, 48)

relation <- lm(w~h)
print(relation)

print(summary(relation))

##############################
a <- array(c("green", "red"), dim = c(3,3,2))
View(a)
print(a)
###########################
print(sum(1:100))

new.function <- function(a) {
  for (i in 1:a)
    b <- i^2
  print(b)
}

new.function(7)
###########################
mydata <- merge(mydata1, mydata2, by=c("ID"))

#####################

mydata <- read.csv("C:/Users/ruhul.akhtar/Downloads/sampledata.csv")
View(mydata7)

sample_n(mydata,3)
sample_frac(mydata,0.1)
x1=distinct(mydata)
x2=distinct(mydata,Index, .keep_all = TRUE)
x2=distinct(mydata,Index, Y2010, .keep_all = TRUE)

mydata2 = select(mydata, Index, State:Y2008)
mydata = select(mydata, -c(Index, State))
mydata3 = select(mydata, -starts_with("Y"))
mydata4 = select(mydata, contains("I"))
mydata5 = select(mydata, State, everything())
mydata6 = rename(mydata, Index1=Index)
mydata7 = filter(mydata, Index=="A")


#####################################################################################

install.packages("Matrix", dependencies = TRUE)
library(tictoc)
tic()
runif(5555,1,1000)
toc()

############################# dplyr #############################

library(dplyr)
library(Hmisc)

data("mtcars")
data("iris")

mydata <- mtcars
View(mtcars)

head(mtcars)

mynewdata <- tbl_df(mydata)
myirisdata <- tbl_df(iris)

View(mynewdata)
filter(mynewdata, cyl>4 & gear>4)

filter(mynewdata, cyl>4)

psych::describe(myirisdata$Species)

filter(myirisdata, Species %in% c('setosa', 'virginica'))
select(mynewdata, cyl,mpg,hp)
select(mynewdata, -cyl, -mpg)
select(mynewdata, -c(cyl, mpg))
select(mynewdata, cyl:gear)

mynewdata %>% 
  select(cyl, wt, gear) %>%
  filter(wt > 2)

mynewdata %>%
  select(cyl, wt, gear) %>%
  arrange(wt)

mynewdata %>%
  select(cyl, wt, gear) %>%
  arrange(desc(wt))

mynewdata %>%
  select(mpg, cyl) %>%
  mutate(new = mpg*cyl)
#or
new <- mynewdata %>% mutate(new = mpg*cyl)
View(new)

describe(myirisdata$Species)
#or
myirisdata %>%
  group_by(Species) %>%
  summarise(average = mean(Sepal.Length, na.rm = TRUE))

myirisdata %>%
  group_by(Species)%>%
  summarise_each(funs (mean, n()), Sepal.Length, Sepal.Width)

mynewdata %>% rename(miles = mpg)

############################# data.table #############################

data(airquality)
mydata <- airquality
View(mydata)

head(mydata,7)

data(iris)
myiris <- iris

library(data.table)

mydata <- data.table(mydata)
mydata
View(mydata)

myiris <- data.table(myiris)
myiris

mydata[2:4,]
myiris[Species == 'setosa']

myiris[Species %in% c('setosa', 'virginca')]

mydata[, Temp]
mydata[,.(Temp, Month)]

x4 <- sample(1111:9999, 10, replace=F)
View(x4)


