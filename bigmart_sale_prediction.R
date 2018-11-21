#####################################################
rm(list = ls())                       
path <- "D:/NOTES/Analytics Notes/Practice/Hackathons/Bigmart Sales Prediction - R"
setwd(path)
load(file = "Bigmart_Sale_Prediction")
install.packages("dummies", dependencies = T)
library(ggplot2)
library(plyr)
library(dummies)
library(dplyr)        
save.image("D:/NOTES/Analytics Notes/Practice/R/practice/IMG_till_date.RData")
save.image("D:/NOTES/Analytics Notes/Practice/Hackathons/Bigmart Sales Prediction - R/
           Bigmart_Sale_Prediction")

################# Big Mart Sales Prediction #############################

train <- read.csv("Train_UWu5bXk.csv")
test <- read.csv("Test_u94Q5KV.csv")

dim(train)
dim(test)
str(train)
table(is.na(train))
colSums(is.na(train))
summary(train)

ggplot(train, aes(x=Item_Visibility, y=Item_Outlet_Sales)) + geom_point(size=2.5, color="orange") 
+ xlab("Item_Visibility") + ylab("Item_Outlet_Sales") + ggtitle("Item_Visibility v/s Item_Outlet_Sales")

ggplot(train, aes(Outlet_Identifier, Item_Outlet_Sales)) + geom_bar(stat = "identity", color = "green") 
+ theme(axis.text.x = element_text(angle = 70, vjust = 0.5, color = "black"))  
+ ggtitle("Outlets vs Total Sales") + theme_bw()

ggplot(train, aes(x=Outlet_Identifier, y=Item_Outlet_Sales)) + geom_point(size=2.5, color="blue") 
+ xlab("Outlet_Identifier") + ylab("Item_Outlet_Sales") 
+ ggtitle("Outlet_Identifier v/s Item_Outlet_Sales")

ggplot(train, aes(Item_Type, Item_Outlet_Sales)) + geom_bar( stat = "identity") 
+ theme(axis.text.x = element_text(angle = 70, vjust = 0.5, color = "navy")) + xlab("Item Type") 
+ ylab("Item Outlet Sales")+ggtitle("Item Type vs Sales")

ggplot(train, aes(Item_Type, Item_MRP)) + geom_boxplot() + ggtitle("Box Plot") 
+ theme(axis.title.x = element_text(angle = 70, vjust = 0.5, color = "red")) + xlab("Item_Type") 
+ ylab("Item_MRP")   + ggtitle("Item Type v/s Item MRP")

dim(train)
dim(test)

test$Item_Outlet_Sales <- 1
combi <- rbind(train, test)

combi$Item_Weight[is.na(combi$Item_Weight)] <- median(combi$Item_Weight, na.rm = TRUE)
                                table(is.na(combi$Item_Weight))

combi$Item_Visibility <- ifelse(combi$Item_Visibility == 0, 
                                median(combi$Item_Visibility),combi$Item_Visibility)

levels(combi$Outlet_Size)[1] <- "Other"

combi$Item_Fat_Content <- revalue(combi$Item_Fat_Content, c("LF" = "Low Fat", "reg" = "Regular"))
combi$Item_Fat_Content <- revalue(combi$Item_Fat_Content, c("low fat" = "Low Fat"))
table(combi$Item_Fat_Content)

a <- combi%>% group_by(Outlet_Identifier)%>% tally()
head(a)
View(a)

names(a)[2] <- "Outlet Count"
combi <- left_join(a, combi, by = "Outlet_Identifier")

b <- combi%>% group_by(Item_Identifier)%>% tally()
head(b)
View(combi)

combi <- merge(b, combi, by = "Item_Identifier")

c <- combi%>% select(Outlet_Establishment_Year)%>% 
  mutate(Outlet_Year = 2013 - combi$Outlet_Establishment_Year)  
head(c)
combi <- full_join(c,combi)
combi <- merge(c, combi, by = "Outlet_Establishment_Year")

q <- substr(combi$Item_Identifier,1,2)
q <- gsub("FD","Food", q)
q <- gsub("DR","Drinks", q)
q <- gsub("NC","Non-Consumable", q)
table(q)

combi$Item_Type_New <- q

combi$Item_Fat_Content <- ifelse(combi$Item_Fat_Content == "Regular",1,0)

sample <- select(combi, Outlet_Location_Type)
demo_sample <- data.frame(model.matrix(~.-1, sample))
head(demo_sample)

combi <- dummy.data.frame(combi, name = 
                c('Outlet_Size', 'Outlet_Location_Type', 'Outlet_Type', 'Item_Type_New'), sep = '_')

