######################### file/path/package ######################################
rm(list = ls())
path <- "Z:/eBay/Item Delivery tpNPS/Coder/Reporting/WIP/Yearly-2016"
setwd(path)
save.image("Z:/eBay/Item Delivery tpNPS/Coder/Reporting/WIP/Yearly-2016")
load(file = "itmdel_yearly'16-17")

install.packages("foreign", dependencies = T)
library(sqldf)
library(readr)
library(dplyr)
library(lubridate)
library(zoo)
library(QuantPsyc)
library(plyr)
library(reshape2)
library(reshape)
library(data.table)
library(LearnBayes)
library(Hmisc)

######################### data_handler ######################################

print(ls())
class(df$trigger_dt) - #finding class 
dfa$dfb <- NULL - #remove data
dfb <- NULL - #remove data
rm(z) - #delete data from environment  
View(df) - #viewing data
names(df) - #variable names
describe(df$FAST_N_FREE_YN_IND) - #frequency disttibution
cumsum(df$Rcvd_when_prmsd) - #cumulative frequency distribution
table(df$FAST_N_FREE_YN_IND) - #frequency distribution of a categorical variable
df= rename(df, variable1=var1) - #rename variable
trimws() - #remove leading and trailing spaces

start.time <- Sys.time() - #measure execution time of a program
runif(5555,1,1000)
end.time <- Sys.time()
end.time - start.time

OR

library(tictoc)
tic()
runif(5555,1,1000)
toc()

library(dplyr) - #Drop Multiple Variables
df = select(mydata, -c(x,y,z))
find_funs("subset") - source("https://sebastiansauer.github.io/Rcode/find_funs.R")
library(scales)  # for percentage scales
################### SQL in R ###############################
names(df)

dfb <- sqldf("select Months, Round(AVG(NPS)*100,2) from df group by Months order by SRVY_RPRTNG_PRD")
View(dfb)

######################### script ######################################

##upload## "readr"
df <- read_csv("Z:/eBay/Item Delivery tpNPS/Coder/Reporting/WIP/Yearly-2016/Jun'16-May'17_All.csv")

##upload restructured data##
df <- read_csv("Z:/eBay/Item Delivery tpNPS/Coder/Reporting/WIP/Yearly-2016/Jun'16-May'17_All_restd_2.csv")

##create month & year##
Month <- strptime(df$trigger_dt, "%m/%d/%Y")
df$Months <- format(Month, "%b'%y")
OR
df$Months <- format(Month, "%y-%b")


df$Months_2[df$Months == "Jun'16"] <- 6
df$Months_2[df$Months == "Jul'16"] <- 7
df$Months_2[df$Months == "Aug'16"] <- 8
df$Months_2[df$Months == "Sep'16"] <- 9
df$Months_2[df$Months == "Oct'16"] <- 10
df$Months_2[df$Months == "Nov'16"] <- 11
df$Months_2[df$Months == "Dec'16"] <- 12
df$Months_2[df$Months == "Jan'17"] <- 1
df$Months_2[df$Months == "Feb'17"] <- 2
df$Months_2[df$Months == "Mar'17"] <- 3
df$Months_2[df$Months == "Apr'17"] <- 4
df$Months_2[df$Months == "May'17"] <- 5


##create quarter & year## "zoo"
df$Quarter=as.yearqtr(df$trigger_dt,"%m/%d/%Y")
OR
df$Quarter=as.yearqtr(as.Date( df$trigger_dt, "%m/%d/%Y" ))

##recode SAP_GLBL_NAME_2##
df$SAP_GLBL_NAME_2[df$SAP_GLBL_NAME == 'Coins'] <- 'Collectibles and Art' 
df$SAP_GLBL_NAME_2[df$SAP_GLBL_NAME == 'Books'] <- 'Collectibles and Art'
df$SAP_GLBL_NAME_2[df$SAP_GLBL_NAME == 'Collectibles'] <- 'Collectibles and Art'
df$SAP_GLBL_NAME_2[df$SAP_GLBL_NAME == 'Sports Memorabilia'] <- 'Collectibles and Art'
df$SAP_GLBL_NAME_2[df$SAP_GLBL_NAME == 'Hobbies & Crafts'] <- 'Collectibles and Art'
df$SAP_GLBL_NAME_2[df$SAP_GLBL_NAME == 'Entertainment Memorabilia'] <- 'Collectibles and Art'
df$SAP_GLBL_NAME_2[df$SAP_GLBL_NAME == 'Music'] <- 'Collectibles and Art'
df$SAP_GLBL_NAME_2[df$SAP_GLBL_NAME == 'Photo'] <- 'Collectibles and Art'
df$SAP_GLBL_NAME_2[df$SAP_GLBL_NAME == 'Antiques'] <- 'Collectibles and Art'
df$SAP_GLBL_NAME_2[df$SAP_GLBL_NAME == 'Art'] <- 'Collectibles and Art'
df$SAP_GLBL_NAME_2[df$SAP_GLBL_NAME == 'Stamps'] <- 'Collectibles and Art'
df$SAP_GLBL_NAME_2[df$SAP_GLBL_NAME == 'Pottery & Glass'] <- 'Collectibles and Art'
df$SAP_GLBL_NAME_2[df$SAP_GLBL_NAME == 'Tickets'] <- 'Collectibles and Art'

df$SAP_GLBL_NAME_2[df$SAP_GLBL_NAME == 'Auto - Parts'] <- 'Automotive Parts & Accessories'
df$SAP_GLBL_NAME_2[df$SAP_GLBL_NAME == 'Travel'] <- 'Automotive Parts & Accessories'

df$SAP_GLBL_NAME_2[df$SAP_GLBL_NAME == 'Baby'] <- 'Children & Baby Products'

df$SAP_GLBL_NAME_2[df$SAP_GLBL_NAME == 'Jewelry, Gems, Watches'] <- 'Clothing, Shoes, & Accessories'
df$SAP_GLBL_NAME_2[df$SAP_GLBL_NAME == 'Clothing & Accessories'] <- 'Clothing, Shoes, & Accessories'
df$SAP_GLBL_NAME_2[df$SAP_GLBL_NAME == 'Health & Beauty'] <- 'Clothing, Shoes, & Accessories'

df$SAP_GLBL_NAME_2[df$SAP_GLBL_NAME == 'Business (Office & Industrial)'] <- 'Business (Office & Industrial)'

df$SAP_GLBL_NAME_2[df$SAP_GLBL_NAME == 'Cell Phones & Accessories'] <- 'Electronics and Computers'
df$SAP_GLBL_NAME_2[df$SAP_GLBL_NAME == 'Computers'] <- 'Electronics and Computers'
df$SAP_GLBL_NAME_2[df$SAP_GLBL_NAME == 'Telecomm'] <- 'Electronics and Computers'
df$SAP_GLBL_NAME_2[df$SAP_GLBL_NAME == 'Consumer Electronics - Video'] <- 'Electronics and Computers'
df$SAP_GLBL_NAME_2[df$SAP_GLBL_NAME == 'Consumer Electronics - Other'] <- 'Electronics and Computers'
df$SAP_GLBL_NAME_2[df$SAP_GLBL_NAME == 'Musical Instruments'] <- 'Electronics and Computers'
df$SAP_GLBL_NAME_2[df$SAP_GLBL_NAME == 'Consumer Electronics - Audio'] <- 'Electronics and Computers'
df$SAP_GLBL_NAME_2[df$SAP_GLBL_NAME == 'Home Appliances'] <- 'Electronics and Computers'
df$SAP_GLBL_NAME_2[df$SAP_GLBL_NAME == 'Video Games'] <- 'Electronics and Computers'
df$SAP_GLBL_NAME_2[df$SAP_GLBL_NAME == 'DVDs & Movies'] <- 'Electronics and Computers'

df$SAP_GLBL_NAME_2[df$SAP_GLBL_NAME == 'Home Improvement'] <- 'Home & Garden'
df$SAP_GLBL_NAME_2[df$SAP_GLBL_NAME == 'Home Furnishing'] <- 'Home & Garden'
df$SAP_GLBL_NAME_2[df$SAP_GLBL_NAME == 'Food & Gourmet'] <- 'Home & Garden'

df$SAP_GLBL_NAME_2[df$SAP_GLBL_NAME == 'Sporting Goods'] <- 'Sporting Goods'

df$SAP_GLBL_NAME_2[df$SAP_GLBL_NAME == 'Toys'] <- 'Toys & Hobbies'
df$SAP_GLBL_NAME_2[df$SAP_GLBL_NAME == 'Dolls & Bears'] <- 'Toys & Hobbies'

##rename FREE_SHPNG_YN_IND##
df$FREE_SHPNG_YN_IND_2[df$FREE_SHPNG_YN_IND == 'Y'] <- 'Yes'
df$FREE_SHPNG_YN_IND_2[df$FREE_SHPNG_YN_IND == 'N'] <- 'No'

##rename FAST_N_FREE_YN_IND##
df$FAST_N_FREE_YN_IND_2[df$FAST_N_FREE_YN_IND == 'Y'] <- 'Yes'
df$FAST_N_FREE_YN_IND_2[df$FAST_N_FREE_YN_IND == 'N'] <- 'No'

##recode SPE_ACT_HT_FULFLD##
df$SPE_ACT_HT_FULFLD_2[df$SPE_ACT_HT_FULFLD == '1 DAY EARLY'] <- 'Early'
df$SPE_ACT_HT_FULFLD_2[df$SPE_ACT_HT_FULFLD == '2 OR MORE DAYS EARLY'] <- 'Early'
df$SPE_ACT_HT_FULFLD_2[df$SPE_ACT_HT_FULFLD == '1 DAY LATE'] <- 'Late'
df$SPE_ACT_HT_FULFLD_2[df$SPE_ACT_HT_FULFLD == '2 OR MORE DAYS LATE'] <- 'Late'
df$SPE_ACT_HT_FULFLD_2[df$SPE_ACT_HT_FULFLD == 'EXACT HANDLING'] <- 'On Time'
df$SPE_ACT_HT_FULFLD_2[df$SPE_ACT_HT_FULFLD == '?'] <- ''

##recode PROM_ACT_DELIV##
df$PROM_ACT_DELIV_2[df$PROM_ACT_DELIV == '2 DAYS EARLY'] <- 'Early'
df$PROM_ACT_DELIV_2[df$PROM_ACT_DELIV == '3 OR MORE DAYS EARLY'] <- 'Early'
df$PROM_ACT_DELIV_2[df$PROM_ACT_DELIV == '1 DAY EARLY'] <- 'Early'
df$PROM_ACT_DELIV_2[df$PROM_ACT_DELIV == '2 - 5 DAYS LATE'] <- 'Late'
df$PROM_ACT_DELIV_2[df$PROM_ACT_DELIV == '1 DAY LATE'] <- 'Late'
df$PROM_ACT_DELIV_2[df$PROM_ACT_DELIV == '5+ DAYS LATE'] <- 'Late'
df$PROM_ACT_DELIV_2[df$PROM_ACT_DELIV == 'EXACT'] <- 'On Time'
df$PROM_ACT_DELIV_2[df$PROM_ACT_DELIV == '?'] <- ''

##recode ITM_CNDTN##
df$ITM_CNDTN_2[df$ITM_CNDTN == 'NEW'] <- 'New'
df$ITM_CNDTN_2[df$ITM_CNDTN == 'NOT SPECIFIED'] <- ''
df$ITM_CNDTN_2[df$ITM_CNDTN == 'REFURBISHED'] <- 'Used/Refurbished'
df$ITM_CNDTN_2[df$ITM_CNDTN == 'UNKNOWN'] <- ''
df$ITM_CNDTN_2[df$ITM_CNDTN == 'USED'] <- 'Used/Refurbished'

##recode mrtl_status_cd##
df$mrtl_status_cd_2[df$mrtl_status_cd == 'S'] <- 'Single'
df$mrtl_status_cd_2[df$mrtl_status_cd == 'A'] <- 'Inf_Married'
df$mrtl_status_cd_2[df$mrtl_status_cd == 'M'] <- 'Married'
df$mrtl_status_cd_2[df$mrtl_status_cd == 'B'] <- 'Inf_Single'
df$mrtl_status_cd_2[df$mrtl_status_cd == '?'] <- ''
df$mrtl_status_cd_2[df$mrtl_status_cd == 'N'] <- ''

##recode LABEL_TRANS##
df$LABEL_TRANS_2[df$LABEL_TRANS == '1'] <- 'Labeled'
df$LABEL_TRANS_2[df$LABEL_TRANS == '0'] <- 'Non-Labeled'

##recode CBT_YN_IND##
df$CBT_YN_IND_2[df$CBT_YN_IND == 'Y'] <- 'Yes'
df$CBT_YN_IND_2[df$CBT_YN_IND == 'N'] <- 'No'

##recode FM_BYR_SGMNTN_DESC##
df$FM_BYR_SGMNTN_DESC_2[df$FM_BYR_SGMNTN_DESC == 'Frequent Low/Mid Spenders'] <- 'Frequent Spenders'
df$FM_BYR_SGMNTN_DESC_2[df$FM_BYR_SGMNTN_DESC == 'Frequent High Spenders'] <- 'Frequent Spenders'
df$FM_BYR_SGMNTN_DESC_2[df$FM_BYR_SGMNTN_DESC == 'Infrequent Low Spenders'] <- 'Infrequent Spenders'
df$FM_BYR_SGMNTN_DESC_2[df$FM_BYR_SGMNTN_DESC == 'Infrequent Mid Spenders'] <- 'Infrequent Spenders'
df$FM_BYR_SGMNTN_DESC_2[df$FM_BYR_SGMNTN_DESC == 'Infrequent High Spenders'] <- 'Infrequent Spenders'
df$FM_BYR_SGMNTN_DESC_2[df$FM_BYR_SGMNTN_DESC == 'New'] <- 'New'
df$FM_BYR_SGMNTN_DESC_2[df$FM_BYR_SGMNTN_DESC == 'Reactivated'] <- 'Reactivated'
df$FM_BYR_SGMNTN_DESC_2[df$FM_BYR_SGMNTN_DESC == 'Uber Loyals'] <- 'Uber Loyals'
df$FM_BYR_SGMNTN_DESC_2[df$FM_BYR_SGMNTN_DESC == 'Unengaged'] <- 'Unengaged'
df$FM_BYR_SGMNTN_DESC_2[df$FM_BYR_SGMNTN_DESC == 'NA'] <- ''
df$FM_BYR_SGMNTN_DESC_2[df$FM_BYR_SGMNTN_DESC == 'Unsegmented'] <- ''
df$FM_BYR_SGMNTN_DESC_2[df$FM_BYR_SGMNTN_DESC == 'Lapsed'] <- ''
df$FM_BYR_SGMNTN_DESC_2[df$FM_BYR_SGMNTN_DESC == 'Dormant'] <- ''

##recode TRKING_SRC_CRE_DT##
df$TRKING_SRC_CRE_DT_2 <- ifelse(df$TRKING_SRC_CRE_DT == '?', "Non-Tracked",  "Tracked")

##recode ITEM_PRICE_AMT##
df$ITEM_PRICE_AMT_2 <- ifelse(df$ITEM_PRICE_AMT >= 0.01 & df$ITEM_PRICE_AMT <= 49.99, "$0.01-49.99", 
                       ifelse(df$ITEM_PRICE_AMT >= 50 & df$ITEM_PRICE_AMT <= 449.99, "$50-449.99",
                       "$450+"))


##recode SHPNG_FEE_AMT##
df$SHPNG_FEE_AMT_2 <- ifelse(df$SHPNG_FEE_AMT == 0, "$0", 
                      ifelse(df$SHPNG_FEE_AMT >= 0.01 & df$SHPNG_FEE_AMT <= 4.99, "$0.01-4.99",
                      ifelse(df$SHPNG_FEE_AMT >= 5 & df$SHPNG_FEE_AMT <= 9.99, "$5-9.99",
                      "$10+")))

##recode NSAT##
df$NPS <- ifelse(df$tp_sat <= 6,-1,
          ifelse((df$tp_sat == 7 | df$tp_sat == 8),0,
          ifelse(df$tp_sat > 8,1,
          NA)))

##rename NPS##
df$NPS2[df$NPS == '-1'] <- 'Det'
df$NPS2[df$NPS == '0'] <- 'Pass'
df$NPS2[df$NPS == '1'] <- 'Prom'

##recode DELIV_TIME_BUCKET##
df$DELIV_TIME_BUCKET_2[df$DELIV_TIME_BUCKET == '0 DAY DELIV'] <- '<=2 Days'
df$DELIV_TIME_BUCKET_2[df$DELIV_TIME_BUCKET == '1 DAY DELIV'] <- '<=2 Days'
df$DELIV_TIME_BUCKET_2[df$DELIV_TIME_BUCKET == '2 DAY DELIV'] <- '<=2 Days'
df$DELIV_TIME_BUCKET_2[df$DELIV_TIME_BUCKET == 'DELIV BEFORE TIME'] <- '<=2 Days'
df$DELIV_TIME_BUCKET_2[df$DELIV_TIME_BUCKET == '3 DAY DELIV'] <- '3-4 Days'
df$DELIV_TIME_BUCKET_2[df$DELIV_TIME_BUCKET == '4 DAY DELIV'] <- '3-4 Days'
df$DELIV_TIME_BUCKET_2[df$DELIV_TIME_BUCKET == '5 DAY DELIV'] <- '5 Days'
df$DELIV_TIME_BUCKET_2[df$DELIV_TIME_BUCKET == '6 DAY DELIV'] <- '6+ Days'
df$DELIV_TIME_BUCKET_2[df$DELIV_TIME_BUCKET == '7+ DAY DELIV'] <- '6+ Days'
df$DELIV_TIME_BUCKET_2[df$DELIV_TIME_BUCKET == 'UNKNOWN'] <- ' '

##recode NSAT##
df$USER_AGE_2 <- ifelse(df$USER_AGE >= 15 & df$USER_AGE <= 24, "15-24", 
                 ifelse(df$USER_AGE >= 25 & df$USER_AGE <= 44, "25-44",
                 ifelse(df$USER_AGE >= 45 & df$USER_AGE <= 64, "45-64",
                 ifelse(df$USER_AGE >= 65,"65+", 
                 "?"))))

write.csv(df, "Z:/eBay/Item Delivery tpNPS/Coder/Reporting/WIP/Yearly-2016/Jul'16-May'17_All_restd.csv", row.names = F)


########## R & D ###############################################################
df %>%
  group_by(Months_2 = 1) %>%
  summarize(NPS = mean(NPS, na.rm = TRUE))

newdata <- subset(df, Months_2 == 1,round(mean(df$NPS)*100,2))

dfc <- df[df[, "Months_2"] == 1,round(mean(df$NPS)*100,2)]


result <- filter(df,df$Months == "Jan'16")
z$NPS_Score <- round(mean(result$NPS)*100,2) 
View(z)

dfa <- NULL

dfa = dcast(df, Months + PROM_ACT_DELIV_2 ~ NPS2, subset=.(Months == "Jan'16"))
dfa
dfa$total = with(dfa, Det+Pass+Prom)
dfa$detpct = (dfa$Det / dfa$total)*100
dfa$passpct = (dfa$Pass / dfa$total)*100
dfa$prompct = (dfa$Prom / dfa$total)*100
dfa$nsat <- round((dfa$prompct - dfa$detpct),2)
View(dfa)

##########################################################

### NPS Calc ### 
round(mean(df$NPS)*100,2) 


### PPD_% Calc ###
y = xtabs(~ NPS2 + NPS2, df)
y
z <- data.frame(y)
z

z[1,"nps"] <- round(z[1,2]/sum(z$Freq)*100,2)
z[2,"nps"] <- round(z[2,2]/sum(z$Freq)*100,2)
z[3,"nps"] <- round(z[3,2]/sum(z$Freq)*100,2)
View(z)


#tp_sat% Calc slide#3
y = xtabs(~ tp_sat + tp_sat, df)
y
z <- data.frame(y)
z

z[1,"nps"] <- round(z[1 ,2]/sum(z$Freq)*100,2)
z[2,"nps"] <- round(z[2 ,2]/sum(z$Freq)*100,2)
z[3,"nps"] <- round(z[3 ,2]/sum(z$Freq)*100,2)
z[4,"nps"] <- round(z[4 ,2]/sum(z$Freq)*100,2)
z[5,"nps"] <- round(z[5 ,2]/sum(z$Freq)*100,2)
z[6,"nps"] <- round(z[6 ,2]/sum(z$Freq)*100,2)
z[7,"nps"] <- round(z[7 ,2]/sum(z$Freq)*100,2)
z[8,"nps"] <- round(z[8 ,2]/sum(z$Freq)*100,2)
z[9,"nps"] <- round(z[9 ,2]/sum(z$Freq)*100,2)
z[10,"nps"] <- round(z[10 ,2]/sum(z$Freq)*100,2)
z[11,"nps"] <- round(z[11 ,2]/sum(z$Freq)*100,2)
View(z)


##regression - overall##
reg <-  lm(tp_sat ~ Rcvd_when_prmsd+Shp_fast+Shp_charges_rsnbl+Pckgd_well, data = df)
summary(reg)
regression <- lm.beta(reg)
View(regression)


#### pivot with count and nsat ####
dfa = dcast(df, Months + NPS2 ~ NPS2)
dfa
dfa$total = with(dfa, Det+Pass+Prom)
dfa$detpct = (dfa$Det / dfa$total)*100
dfa$passpct = (dfa$Pass / dfa$total)*100
dfa$prompct = (dfa$Prom / dfa$total)*100
dfa$nsat <- round((dfa$prompct - dfa$detpct),2)
View(dfa)


#### Monthly pivot with count and nsat ####
dfa = dcast(df, Months + PROM_ACT_DELIV_2 + DELIV_TIME_BUCKET_2 ~ NPS2, subset=.(Months == "1-Jun'16"))
dfa
dfa$total = with(dfa, Det+Pass+Prom)
dfa$detpct = (dfa$Det / dfa$total)*100
dfa$passpct = (dfa$Pass / dfa$total)*100
dfa$prompct = (dfa$Prom / dfa$total)*100
dfa$nsat <- round((dfa$prompct - dfa$detpct),2)
View(dfa)


#### Monthly pivot with count and nsat for multiple variable #### "data.table"
dfa = dcast(df, Months + PROM_ACT_DELIV_2 + DELIV_TIME_BUCKET_2 ~ NPS2)
dfa
dfa$total = with(dfa, Det+Pass+Prom)
dfa$detpct = (dfa$Det / dfa$total)*100
dfa$passpct = (dfa$Pass / dfa$total)*100
dfa$prompct = (dfa$Prom / dfa$total)*100
dfa$nsat <- round((dfa$prompct - dfa$detpct),2)
View(dfa)

###################################################

#### pivot with count and nsat ####
dfa = dcast(df, Months ~ NPS2)
dfa
dfa$total = with(dfa, Det+Pass+Prom)
dfa$detpct = (dfa$Det / dfa$total)*100
dfa$passpct = (dfa$Pass / dfa$total)*100
dfa$prompct = (dfa$Prom / dfa$total)*100
dfa$nsat <- round((dfa$prompct - dfa$detpct),2)
View(dfa)


Jun16 <- subset(df, Months_2 == 6)
Jul16 <- subset(df, Months_2 == 7)
Aug16 <- subset(df, Months_2 == 8)
Sep16 <- subset(df, Months_2 == 9)
Oct16 <- subset(df, Months_2 == 10)
Nov16 <- subset(df, Months_2 == 11)
Dec16 <- subset(df, Months_2 == 12)
Jan17 <- subset(df, Months_2 == 1)
Feb17 <- subset(df, Months_2 == 2)
Mar17 <- subset(df, Months_2 == 3)
Apr17 <- subset(df, Months_2 == 4)
May17 <- subset(df, Months_2 == 5)

### NPS ###

y <- matrix(c(0,0), 12, 2)
colnames(y) <- c("Month","NPS")
y[,1] <- c("Jun16", "Jul16", "Aug16", "Sep16", "Oct16","Nov16", "Dec16", 
           "Jan17", "Feb17", "Mar17", "Apr17", "May17")
y[1,2] <- round(mean(Jun16$NPS)*100,2) 
y[2,2] <- round(mean(Jul16$NPS)*100,2)
y[3,2] <- round(mean(Aug16$NPS)*100,2) 
y[4,2] <- round(mean(Sep16$NPS)*100,2) 
y[5,2] <- round(mean(Oct16$NPS)*100,2) 
y[6,2] <- round(mean(Nov16$NPS)*100,2) 
y[7,2] <- round(mean(Dec16$NPS)*100,2) 
y[8,2] <- round(mean(Jan17$NPS)*100,2) 
y[9,2] <- round(mean(Feb17$NPS)*100,2) 
y[10,2] <- round(mean(Mar17$NPS)*100,2)
y[11,2] <- round(mean(Apr17$NPS)*100,2)
y[12,2] <- round(mean(May17$NPS)*100,2)
View(y)

### Prom, Pass & Det ###
x <- matrix(c(0,0),12,4)
colnames(x) <- c("Month","Prom", "Pass", "Det")
x[,1] <- c("Jun16", "Jul16", "Aug16", "Sep16", "Oct16","Nov16", "Dec16", 
           "Jan17", "Feb17", "Mar17", "Apr17", "May17")

View(x1)
x1 = xtabs(~ NPS2 + NPS2, Jun16)
x[1,2] <- round(x1[3,2]/sum(x1$Freq)*100,2)
x[1,3] <- round(x1[2,2]/sum(x1$Freq)*100,2)
x[1,4] <- round(x1[1,2]/sum(x1$Freq)*100,2)


x1[1,"nps"] <- round(x1[1,2]/sum(x1$Freq)*100,2)
x1[2,"nps"] <- round(x1[2,2]/sum(x1$Freq)*100,2)
x1[3,"nps"] <- round(x1[3,2]/sum(x1$Freq)*100,2)

install.packages (vector-of-package-names, repos="ftp://price.utstat.utoronto.ca")
install.packages(vector-of-package-names, repos="ftp://price.utstat.utoronto.ca")
