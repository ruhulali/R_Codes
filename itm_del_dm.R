######################### file/path/package ######################################
rm(list = ls())
path <- "D:/CAREER COLLISION/OFFICE/WORK/WIP"
setwd(path)

install.packages("LearnBayes", dependencies = T)

library(readr)
library(tidyverse)
library(dplyr)
library(lubridate)
library(zoo)
library(QuantPsyc)
library(plyr)
library(reshape2)
library(reshape)
library(data.table)
library(LearnBayes)
library(psych)
library(tibble)

find_funs(".") 
source("https://sebastiansauer.github.io/Rcode/find_funs.R")

install.packages("sos")
library(sos)
findFn("CrossTable")

######################### script ######################################

##upload##
library(readr)
df <- read_csv("D:/CAREER COLLISION/OFFICE/WORK/WIP/Item_Delivery_tpNSAT_US_Jan18.csv")

##upload restructured data##
df <- read_csv("Z:/eBay/Item Delivery tpNPS/US/Reporting/WIP/12_Dec'17/nov17_itm_del_restructured.csv")

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
          ifelse(df$tp_sat > 8,1,NA)))

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
                 ifelse(df$USER_AGE >= 65,"65+", "?"))))

write.csv(df, "D:/CAREER COLLISION/OFFICE/WORK/WIP/jan17_itm_del_restructured.csv", row.names = F)

##########################################################

### NPS Calc slide#3 ###
round(mean(df$NPS)*100,2)


### PPD_% Calc slide#3 ###
y = xtabs(~ NPS2 + NPS2, df)
y
z1 <- data.frame(y)
z1[1,"nps"] <- round(z1[1,2]/sum(z1$Freq)*100,2)
z1[2,"nps"] <- round(z1[2,2]/sum(z1$Freq)*100,2)
z1[3,"nps"] <- round(z1[3,2]/sum(z1$Freq)*100,2)
z1
write.csv(z1, "D:/CAREER COLLISION/OFFICE/WORK/WIP/data_tables/table1.csv", row.names = F)


###tp_sat% Calc slide#3 ###
y = xtabs(~ tp_sat + tp_sat, df)
y
z2 <- data.frame(y)
z2[1,"nps"] <- round(z2[1 ,2]/sum(z2$Freq)*100,2)
z2[2,"nps"] <- round(z2[2 ,2]/sum(z2$Freq)*100,2)
z2[3,"nps"] <- round(z2[3 ,2]/sum(z2$Freq)*100,2)
z2[4,"nps"] <- round(z2[4 ,2]/sum(z2$Freq)*100,2)
z2[5,"nps"] <- round(z2[5 ,2]/sum(z2$Freq)*100,2)
z2[6,"nps"] <- round(z2[6 ,2]/sum(z2$Freq)*100,2)
z2[7,"nps"] <- round(z2[7 ,2]/sum(z2$Freq)*100,2)
z2[8,"nps"] <- round(z2[8 ,2]/sum(z2$Freq)*100,2)
z2[9,"nps"] <- round(z2[9 ,2]/sum(z2$Freq)*100,2)
z2[10,"nps"] <- round(z2[10 ,2]/sum(z2$Freq)*100,2)
z2[11,"nps"] <- round(z2[11 ,2]/sum(z2$Freq)*100,2)
z2
write.csv(z2, "D:/CAREER COLLISION/OFFICE/WORK/WIP/data_tables/table2.csv", row.names = F)


### regression - overall ###
reg <-  lm(tp_sat ~ Rcvd_when_prmsd+Shp_fast+Shp_charges_rsnbl+Pckgd_well, data = df)
summary(reg)
anova(reg)
plot(reg)

library(QuantPsyc)
z3 <- lm.beta(reg)
z3
View(z3)
write.csv(z3, "D:/CAREER COLLISION/OFFICE/WORK/WIP/data_tables/table3.csv", row.names = T)

names(df)

#### pivot with count and nsat ####

library(data.table)
library(reshape2)
dfa = dcast(df, PROM_ACT_DELIV_2 ~ NPS2)
dfa = dcast(df, PRDCTV_GNDR + MBL_DVIC_NAME ~ NPS2, subset=.(PRDCTV_GNDR == "M"))
dfa
dfa$total = with(dfa, Det+Pass+Prom)
dfa$detpct = (dfa$Det / dfa$total)*100
dfa$passpct = (dfa$Pass / dfa$total)*100
dfa$prompct = (dfa$Prom / dfa$total)*100
dfa$nsat <- round((dfa$prompct - dfa$detpct),2)
dfa
View(dfa)
write.csv(dfa, "Z:/eBay/Item Delivery tpNPS/US/Reporting/WIP/12_Dec'17/nov17_itm_del_tables.csv", row.names = F)


#### total_script ####

df1 = dcast(df, PROM_ACT_DELIV_2 ~ NPS2)
df1
df1$total = with(df1, Det+Pass+Prom)
df1$detpct = (df1$Det / df1$total)*100
df1$passpct = (df1$Pass / df1$total)*100
df1$prompct = (df1$Prom / df1$total)*100
df1$nsat <- round((df1$prompct - df1$detpct),2)
df1
write.csv(df1, "D:/CAREER COLLISION/OFFICE/WORK/WIP/data_tables/table4.csv", row.names = F)

df2 = dcast(df, DELIV_TIME_BUCKET_2 ~ NPS2)
df2
df2$total = with(df2, Det+Pass+Prom)
df2$detpct = (df2$Det / df2$total)*100
df2$passpct = (df2$Pass / df2$total)*100
df2$prompct = (df2$Prom / df2$total)*100
df2$nsat <- round((df2$prompct - df2$detpct),2)
df2
write.csv(df2, "D:/CAREER COLLISION/OFFICE/WORK/WIP/data_tables/table5.csv", row.names = F)

df3 = dcast(df, SPE_ACT_HT_FULFLD_2 ~ NPS2)
df3
df3$total = with(df3, Det+Pass+Prom)
df3$detpct = (df3$Det / df3$total)*100
df3$passpct = (df3$Pass / df3$total)*100
df3$prompct = (df3$Prom / df3$total)*100
df3$nsat <- round((df3$prompct - df3$detpct),2)
df3
write.csv(df3, "D:/CAREER COLLISION/OFFICE/WORK/WIP/data_tables/table6.csv", row.names = F)

df4 = dcast(df, FREE_SHPNG_YN_IND_2 ~ NPS2)
df4
df4$total = with(df4, Det+Pass+Prom)
df4$detpct = (df4$Det / df4$total)*100
df4$passpct = (df4$Pass / df4$total)*100
df4$prompct = (df4$Prom / df4$total)*100
df4$nsat <- round((df4$prompct - df4$detpct),2)
df4
write.csv(df4, "D:/CAREER COLLISION/OFFICE/WORK/WIP/data_tables/table7.csv", row.names = F)

df5 = dcast(df, SHPNG_FEE_AMT_2 ~ NPS2)
df5
df5$total = with(df5, Det+Pass+Prom)
df5$detpct = (df5$Det / df5$total)*100
df5$passpct = (df5$Pass / df5$total)*100
df5$prompct = (df5$Prom / df5$total)*100
df5$nsat <- round((df5$prompct - df5$detpct),2)
df5
write.csv(df5, "D:/CAREER COLLISION/OFFICE/WORK/WIP/data_tables/table8.csv", row.names = F)

df6 = dcast(df, FAST_N_FREE_YN_IND_2 ~ NPS2)
df6
df6$total = with(df6, Det+Pass+Prom)
df6$detpct = (df6$Det / df6$total)*100
df6$passpct = (df6$Pass / df6$total)*100
df6$prompct = (df6$Prom / df6$total)*100
df6$nsat <- round((df6$prompct - df6$detpct),2)
df6
write.csv(df6, "D:/CAREER COLLISION/OFFICE/WORK/WIP/data_tables/table9.csv", row.names = F)

df7 = dcast(df, LABEL_TRANS_2 ~ NPS2)
df7
df7$total = with(df7, Det+Pass+Prom)
df7$detpct = (df7$Det / df7$total)*100
df7$passpct = (df7$Pass / df7$total)*100
df7$prompct = (df7$Prom / df7$total)*100
df7$nsat <- round((df7$prompct - df7$detpct),2)
df7
write.csv(df7, "D:/CAREER COLLISION/OFFICE/WORK/WIP/data_tables/table10.csv", row.names = F)

df8 = dcast(df, TRKING_SRC_CRE_DT_2 ~ NPS2)
df8
df8$total = with(df8, Det+Pass+Prom)
df8$detpct = (df8$Det / df8$total)*100
df8$passpct = (df8$Pass / df8$total)*100
df8$prompct = (df8$Prom / df8$total)*100
df8$nsat <- round((df8$prompct - df8$detpct),2)
df8
write.csv(df8, "D:/CAREER COLLISION/OFFICE/WORK/WIP/data_tables/table11.csv", row.names = F)

df9 = dcast(df, LABEL_TRANS_2 + TRKING_SRC_CRE_DT_2 ~ NPS2)
df9
df9$total = with(df9, Det+Pass+Prom)
df9$detpct = (df9$Det / df9$total)*100
df9$passpct = (df9$Pass / df9$total)*100
df9$prompct = (df9$Prom / df9$total)*100
df9$nsat <- round((df9$prompct - df9$detpct),2)
df9
write.csv(df9, "D:/CAREER COLLISION/OFFICE/WORK/WIP/data_tables/table12.csv", row.names = F)

df10 = dcast(df, CBT_YN_IND_2 ~ NPS2)
df10
df10$total = with(df10, Det+Pass+Prom)
df10$detpct = (df10$Det / df10$total)*100
df10$passpct = (df10$Pass / df10$total)*100
df10$prompct = (df10$Prom / df10$total)*100
df10$nsat <- round((df10$prompct - df10$detpct),2)
df10
write.csv(df10, "D:/CAREER COLLISION/OFFICE/WORK/WIP/data_tables/table13.csv", row.names = F)

df11 = dcast(df, ITM_CNDTN_2 ~ NPS2)
df11
df11$total = with(df11, Det+Pass+Prom)
df11$detpct = (df11$Det / df11$total)*100
df11$passpct = (df11$Pass / df11$total)*100
df11$prompct = (df11$Prom / df11$total)*100
df11$nsat <- round((df11$prompct - df11$detpct),2)
df11
write.csv(df11, "D:/CAREER COLLISION/OFFICE/WORK/WIP/data_tables/table14.csv", row.names = F)

df12 = dcast(df, ITEM_PRICE_AMT_2 ~ NPS2)
df12
df12$total = with(df12, Det+Pass+Prom)
df12$detpct = (df12$Det / df12$total)*100
df12$passpct = (df12$Pass / df12$total)*100
df12$prompct = (df12$Prom / df12$total)*100
df12$nsat <- round((df12$prompct - df12$detpct),2)
df12
write.csv(df12, "D:/CAREER COLLISION/OFFICE/WORK/WIP/data_tables/table15.csv", row.names = F)

df13 = dcast(df, FM_BYR_SGMNTN_DESC_2 ~ NPS2)
df13
df13$total = with(df13, Det+Pass+Prom)
df13$detpct = (df13$Det / df13$total)*100
df13$passpct = (df13$Pass / df13$total)*100
df13$prompct = (df13$Prom / df13$total)*100
df13$nsat <- round((df13$prompct - df13$detpct),2)
df13
write.csv(df13, "D:/CAREER COLLISION/OFFICE/WORK/WIP/data_tables/table16.csv", row.names = F)

df14 = dcast(df, SAP_GLBL_NAME_2 ~ NPS2)
df14
df14$total = with(df14, Det+Pass+Prom)
df14$detpct = (df14$Det / df14$total)*100
df14$passpct = (df14$Pass / df14$total)*100
df14$prompct = (df14$Prom / df14$total)*100
df14$nsat <- round((df14$prompct - df14$detpct),2)
df14
write.csv(df14, "D:/CAREER COLLISION/OFFICE/WORK/WIP/data_tables/table17.csv", row.names = F)

df15 = dcast(df, MBL_DVIC_NAME ~ NPS2)
df15
df15$total = with(df15, Det+Pass+Prom)
df15$detpct = (df15$Det / df15$total)*100
df15$passpct = (df15$Pass / df15$total)*100
df15$prompct = (df15$Prom / df15$total)*100
df15$nsat <- round((df15$prompct - df15$detpct),2)
df15
write.csv(df15, "D:/CAREER COLLISION/OFFICE/WORK/WIP/data_tables/table18.csv", row.names = F)

df16 = dcast(df, PRDCTV_GNDR ~ NPS2)
df16
df16$total = with(df16, Det+Pass+Prom)
df16$detpct = (df16$Det / df16$total)*100
df16$passpct = (df16$Pass / df16$total)*100
df16$prompct = (df16$Prom / df16$total)*100
df16$nsat <- round((df16$prompct - df16$detpct),2)
df16
write.csv(df16, "D:/CAREER COLLISION/OFFICE/WORK/WIP/data_tables/table19.csv", row.names = F)

df17 = dcast(df, PRDCTV_GNDR + MBL_DVIC_NAME ~ NPS2, subset=.(PRDCTV_GNDR == "M"))
df17
df17$total = with(df17, Det+Pass+Prom)
df17$detpct = (df17$Det / df17$total)*100
df17$passpct = (df17$Pass / df17$total)*100
df17$prompct = (df17$Prom / df17$total)*100
df17$nsat <- round((df17$prompct - df17$detpct),2)
df17
write.csv(df17, "D:/CAREER COLLISION/OFFICE/WORK/WIP/data_tables/table20.csv", row.names = F)

df18 = dcast(df, PRDCTV_GNDR + MBL_DVIC_NAME ~ NPS2, subset=.(PRDCTV_GNDR == "F"))
df18
df18$total = with(df18, Det+Pass+Prom)
df18$detpct = (df18$Det / df18$total)*100
df18$passpct = (df18$Pass / df18$total)*100
df18$prompct = (df18$Prom / df18$total)*100
df18$nsat <- round((df18$prompct - df18$detpct),2)
df18
write.csv(df18, "D:/CAREER COLLISION/OFFICE/WORK/WIP/data_tables/table21.csv", row.names = F)

df19 = dcast(df, PRDCTV_GNDR + USER_AGE_2 ~ NPS2)
df19
df19$total = with(df19, Det+Pass+Prom)
df19$detpct = (df19$Det / df19$total)*100
df19$passpct = (df19$Pass / df19$total)*100
df19$prompct = (df19$Prom / df19$total)*100
df19$nsat <- round((df19$prompct - df19$detpct),2)
df19
write.csv(df19, "D:/CAREER COLLISION/OFFICE/WORK/WIP/data_tables/table22.csv", row.names = F)

df20 = data.frame(table(df$USER_AGE_2))
df20$percent= round(df20$Freq / sum(df20$Freq)*100,2)
df20
write.csv(df20, "D:/CAREER COLLISION/OFFICE/WORK/WIP/data_tables/table23.csv", row.names = F)

df21 = dcast(df, PRDCTV_GNDR + USER_AGE_2 ~ NPS2)
df21
df21$total = with(df21, Det+Pass+Prom)
df21$detpct = (df21$Det / df21$total)*100
df21$passpct = (df21$Pass / df21$total)*100
df21$prompct = (df21$Prom / df21$total)*100
df21$nsat <- round((df21$prompct - df21$detpct),2)
df21
write.csv(df21, "D:/CAREER COLLISION/OFFICE/WORK/WIP/data_tables/table24.csv", row.names = F)

df22 = dcast(df, PRDCTV_GNDR + mrtl_status_cd_2 ~ NPS2)
df22
df22$total = with(df22, Det+Pass+Prom)
df22$detpct = (df22$Det / df22$total)*100
df22$passpct = (df22$Pass / df22$total)*100
df22$prompct = (df22$Prom / df22$total)*100
df22$nsat <- round((df22$prompct - df22$detpct),2)
df22
write.csv(df22, "D:/CAREER COLLISION/OFFICE/WORK/WIP/data_tables/table24.csv", row.names = F)

### merging files syntax-1 (when all columns are same)###

setwd("Z:/eBay/Item Delivery tpNPS/US/Reporting/WIP/12_Dec'17/data_tables/")
filenames <- list.files(full.names=TRUE)
All <- lapply(filenames,function(i){read.csv(i, header=F)})
dfz <- do.call(rbind.data.frame, All)
write.csv(dfz,"Z:/eBay/Item Delivery tpNPS/US/Reporting/WIP/12_Dec'17/data_tables/all_tables.csv",row.names=F)

### merging files syntax-2 (irrespective of no. of columns)###

write.table(z1, "D:/CAREER COLLISION/OFFICE/WORK/WIP/data_tables/table_all.csv",row.names=F,col.names=T,sep=",")
write.table(z2, "D:/CAREER COLLISION/OFFICE/WORK/WIP/data_tables/table_all.csv",row.names=F,col.names=T,sep=",",append=T)
write.table(z3, "D:/CAREER COLLISION/OFFICE/WORK/WIP/data_tables/table_all.csv",row.names=T,sep=",",append=T)
write.table(df1, "D:/CAREER COLLISION/OFFICE/WORK/WIP/data_tables/table_all.csv",row.names=F,col.names=T,sep=",",append=T)
write.table(df2, "D:/CAREER COLLISION/OFFICE/WORK/WIP/data_tables/table_all.csv",row.names=F,col.names=T,sep=",",append=T)
write.table(df3, "D:/CAREER COLLISION/OFFICE/WORK/WIP/data_tables/table_all.csv",row.names=F,col.names=T,sep=",",append=T)
write.table(df4, "D:/CAREER COLLISION/OFFICE/WORK/WIP/data_tables/table_all.csv",row.names=F,col.names=T,sep=",",append=T)
write.table(df5, "D:/CAREER COLLISION/OFFICE/WORK/WIP/data_tables/table_all.csv",row.names=F,col.names=T,sep=",",append=T)
write.table(df6, "D:/CAREER COLLISION/OFFICE/WORK/WIP/data_tables/table_all.csv",row.names=F,col.names=T,sep=",",append=T)
write.table(df7, "D:/CAREER COLLISION/OFFICE/WORK/WIP/data_tables/table_all.csv",row.names=F,col.names=T,sep=",",append=T)
write.table(df8, "D:/CAREER COLLISION/OFFICE/WORK/WIP/data_tables/table_all.csv",row.names=F,col.names=T,sep=",",append=T)
write.table(df9, "D:/CAREER COLLISION/OFFICE/WORK/WIP/data_tables/table_all.csv",row.names=F,col.names=T,sep=",",append=T)
write.table(df10, "D:/CAREER COLLISION/OFFICE/WORK/WIP/data_tables/table_all.csv",row.names=F,col.names=T,sep=",",append=T)
write.table(df11, "D:/CAREER COLLISION/OFFICE/WORK/WIP/data_tables/table_all.csv",row.names=F,col.names=T,sep=",",append=T)
write.table(df12, "D:/CAREER COLLISION/OFFICE/WORK/WIP/data_tables/table_all.csv",row.names=F,col.names=T,sep=",",append=T)
write.table(df13, "D:/CAREER COLLISION/OFFICE/WORK/WIP/data_tables/table_all.csv",row.names=F,col.names=T,sep=",",append=T)
write.table(df14, "D:/CAREER COLLISION/OFFICE/WORK/WIP/data_tables/table_all.csv",row.names=F,col.names=T,sep=",",append=T)
write.table(df15, "D:/CAREER COLLISION/OFFICE/WORK/WIP/data_tables/table_all.csv",row.names=F,col.names=T,sep=",",append=T)
write.table(df16, "D:/CAREER COLLISION/OFFICE/WORK/WIP/data_tables/table_all.csv",row.names=F,col.names=T,sep=",",append=T)
write.table(df17, "D:/CAREER COLLISION/OFFICE/WORK/WIP/data_tables/table_all.csv",row.names=F,col.names=T,sep=",",append=T)
write.table(df18, "D:/CAREER COLLISION/OFFICE/WORK/WIP/data_tables/table_all.csv",row.names=F,col.names=T,sep=",",append=T)
write.table(df19, "D:/CAREER COLLISION/OFFICE/WORK/WIP/data_tables/table_all.csv",row.names=F,col.names=T,sep=",",append=T)
write.table(df20, "D:/CAREER COLLISION/OFFICE/WORK/WIP/data_tables/table_all.csv",row.names=F,col.names=T,sep=",",append=T)
write.table(df21, "D:/CAREER COLLISION/OFFICE/WORK/WIP/data_tables/table_all.csv",row.names=F,col.names=T,sep=",",append=T)
write.table(df22, "D:/CAREER COLLISION/OFFICE/WORK/WIP/data_tables/table_all.csv",row.names=F,col.names=T,sep=",",append=T)

###################################

ggplot(df1, aes(x= nsat,  group=CBT_YN_IND_2)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
  geom_text(aes( label = scales::percent(..prop..),y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(y = "Percent", fill="nsat") + facet_grid(~CBT_YN_IND_2) +
  scale_y_continuous(labels = scales::percent) + 
  CrossTable(df1$nsat,df1$CBT_YN_IND_2, prop.chisq=F, format="SPSS")


###################################################

dfa = dcast(df, PRDCTV_GNDR + MBL_DVIC_NAME ~ NPS2, subset=.(PRDCTV_GNDR == "M"))

dfa = dcast(df, PRDCTV_GNDR + MBL_DVIC_NAME ~ NPS2)
dfa
dfa$total = with(dfa, Det+Pass+Prom)
dfa$detpct = (dfa$Det / dfa$total)*100
dfa$passpct = (dfa$Pass / dfa$total)*100
dfa$prompct = (dfa$Prom / dfa$total)*100
dfa$nsat <- round((dfa$prompct - dfa$detpct),2)
View(dfa)

###################################################
## geom_bar chart

library(dplyr)
summary = ExampleM %>% group_by(Year, variable, value) %>%
  tally %>%
  group_by(Year, variable) %>%
  mutate(pct = n/sum(n),
         n.pos = cumsum(n) - 0.5*n)

summary = df %>% group_by(PROM_ACT_DELIV_2, NPS) %>%
  tally %>%
  group_by(PROM_ACT_DELIV_2, NPS) %>%
  mutate(pct = n/sum(n),
         n.pos = cumsum(n) - 0.5*n)

ggplot(summary, aes(x=NPS, y=n, fill=NPS)) +
  geom_bar(stat="identity") +
  facet_grid(.~PROM_ACT_DELIV_2) + 
  geom_text(aes(label=paste0(sprintf("%1.1f", pct*100),"%"), y=n.pos), 
            colour="red") 

###################################################
dfa = dcast(df, PROM_ACT_DELIV_2 ~ NPS)
dfa.sum <- colSums(dfa)

dfa$Pct <- dfa$Total / sum(dfa$Total)

######
dfa = dcast(df, USER_AGE_2 ~ NPS2)
dfa
dfa$total = with(dfa, Det+Pass+Prom)
dfa$detpct = (dfa$Det / dfa$total)*100
dfa$passpct = (dfa$Pass / dfa$total)*100
dfa$prompct = (dfa$Prom / dfa$total)*100
dfa$nsat <- round(dfa$prompct - dfa$detpct)
View(dfa)
######

names(df)

  
y = xtabs(~ NPS2 + PROM_ACT_DELIV_2, df)
View(y)
z <- data.frame(y)
View(z)

prom <- round(z[1,2]/sum(z$Freq)*100)
pass <- round(z[2,2]/sum(z$Freq)*100)
det <- round(z[3,2]/sum(z$Freq)*100)

prom
pass
det

########
dfa = dcast(df, PROM_ACT_DELIV_2 ~ NPS, value.var = summarise(dfa))
dfa

View(df)

dff <- cast(df, PROM_ACT_DELIV_2 ~ NPS, sum)
dff
dff <- cast(df, NPS ~ PROM_ACT_DELIV_2, sum, margins=c("grand_row"))

df1 <- ddply(df, .(PROM_ACT_DELIV_2), summarise, NPS = NPS, pct = NPS / sum(NPS))
df1
dfc <- cast(df1, PROM_ACT_DELIV_2 ~ NPS)
dfc

a <- prop.table(tapply(df$NPS, df[1:2], sum), 1)
a

prop.table( xtabs(PROM_ACT_DELIV_2 ~., df$NPS), 1 )



df %>%
  group_by(PROM_ACT_DELIV_2, NPS) %>%
  summarise(n = n())

df %>%
  group_by(PROM_ACT_DELIV_2, NPS) %>%
  summarise (n = n()) %>%
  mutate(freq = n / sum(n))

df %>%
  count(df$PROM_ACT_DELIV_2, df$NPS) %>%
  mutate(freq = n / sum(n))

df %>%
  count(df$PROM_ACT_DELIV_2, NPS) %>%
  mutate(freq = n / sum(n)) %>%
  ungroup()

library(dplyr)
install.packages("dtplyr", dependencies = T)
library(dtplyr)
data(mtcars)
mtcars = tbl_dt(mtcars)

mtcars %>%
  group_by(am, gear) %>%
  summarise(n = n())

dat <- data.frame(Operation = c("Login", "Posted", "Deleted"), `Total.Count` = c(5, 25, 40), check.names = FALSE)
dat
dat$Pct <- dat$Total.Count / sum(dat$Total.Count)
dat



library(data.table)    
cbind(dfa, dfa[,list(sum=-1+0+1)])


dfa %>%
  group_by(PROM_ACT_DELIV_2) %>%
  summarise(Sum=sum(value))



is.data.table(dfa)== TRUE


