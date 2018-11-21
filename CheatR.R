find_funs("mosaic")
source("https://sebastiansauer.github.io/Rcode/find_funs.R")

path <- "D:/CAREER COLLISION/OFFICE/WORK/Item_Delivery_Old"
data <- read.csv("D:/CAREER COLLISION/OFFICE/WORK/Item_Delivery_Old/sep'17_itm_del_restructured.csv")

install.packages("vcd", dependencies = T)
library(vcd)
names(data)

### Frequency tables and graphics depicting frequency tables ###
table(df$ITM_CNDTN_2) #Frequency table showing counts
xtabs(~df$ITM_CNDTN_2)  #Same, but uses formula interface
freq1<-table(df$ITM_CNDTN_2) #Create a table for further use
prop.table(freq1) #Table with proportions
barplot(freq1,main="Count of CBT") #Barchart of the table
mosaicplot(freq1) #Mosaic plot of the table
mosaic(freq1) #Mosaic plot of the table (requires packace {vcd}


### Crosstabulations ###
### Producing tables containing only frequencies (counts):
table(df$CBT_YN_IND_2,df$USER_AGE_2) #Bivariate table
ftable(df$CBT_YN_IND_2~df$USER_AGE_2) #Same using formula interface
xtabs(~df$CBT_YN_IND_2+df$USER_AGE_2) #Same using formula interface
xtabs(~df$CBT_YN_IND_2+df$USER_AGE_2) #Three variables
ftable(xtabs(~df$CBT_YN_IND_2+df$USER_AGE_2)) #Same, but alternative presentation
tab1<- table(df$CBT_YN_IND_2,df$USER_AGE_2) #store table as object for further use
addmargins(tab1) #add margins (row and column sums to table)
margin.table(tab1,1) #display row margins
margin.table(tab1,2) #display column margins

### Proportions computed from a table of counts:
tab1<- table(df$CBT_YN_IND_2,df$USER_AGE_2) #store frequency count table as object for further use
prop.table(tab1) #total proportions
prop.table(tab1,2) #proportions columnwise
prop.table(tab1,1) #proportions rowwise
ftable(prop.table(tab1)) #alternative presentation

