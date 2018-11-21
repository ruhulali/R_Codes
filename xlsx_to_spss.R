setwd("Z:/eBay/Item Delivery tpNPS/US/Coder/Allocation/12_Dec'17")

library(xlsx)
mydata <- read.xlsx("duplicate_removal_file.xlsx",sheetName="Sheet1")


library(foreign)
write.foreign(mydata, "Z:/eBay/Item Delivery tpNPS/US/Coder/Allocation/12_Dec'17/us_dec17.txt", 
                      "Z:/eBay/Item Delivery tpNPS/US/Coder/Allocation/12_Dec'17/us_dec17.sps",   package="SPSS")
