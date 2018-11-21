rm(list = ls())                       
install.packages("openxlsx", dependencies = T)
install.packages("xlsx", dependencies = T)
install.packages("rJava", dependencies = T)
library(openxlsx)
library(xlsx)
library(rJava)

merge1 <- read.xlsx("Z:/eBay/Return MBG/March/DE/Translation/INR/March_Buyer_INR_Buyer Fault.xlsx", 
                    sheet = 1, startRow = 1, colNames = TRUE,
                    rowNames = FALSE, detectDates = TRUE, skipEmptyRows = FALSE,
                    rows = NULL, cols = NULL, check.names = FALSE, namedRegion = NULL)

merge2 <- read.xlsx("Z:/eBay/Return MBG/March/DE/Translation/INR/March_Buyer_INR_Buyer Fault.xlsx", 
                    sheet = 2, startRow = 1, colNames = TRUE,
                    rowNames = FALSE, detectDates = TRUE, skipEmptyRows = FALSE,
                    rows = NULL, cols = NULL, check.names = FALSE, namedRegion = NULL)

merge3 <- read.xlsx("Z:/eBay/Return MBG/March/DE/Translation/INR/March_Buyer_INR_Buyer Fault.xlsx", 
                    sheet = 3, startRow = 1, colNames = TRUE,
                    rowNames = FALSE, detectDates = TRUE, skipEmptyRows = FALSE,
                    rows = NULL, cols = NULL, check.names = FALSE, namedRegion = NULL)

View(buyer_inr_buyer_fault)
buyer_inr_buyer_fault <- rbind(merge1,merge2,merge3)
buyer_inr_buyer_fault$Responsedate <- convertToDateTime(buyer_inr_buyer_fault$Responsedate)
write.xlsx(buyer_inr_buyer_fault, file = "Z:/eBay/Return MBG/March/DE/Translation/INR/buyer_inr_buyer_fault.xlsx")

