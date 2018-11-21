###############################################################
rm(list = ls())
install.packages("readr", dependencies = T)
install.packages("dplyr", dependencies = T)
library(dplyr)
library(data.table)
library(readr)
###############################################################

data.orig <- read_csv("Z:/Team/Individual/Madhuri/2016/eBay Delivery/Tools/R Training/Aug_US_RID.csv")
Distributions <- read_csv("Z:/Team/Individual/Madhuri/2016/eBay Delivery/Tools/R Training/Dist_Aug_UK.csv")
View(Distributions)
View(data.orig)

names(data.orig)

data.orig <- select(data.orig, RID, ncsat_nps, segment)
View(data.orig)

data.orig$NPS <- ifelse(data.orig$ncsat_nps <= 6,1,
                                   ifelse((data.orig$ncsat_nps == 7 | data.orig$ncsat_nps == 8),2
                                          ,ifelse(data.orig$ncsat_nps > 8,3,NA)))
View(data.orig)

data.pivot <- dcast(data.orig, segment ~ NPS, length, value.var="segment", fill=0)
View(data.pivot)

r <- ifelse(max(Distributions$segment == nrow(Distributions)), nrow(Distributions), max(Distributions$segment))
View(r)
Distributions.count <- round(select(Distributions, Det, Pass, Prom)*data.pivot[1:r,c("1","2","3")],0)
View(Distributions.count)

total <- dim(data.orig)[1]
SN <- seq(1,total,1)
data.orig <- cbind(SN, data.orig)
View(data.orig)


Distributions.trans <- t(Distributions.count)
View(Distributions.trans)
Distributions.melted <- melt(Distributions.trans)
View(Distributions.melted)

cutoff <- Distributions.melted$value
View(cutoff)

selector <- function(df, seg.no, ppd, cutoff){
  filter <- as.vector(0)
  new.x <- 0
  random.new <- as.vector(0)
  i <- 1
  while(new.x < cutoff){
    random <- sample(1:total,1)
    if((random %in% random.new == F)){
      random.new[i] <- random
      a <- ifelse((df$SN %in% random),1,0)
      #filter <- ifelse((!is.na(df$segment == seg.no)) & (!is.na(df$NPS == ppd)), filter + a, filter)
      filter <- ifelse((df$segment == seg.no) & (df$NPS == ppd), filter + a, filter)
      new.x <- sum(filter == 1)
    }
    i <-  i + 1
    print(paste(i,new.x))
  }
  return(filter)
}

filters <- rep(0, total)
no.seg <- max(Distributions$segment)

gc()

l <- 1
for(j in 1:no.seg){
  for(k in 1:3){
    filters.temp <- selector(data.orig, j, k, cutoff[l])
    if(j == 1 & k == 1 & l == 1){
      filters <- filters.temp
    }
    else{
      filters <- cbind(filters, filters.temp)
    }
    l <- l + 1
    cat("\014")
    cat(l, "...[", round(((l-1)/(3*no.seg))*100, 1), "% Done] \n")
    gc()
  }
}

colSums(filters)
a <- rowSums(filters)
max(a)

data.orig$selector <- rowSums(filters)
View(data.orig)

write.csv(data.orig, "Z:/Team/Individual/Madhuri/2016/eBay Delivery/Tools/R Training/uk_master_sample_file_aug16.csv", row.names = F)
  
#############################################

data.seg <- rep(list(data.frame()),3*no.seg)

m <- 1
for(i in 1:no.seg){
  for(j in 1:3){
    data.seg[[m]] <- subset(data.orig, (data.orig$segment == i & data.orig$NPS == j & data.orig$selector == 1))
    m <- m+1
    cat("\014")
    cat(m, "...[", round(((m-1)/(3*no.seg))*100, 1), "% Done] \n")
    gc()
  }
}

file.names <- Distributions$Segment_name

a <- seq(1, 3*no.seg, 3)
d <- 0

j <- 1
  for(i in a){
      d[i] <- paste0(file.names[j],"_","detr")
      d[i+1] <- paste0(file.names[j],"_","pass")
      d[i+2] <- paste0(file.names[j],"_","prom")
      j <- j+1
    }

for(i in 1:(3*no.seg)) {
  file.name <- paste0("Z:/Team/Individual/Dileep/Ebay perc/Ebay perc/Segmented Files/UK/", d[i], ".csv")
  write.csv(data.seg[[i]], file.name, row.names = F)
  cat("\014")
  cat(i, "...[", round((i/(3*no.seg))*100, 1), "% Done] \n")
}
