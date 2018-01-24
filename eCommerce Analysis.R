setwd("C:/Users/Ya Ting Chang/Desktop/kaggle/ecommerce-data")
library(tidyverse)
ecdf_raw<-read_csv("data.csv")
ecdf <- ecdf_raw
#which field has NA
apply(ecdf,2,function(x){sum(is.na(x))})
#determine the possible explaination for NA 
idNa <- ecdf[which(is.na(ecdf$CustomerID)),]
apply(idNa,2,function(x){length(unique(x))})

#remove rows which customerID is NA
ecdf<-ecdf[!is.na(ecdf$CustomerID),]

## distribution of quantity & Price ## 
quantile(ecdf$Quantity)
# transation with negative quantity? => returned goods or discount
ecdf[ecdf$Quantity<0,]

## distribution of price ##
quantile(ecdf$UnitPrice)
# define price larger than 145 as outlier
# since they account for 0.05% only
quantile(ecdf$UnitPrice,probs=seq(0.99,1,0.0005))
ecdf <- ecdf[ecdf$UnitPrice<146,]
# remove price equal to zero
ecdf <- ecdf[!ecdf$UnitPrice==0,]

## Exploration(by customers)##
# 1.count the return
customer <- data.frame(CustomerID=unique(ecdf$CustomerID))

cust.stat <- ecdf %>%
  filter(str_detect(InvoiceNo,"^C")&Description != "Discount") %>%
  group_by(CustomerID) %>%
  summarise(return=n())
cust.stat <- left_join(customer,cust.stat,by="CustomerID")
cust.stat <- cust.stat %>%
  mutate(return=ifelse(is.na(return),0,return))

# 2.average consumption





