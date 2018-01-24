library(tidyverse)
library(lubridate)
ecdf <-read_csv("data.csv")

## Handling missing value and outlier ##
#Determine the possible explaination for NA 
apply(ecdf,2,function(x){sum(is.na(x))})
idNa <- ecdf[which(is.na(ecdf$CustomerID)),]
apply(idNa,2,function(x){length(unique(x))})

#distribution of unit price and quantity:
#1).qunatity<0 -> cancelled/returned orders OR discount
quantile(ecdf$Quantity)
ecdf[ecdf$Quantity<0,]
#2).unit price -> define price larger than 145 as outlier 
quantile(ecdf$UnitPrice)
quantile(ecdf$UnitPrice,probs=seq(0.99,1,0.0005))

# remove rows:
# 1). customerID is NA
# 2). unitprice equals to 0
# 3). uniprice outlier ($145) 
ecdf<-ecdf[!is.na(ecdf$CustomerID),]
ecdf <- ecdf[ecdf$UnitPrice<146,]
ecdf <- ecdf[!ecdf$UnitPrice==0,]


## Exploration(by customers)##
cust.stat <- data.frame(CustomerID=unique(ecdf$CustomerID))
joinbyID <- function(df,id){
  cust.stat <- cust.stat %>%
    left_join(df,by=id)
  return(cust.stat)
}

# total transactions:
trans <- ecdf %>%
  filter(!str_detect(InvoiceNo,"C")) %>%
  group_by(CustomerID) %>%
  summarise(transactions=length(unique(InvoiceNo)))
cust.stat <- joinbyID(trans,"CustomerID")

# returns:
# 1). some customers might not return ALL products they purchased
# 2). for returns>transactions: return products purchased earlier than year 2010 (49rows)
ret <- ecdf %>%
  filter(str_detect(InvoiceNo,"C")&Description != "Discount") %>%
  group_by(CustomerID) %>%
  summarise(return=length(unique(InvoiceNo)))
cust.stat <- joinbyID(ret,"CustomerID")
cust.stat <- cust.stat %>%
  mutate(return=ifelse(is.na(return),0,return))

# Units per transaction (median)
upt_temp <- ecdf %>%
  filter(!str_detect(InvoiceNo,"C")) %>%
  group_by(CustomerID,InvoiceNo) %>%
  summarise(ttl=sum(Quantity))
upt <- upt_temp %>%
  group_by(CustomerID) %>%
  summarise(unitPT=median(ttl))
cust.stat <- joinbyID(upt,"CustomerID")

# Time Interval(day):
ecdf$date <- mdy(str_split(ecdf$InvoiceDate," ",simplify = TRUE)[,1])
TI <- ecdf %>%
  filter(!str_detect(InvoiceNo,"C")) %>%
  select(CustomerID,InvoiceNo,InvoiceDate,date)
TI <- unique(TI[,1:4])  
TI <- TI %>%
  arrange(CustomerID,InvoiceNo,date)

