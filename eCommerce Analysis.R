library(tidyverse)
library(lubridate)
library(stringr)
source("utils.R") # to include functions in utils.R

# ecdf <-read_csv("data.csv")

# ##############################

# ## Handling missing value and outlier ##
# #Determine the possible explaination for NA 
# apply(ecdf,2,function(x){sum(is.na(x))})
# idNa <- ecdf[which(is.na(ecdf$CustomerID)),]
# apply(idNa,2,function(x){length(unique(x))})

# #distribution of unit price and quantity:
# #1).qunatity<0 -> cancelled/returned orders OR discount
# quantile(ecdf$Quantity)
# ecdf[ecdf$Quantity<0,]
# #2).unit price -> define price larger than 145 as outlier 
# quantile(ecdf$UnitPrice)
# quantile(ecdf$UnitPrice,probs=seq(0.99,1,0.0005))

# # remove rows:
# # 1). customerID is NA
# # 2). unitprice equals to 0
# # 3). uniprice outlier ($145) 
# ecdf<-ecdf[!is.na(ecdf$CustomerID),]
# ecdf <- ecdf[ecdf$UnitPrice<146,]
# ecdf <- ecdf[!ecdf$UnitPrice==0,]
# saveRDS(ecdf, file="ecdf.Rds")

ecdf <- readRDS("ecdf.Rds")

## Exploration(by customers)##
cust.stat <- data.frame(CustomerID=unique(ecdf$CustomerID))

# total transactions #
trans <- ecdf %>%
  filter(!str_detect(InvoiceNo,"C")) %>%
  group_by(CustomerID) %>%
  summarise(transactions=length(unique(InvoiceNo)))
cust.stat <- my.joinbyID(trans,"CustomerID")

# returns #
# 1). some customers might not return ALL products they purchased
# 2). for returns>transactions: return products purchased earlier than year 2010 (49rows)
ret <- ecdf %>%
  filter(str_detect(InvoiceNo,"C")&Description != "Discount") %>%
  group_by(CustomerID) %>%
  summarise(return=length(unique(InvoiceNo)))
cust.stat <- my.joinbyID(ret,"CustomerID")
cust.stat <- cust.stat %>%
  mutate(return=ifelse(is.na(return),0,return))

# return rate #
cust <- cust.stat %>%
  mutate(returnRate=round(return/transactions,2))

# returned amount #
rpp <- ecdf %>%
  filter(str_detect(InvoiceNo,"C")&Description != "Discount") %>%
  group_by(CustomerID) %>%
  summarise(returnPrice = sum(abs(UnitPrice*Quantity)))
cust.stat <- my.joinbyID(rpp,"CustomerID")

# Units per transaction (median) #
upt_temp <- ecdf %>%
  filter(!str_detect(InvoiceNo,"C")) %>%
  group_by(CustomerID,InvoiceNo) %>%
  summarise(ttl=sum(Quantity))
upt <- upt_temp %>%
  group_by(CustomerID) %>%
  summarise(unitPT=median(ttl))
cust.stat <- my.joinbyID(upt,"CustomerID")

# Items per transaction (median) #
ipt_temp <- ecdf %>%
  filter(!str_detect(InvoiceNo,"C")) %>%
  group_by(CustomerID,InvoiceNo) %>%
  summarise(ttl=n())

ipt <- ipt_temp %>%
  group_by(CustomerID) %>%
  summarise(itemPT=median(ttl))

cust.stat <- my.joinbyID(ipt,"CustomerID")


# Time Interval(day): #
ecdf$date <- mdy(str_split(ecdf$InvoiceDate," ",simplify = TRUE)[,1])
## another way:
# ecdf %>%
#   mutate(date=mdy_hm(ecdf$InvoiceDate))

TI <- ecdf %>%
  filter(!str_detect(InvoiceNo,"C")) %>%
  select(CustomerID,InvoiceNo,InvoiceDate,date)
TI <- unique(TI[,1:4])

TI <- TI %>% mutate(InvoiceHour=substr(InvoiceDate,(nchar(InvoiceDate)-4),nchar(InvoiceDate)-3),
                    InvoiceMon=month(TI$date),
                    InvoiceWeekDays=weekdays(TI$date))

TI$InvoiceWeekDays <- factor(TI$InvoiceWeekDays,levels = c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday"))
TI$InvoiceHour <- as.numeric(TI$InvoiceHour)
TI$InvoiceMon <- as.numeric(TI$InvoiceMon)
# my.getSeason and my.getPeriod
TI <- TI %>% mutate(InvoiceSeason=my.getSeason(InvoiceMon),
                    InvoicePeriod=my.getPeriod(InvoiceHour))

# because the below features are discrete => to factor
TI$InvoiceHour <- factor(TI$InvoiceHour, levels=seq(min(TI$InvoiceHour), max(TI$InvoiceHour)))
TI$InvoiceMon <- factor(TI$InvoiceMon, levels=seq(min(TI$InvoiceMon), max(TI$InvoiceMon)))
TI$InvoiceSeason <- factor(TI$InvoiceSeason, levels = c("spring","summer","autumn","winter"))
TI$InvoicePeriod <- factor(TI$InvoicePeriod, levels = c("morning","noon","afternoon","night"))

# plot 
my.stat.plot(TI, "InvoiceHour", "plot")
my.stat.plot(TI, "InvoicePeriod", "plot")
my.stat.plot(TI, "InvoiceSeason", "plot")
my.stat.plot(TI, "InvoiceMon", "plot")

library(reshape2)
# cast to wide columns
period.stat <- dcast(TI, CustomerID~InvoicePeriod, fun.aggregate = length, fill = 0)
season.stat <- dcast(TI, CustomerID~InvoiceSeason, fun.aggregate = length, fill = 0)
weekday.stat <- dcast(TI, CustomerID~InvoiceWeekDays, fun.aggregate = length, fill=0)

TI <- TI %>%
  arrange(CustomerID,InvoiceNo,date) %>%
  group_by(CustomerID) %>%
  summarise(ave.interval=round(mean(diff(date)),2))

TI$ave.interval <- my.fillNA(TI$ave.interval,0)
TI$ave.interval <- as.numeric(TI$ave.interval)

cust.stat <- my.joinbyID(TI,"CustomerID")
cust.stat <- my.joinbyID(period.stat, "CustomerID")
cust.stat <- my.joinbyID(season.stat, "CustomerID")
cust.stat <- my.joinbyID(weekday.stat, "CustomerID")

# Total Spending #
# Total Spending <0 : return products purchased before 2010
ttl<- ecdf %>%
  group_by(CustomerID) %>%
  summarise(ttlSpend=sum(Quantity*UnitPrice))
cust.stat <- my.joinbyID(ttl,"CustomerID")

# Average Spending # 
cust.stat <- cust.stat %>% group_by(CustomerID) %>%
    mutate(aveSpend= 
               if(ttlSpend>0 && transactions==return){
                   round(ttlSpend/transactions,2)
                }else if(ttlSpend>0){
                   round(ttlSpend/(transactions-return),2)
                }else ttlSpend)

# average unit price #
aveUP <- ecdf %>%
  filter(!str_detect(InvoiceNo,"C")) %>%
  group_by(CustomerID) %>%
  summarise(aveUnitPrice=median(UnitPrice))
cust.stat <- my.joinbyID(aveUP,"CustomerID")

# returned amount/total spending #
cust.stat <- cust.stat %>%
  mutate(`%retAmount`=round(returnPrice/ttlSpend,4))

# country #
country <- unique(ecdf[,7:8])
dup <- which(duplicated(country$CustomerID))
country <- country[-dup,]
cust.stat <- my.joinbyID(country, "CustomerID")

