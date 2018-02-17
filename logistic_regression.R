# install new packages if not installed before
requireLibrary <- c("tidyverse", "lubridate", "stringr", "reshape2","MLmetrics")
for(lib in requireLibrary){
    if(!(lib %in% installed.packages())) install.packages(lib)
}
library(tidyverse)
library(lubridate)
library(stringr)
source("utils.R")
library(MLmetrics)

# Question: whether customers will come back in the following three months?
# Observation period: 2010/12 - 2011/9
# Test period       : 2011/10 - 2011/12

ecdf <- readRDS("ecdf.Rds")
ecdf$date <- mdy(str_split(ecdf$InvoiceDate," ",simplify = TRUE)[,1])

# select the log during 2010/12 - 2011/9 as observe
i <- which(year(ecdf$date) == 2010)
i <- c(i, which((year(ecdf$date)<=2011 & month(ecdf$date) <= 11)))
observe <- ecdf[i,]
test <- ecdf[-i,]

## generate the feauters we created in eCommerce Analysis.R
## use observe period
ecdf <- observe

## Exploration(by customers)##
cust.stat <- data.frame(CustomerID=unique(ecdf$CustomerID))

# total transactions #
trans <- ecdf %>% 
    filter(!str_detect(InvoiceNo,"C")) %>%
    group_by(CustomerID) %>%
    summarise(transactions=length(unique(InvoiceNo)))
cust.stat <- my.joinbyID(trans,"CustomerID")

# brs <- c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,20,50,100,Inf)
# name <- c(seq(1,15),"16-20","21-50","51-100",">100")
# df <- table(cut(trans$transactions,breaks = brs))
# png("hist_transacation.png",width = 1024, height=512, units='px')
# pp <- barplot(df, axes=F, xlab="Number of transactions", ylab="Number of customers",xaxt='n',ylim=c(0,1500))
# axis(side=1, at=pp[,1],labels = name)
# axis(side=2, at=seq(0,1500,100),labels = seq(0,1500,100),las=2)
# dev.off()

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
cust.stat$returnPrice <- my.fillNA(cust.stat$returnPrice, fill=0)

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
ecdf <- ecdf %>% mutate(date=mdy_hm(ecdf$InvoiceDate))

TI <- ecdf %>%
    filter(!str_detect(InvoiceNo,"C")) %>%
    select(CustomerID,InvoiceNo,InvoiceDate,date)

TI <- unique(TI[,1:4])

TI <- TI %>% mutate(InvoiceHour=hour(date) %>% as.factor(),
                    InvoiceMon=month(date) %>% as.factor(),
                    InvoiceWeekDays=weekdays(date) %>% factor(levels = c("Monday","Tuesday","Wednesday","Thursday","Friday","Sunday")),
                    InvoiceSeason=month(date) %>% my.getSeason() %>% factor(levels = c("spring","summer","autumn","winter")),
                    InvoicePeriod=hour(date) %>% my.getPeriod() %>% factor(levels = c("morning","noon","afternoon","night"))
)

# TI$InvoiceWeekDays <- factor(TI$InvoiceWeekDays,levels = c("Monday","Tuesday","Wednesday","Thursday","Friday","Sunday")) # no Saturday
# TI$InvoiceHour <- as.numeric(TI$InvoiceHour)
# TI$InvoiceMon <- as.numeric(TI$InvoiceMon)
# my.getSeason and my.getPeriod #
# TI <- TI %>% mutate(InvoiceSeason=my.getSeason(InvoiceMon) %>% factor(levels = c("spring","summer","autumn","winter")),
#                     InvoicePeriod=my.getPeriod(InvoiceHour) %>% factor(levels = c("morning","noon","afternoon","night")))

# because the below features are discrete => to factor #
# TI$InvoiceHour <- factor(TI$InvoiceHour, levels=seq(min(TI$InvoiceHour), max(TI$InvoiceHour)))
# TI$InvoiceMon <- factor(TI$InvoiceMon, levels=seq(min(TI$InvoiceMon), max(TI$InvoiceMon)))
# TI$InvoiceSeason <- factor(TI$InvoiceSeason, levels = c("spring","summer","autumn","winter"))
# TI$InvoicePeriod <- factor(TI$InvoicePeriod, levels = c("morning","noon","afternoon","night"))

# plot #
# my.stat.plot(TI, "InvoiceHour", "plot")
# my.stat.plot(TI, "InvoicePeriod", "plot")
# my.stat.plot(TI, "InvoiceSeason", "plot")
# my.stat.plot(TI, "InvoiceMon", "plot")

# cast to wide columns #
period.stat <- dcast(TI, CustomerID~InvoicePeriod, fun.aggregate = length, fill = 0)
season.stat <- dcast(TI, CustomerID~InvoiceSeason, fun.aggregate = length, fill = 0)
weekday.stat <- dcast(TI, CustomerID~InvoiceWeekDays, fun.aggregate = length, fill=0)

# ave.interval #
TI <- TI %>%
    arrange(CustomerID,InvoiceNo,date) %>%
    group_by(CustomerID) %>%
    # date(): only date without detail hh:mm:ss
    summarise(ave.interval= date %>% date() %>% diff() %>% mean() %>% round(digits = 2) %>% my.fillNA(fill=0) %>% as.numeric())

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

# brs <- c(0,100,200,300,400,500,750,1000,1500,2000,3500,5000,Inf)
# name <- c(seq(100,500,100),"500-750","750-1000","1000-1500", "1500-2000","2000-3500","3500-5000",">5000")
# df <- table(cut(cust.stat$ttlSpend,breaks = brs))
# png("hist_ttlSpend.png",width = 1024, height=512, units='px')
# pp <- barplot(df, axes=F, xlab="Total expense", ylab="Number of customers",xaxt='n')
# axis(side=1, at=pp[,1],labels = name)
# axis(side=2, at=seq(0,600,50),labels = seq(0,600,50),las=2)
# dev.off()

# Average Spending # 
cust.stat <- cust.stat %>% group_by(CustomerID) %>%
    mutate(aveSpend=ttlSpend/transactions)
# if(ttlSpend>0 && transactions==return){
#     round(ttlSpend/transactions,2)
#  }else if(ttlSpend>0){
#     round(ttlSpend/(transactions-return),2)
#  }else ttlSpend)

# average unit price #
aveUP <- ecdf %>%
    filter(!str_detect(InvoiceNo,"C")) %>%
    group_by(CustomerID) %>%
    summarise(aveUnitPrice=median(UnitPrice))
cust.stat <- my.joinbyID(aveUP,"CustomerID")

# returned amount/total spending #
cust.stat <- cust.stat %>%
    mutate(retAmount=round(returnPrice/ttlSpend,4) %>% my.fillInf(fill=1))
# NOTICE: will cause Inf when divided by 0, so here we fillInf by 1 #
# cust.stat$retAmount <- my.fillInf(cust.stat$retAmount, 1)


###############################################################
### product description clustering and its related features ###
###############################################################

# source("desc_kmeans.R") #
desc <- readRDS("desc_cluster.Rds")

# append description's cluster to ecdf #
ecdf$cluster <- desc[match(ecdf$Description,desc$Description),]$cluster

# calculate a customer spend how much on products of each cluster #
tmp <- ecdf %>% group_by(CustomerID, cluster) %>% summarize(spend=sum(Quantity*UnitPrice))

# convert to wide array using dcast, customer vs cluster #
cluster.spend.stat <- dcast(tmp, CustomerID~cluster, value.var = "spend", fill=0)

# rename for better understandning #
colnames(cluster.spend.stat)[-1] <- paste0("ttlSpend.cluster", colnames(cluster.spend.stat)[-1])
rm(tmp) # remove the temporary data.frame

# merge into cust.stat #
cust.stat <- my.joinbyID(cluster.spend.stat, "CustomerID")

#######################
# predictive modeling #
#######################

# generate our answer from test period #
# we calculate the ttlSpend in test period
ans <- test %>% group_by(CustomerID) %>% summarize(ttlSpend=sum(Quantity*UnitPrice))

# match the answers to cust.stat 
cust.stat$futureSpend <- ans[match(cust.stat$CustomerID, ans$CustomerID),]$ttlSpend

# obtain data X
X <- cust.stat %>% 
    as.data.frame() %>% # make sure the data type of data.frame
    mutate(Y = (!is.na(futureSpend))+0) %>% # prediction target, if futureSpend is not NA => come back; otherwise, not come back #
    select(-CustomerID,-futureSpend)    # remove two features: CustomerID and futureSpend #

# train test split #
i.train <- sample(1:nrow(X))[1:(nrow(X)*0.8)]
i.test  <- setdiff(seq(1,nrow(X)), i.train)

Xtrain <- X[i.train,]
Xtest  <- X[i.test,]

# Logistic regression #
logic.model <- glm(Y~., data=Xtrain, family='binomial')

# predict the label of test dataset #
prob <- predict(logic.model, newdata=Xtest, type='response')

# calculate the accuracy #
# because the original dataset => 0.397 #
print(classification_metric(Xtest$Y, prob>=mean(Xtrain$Y)))

AUC(prob, Xtest$Y)

###########################
# k-fold cross validation #
###########################
X <- X[sample(1:nrow(X)),]
k = 5
folds <- seq(1, nrow(X), length.out=k+1) %>% round(digits = 0)
result <- list()
for(i in seq(1,k)){
    Xtest <- X[folds[i]:folds[i+1],]
    Xtrain <- X[-(folds[i]:folds[i+1]),]
    
    # Logistic regression #
    logic.model <- glm(Y~., data=Xtrain, family='binomial')
    
    # predict the label of test dataset #
    prob <- predict(logic.model, newdata=Xtest, type='response')
    
    # calculate the accuracy #
    # because the original dataset => 0.397 #
    res <- classification_metric(Xtest$Y, prob>=mean(Xtrain$Y)) %>% unlist()
    library(MLmetrics)
    res$AUC <- AUC(prob, Xtest$Y)
    result[[i]] <- unlist(res)
}
result <- do.call("rbind", result) %>% as.data.frame()
print(apply(result,2,mean))

