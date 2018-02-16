# install new packages if not installed before
requireLibrary <- c("Rtsne", "tidyverse", "lubridate", "stringr", "reshape2", "factoextra")
for(lib in requireLibrary){
    if(!(lib %in% installed.packages())) install.packages(lib)
}
library(factoextra)
library(tidyverse)
library(lubridate)
library(stringr)
source("utils.R") # to include functions in utils.R
library(cluster)


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

TI$InvoiceWeekDays <- factor(TI$InvoiceWeekDays,levels = c("Monday","Tuesday","Wednesday","Thursday","Friday","Sunday")) # no Saturday
TI$InvoiceHour <- as.numeric(TI$InvoiceHour)
TI$InvoiceMon <- as.numeric(TI$InvoiceMon)
# my.getSeason and my.getPeriod #
TI <- TI %>% mutate(InvoiceSeason=my.getSeason(InvoiceMon),
                    InvoicePeriod=my.getPeriod(InvoiceHour))

# because the below features are discrete => to factor #
TI$InvoiceHour <- factor(TI$InvoiceHour, levels=seq(min(TI$InvoiceHour), max(TI$InvoiceHour)))
TI$InvoiceMon <- factor(TI$InvoiceMon, levels=seq(min(TI$InvoiceMon), max(TI$InvoiceMon)))
TI$InvoiceSeason <- factor(TI$InvoiceSeason, levels = c("spring","summer","autumn","winter"))
TI$InvoicePeriod <- factor(TI$InvoicePeriod, levels = c("morning","noon","afternoon","night"))

# plot #
my.stat.plot(TI, "InvoiceHour", "plot")
my.stat.plot(TI, "InvoicePeriod", "plot")
my.stat.plot(TI, "InvoiceSeason", "plot")
my.stat.plot(TI, "InvoiceMon", "plot")

library(reshape2)
# cast to wide columns #
period.stat <- dcast(TI, CustomerID~InvoicePeriod, fun.aggregate = length, fill = 0)
season.stat <- dcast(TI, CustomerID~InvoiceSeason, fun.aggregate = length, fill = 0)
weekday.stat <- dcast(TI, CustomerID~InvoiceWeekDays, fun.aggregate = length, fill=0)

# ave.interval #
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
  mutate(retAmount=round(returnPrice/ttlSpend,4))
# NOTICE: will cause Inf when divided by 0, so here we fillInf by 1 #
cust.stat$retAmount <- my.fillInf(cust.stat$retAmount, 1)


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

######################################
# dimension reduction and clustering #
######################################

# remove customers whose transaction count is NA (30 people)
cust.stat <- cust.stat[-which(is.na(cust.stat$transactions)),]

# only focus on ttlSpend > 0
cust.stat <- cust.stat[cust.stat$ttlSpend>0,]
 
# data to dimension reduction, remove  CustomerID (1st column) and Country
tmp = cust.stat[,-1]

# set a formula for PCA, "~." means all features are used
pca = prcomp(formula = ~., data = tmp, center=TRUE, scale=TRUE)

vars <- (pca$sdev)^2        # explained variance of each transformed dimension
props <- vars / sum(vars)   # explained variance ratio 
cumulative.props <- cumsum(props)  # cumulative explained variance ratio
plot(cumulative.props)
abline(h=0.8)

# use the first 10 transformed dimensions by PCA as features to kmeans clustering #
fviz_nbclust(pca$x[,1:10], 
             FUNcluster = kmeans,   # K-Means
             method = "silhouette", # Avg. Silhouette
             k.max = 10             # max number of clusters
) + labs(title="silhouette Method for K-Means")
# Result: suggested number of clusters = 2 -> it is not good

# Thus, we apply T-sne to conduct dimension reduction
# Reference: https://www.analyticsvidhya.com/blog/2017/01/t-sne-implementation-r-python/
library(Rtsne)

# conduct tsne using default setup, output dimension = 2 #
tsne <- Rtsne(tmp)

# adopt the transformed two dimensions as features to conduct kmeans clustering #
fviz_nbclust(tsne$Y, 
             FUNcluster = kmeans,   # K-Means
             method = "silhouette", # Avg. Silhouette
             k.max = 10           # max number of clusters
) + labs(title="silhouette Method for K-Means")

# according to the above plot, the number of clusters is 5 #
mycluster <- kmeans(tsne$Y,centers = 4)

# plot clustering result by clusplot #
clusplot(tsne$Y, mycluster$cluster, color=TRUE, shade=TRUE, labels=0, lines=0)

library(cluster)
png("customer_tsne_kmeans.png",width=1200, height=1200, units = 'px', res = 180)
clusplot(tsne$Y, mycluster$cluster, color=TRUE, shade=TRUE, labels=0, lines=0, 
         main="Customer Clustering using T-sne Transformed features", col.p = mycol.dark[2],col.clus = mycol.dark[c(1,3,4,5)], cex = 0.5 )
dev.off()
# resulting cluster appends into our cust.stat#
cust.stat$res.Cluster <- factor(mycluster$cluster, levels = seq(1,4))

## need further analysis on the resulting clustering ##
## visualize each feature by clusters
library(ggplot2)

# boxplot of each feature by cluster #
for( this in colnames(cust.stat)[-c(1,grep("Country", colnames(cust.stat)),ncol(cust.stat))]){
    bounds <- quantile(cust.stat[[this]],c(0.01,0.99))
    pd <- cust.stat[between(cust.stat[[this]],bounds[1],bounds[2]),]
    pd <- data.frame(x=pd$res.Cluster, y = pd[[this]])
    ggplot(data=pd, aes(x=x, y=y, fill=x)) +
        geom_boxplot()+
        theme_bw()+
        scale_fill_manual(values=mycol[4:8])+
        xlab("Customer Cluster")+
        ylab(this)
    ggsave(paste0("plot/byCluster_",this,".png"))
}

for( this in colnames(cust.stat)[-c(1,grep("Country", colnames(cust.stat)),
                                    grep("ttlSpend.cluster",colnames(cust.stat)),ncol(cust.stat))]){
    bounds <- quantile(cust.stat[[this]],c(0.01,0.99))
    pd <- cust.stat[between(cust.stat[[this]],bounds[1],bounds[2]),]
    pd <- data.frame(variable=pd$res.Cluster, value = pd[[this]])
    pd <- my.summarySE(pd, measurevar="value", groupvars = "variable")
    ggplot(data=pd, aes(x=variable, y=value, fill=variable)) +
        geom_bar(position=position_dodge(.9), colour="black", stat="identity") +
        geom_errorbar(position=position_dodge(.9), width=.25, aes(ymin=value-ci, ymax=value+ci)) +
        theme_bw()+
        scale_fill_manual(values=mycol)+
        xlab("Customer Cluster")+
        ylab(this)
    ggsave(paste0("plot/byCluster_",this,".png"))
}

pd <- cust.stat[,c(grep("ttlSpend.cluster", colnames(cust.stat)),ncol(cust.stat))]
pd <- melt(pd, id.vars = "res.Cluster")
bounds <- quantile(pd[["value"]],c(0.05,0.95))
pd <- pd[between(pd[["value"]],bounds[1],bounds[2]),]
pd = my.summarySE(pd,measurevar="value",groupvars=c("res.Cluster", "variable"))


for(i in seq(1,4)){
    ggplot(pd[pd$res.Cluster==i,], aes(x=variable, y=value, fill=variable),log10="y") +
        geom_bar(position=position_dodge(.9), colour="black", stat="identity") +
        geom_errorbar(position=position_dodge(.9), width=.25, aes(ymin=value-ci, ymax=value+ci)) +
        theme_bw()+
        scale_fill_manual(values=mycol)+
        # facet_wrap(~res.Cluster)+
        theme(axis.text.x = element_text(angle = 30, hjust = 1))
    ggsave(paste0("plot/byCluster",i,"_product_distribution.png"))
}

