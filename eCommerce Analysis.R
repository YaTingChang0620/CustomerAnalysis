# install new packages if not installed before
requireLibrary <- c("Rtsne", "tidyverse", "lubridate", "stringr", "reshape2", "factoextra")
for(lib in requireLibrary){
    if(!(lib %in% installed.packages())) install.packages(lib)
}
source("utils.R") # to include functions in utils.R
library(ggplot2)
library(Rtsne)
library(factoextra)
library(lubridate)
library(stringr)
library(cluster)
library(tidyverse)
library(reshape2)

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
    # get the difference in day between two consecutive entries %>% mean () %>% round to significant digits 2 %>% fill NA by 0
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
cust.stat <- cust.stat %>% filter(!is.na(transactions))

# only focus on ttlSpend > 0
cust.stat <- cust.stat %>% filter(ttlSpend>0) 

# remove  CustomerID (1st column) and Country for dimension reduction
# set a formula for PCA, "~." means all features are used
pca = prcomp(formula = ~., data = cust.stat[,-1], center=TRUE, scale=TRUE)

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

# conduct tsne using default setup, output dimension = 2 #
tsne <- Rtsne(cust.stat[,-1])

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

# library(cluster)
png("customer_tsne_kmeans.png",width=1200, height=1200, units = 'px', res = 180)
clusplot(tsne$Y, mycluster$cluster, color=TRUE, shade=TRUE, labels=0, lines=0, 
         main="Customer Clustering using T-sne Transformed features", col.p = mycol.dark[2],col.clus = mycol.dark[c(1,3,4,5)], cex = 0.5 )
dev.off()

# resulting cluster appends into our cust.stat#
cust.stat = cust.stat %>% as.data.frame() %>% mutate(res.Cluster = factor(mycluster$cluster, levels = seq(1,4)))

## need further analysis on the resulting clustering ##
## visualize each feature by clusters

# boxplot of each feature by cluster #
for( this in colnames(cust.stat)[-c(1,ncol(cust.stat))]){
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

for(this in colnames(cust.stat)[-c(1,grep("ttlSpend.cluster",colnames(cust.stat)),ncol(cust.stat))]){
    bounds <- quantile(cust.stat[[this]],c(0.01,0.99))
    pd <- cust.stat[between(cust.stat[[this]],bounds[1],bounds[2]),]
    pd <- data.frame(variable=pd$res.Cluster, value = pd[[this]])
    #pd <- my.summarySE(pd, measurevar="value", groupvars = "variable")
    ggplot(data=pd, aes(x=variable, y=value, fill=variable)) +
        geom_bar(position=position_dodge(.9), stat="identity") +
        theme_bw()+
        scale_fill_manual(values=mycol)+
        xlab("Customer Cluster")+
        ylab(this)
    ggsave(paste0("plot/byCluster_",this,".png"))
}


# would like to observe the expense in each product clusters by each customer cluster #
pd <- cust.stat %>% 
    group_by(res.Cluster) %>% 
    summarise(ave.spend.cluster1 = median(ttlSpend.cluster1), # can use mean or median (median is more interesting XD)
                ave.spend.cluster2 = median(ttlSpend.cluster2),
                ave.spend.cluster3 = median(ttlSpend.cluster3),
                ave.spend.cluster4 = median(ttlSpend.cluster4),
                ave.spend.cluster5 = median(ttlSpend.cluster5),
                ave.spend.cluster6 = median(ttlSpend.cluster6),
                ave.spend.cluster7 = median(ttlSpend.cluster7),
                ave.apend.cluster8 = median(ttlSpend.cluster8)) %>% melt(id.vars="res.Cluster")

for(i in seq(1,4)){ # from 1 to 4, because we have 4 customer clusters
    ggplot(pd %>% filter(res.Cluster==i), aes(x=variable, y=value, fill=variable),log10="y") +
        geom_bar(position=position_dodge(.9), stat="identity") +
        theme_bw()+ # make background white
        scale_fill_manual(values=mycol)+
        theme(axis.text.x = element_text(angle = 30, hjust = 1)) # rotate labels on x-axis
    ggsave(paste0("plot/byCluster",i,"_product_distribution.png"))
}

