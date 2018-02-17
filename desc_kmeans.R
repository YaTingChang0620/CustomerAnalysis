library(factoextra)
library(tidyverse)
library(reshape2)
library(wordcloud)
source("utils.R")

local({
    ecdf <- readRDS("ecdf.Rds")         # from eCommerce Analysis.R
    
    d <- read_csv("desc_matrix.csv")    # from description_matrix.ipynb
    d <- d[which(!is.na(match(d$Description,ecdf$Description))),]
    
    # get statistics by Description
    desc.stat <- ecdf %>% group_by(Description) %>% summarize(price.level = mean(UnitPrice))
    
    # cut into several pre-determined bins (observations from quantile)
    desc.stat <- desc.stat %>% 
                    mutate(price.level = cut(price.level, breaks=c(0,1,1.5,2,3,5,8,Inf)) %>% as.numeric())
    # price.level: 1,2,3,4,5,6,7
    desc.stat <- desc.stat %>% mutate(price.level = paste0("PL", price.level))
    
    ### plot the distribution of price levels
    # name <- c("1","1-1.5","1.5-2","2-3","3-5","5-8",">8")
    # df <- table(desc.stat$price.level)
    # png("hist_price_level.png")
    # pp <- barplot(df, axes=F, xlab="Price level", ylab="Number of products",yaxt='n')
    # axis(side=1, at=pp[,1], labels=name)
    # axis(side=2, at=seq(0,1500,100),labels = seq(0,1500,100),las=2)
    # dev.off()
    
    # dcast to wide array
    desc.stat <- dcast(desc.stat, Description~price.level, fun.aggregate = length)
    
    # merge into desc_matrix
    d <- merge(d,desc.stat,by="Description")
    
    # same procedures as the above but in viewpoint of quantity
    desc.stat <- ecdf %>% group_by(Description) %>% summarize(quantity.level = median(Quantity))
    desc.stat <- desc.stat %>% mutate(quantity.level = cut(quantity.level, breaks = c(-Inf,1,3,5,7,9,15,Inf)) %>% as.numeric())
    desc.stat <- desc.stat %>% mutate(quantity.level = paste0("QL", quantity.level))
    
    ### plot the distribution of quantity levels
    # brs <- c(-Inf,1,3,5,7,9,15,Inf)
    # name <- c("1","2-3","4-5","6-7","8-9","10-15",">15")
    # df <- table(desc.stat$quantity.level)
    # png("hist_quantity_level.png")
    # pp <- barplot(df, axes=F, xlab="Quantity level", ylab="Number of products",xaxt='n',yaxt='n')
    # axis(side=1, at=pp[,1],labels = name)
    # axis(side=2, at=seq(0,1500,100),labels = seq(0,1500,100),las=2)
    # dev.off()
    
    desc.stat <- dcast(desc.stat, Description~quantity.level,fun.aggregate = length)
    d <- merge(d,desc.stat,by="Description")
    
    # remove the column "Description" and then find the best cluster number
    # tutorial: https://rpubs.com/skydome20/R-Note9-Clustering
    png("silhouette_kmeans.png")
    fviz_nbclust(d %>% select(-Description), 
                 FUNcluster = kmeans,   # K-Means
                 method = "silhouette", # Avg. Silhouette
                 k.max = 10            # max number of clusters
    ) + labs(title="silhouette Method for K-Means")
    dev.off()
    
    # run our kmeans without column "Description"
    kmeans.cluster <- kmeans(d %>% select(-Description), centers=8)
    
    # observe the distribution of clusters
    kmeans.cluster$cluster %>% table()
    
    # assign each description to its corresponding cluster
    d$cluster <- kmeans.cluster$cluster
 
    for(k in unique(d$cluster)){
        png(paste0("plot/stat_cluster",k,".png"), width = 1800, height=600, res=200, units = 'px')
        par(mfrow=c(1,3)) # plot with 1 row, 3 column

        # get a subset which belongs to cluster k
        # remove Description, cluster, art (due to too many products having this 'art' keyword, we remove it here for better word cloud visualization)
        tmp <- d %>% filter(cluster==k) %>% select(-Description,-cluster, -art)

        i.PL <- grep("PL",colnames(tmp)) # column indices of price levels
        i.QL <- grep("QL",colnames(tmp)) # column indices of quantity levels
        pd <- tmp[,-c(i.PL,i.QL)]        # remove columns of both price and quantity levels => keyword matrix

        # wordcloud 
        # keyword matrixs of products in this cluster
        pd <- pd %>% 
            colSums() %>%   # calculate the total occcurences of each keyword (below)
                            # bag  tree  christmas   ...
                            #  5    10       50      ...
            melt() %>%      # to a long array
            mutate(word = rownames(.), percent =  round(value/sum(value)*1000,0 )) # calculate (percentage * 1000) for better visualization

        wordcloud(pd$word,pd$percent,min.freq = 1, random.order = F, ordered.colors = F,colors = rev(mycol.dark))

        # Price level
        pd <- tmp[,i.PL]
        pd <- colSums(pd) # obtain the count in each level
        pd <- pd/sum(pd)  # normalize to percentage
        barplot(pd, col = mycol[1:7], xlim = c(0,1), horiz = TRUE, las=1, main = paste0("Cluster ",k," - Price Level"), xlab = "Ratio")

        # Quantity level
        pd <- tmp[,i.QL]
        pd <- colSums(pd) # obtain the count in each level
        pd <- pd/sum(pd)  # normalize to percentage
        barplot(pd, col = mycol[1:7], xlim = c(0,1), horiz = TRUE, las=1, main = paste0("Cluster ",k," - Quantity Level"), xlab = "Ratio")
        dev.off()
    }

    saveRDS(d, file="desc_cluster.rds")
})

