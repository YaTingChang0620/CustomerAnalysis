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
    desc.stat$price.level <- as.numeric(cut(desc.stat$price.level, breaks = c(0,1,1.5,2,3,5,8,Inf)))
    
    # dcast to wide array
    desc.stat <- dcast(desc.stat, Description~price.level,fun.aggregate = length)
    
    # rename columns
    colnames(desc.stat)[-1] <- paste0("PL",colnames(desc.stat)[-1])
    
    # merge into desc_matrix
    d <- merge(d,desc.stat,by="Description")
    
    # same procedures as the above but in viewpoint of quantity
    desc.stat <- ecdf %>% group_by(Description) %>% summarize(quantity.level = median(Quantity))
    desc.stat$quantity.level <- as.numeric(cut(desc.stat$quantity.level, breaks = c(-Inf,1,3,5,7,9,15,Inf)))
    desc.stat <- dcast(desc.stat, Description~quantity.level,fun.aggregate = length)
    colnames(desc.stat)[-1] <- paste0("QL",colnames(desc.stat)[-1])
    d <- merge(d,desc.stat,by="Description")
    
    # remove "Description" column for Kmeans
    x = d[,-1]
    
    # find the best cluster number
    # tutorial: https://rpubs.com/skydome20/R-Note9-Clustering
    png("silhouette_kmeans.png")
    fviz_nbclust(x, 
                 FUNcluster = kmeans,   # K-Means
                 method = "silhouette", # Avg. Silhouette
                 k.max = 10            # max number of clusters
    ) + labs(title="silhouette Method for K-Means")
    dev.off()
    # 
    
    # run our kmeans
    kmeans.cluster <- kmeans(x, centers=8)
    
    # observe the distribution of clusters
    kmeans.cluster$cluster %>% table()
    
    # assign each description to its corresponding cluster
    d$cluster <- kmeans.cluster$cluster
    
    tmp <- d[d$cluster==1,-c(1,ncol(d))]
    i.PL <- grep("PL",colnames(tmp))
    i.QL <- grep("QL",colnames(tmp))
    pd <- tmp[,-c(i.PL,i.QL)]
    
 
    for(k in unique(d$cluster)){
        png(paste0("plot/stat_cluster",k,".png"), width = 1800, height=600, res=200, units = 'px')
        par(mfrow=c(1,3)) # plot with 1 row, 3 column

        # get a subset which belongs to cluster k
        tmp <- d[d$cluster==k,-c(1,ncol(d))]

        i.PL <- grep("PL",colnames(tmp))
        i.QL <- grep("QL",colnames(tmp))
        pd <- tmp[,-c(i.PL,i.QL)]

        # wordcloud
        pd = pd %>% gather(key=word,value=freq) %>%
          group_by(word) %>%
          summarise(freq = sum(freq)) %>%
          mutate(freq = round(freq/sum(freq)*1000,0)) %>%
          filter(word != 'art')

        wordcloud(pd$word,pd$freq,min.freq = 1, random.order = F, ordered.colors = F,colors = rev(mycol.dark))

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

