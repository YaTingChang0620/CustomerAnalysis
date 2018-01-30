library(factoextra)
library(tidyverse)
library(reshape2)

ecdf <- readRDS("ecdf.Rds")

d <- read_csv("desc_matrix.csv")
d <- d[which(!is.na(match(d$Description,ecdf$Description))),]

### 
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
desc.stat <- ecdf %>% group_by(Description) %>% summarize(quantity.level = mean(Quantity))
desc.stat$quantity.level <- as.numeric(cut(desc.stat$quantity.level, breaks = c(-Inf,1,3,5,7,9,15,Inf)))
desc.stat <- dcast(desc.stat, Description~quantity.level,fun.aggregate = length)
colnames(desc.stat)[-1] <- paste0("QL",colnames(desc.stat)[-1])
d <- merge(d,desc.stat,by="Description")

# remove "Description" column for Kmeans
x = d[,-1]

# find the best cluster number
# tutorial: https://rpubs.com/skydome20/R-Note9-Clustering
fviz_nbclust(x, 
             FUNcluster = kmeans,   # K-Means
             method = "silhouette", # Avg. Silhouette
             k.max = 15             # max number of clusters
) + labs(title="silhouette Method for K-Means")

# run our kmeans
kmeans.cluster <- kmeans(x, centers=8)

# observe the distribution of clusters
kmeans.cluster$cluster %>% table()

# assign each description to its corresponding cluster
d$cluster <- kmeans.cluster$cluster

