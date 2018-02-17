library(tidyverse)
library(lubridate)
library(stringr)

### user-defined color array ###
mycol <- c('#fb8072','#80b1d3','#fdb462','#b3de69','#fccde5','#8dd3c7','#ffffb3','#bebada','#d9d9d9','#bc80bd')
mycol.dark <- c('#e41a1c','#377eb8','#4daf4a','#984ea3','#ff7f00','#6a3d9a','#a65628','#f781bf','#999999')
### user-defined functions ###
my.fillNA <- function(x, fill){
    # x: a vector with NA
    # fill: any value would like to replace NA
    i <- which(is.na(x))
    if(length(i)>0){
        x[i] <- rep(fill, length(i))
    }
    return(x)
}

my.fillInf <- function(x, fill){
    # x: a vector with NA
    # fill: any value would like to replace NA
    i <- which(is.infinite(x))
    if(length(i)>0){
        x[i] <- rep(fill, length(i))
    }
    return(x)
}

my.joinbyID <- function(df,id){
    cust.stat <- cust.stat %>%
        left_join(df,by=id)
    return(cust.stat)
}

my.getSeason <- function(mon){
    res <- rep(NA, length(mon))
    res[mon<=12] <- "winter"
    res[mon<=9] <- "autumn"
    res[mon<=6] <- "summer"
    res[mon<=3] <- "spring"
    return(res)
}
my.getPeriod <- function(hour){
    res <- rep(NA, length(hour))
    res[hour<=max(hour)] <- "night"
    res[hour<=16] <- "afternoon"
    res[hour<=13] <- "noon"
    res[hour<=10] <- "morning"
    return(res)
}

my.stat.plot <- function(df, colname, dir="plot"){
    # df: data frame in use
    # colname: column name
    # dir: directory to save plots
    
    # create dir if not exist
    if(!dir.exists(dir)){
        dir.create(dir)
        print(paste0("create ", dir))
    }
    
    # plot
    png(paste0(dir,"/",colname,".png"))
    # categorical features => plot barplot by categories
    if(class(df[[colname]]) %in% c("factor","character")){
        ntype = length(unique(df[[colname]]))
        if(ntype <= 10){
            barplot(table(df[[colname]]), main = colname, col = mycol[1:ntype], las=1)
        }else{
            barplot(table(df[[colname]]), main = colname, las=1) # las=1 makes y-ticks vertical to axis
        }
        # numeric features => plot empirical cumulative density function
    }else{
        plot.ecdf(df[[colname]], main = colname)
    }
    dev.off()
}

classification_metric <- function(actual_class, predicted_class){
    #stopifnot(length(unique(predicted_class))==length(unique(actual_class)))
    res = list()
    
    conf_mat = table(actual_class, predicted_class) # left is answer, top is prediction
    TN = conf_mat[1,1]
    FN = conf_mat[1,2]
    FP = conf_mat[2,1]
    TP = conf_mat[2,2]
    
    res[["accuracy"]] = (TN+TP)/(TN+FN+FP+TP)
    res[["sensitivity"]] = (TP)/(TP+FN)
    res[["specificity"]] = (TN)/(TN+FP)
    res[["precision"]] = (TP)/(TP+FP)
    res[["recall"]] = (TP)/(TP+FN) # same as sensitivity
    
    return(res)
}


