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


my.summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                         conf.interval=.95, .drop=TRUE) {
    library(plyr)
    
    # New version of length which can handle NA's: if na.rm==T, don't count them
    length2 <- function (x, na.rm=FALSE) {
        if (na.rm) sum(!is.na(x))
        else       length(x)
    }
    
    # This does the summary. For each group's data frame, return a vector with
    # N, mean, and sd
    datac <- ddply(data, groupvars, .drop=.drop,
                   .fun = function(xx, col) {
                       c(N    = length2(xx[[col]], na.rm=na.rm),
                         mean = mean   (xx[[col]], na.rm=na.rm),
                         sd   = sd     (xx[[col]], na.rm=na.rm)
                       )
                   },
                   measurevar
    )
    
    # Rename the "mean" column    
    datac <- rename(datac, c("mean" = measurevar))
    
    datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
    
    # Confidence interval multiplier for standard error
    # Calculate t-statistic for confidence interval: 
    # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
    ciMult <- qt(conf.interval/2 + .5, datac$N-1)
    datac$ci <- datac$se * ciMult
    
    return(datac)
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


