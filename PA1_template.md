# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

```r
    setwd('C:/Users/kfollmer/Documents/R/Data Science Course/Reproducible Research/RepData_PeerAssessment1')
    data <- read.csv('activity.csv')
    dat_clean <- data[!is.na(data$steps),]
```


## What is mean total number of steps taken per day?

```r
    hist(dat_clean$steps, xlab = "Steps Taken", main = "Histogram of Steps")
```

![](PA1_template_files/figure-html/stepSummary-1.png) 

```r
    meanSteps <- mean(dat_clean$steps)
    medSteps <- median(dat_clean$steps)
```

## What is the average daily activity pattern?

```r
    d3 <- cbind(dat_clean$interval,dat_clean$steps)
    d4 <- aggregate(d3, list(Interval = d3[,1]),mean)
    d4 <- d4[,2:3]
    colnames(d4) <- c("interval", "steps")
    plot(d4$interval, d4$steps,xlab = "Interval",ylab = "Mean of Steps",type = 'l')
```

![](PA1_template_files/figure-html/linegraph-1.png) 

```r
    maxInterval <- d4[d4$steps==max(d4$steps),]
    maxInterval
```

```
##     interval    steps
## 104      835 206.1698
```
## Imputing missing values
How many observations are missing steps values?

```r
    nrow(data[is.na(data$steps),])
```

```
## [1] 2304
```
Add the mean value for that interval to account for the missing intervals

```r
    edit <- data[is.na(data$steps),2:3]
    editUpdate <- merge(edit,d4,by="interval")
    library(data.table)
    editUpdate <- data.table(editUpdate)
    setcolorder(editUpdate,c("steps","date","interval"))
    proxyDat <- rbind(dat_clean,editUpdate, fill=TRUE)
```

## Are there differences in activity patterns between weekdays and weekends?
First, we set up the data

```r
d5 <- cbind(proxyDat,dow = weekdays(as.Date(proxyDat$date)))
dow <- c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")
label <- c("weekday","weekday","weekday","weekday","weekday","weekend","weekend")
lookup <- cbind(dow,label)
d6 <- merge(d5, lookup, by="dow")
```
Then, I plot using lattice

```r
library(lattice)
d6 <- data.frame(d6)
d6a <- aggregate(steps~interval+label,d6,mean)
xyplot(d6a$steps~d6a$interval|d6a$label, par = c(2,1),type = 'l', xlab = "Interval", ylab = "Average Steps")
```

![](PA1_template_files/figure-html/weekdayPlot-1.png) 
