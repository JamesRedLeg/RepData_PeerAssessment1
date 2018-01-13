---
title: "Reproducible Research Peer Assessment 1"
author: "James C. Birk"
date: "January 12, 2018"
output:
  html_document: 
    fig_caption: yes
    keep_md: yes
  'html_document: keep_md:TRUE': default
---



## Initial Setup and Preprocessing of our assignment data

```r
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
library(lattice)
activityData<- read.csv("C://Users/James/Documents/R/activity.csv")
summary(activityData)
```

```
##      steps                date          interval     
##  Min.   :  0.00   2012-10-01:  288   Min.   :   0.0  
##  1st Qu.:  0.00   2012-10-02:  288   1st Qu.: 588.8  
##  Median :  0.00   2012-10-03:  288   Median :1177.5  
##  Mean   : 37.38   2012-10-04:  288   Mean   :1177.5  
##  3rd Qu.: 12.00   2012-10-05:  288   3rd Qu.:1766.2  
##  Max.   :806.00   2012-10-06:  288   Max.   :2355.0  
##  NA's   :2304     (Other)   :15840
```
I also want to see the structure of the data as well as the header.

```r
str(activityData)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

```r
head(activityData)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```
I have three elements, and I see that the dates are currently coded as factors and not as dates, so I fix that next.

```r
activityData$date<- as.Date(activityData$date)
str(activityData)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```
Now I can start addressing the assigment questions. 
##What is the mean total number of steps taken per day?  

Make a histogram and display it:

```r
sum_steps<-aggregate(activityData$steps, by=list(activityData$date), FUN=sum, na.rm=TRUE)
names(sum_steps)<- c("date", "total")
hist(sum_steps$total, 
     breaks=seq(from=0, to=25000, by=2000),
     col="green", 
     xlab="Total number of steps", 
     ylim=c(0, 25), 
     main="Histogram of the total number of steps taken each day\n(NA removed)")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

```r
mean(sum_steps$total)
```

```
## [1] 9354.23
```

```r
median(sum_steps$total)
```

```
## [1] 10395
```
##What is the average daily activity pattern? Which 5-minute interval,on average across all the days in the dataset, contains the maximum number of steps?

```r
## Determine the means across each interval
 mean_steps <- aggregate(activityData$steps, 
                       by=list(activityData$interval), 
                       FUN=mean, 
                       na.rm=TRUE)
names(mean_steps)<- c("interval", "mean")
## Develop a time series plot and answer the question
plot(mean_steps$interval, 
    mean_steps$mean, 
      type="l", 
      col="green", 
     lwd=3, 
      xlab="Interval [minutes]", 
     ylab="Average number of steps", 
      main="Time-series of the average number of steps per interval\n(NA removed)")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->
**What is that peak interval?**

```r
## Find the maximum mean and determine which interval it is in
## R studio had no problem using the max function with missing values, but KnitR did not like it
RobustMax <- function(x) {if (length(x)>0) max(x) else -Inf}
themax_Mean<- which(mean_steps$means ==RobustMax(mean_steps$means))
themax_Interval<- mean_steps[themax_Mean,1]
```
The answer is interval **835**

##Imputing Missing Values
**Calculate and report the total number of missing values in the data set.**

```r
##Count the missing values
countthe_NAs<- sum(is.na(activityData$steps))
countthe_NAs
```

```
## [1] 2304
```

```r
##Determine where the missing values are
wherethe_NAs<- which(is.na(activityData$steps))
##Use the mean of the number steps to fill in the missing values 
more_means<- rep(mean(activityData$steps, na.rm=TRUE), times=length(wherethe_NAs))
## And now we have an updated data set to plot
activityData[wherethe_NAs, "steps"] <- more_means
```

**Make a new histogram and calculate and report the mean and median**

```r
newsum_steps<- aggregate(activityData$steps, by=list(activityData$date), FUN=sum)
names(newsum_steps)<- c("date", "total")
hist(newsum_steps$total, 
    breaks=seq(from=0, to=25000, by=2500),
      col="green", 
      xlab="Total number of steps", 
      ylim=c(0, 30), 
      main="Histogram of the total number of steps taken each day", sub="With imputed values in place of NA")
```

![](PA1_template_files/figure-html/unnamed-chunk-8-1.png)<!-- -->

Here are the mean and median for the new data

```r
mean(newsum_steps$total)
```

```
## [1] 10766.19
```

```r
median(newsum_steps$total)
```

```
## [1] 10766.19
```
Imputing our missing data with the means of each interval has expanded our data set, since there are no longer missing values that we are disregarding. Additionally, the mean and median are equal and have both increased.  

##Are there different activity patterns for weekdays and weekend?
**Create a new factor variable in the dataset to indicate weekday and weekend**


```r
##Differentiate between weekdays and the weekend
activityData2<- activityData%>%
   mutate(weektype= ifelse(weekdays(activityData$date)=="Saturday" | weekdays(activityData$date)=="Sunday", "Weekend", "Weekday"))
```
**Make a panel plot of the 5-minute intervals and the average # of steps taken for both types of days**


```r
mean_data <- aggregate(activityData2$steps, 
                       by=list(activityData2$weektype, 
                               activityData2$date, activityData2$interval), mean)

names(mean_data) <- c("daytype", "Date", "interval", "mean")


xyplot(mean ~ interval | daytype, mean_data, 
       type="l", 
       lwd=1, 
       xlab="Interval", 
       ylab="Number of steps", 
       layout=c(1,2))
```

![](PA1_template_files/figure-html/panelPlot-1.png)<!-- -->
