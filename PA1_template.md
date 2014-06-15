# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

First, we set the working directory where the extracted csv-file was placed. This file is loaded into the object "raw"


```r
setwd("D:/Documents/R/DataScience/git/RepData_PeerAssessment1")
raw <- read.csv("activity.csv")
```

Second, the column containing the dates is formated as such for later recognition of weekdays


```r
tidy <- raw
tidy$date <- as.Date(raw$date, format = "%Y-%m-%d")
```


## What is mean total number of steps taken per day?

Here is a histogram of the total number of steps taken each day:


```r
library(plyr)
steps.day <- ddply(tidy, .(date), summarize, sum=sum(steps))
hist(steps.day$sum, xlab = "number of steps taken each day", ylab = "number of days within the 2-month-range", main = "Histogram (ignoring missing values)")
```

![plot of chunk histogram steps per day](figure/histogram steps per day.png) 

Here is the mean and median total number of steps taken per day:


```r
steps.day.mean <- ddply(tidy, .(date), summarize, mean=mean(steps), median=median(steps))
steps.day.mean
```

```
##          date    mean median
## 1  2012-10-01      NA     NA
## 2  2012-10-02  0.4375      0
## 3  2012-10-03 39.4167      0
## 4  2012-10-04 42.0694      0
## 5  2012-10-05 46.1597      0
## 6  2012-10-06 53.5417      0
## 7  2012-10-07 38.2465      0
## 8  2012-10-08      NA     NA
## 9  2012-10-09 44.4826      0
## 10 2012-10-10 34.3750      0
## 11 2012-10-11 35.7778      0
## 12 2012-10-12 60.3542      0
## 13 2012-10-13 43.1458      0
## 14 2012-10-14 52.4236      0
## 15 2012-10-15 35.2049      0
## 16 2012-10-16 52.3750      0
## 17 2012-10-17 46.7083      0
## 18 2012-10-18 34.9167      0
## 19 2012-10-19 41.0729      0
## 20 2012-10-20 36.0938      0
## 21 2012-10-21 30.6285      0
## 22 2012-10-22 46.7361      0
## 23 2012-10-23 30.9653      0
## 24 2012-10-24 29.0104      0
## 25 2012-10-25  8.6528      0
## 26 2012-10-26 23.5347      0
## 27 2012-10-27 35.1354      0
## 28 2012-10-28 39.7847      0
## 29 2012-10-29 17.4236      0
## 30 2012-10-30 34.0938      0
## 31 2012-10-31 53.5208      0
## 32 2012-11-01      NA     NA
## 33 2012-11-02 36.8056      0
## 34 2012-11-03 36.7049      0
## 35 2012-11-04      NA     NA
## 36 2012-11-05 36.2465      0
## 37 2012-11-06 28.9375      0
## 38 2012-11-07 44.7326      0
## 39 2012-11-08 11.1771      0
## 40 2012-11-09      NA     NA
## 41 2012-11-10      NA     NA
## 42 2012-11-11 43.7778      0
## 43 2012-11-12 37.3785      0
## 44 2012-11-13 25.4722      0
## 45 2012-11-14      NA     NA
## 46 2012-11-15  0.1424      0
## 47 2012-11-16 18.8924      0
## 48 2012-11-17 49.7882      0
## 49 2012-11-18 52.4653      0
## 50 2012-11-19 30.6979      0
## 51 2012-11-20 15.5278      0
## 52 2012-11-21 44.3993      0
## 53 2012-11-22 70.9271      0
## 54 2012-11-23 73.5903      0
## 55 2012-11-24 50.2708      0
## 56 2012-11-25 41.0903      0
## 57 2012-11-26 38.7569      0
## 58 2012-11-27 47.3819      0
## 59 2012-11-28 35.3576      0
## 60 2012-11-29 24.4688      0
## 61 2012-11-30      NA     NA
```


## What is the average daily activity pattern?

The daily activity pattern averages across all days; for the time series plot, missing values have to be treated, here simply ignored.


```r
steps.interval <- ddply(tidy, .(interval), summarize, average=mean(steps, na.rm=TRUE))
plot(steps.interval, type = 'l', xlab = "Interval", ylab = "Average number of steps")
```

![plot of chunk time series plot of 5min intervals over all days](figure/time series plot of 5min intervals over all days.png) 

```r
max.int <- steps.interval$interval[steps.interval$average==max(steps.interval$average)]
```

The 5-minute interval 835 contains the maximum number of steps.


## Imputing missing values


```r
missing <- sum(is.na(tidy$steps))
```

There are 2304 missing values, i.e. number of rows with NAs, in the dataset.

A new dataset is created, named tidy2, with missing date replaced by the mean for that 5-min interval.


```r
tidy2 <- tidy
for (i in 1:dim(tidy)[1]) {
  if (is.na(tidy2[i,1])==TRUE) {
    tidy2[i,1] <- steps.interval$average[steps.interval$interval==tidy2[i,3]]
  }
}
```

Repeat the previous step done on tidy with tidy2: 

Here is a histogram of the total number of steps taken each day (missing values replaced by mean for that 5-min interval)


```r
library(plyr)
steps.day2 <- ddply(tidy2, .(date), summarize, sum=sum(steps))
hist(steps.day2$sum, xlab = "number of steps taken each day", ylab = "number of days within the 2-month-range", main = "Histogram (missing values replaced by 5-min average)")
```

![plot of chunk histogram steps per day2](figure/histogram steps per day2.png) 

Here is the mean and median total number of steps taken per day (missing values replaced by mean for that 5-min interval)


```r
steps.day.mean2 <- ddply(tidy2, .(date), summarize, mean=mean(steps), median=median(steps))
steps.day.mean2
```

```
##          date    mean median
## 1  2012-10-01 37.3826  34.11
## 2  2012-10-02  0.4375   0.00
## 3  2012-10-03 39.4167   0.00
## 4  2012-10-04 42.0694   0.00
## 5  2012-10-05 46.1597   0.00
## 6  2012-10-06 53.5417   0.00
## 7  2012-10-07 38.2465   0.00
## 8  2012-10-08 37.3826  34.11
## 9  2012-10-09 44.4826   0.00
## 10 2012-10-10 34.3750   0.00
## 11 2012-10-11 35.7778   0.00
## 12 2012-10-12 60.3542   0.00
## 13 2012-10-13 43.1458   0.00
## 14 2012-10-14 52.4236   0.00
## 15 2012-10-15 35.2049   0.00
## 16 2012-10-16 52.3750   0.00
## 17 2012-10-17 46.7083   0.00
## 18 2012-10-18 34.9167   0.00
## 19 2012-10-19 41.0729   0.00
## 20 2012-10-20 36.0938   0.00
## 21 2012-10-21 30.6285   0.00
## 22 2012-10-22 46.7361   0.00
## 23 2012-10-23 30.9653   0.00
## 24 2012-10-24 29.0104   0.00
## 25 2012-10-25  8.6528   0.00
## 26 2012-10-26 23.5347   0.00
## 27 2012-10-27 35.1354   0.00
## 28 2012-10-28 39.7847   0.00
## 29 2012-10-29 17.4236   0.00
## 30 2012-10-30 34.0938   0.00
## 31 2012-10-31 53.5208   0.00
## 32 2012-11-01 37.3826  34.11
## 33 2012-11-02 36.8056   0.00
## 34 2012-11-03 36.7049   0.00
## 35 2012-11-04 37.3826  34.11
## 36 2012-11-05 36.2465   0.00
## 37 2012-11-06 28.9375   0.00
## 38 2012-11-07 44.7326   0.00
## 39 2012-11-08 11.1771   0.00
## 40 2012-11-09 37.3826  34.11
## 41 2012-11-10 37.3826  34.11
## 42 2012-11-11 43.7778   0.00
## 43 2012-11-12 37.3785   0.00
## 44 2012-11-13 25.4722   0.00
## 45 2012-11-14 37.3826  34.11
## 46 2012-11-15  0.1424   0.00
## 47 2012-11-16 18.8924   0.00
## 48 2012-11-17 49.7882   0.00
## 49 2012-11-18 52.4653   0.00
## 50 2012-11-19 30.6979   0.00
## 51 2012-11-20 15.5278   0.00
## 52 2012-11-21 44.3993   0.00
## 53 2012-11-22 70.9271   0.00
## 54 2012-11-23 73.5903   0.00
## 55 2012-11-24 50.2708   0.00
## 56 2012-11-25 41.0903   0.00
## 57 2012-11-26 38.7569   0.00
## 58 2012-11-27 47.3819   0.00
## 59 2012-11-28 35.3576   0.00
## 60 2012-11-29 24.4688   0.00
## 61 2012-11-30 37.3826  34.11
```

The NA's affected only eight days, but those entirely (8 time 288 equals the total of 2304 missing values). Therefore, the mean over a day was not useful to replace the NA's. Consequently, only those 8 days were affected, and all of them received identical values, i.e. for each of the 288 5-min intervals the average of the 53 valid days.  
In the histogram, the middle bar grew by 8.  
In the mean/median table, these 8 days received identical mean/median replacing the previous NAs; the other 53 days remaining unchanged.


## Are there differences in activity patterns between weekdays and weekends?

A new factor variable is created in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday day or weekend day.


```r
tidy2$wd.type <- weekdays(tidy2$date)
tidy2$wd.type <- gsub("Montag", "weekday", tidy2$wd.type)
tidy2$wd.type <- gsub("Dienstag", "weekday", tidy2$wd.type)
tidy2$wd.type <- gsub("Mittwoch", "weekday", tidy2$wd.type)
tidy2$wd.type <- gsub("Donnerstag", "weekday", tidy2$wd.type)
tidy2$wd.type <- gsub("Freitag", "weekday", tidy2$wd.type)
tidy2$wd.type <- gsub("Samstag", "weekend", tidy2$wd.type)
tidy2$wd.type <- gsub("Sonntag", "weekend", tidy2$wd.type)
tidy2$wd.type <- as.factor(tidy2$wd.type)
```

The daily activity pattern averages across all days, separating between weekday days and weekend days. 


```r
steps.interval.wd <- ddply(tidy2, .(interval, wd.type), summarize, average=mean(steps))
library(lattice)
xyplot(average ~ interval | wd.type, data = steps.interval.wd, layout = c(1,2), type = 'l', xlab = "Interval", ylab = "Average number of steps")
```

![plot of chunk time series plot of 5min intervals over all weekdays and weekends respectively](figure/time series plot of 5min intervals over all weekdays and weekends respectively.png) 

