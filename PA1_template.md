Reproducible Research: Peer Assessment 1
========================================



## Loading and preprocessing the data

Unpacking the data and loading it:


```r
setwd("C:/Users/a1032305/R/RR/RepData_PeerAssessment1")
unzip(zipfile="activity.zip")
data <- read.csv("activity.csv")
```


## What is mean total number of steps taken per day?

Plotting a histogram of the steps taken per day:

```r
library(ggplot2)
total.steps <- tapply(data$steps, data$date, FUN=sum, na.rm=TRUE)
qplot(total.steps, binwidth=1000, xlab="total number of steps per day")
```

![plot of chunk stepsperday](figure/stepsperday-1.png) 

```r
mean_val <- mean(total.steps, na.rm=TRUE)
median_val <- median(total.steps, na.rm=TRUE)
mean_val
```

```
## [1] 9354.23
```

```r
median_val
```

```
## [1] 10395
```

The mean value of steps per day is 9354.2295082. 
The median value of steps per day is 10395.

## What is the average daily activity pattern?

Time series plot of the all steps, displayed in 5-minutes intervals:

```r
library(ggplot2)
averages <- aggregate(x=list(steps=data$steps), by=list(interval=data$interval),
                      FUN=mean, na.rm=TRUE)
ggplot(data=averages, aes(x=interval, y=steps)) +
  geom_line() +
  xlab("5-minute interval") +
  ylab("average number of steps taken")
```

![plot of chunk dailypattern](figure/dailypattern-1.png) 

Calculating the 5-min interval wiht the max number of steps:


```r
avs<-averages[which.max(averages$steps),]
interv<-avs[1,1]
steps<-avs[1,2]
interv
```

```
## [1] 835
```

```r
steps
```

```
## [1] 206.1698
```

The max number of steps (206.1698113) occurs in interval number 835.

## Imputing missing values


Calculating the number of missing values: 


```r
missing <- is.na(data$steps)
missing_n <- table(missing)[2]
missing_n
```

```
## TRUE 
## 2304
```

The total number of missing values is 2304.


Replacing the missing values wiht 5 min average:


```r
fill.value <- function(steps, interval) {
  filled <- NA
  if (!is.na(steps))
    filled <- c(steps)
  else
    filled <- (averages[averages$interval==interval, "steps"])
  return(filled)
}
filled.data <- data
filled.data$steps <- mapply(fill.value, filled.data$steps, filled.data$interval)
```


Drawing a histogram:

```r
total.steps <- tapply(filled.data$steps, filled.data$date, FUN=sum)
qplot(total.steps, binwidth=1000, xlab="total number of steps taken each day")
```

![plot of chunk drawhistogram](figure/drawhistogram-1.png) 

```r
new_mean <- mean(total.steps)
new_median <- median(total.steps)
new_mean
```

```
## [1] 10766.19
```

```r
new_median
```

```
## [1] 10766.19
```

Having the dataset wiht substitued missing data, the new mean and the new median are higher than the initial values.

## Are there differences in activity patterns between weekdays and weekends?

Creating funtion to find out if a day is a weekday or a weekend day:


```r
weekday.or.weekend <- function(date) {
  day <- weekdays(date)
  if (day %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"))
    return("weekday")
  else if (day %in% c("Saturday", "Sunday"))
    return("weekend")
  else
    stop("invalid date")
}
filled.data$date <- as.Date(filled.data$date)
filled.data$day <- sapply(filled.data$date, FUN=weekday.or.weekend)
```

Plotting a comaprison between Weekdays and Weekenddays:


```r
averages <- aggregate(steps ~ interval + day, data=filled.data, mean)
ggplot(averages, aes(interval, steps)) + geom_line() + facet_grid(day ~ .) +
  xlab("5-minute interval") + ylab("Number of steps")
```

![plot of chunk makeplot](figure/makeplot-1.png) 

