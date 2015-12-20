---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data
I will load the libraries that will be used in this assignment as well as 
set the LocalTime to get weekdays in English

```r
library(dplyr)
library(lattice)
Sys.setlocale("LC_TIME", "English")
```

```
## [1] "English_United States.1252"
```

```r
data <- read.csv(file="./data/activity.csv", head=TRUE, sep=",")
head(data)
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

```r
names(data)
```

```
## [1] "steps"    "date"     "interval"
```

## What is mean total number of steps taken per day?
This assignment description is:

1. Calculate the total number of steps taken per day

2. If you do not understand the difference between a histogram and a barplot, research
the difference between them. Make a histogram of the total number of steps taken each
day

3. Calculate and report the mean and median of the total number of steps taken per day

Missing days (NA values) will be ignored


```r
total_steps <- aggregate(steps ~ date, data=data, FUN=sum, na.rm=TRUE)
mean(total_steps$steps)
```

```
## [1] 10766.19
```

```r
median(total_steps$steps)
```

```
## [1] 10765
```


## What is the average daily activity pattern?
This assignment description is:

1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis)
and the average number of steps taken, averaged across all days (y-axis)

2. Which 5-minute interval, on average across all the days in the dataset, contains
the maximum number of steps?


```r
activity <- aggregate(steps ~ interval, data=data, FUN=mean, na.rm=TRUE)

plot(activity$interval, activity$steps, type='l', col=1,
   main="Average number of steps", xlab="Interval",
   ylab="Average number of steps")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png) 

```r
max_steps_in_interval_row <- which.max(activity$steps)

# 5-minute interval desired
activity[max_steps_in_interval_row, ]
```

```
##     interval    steps
## 104      835 206.1698
```

## Imputing missing values
This assignment description is:

1. Calculate and report the total number of missing values in the dataset (i.e.
  the total number of rows with NAs)

2. Devise a strategy for filling in all of the missing values in the dataset. The
strategy does not need to be sophisticated. For example, you could use the
mean/median for that day, or the mean for that 5-minute interval, etc.

3. Create a new dataset that is equal to the original dataset but with the missing
data filled in.

4. Make a histogram of the total number of steps taken each day and Calculate and
report the mean and median total number of steps taken per day. Do these values
differ from the estimates from the first part of the assignment? What is the impact
of imputing missing data on the estimates of the total daily number of steps?


```r
NA_counter <- sum(is.na(data))

#number of NAs
NA_counter
```

```
## [1] 2304
```

```r
#fill NA with mean of steps per interval in day
mean_activity <- aggregate(steps ~ interval, data=data, FUN=mean, na.rm=TRUE)
new_data <- data
for (i in 1:nrow(data)) {
  if(is.na(data$steps[i])) {
    interval <- data$interval[i]
    interval_row <- which(mean_activity$interval == interval)
    step <- mean_activity$steps[interval_row]
    new_data$steps[i] <- step
  }
}

new_activity <- aggregate(steps ~ date, data=new_data, FUN=sum, na.rm=TRUE)
hist(new_activity$steps, main = "Total steps by day")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png) 

```r
#new mean
mean(new_activity$steps)
```

```
## [1] 10766.19
```

```r
#new median
median(new_activity$steps)
```

```
## [1] 10766.19
```

The average value remains the sam after filling NA values, but the median value
changed a little bit, it became equal to the average value.

## Are there differences in activity patterns between weekdays and weekends?
This assignment description is:

1. Create a new factor variable in the dataset with two levels - "weekday" and
"weekend" indicating whether a given date is a weekday or weekend day.

2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute
interval (x-axis) and the average number of steps taken, averaged across all weekday
days or weekend days (y-axis).


```r
new_data$date <- as.Date(new_data$date, "%Y-%m-%d")
day <- weekdays(new_data$date)
day_type <- vector()
for (i in 1:nrow(new_data)) {
  if(day[i] == "Saturday" || day[i] == "Sunday") {
    day_type[i] <- 'weekend'
  } else {
    day_type[i] <- 'weekday'
  }
}

new_data$day_type <- day_type
new_data$day_type <- factor(new_data$day_type)

steps_per_day <- aggregate(steps ~ interval + day_type, data=new_data, na.rm=TRUE, FUN=mean)
xyplot(steps ~ interval | day_type, steps_per_day, type = "l", layout = c(1, 2),
  xlab = "Interval", ylab = "Number of steps")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png) 
