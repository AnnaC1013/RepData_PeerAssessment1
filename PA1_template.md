---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---

## Loading and preprocessing the data

library(plyr)

library(lubridate)

library(knitr)

1. Load the data
2. Process/transform the data (if necessary) into a format suitable for your analysis

```{r}
activity <- read.csv("H:/Data Sci JHU/data/activity.csv")
```

## What is mean total number of steps taken per day?
-- Ignore the missing values for now

1. Calcualte the total number of steps taken per day.

```{r}
stepsperday <- tapply(activity$steps, activity$date, function(x) sum(x, na.rm=TRUE))
```

2. Make a histogram of the total number of steps taken each day.

Fig 1

```{r}
hist(stepsperday, 
      breaks=15,  
      col="blue", 
      xlab="Number of Steps", 
      main="Fig 1: Daily Steps")
```

3. Calculate and report the mean and median of the total number of steps taken per day.

```{r}
stepsmean <- mean(stepsperday, na.rm=TRUE)

stepsmean
```
```{r}
stepsmedian <- median(stepsperday, na.rm=TRUE)

stepsmedian
```

## What is the average daily activity pattern?

1. Make a time series plot (ie type="l") of the 5-min intervals (x-axis) and the average number of steps taken, averaged across all days (y-axis).

Fig 2

```{r}
library(lattice)

steptimeplot <- aggregate(activity$steps ~ activity$interval, activity, FUN=mean, na.rm=TRUE)

names(steptimeplot) <- c("interval", "mean_steps")

xyplot(steptimeplot$mean_steps ~ steptimeplot$interval,  
          type="l", 
          ylab="Mean steps", 
          xlab="5min interval measures", 
          main="Fig 2: Time Series Plot")
```

2. Which 5-min interval, on average across all the days in the dataset, contains the max number of steps?

```{r}
maxsteps <- which.max(steptimeplot$mean_steps)

maxsteps
```

## Imputing missing values
-- Missing values coded as NA

1. Calculate and report the total number of missing values in the dataset (ie the total number of rows with NAs).

```{r}
sum(is.na(activity$steps))
```

2. Devise a strategy for filling in all of the missing values in the dataset. 

```{r}
sub_na <- activity[is.na(activity),]
sub_na$steps <- merge(steptimeplot, sub_na)$mean_steps
```

3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

```{r}
filledactivity <- activity
filledactivity[is.na(activity),] <- sub_na
dailystepsfilled <- tapply(filledactivity$steps, filledactivity$date, function(x) mean(x))
```

4. Make a histogram of the total number of steps taken each day. Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part? What is the impact of imputing missing data on the estimates of the total daily number of steps?

Fig 3

```{r}
hist(dailystepsfilled, 
      breaks=15, 
      col="red", 
      xlab="Number of steps (NA=mean)", 
      main="Fig 3: Daily Steps")
```

## Are there differences in activity patterns between weekdays and weekends?
-- Use the filled in dataset

1. Create a new factor variable in the dataset with two levels- "weekday" and "weekend"

```{r}
daytype <- function(date) {
            if (weekdays(as.Date(date)) %in% c("Saturday", "Sunday"))  {
                      "Weekend"
            } else {
                      "Weekday"
            }
}

filledactivity$daytype <- as.factor(sapply(filledactivity$date, daytype))
filledactivity$day <- sapply(filledactivity$date, FUN=daytype)
```

2. Make a panel plot containing a time series plot (ie type="l") of the 5-min interval (x-axis) and the average number of steps taken, averages across all weekdays or weekend days (y-axis). 

Fig 4

```{r}
library(ggplot2)

averages <- aggregate(steps ~ interval+day, data=filledactivity, mean)

ggplot(averages, aes(interval, steps)) + geom_line() + facet_grid(day ~ .) + xlab("5min interval measures") + ylab("Number of steps")
```