# Reproducible Research: Peer Assessment 1

## Load packages


```r
library(dplyr)
library(ggplot2)
```

## Loading and preprocessing the data


```r
activity <- read.csv(unz("activity.zip", "activity.csv"))
activity[ , 2] <- as.Date(activity[ , 2])

summary(activity)
```

```
##      steps             date               interval     
##  Min.   :  0.00   Min.   :2012-10-01   Min.   :   0.0  
##  1st Qu.:  0.00   1st Qu.:2012-10-16   1st Qu.: 588.8  
##  Median :  0.00   Median :2012-10-31   Median :1177.5  
##  Mean   : 37.38   Mean   :2012-10-31   Mean   :1177.5  
##  3rd Qu.: 12.00   3rd Qu.:2012-11-15   3rd Qu.:1766.2  
##  Max.   :806.00   Max.   :2012-11-30   Max.   :2355.0  
##  NA's   :2304
```

## What is mean total number of steps taken per day?

For this part of the assignment, you can ignore the missing values in
the dataset.

1. Make a histogram of the total number of steps taken each day


```r
activity_daily <- activity %>%
                  filter(!is.na(steps)) %>%
                  group_by(date) %>%
                  summarise(steps = sum(steps))

ggplot(activity_daily, aes(steps)) + geom_histogram(binwidth = 5000)
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png) 

2. Calculate and report the **mean** and **median** total number of steps taken per day


```r
mean(activity_daily$steps)
```

```
## [1] 10766.19
```

```r
median(activity_daily$steps)
```

```
## [1] 10765
```

## What is the average daily activity pattern?

1. Make a time series plot (i.e. `type = "l"`) of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
activity_average <- activity %>%
                    filter(!is.na(steps)) %>%
                    group_by(interval) %>%
                    summarise(steps = mean(steps))

ggplot(activity_average, aes(interval, steps)) + geom_line()
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png) 

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
activity_average[which.max(activity_average$steps), ]$interval
```

```
## [1] 835
```

## Imputing missing values

Note that there are a number of days/intervals where there are missing
values (coded as `NA`). The presence of missing days may introduce
bias into some calculations or summaries of the data.

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with `NA`s)


```r
sum(is.na(activity$steps))
```

```
## [1] 2304
```

2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

- NAs will be filled with the mean for the respective 5 minute interval 

3. Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
activity_NAs <- activity[is.na(activity$steps), ] 
activity_NAs <- left_join(activity_NAs, activity_average, by ="interval")

activity_NAs_filled <- activity
activity_NAs_filled[is.na(activity_NAs_filled$steps), 1] <- activity_NAs[ , 4]
```

4. Make a histogram of the total number of steps taken each day and Calculate and report the **mean** and **median** total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
activity_daily_NAs_filled <- activity_NAs_filled %>%
                               group_by(date) %>%
                               summarise(steps = sum(steps))

ggplot(activity_daily_NAs_filled, aes(steps)) + geom_histogram(binwidth = 5000)
```

![](PA1_template_files/figure-html/unnamed-chunk-9-1.png) 

```r
mean(activity_daily_NAs_filled$steps)
```

```
## [1] 10766.19
```

```r
median(activity_daily_NAs_filled$steps)
```

```
## [1] 10766.19
```

- The mean was not changed by filling the NA values.  
- The median increased slightly by filling the NA values.

## Are there differences in activity patterns between weekdays and weekends?

For this part the `weekdays()` function may be of some help here. Use
the dataset with the filled-in missing values for this part.

1. Create a new factor variable in the dataset with two levels -- "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.


```r
activity_NAs_filled$weekday <-   ifelse(as.POSIXlt(activity_NAs_filled$date)$wday%%6 == 0, "weekend", "weekday")
activity_NAs_filled$weekday <- factor(activity_NAs_filled$weekday, levels = c("weekday", "weekend"))
```

1. Make a panel plot containing a time series plot (i.e. `type = "l"`) of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). 


```r
activity_average_NAs_filled <- activity_NAs_filled %>%
                                group_by(weekday, interval) %>%
                                summarise(steps = mean(steps))

ggplot(activity_average_NAs_filled, aes(interval, steps)) + 
  geom_line() + 
  facet_grid(weekday~.)
```

![](PA1_template_files/figure-html/unnamed-chunk-11-1.png) 
