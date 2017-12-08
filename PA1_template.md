# PA1_template
Azmanvai  
December 8, 2017  




###Loading and preprocessing the data

Show any code that is needed to

1. Load the data (i.e. read.csv())
2. Process/transform the data (if necessary) into a format suitable for your analysis


```r
activity <- read.csv("activity.csv")
activity$date <- as.Date(activity$date, "%Y-%m-%d")
```


###What is mean total number of steps taken per day?

For this part of the assignment, you can ignore the missing values in the dataset

1. Calculate the total number of steps taken per day

```r
stepday <- aggregate(steps ~ date, data = activity, sum, na.rm = TRUE)
```

2. Make a histogram of the total number of steps taken each day

```r
hist(stepday$steps, freq = TRUE, xlab = "Steps per day", main = "Histogram of Steps per day")
```

![](PA1_template_files/figure-html/histogram-1.png)<!-- -->

3. Calculate and report the mean and median of the total number of steps taken per day

```r
average <- mean(stepday$steps)
med <- median(stepday$steps)
print(average)
```

```
## [1] 10766.19
```

```r
print(med)
```

```
## [1] 10765
```


###What is the average daily activity pattern?

1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
avgstepint <- aggregate(steps ~ interval, data = activity, mean, na.rm = TRUE)
with(avgstepint, plot(interval, steps, type = "l", xlab = "Interval", ylab = "Average steps", main = "Average Steps per Interval"))
```

![](PA1_template_files/figure-html/timeseries-1.png)<!-- -->

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
maxstep <- with(avgstepint, interval[steps == max(steps)])
print(maxstep)
```

```
## [1] 835
```


###Imputing missing values

Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
missing <- sum(is.na(activity$steps))
print(missing)
```

```
## [1] 2304
```

2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc. Create a new dataset that is equal to the original dataset but with the missing data filled in


```r
##use mean for the 5-minute interval
newactivity <- activity
for(i in 1:17568){
          if (is.na(newactivity$steps[i])){
                    int <- newactivity$interval[i]
                    newactivity$steps[i] <- avgstepint$steps[avgstepint$interval == int]
          }
}
sum(is.na(newactivity$steps))
```

```
## [1] 0
```

```r
str(newactivity)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : num  1.717 0.3396 0.1321 0.1509 0.0755 ...
##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

3. Make a histogram of the total number of steps taken each day

```r
newstepday <- aggregate(steps ~ date, data = newactivity, sum, na.rm = TRUE)
hist(newstepday$steps, freq = TRUE, xlab = "New Steps per day", main = "Histogram of New Steps per day")
```

![](PA1_template_files/figure-html/newstepday-1.png)<!-- -->

- Calculate and report the mean and median total number of steps taken per day

```r
newaverage <- mean(stepday$steps)
newmed <- median(stepday$steps)
print(newaverage)
```

```
## [1] 10766.19
```

```r
print(newmed)
```

```
## [1] 10765
```

- Do these values differ from the estimates from the first part of the assignment? 

```r
print(average != newaverage)
```

```
## [1] FALSE
```

```r
print(med != newmed)
```

```
## [1] FALSE
```

- What is the impact of imputing missing data on the estimates of the total daily number of steps?
          Average or median values may be similar or different depending on imputing method


###Are there differences in activity patterns between weekdays and weekends?

For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

1. Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day

```r
newactivity$day <- weekdays(newactivity$date, abbreviate = FALSE)
for(i in 1:17568){
          if(newactivity$day[i] == "Saturday"| newactivity$day[i] == "Sunday"){
                    newactivity$week[i] <- "weekend"
          } else {
                    newactivity$week[i] <- "weekday"
          }
          
}
newactivity$week <- as.factor(newactivity$week)
str(newactivity)
```

```
## 'data.frame':	17568 obs. of  5 variables:
##  $ steps   : num  1.717 0.3396 0.1321 0.1509 0.0755 ...
##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
##  $ day     : chr  "Monday" "Monday" "Monday" "Monday" ...
##  $ week    : Factor w/ 2 levels "weekday","weekend": 1 1 1 1 1 1 1 1 1 1 ...
```

2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data

```r
newactivitymean <- aggregate(steps ~ interval + week, data = newactivity, mean, na.rm = TRUE )
library(lattice)
xyplot(steps ~ interval | week, data = newactivitymean, type = "l", layout = c(1,2))
```

![](PA1_template_files/figure-html/plot-1.png)<!-- -->
