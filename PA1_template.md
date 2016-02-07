# Reproducible Research: Peer Assessment 1

##Loading the data


Load the data

```r
activity=read.csv("activity.csv")
```
Process/transform the data (if necessary) into a format suitable for your analysis

```r
total_steps<-aggregate(steps~date,data=activity,sum,na.rm=TRUE)
```

##What is mean total number of steps taken per day?

Make a histogram of the total number of steps taken each day

```r
hist(total_steps$steps, main = "Total Daily Steps", xlab = "Steps Taken")
```

![](sample_files/figure-html/unnamed-chunk-3-1.png)

Calculate and report the **mean** and **median** total number of steps taken 
per day 


```r
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
The **mean** total number of steps taken per day is 
    1.0766189\times 10^{4} steps.
The **median** total number of steps taken per day is 
    10765 steps.
    
##What is the average daily activity pattern?


Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
steps_interval<-aggregate(steps~interval,data=activity,mean,na.rm=TRUE)
plot(steps~interval,data=steps_interval,type="l")
```

![](sample_files/figure-html/unnamed-chunk-5-1.png)

Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps? 

```r
steps_interval[which.max(steps_interval$steps),]$interval
```

```
## [1] 835
```

It is the **835** interval.

##Imputing missing values


Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
sum(is.na(activity$steps))
```

```
## [1] 2304
```
Total 2304 rows are missing.

Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.


```r
#I filled in the missing values with the mean for that 5-minute interval.
interval_steps<-function(interval){
    steps_interval[steps_interval$interval==interval,]$steps
}
```

Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
# Make a new dataset with the original data
activity_filled<-activity
# Count of missing data filled in
count=0
for(i in 1:nrow(activity_filled)){
    if(is.na(activity_filled[i,]$steps)){
        activity_filled[i,]$steps<-interval_steps(activity_filled[i,]$interval)
        count=count+1
    }
}
cat("Total of NA values replaced:",count,"\r")  
```

```
## Total of NA values replaced: 2304 
```

Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. 

```r
total_steps2<-aggregate(steps~date,data=activity_filled,sum)
hist(total_steps2$steps, main = "Imputed Total Steps", xlab = "Total Steps")
```

![](sample_files/figure-html/unnamed-chunk-10-1.png)

```r
mean(total_steps2$steps)
```

```
## [1] 10766.19
```

```r
median(total_steps2$steps)
```

```
## [1] 10766.19
```
The **mean** total number of steps taken per day is 
1.0766189\times 10^{4} steps.
The **median** total number of steps taken per day is 
1.0766189\times 10^{4} steps.

Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

: Because we used the mean value for each 5-minute interval, the mean computation did not change with the inclusion of the imputed values.  The median value shows some variance, perhaps due to the location of the previously missing values.

##Are there differences in activity patterns between weekdays and weekends?


Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

```r
activity_filled$day=ifelse(as.POSIXlt(as.Date(activity_filled$date))$wday%%6==0,
                          "weekend","weekday")
# For Sunday and Saturday : weekend, Other days : weekday 
activity_filled$day=factor(activity_filled$day,levels=c("weekday","weekend"))
```


Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). The plot should look something like the following, which was creating using simulated data:

```r
steps_interval2=aggregate(steps~interval+day,activity_filled,mean)
library(lattice)
xyplot(steps~interval|factor(day),data=steps_interval2,aspect=1/2,type="l")
```

![](sample_files/figure-html/unnamed-chunk-12-1.png)
