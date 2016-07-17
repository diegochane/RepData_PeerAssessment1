# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data
First the script downloads the file, extracts, and reads the data into R.

```r
url="https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
download.file(url,destfile = "Data.zip", method = "curl")
unzip("Data.zip")
raw.data=read.csv("activity.csv", stringsAsFactors = F)
raw.data$date=as.Date(raw.data$date,format="%Y-%m-%d")
```

## What is mean total number of steps taken per day?

Now that we have the data in R, the raw data is aggregated in order to get the total steps per day. Later the histogram is built.

```r
total.steps=raw.data[complete.cases(raw.data),]
total.steps=aggregate(total.steps$steps, by=list(total.steps$date),"sum")
names(total.steps)=c("Date", "Steps")
total.steps
```

```
##          Date Steps
## 1  2012-10-02   126
## 2  2012-10-03 11352
## 3  2012-10-04 12116
## 4  2012-10-05 13294
## 5  2012-10-06 15420
## 6  2012-10-07 11015
## 7  2012-10-09 12811
## 8  2012-10-10  9900
## 9  2012-10-11 10304
## 10 2012-10-12 17382
## 11 2012-10-13 12426
## 12 2012-10-14 15098
## 13 2012-10-15 10139
## 14 2012-10-16 15084
## 15 2012-10-17 13452
## 16 2012-10-18 10056
## 17 2012-10-19 11829
## 18 2012-10-20 10395
## 19 2012-10-21  8821
## 20 2012-10-22 13460
## 21 2012-10-23  8918
## 22 2012-10-24  8355
## 23 2012-10-25  2492
## 24 2012-10-26  6778
## 25 2012-10-27 10119
## 26 2012-10-28 11458
## 27 2012-10-29  5018
## 28 2012-10-30  9819
## 29 2012-10-31 15414
## 30 2012-11-02 10600
## 31 2012-11-03 10571
## 32 2012-11-05 10439
## 33 2012-11-06  8334
## 34 2012-11-07 12883
## 35 2012-11-08  3219
## 36 2012-11-11 12608
## 37 2012-11-12 10765
## 38 2012-11-13  7336
## 39 2012-11-15    41
## 40 2012-11-16  5441
## 41 2012-11-17 14339
## 42 2012-11-18 15110
## 43 2012-11-19  8841
## 44 2012-11-20  4472
## 45 2012-11-21 12787
## 46 2012-11-22 20427
## 47 2012-11-23 21194
## 48 2012-11-24 14478
## 49 2012-11-25 11834
## 50 2012-11-26 11162
## 51 2012-11-27 13646
## 52 2012-11-28 10183
## 53 2012-11-29  7047
```

```r
library(ggplot2)
```

```
## Warning: package 'ggplot2' was built under R version 3.2.4
```

```r
ggplot(total.steps,aes(x=Steps))+
    geom_histogram(fill="black", binwidth = 1000)+labs(title="Steps per day",                                        x="Steps",y="Frequency")
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)

Once we have the historgam, the mean and the median are calculated.


```r
mean.steps=mean(total.steps$Steps,na.rm=T)
mean.steps
```

```
## [1] 10766.19
```

```r
median.steps=median(total.steps$Steps, na.rm=T)
median.steps
```

```
## [1] 10765
```
## What is the average daily activity pattern?

Now the steps data must be aggregated into the time intervals using the mean of the steps.Later the time series is built.


```r
daily.pat=raw.data[complete.cases(raw.data),]
daily.pat=aggregate(daily.pat$steps, by=list(daily.pat$interval),"mean")
names(daily.pat)=c("Interval", "Steps")
ggplot(daily.pat,aes(x=Interval, y=Steps))+geom_line(col="red")+
    labs(title="Steps per interval", x="Time Interval", y="Avg. Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)

After this, the interval with the maximum number of steps is calulcated.


```r
max.interval=daily.pat$Interval[which.max(daily.pat$Steps)]
max.interval
```

```
## [1] 835
```

## Imputing missing values

First we need to find out how many rows have missing values.


```r
miss.val=sum(is.na(raw.data$steps))
miss.val
```

```
## [1] 2304
```

As we can see, there is a considerable number of missing values. In order to fill them the average steps taken for that interval will be used. Also a column will be added with the purpose of tagging the values that were added with this policy.


```r
clean.data=raw.data
clean.data=transform(clean.data,steps=ifelse(is.na(clean.data$steps),
    daily.pat$Steps[match(clean.data$interval,daily.pat$Interval)],clean.data$steps))
sum(is.na(clean.data))
```

```
## [1] 0
```

```r
clean.data$Type="Original"
clean.data$Type[is.na(raw.data$steps)]="Added"
```

Now that we have the new dataset the histogram of steps is recalculated.


```r
added.data=clean.data[clean.data$Type=="Added",]
total.steps2=aggregate(added.data$steps, by=list(added.data$date),"sum")
names(total.steps2)=c("Date", "Steps")
total.steps$Type="Original"
total.steps2$Type="Added"
total.steps.both=rbind.data.frame(total.steps,total.steps2)
total.steps.both$Type=factor(total.steps.both$Type, levels=c("Original","Added"))
ggplot(total.steps.both,aes(x=Steps,col=Type, fill=Type, order=rev(Type)))+
    geom_histogram(binwidth = 1000)+labs(title="Steps per day",x="Steps", y="Frequency")
```

![](PA1_template_files/figure-html/unnamed-chunk-8-1.png)

Also the new mean, and the new median is calculated.


```r
mean.steps2=mean(total.steps.both$Steps)
mean.steps2
```

```
## [1] 10766.19
```

```r
median.steps2=median(total.steps.both$Steps)
median.steps2 
```

```
## [1] 10766.19
```

## Are there differences in activity patterns between weekdays and weekends?

In order to make this analysis, we need to know the day each date represents. This script was made with such purpose.


```r
clean.data$weekdays="Weekday"
logical=weekdays(clean.data$date)=="Saturday"|weekdays(clean.data$date)=="Sunday"
clean.data$weekdays[logical]="Weekend"
clean.data$weekdays=factor(clean.data$weekdays)
```

Once we know the type of day each observation was made we can aggregate the datasets and create the histogram.


```r
day.data=clean.data[clean.data$weekdays=="Weekday",]
end.data=clean.data[clean.data$weekdays=="Weekend",]
day.pat=aggregate(day.data$steps, by=list(day.data$interval),"mean")
names(day.pat)=c("Interval", "Steps")
day.pat$Weekday="Weekday"
end.pat=aggregate(end.data$steps, by=list(end.data$interval),"mean")
names(end.pat)=c("Interval", "Steps")
end.pat$Weekday="Weekend"
day.pat=rbind(day.pat,end.pat)
ggplot(day.pat,aes(x=Interval,y=Steps,col=Weekday))+geom_line()+
    facet_grid(Weekday~.)+
    labs(title="Steps per interval", x="Time Interval", y="Avg. Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-11-1.png)

As we can see, on average users walk mode during weekdays than weekends. Also, people start moving earlier in the day on weekdays compared to weekends.
