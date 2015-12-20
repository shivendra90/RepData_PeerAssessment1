---
title: "Programming Assignment 1"
author: "Shivendra Sharma"
date: "21 December 2015"
output: html_document
---
This assignment is intended to perform or extract certain measures through data collected by fitness enthusiasts who might want to observe their fitness behaviour and patterns.  The data here has been downloaded from the course website itself and we'll be exploring some statistics associated with it.

Firstly, load the relevant packages, namely dplyr, ggplot2 and knitr.


```r
library(dplyr)
library(ggplot2)
library(knitr)
```
Dowload the dataset and store it.

```r
url <- 'https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip'
download.file(url, destfile = 'activity.zip', method = 'curl')
```

Read and summarise the dataset to answer questions related to the first part. Necessary changes pertaining to changing the class of the concerned columns is also important.

```r
activity <- read.csv('activity.csv', sep = ',', header = TRUE)
class(activity$steps)
```

```
## [1] "integer"
```

```r
activity$steps <-as.numeric(activity$steps)
class(activity$interval)
```

```
## [1] "integer"
```

```r
activity$interval <- as.numeric(activity$interval)
activity$date <- as.Date(activity$date, format = '%Y-%m-%d')
activity <- mutate(activity, day = weekdays(activity$date))
```

Calculate total number of steps as instructed in the first part

```r
sum(activity$steps, na.rm = TRUE)
```

```
## [1] 570608
```

Calculate mean and median

```r
mean(activity$steps, na.rm = TRUE)
```

```
## [1] 37.3826
```

```r
median(activity$steps, na.rm = TRUE)
```

```
## [1] 0
```

Time for the next section that requires plotting a bar diagram of total steps taken each day. We do this by forming a grouped data frame and subsequently making a simple bar diagram.

```r
grouped <- activity %>% group_by(date) %>% summarise(steps = sum(steps))
```
# Plot a simple bar diagram with a frequency polygon

```r
ggplot(grouped, aes(date, steps)) +geom_bar(stat = 'identity') + geom_freqpoly(stat = 'identity')
```

```
## Warning: Removed 8 rows containing missing values (position_stack).
```

```
## Warning: Removed 2 rows containing missing values (geom_path).
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7-1.png) 

In this section, we plot a time series of number of average steps taken each day, grouped according to each interval. As we observe, the interval around 580 posesses the maximum number of steps (found out by simple summary() function on the dataset)

```r
group.intervals <- activity %>% group_by(interval) %>% summarise(steps = mean(steps))
ggplot(group.intervals, aes(x = interval, y = steps)) + geom_freqpoly(stat = 'identity', colour = 'red')
```

```
## Error in seq.default(from = best$lmin, to = best$lmax, by = best$lstep): 'from' must be of length 1
```

Reporting NA values is rather a simple process but I was confused on replacing the NA values. Since the median found was zero, I replaced the NAs with 0 and made a simple histogram. The differences in values seems to be there.

```r
sum(is.na(activity))
```

```
## [1] 2304
```

```r
# Replace the NAs with some values. Assuming the median to be 0, replace the NAs with zero
activity[is.na(activity)] <- 0

# Create the new dataset. Simply copy-paste to replicate exact data with NAs filled up
revised <- activity
revised$steps <- as.numeric(revised$steps)

# Create grouped data
group.revised <- revised %>% group_by(date) %>% summarise(steps = sum(steps))

# Plot a similar histogram with the revised dataset
ggplot(group.revised, aes(date,steps)) + geom_bar(stat = 'identity') + geom_freqpoly(stat = 'identity')
```

![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9-1.png) 

```r
summary(group.revised)
```

```
##       date                steps      
##  Min.   :2012-10-01   Min.   :    0  
##  1st Qu.:2012-10-16   1st Qu.: 6778  
##  Median :2012-10-31   Median :10395  
##  Mean   :2012-10-31   Mean   : 9354  
##  3rd Qu.:2012-11-15   3rd Qu.:12811  
##  Max.   :2012-11-30   Max.   :21194
```

This ection is for the last part. Insert a 'weekdays' column in the activity dataset and make plots according to each day. A simialr pattern for all days is observed.

```r
class(activity$steps)
```

```
## [1] "numeric"
```

```r
activity$steps <- as.numeric(activity$steps)
day.data <- activity %>% group_by(date) %>% summarise(avg.steps = mean(steps))
day.data <- mutate(day.data, weekday = weekdays(day.data$date))
ggplot(activity, aes(x = interval , y = steps)) + geom_freqpoly(stat = 'identity', colour = 'red') + facet_grid(.~day)
```

![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-10-1.png) 

End.

Thank you for reading through.
