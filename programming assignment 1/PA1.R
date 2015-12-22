Library(dplyr)
library(ggplot2)

# Dowload the dataset
url <- 'https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip'
download.file(url, destfile = 'activity.zip', method = 'curl')

# Summarise
activity <- read.csv('activity.csv', sep = ',', header = TRUE)
View(activity)
class(activity$steps)
activity$steps <-as.numeric(activity$steps)
class(activity$interval)
activity$interval <- as.numeric(activity$interval)
activity$date <- as.Date(activity$date, format = '%Y-%m-%d')

# Calculate total number of steps
sum(activity$steps, na.rm = TRUE)

# calculate mean and median
mean(activity$steps, na.rm = TRUE)
median(activity$steps, na.rm = TRUE)

# Form a data frame with steps acording to each date
grouped <- activity %>% group_by(date) %>% summarise(steps = sum(steps))

# Plot a simple bar diagram with a frequency polygon
png('plot1.png', 800,600)
ggplot(grouped, aes(date, steps)) +geom_bar(stat = 'identity') + geom_freqpoly(stat = 'identity', colour = 'red')

# Plot timeseries of average steps accoridng to each interval
byInter = group_by(activity, interval)
inter.final <- summarise(byInter, steps = sum(steps, na.rm = T), mean = mean(steps, na.rm = T), median = median(steps, na.rm = T))
inter.final
png('plot2.png', 800,600)
ggplot(inter.final, aes(x = interval, y = mean)) + geom_freqpoly(stat = 'identity', colour = 'red') + labs(x = 'Interval', y = 'Avg. steps')

# Reporting NA values
sum(is.na(activity))

# Replace the NAs with some values. Assuming the median to be 0, replace the NAs with zero
activity[is.na(activity)] <- 0

# Create the new dataset. Simply copy-paste to replicate exact data with NAs filled up
revised <- activity
revised$steps <- as.numeric(revised$steps)

# Create grouped data
group.revised <- revised %>% group_by(date) %>% summarise(steps = sum(steps))

# Plot a similar histogram with the revised dataset
png('plot3.png', 800,600)
ggplot(group.revised, aes(date,steps)) + geom_bar(stat = 'identity') + geom_freqpoly(stat = 'identity', colour = 'red')

# Insert a 'weekdays' column in the activity dataset and make a plot according to dates
class(activity$steps)
activity$steps <- as.numeric(activity$steps)
day.data <- activity %>% group_by(date) %>% summarise(avg.steps = mean(steps))
day.data <- mutate(day.data, weekday = weekdays(day.data$date))
png('plot4.png', 800,600)
ggplot(activity, aes(x = interval , y = steps)) + geom_freqpoly(stat = 'identity', colour = 'red') + facet_grid(.~day)
