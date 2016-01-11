# Loading and processing data

## Set working directory
setwd("/home/jandas/R/courseraR/RepResearch/Project1/Data")

## Call libraries
library(dplyr)
library(ggplot2)


## Extract and transform data to keep it on correct formats
activity <- read.csv(unz("repdata-data-activity.zip", "activity.csv"), header=T, quote='"', colClasses=c(numeric(), character(), character()),row.names = NULL)
activity$date <- as.Date(activity$date)

## What is mean total number of steps taken per day?

## Calculate total number of steps taken everyday
activityDay = filter(activity, !is.na(steps)) # remove missing values
activityDay = group_by(activityDay, date)
activityDaySum<-summarize(activityDay, steps=sum(steps))

hist(activityDaySum$steps, col="skyblue", breaks=12, main= "Histogram Steps taken by day ", xlab="Steps")

mean(activityDaySum$steps)
median(activityDaySum$steps)

## What is the average daily activity pattern?

activityInterval = group_by(activityDay, interval)
activityIntervalMean<-summarize(activityInterval, mean=mean(steps))


plot(activityIntervalMean$interval, activityIntervalMean$mean, type="l", main= "Average daily activity ", xlab="Interval", ylab="Steps mean")


### Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

activityIntervalMean[activityIntervalMean$mean==max(activityIntervalMean$mean),]

## Imputing missing values

### Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

nrow(filter(activity, is.na(steps)))

##Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
Missing values are filled by the mean of each 5-minute interval.


##Create a new dataset that is equal to the original dataset but with the missing data filled in.

activityfilled<-left_join(activity,activityIntervalMean, by="interval")
activityfilled<-transform(activityfilled, steps=ifelse(is.na(steps), mean, steps))


activityfilledDay = group_by(activityfilled, date)
activityfilledDaySum<-summarize(activityfilledDay, steps=sum(steps))

##Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

hist(activityfilledDaySum$steps, col="skyblue", breaks=12, main= "Histogram Steps taken by day ", xlab="Steps")

#Mean
mean(activityfilledDaySum$steps)
#Median
median(activityfilledDaySum$steps)


## Are there differences in activity patterns between weekdays and weekends?


activityfilled$weektype<-factor(ifelse(as.POSIXlt(activityfilled$date)$wday<6, "weekday","weekend"))
activityfilledWeek = group_by(activityfilled, interval, weektype)
activityfilledWeekMean<-summarize(activityfilledWeek, steps=mean(steps))

g <-ggplot(activityfilledWeekMean, aes(interval, steps)) + geom_line(color="red")  +  facet_grid(weektype ~ .)+ labs(x= "Interval") + labs(y= "Mean Steps") + labs(title = "Steps taken by interval")
print(g)
