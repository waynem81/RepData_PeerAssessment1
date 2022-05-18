---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---

echo = TRUE

## Loading and preprocessing the data
activity <- read.csv("activity/activity.csv")

head(activity)
dim(activity)[1]
names(activity)

head(activity$date)


## What is mean total number of steps taken per day?
## 1. Calculate the total number of steps taken per day
stepsByDay<-aggregate(steps~date, activity, sum)

## 2. If you do not understand the difference between a histogram and a barplot, 
##    research the difference between them. Make a histogram of the total number
##    of steps taken each day
hist(stepsByDay$steps, xlab="Steps", ylab="ays", main="Steps By Day")

png("stepsByDay.png")
hist(stepsByDay$steps, xlab="Steps", ylab="ays", main="Steps By Day")
dev.off()

## 3. Calculate and report the mean and median of the total number of steps taken 
##    per day
myMean<-mean(stepsByDay$steps)
myMean
#mean is 10766.19

myMedian<-median(stepsByDay$steps)
myMedian 
#median is 10765


## What is the average daily activity pattern?
## 1. Make a time series plot (i.e. type = "l") of the 5-minute 
##    interval (x-axis) and the average number of steps taken, 
##    averaged across all days (y-axis)
avgIntervals<-aggregate(steps~interval, activity, mean)

with(avgIntervals, plot(interval, steps, type = "l"))
png("avgIntervals.png")
with(avgIntervals, plot(interval, steps, type = "l"))
dev.off()

## 2. Which 5-minute interval, on average across all the days 
##    in the dataset, contains the maximum number of steps?
avgIntervals[which.max(avgIntervals[,2]),1]
## 835


## Imputing missing values
## 1. Calculate and report the total number of missing values in the 
##    dataset (i.e. the total number of rows with NAs)
missingValues <- is.na(activity[,1])
sum(missingValues)
## 2304 missing values

## 2. Devise a strategy for filling in all of the missing values in 
##    the dataset. The strategy does not need to be sophisticated. 
##    For example, you could use the mean/median for that day, 
##    or the mean for that 5-minute interval, etc.
meanByDay <- mean(avgIntervals$steps)
meanByDay

## 3. Create a new dataset that is equal to the original dataset but 
##    with the missing data filled in.
activityNew<-activity
activityNew[missingValues,1]<-meanByDay
head(activityNew)

## 4. Make a histogram of the total number of steps taken each day 
##    and Calculate and report the mean and median total number of 
##    steps taken per day. Do these values differ from the estimates
##    from the first part of the assignment? What is the impact of 
##    imputing missing data on the estimates of the total daily number
##    of steps?
stepsByDayNew<-aggregate(steps~date, activityNew, sum)

hist(stepsByDayNew$steps, xlab="Steps", ylab="Days", main="Steps Per Day Using Mean By Day For Missing Values")
png("stepsByDayNew.png")
hist(stepsByDayNew$steps, xlab="Steps", ylab="Days", main="Steps Per Day Using Mean By Day For Missing Values")
dev.off()

newMean<-mean(stepsByDayNew$steps)
newMean
##new mean is 10766.19

newMedian<-median(stepsByDayNew$steps)
myMedian
newMedian 
##original median is 10765
##new median is 10766.19

## mean stays the same, but median changes


## Are there differences in activity patterns between weekdays and weekends?
## 1. Create a new factor variable in the dataset with two 
##    levels – “weekday” and “weekend” indicating whether a 
##    given date is a weekday or weekend day.
activityNew$date <- as.Date(activityNew$date)
library(dplyr)

activityNewer <- activityNew%>%
        mutate(dayFlag= ifelse(weekdays(activityNew$date)=="Saturday" | weekdays(activityNew$date)=="Sunday", "Weekend", "Weekday"))
head(activityNewer)


## 2. Make a panel plot containing a time series plot (i.e. type = "l") 
##    of the 5-minute interval (x-axis) and the average number of steps 
##    taken, averaged across all weekday days or weekend days (y-axis).
##    See the README file in the GitHub repository to see an example of 
##    what this plot should look like using simulated data.
avgStepsByDayFlag <- activityNewer %>%
  group_by(dayFlag, interval) %>%
  summarize(avgStepsByDay=sum(steps))

head(avgStepsByDayFlag)

library(lattice)


with(avgStepsByDayFlag, 
      xyplot(avgStepsByDay ~ interval | dayFlag, 
      type = "l",      
      main = "Steps By Day Flag and Interval",
      xlab = "Days",
      ylab = "Average Steps"))

png("avgStepsByDayFlag")
with(avgStepsByDayFlag, 
      xyplot(avgStepsByDay ~ interval | dayFlag, 
      type = "l",      
      main = "Steps By Day Flag and Interval",
      xlab = "Days",
      ylab = "Average Steps"))
dev.off()
