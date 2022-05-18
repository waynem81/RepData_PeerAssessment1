
### Loading and preprocessing the data
activity <- read.csv("activity/activity.csv")

head(activity)
dim(activity)[1]
names(activity)

sum(!is.na(activity$steps))

library(lubridate)
activity$date <- ymd(activity$date)

unique(activity$date)

head(activity$date)

### What is mean total number of steps taken per day?
stepsByDay<-aggregate(steps~date, activity, sum)

png("stepsByDay.png")
hist(stepsByDay$steps, xlab="Steps", ylab="ays", main="Steps By Day")
dev.off()

myMean<-mean(stepsByDay$steps)
myMean
#mean is 10766.19

myMedian<-median(stepsByDay$steps)
myMedian 
#median is 10765

### What is the average daily activity pattern?
avgIntervals<-aggregate(steps~interval, activity, mean)

png("avgIntervals.png")
with(avgIntervals, plot(interval, steps, type = "l"))
dev.off()
