# Douglas Wygal
# Reproducible Research Project 1
# 2019-01-12

###############################################################################
## Loading and preprocessing the data
# 1.  Code for reading in the dataset and/or processing the data
#     activity.csv must reside in your working directory  
if(!exists("activity")){
  activity<-read.csv("activity.csv")
}

###############################################################################
## What is mean total number of steps taken per day?
# 
#     Checkout the dataset
head(activity)

# 1. Calculate the total number of steps taken per day
steps.day <- tapply(activity$steps, activity$date, sum)
# Load reshape2 library to get melt & dcast functions

# 2.  Make a histogram of the total number of steps taken each day
hist(steps.day, main = "Total Numer of Steps Taken Each Day",
     xlab = "Steps Count", ylab = "Frequency", 
     breaks = 10, col = "green")

#     Checkout the dataset
# View(steps.day)
# 3.  Calculate and report the mean and median of the total number of steps 
#     taken per day 
steps.day.mean <- mean(steps.day, na.rm = TRUE)
steps.day.median <- median(steps.day, na.rm = TRUE)

# abline(v = steps.day.mean, lty = 1, lwd = 2, col = "red")
# abline(v = steps.day.median, lty = 2, lwd = 2, col = "blue")
# legend(x = "topright", c("Mean", "Median"), col = c("red", "black"), 
#       lty = c(1, 2), lwd = c(2, 2))
###############################################################################

###############################################################################
## What is the average daily activity pattern?
# 1.  Make a time series plot (i.e. type = "l"type="l") of the 5-minute interval 
# (x-axis) and the average number of steps taken, averaged across all days 
# (y-axis)
steps.interval<-aggregate(steps~interval,data=activity,FUN=mean,
                                                            na.action=na.omit)
steps.interval$steps<-round(steps.interval$steps)

#     Checkout the dataset
# View(steps.interval)
plot(steps.interval, xlab = "Time Interval", ylab = "Number of Steps", 
main = "Average Daily Activity Pattern", type = "l")
# 2. Which 5-minute interval, on average across all the days in the dataset, 
# contains the maximum number of steps?
max.interval.value<-as.numeric(steps.interval$steps[which.max(
                                                        steps.interval$steps)])
max.interval<-steps.interval$interval[which.max(steps.interval$steps)]

###############################################################################
##  Inputting missing values
# 1.  Calculate and report the total number of missing values in the dataset 
# (i.e. the total number of rows with NAs)
na.count<-sum(is.na(activity$steps))
# 2.  Devise a strategy for filling in all of the missing values in the dataset. 
# calculate the mean of the steps by interval
# fill in the missing values with the mean for that interval

steps.filled<-ifelse(is.na(activity$steps), 
                        round(steps.interval$steps[match(activity$interval, 
                                  steps.interval$interval)],0), activity$steps)
# 3. Create a new dataframe with NAs removed
activity2<-data.frame(steps=steps.filled, interval=activity$interval, 
                                                            date=activity$date)
# 4. Make a histogram of the total number of steps taken each day
hist(steps.day, main = "Steps Taken Each Day with NAs Removed",
     xlab = "Step Count", ylab = "Frequency", 
     breaks = 10, col = "blue")
# Calculate the mean and median
steps.filled.day<-tapply(activity2$steps, activity$date, sum)
steps.day.mean <- mean(steps.filled.day, na.rm = TRUE)
steps.day.median <- median(steps.filled.day, na.rm = TRUE)

###############################################################################
## Are there differences in activity patterns between weekdays and weekends?
# 1.  Create a new factor variable in the dataset with two levels – “weekday” 
# and “weekend” indicating whether a given date is a weekday or weekend day.
activity2$FormattedDate <- as.Date(activity2$date, format = "%Y-%m-%d")
# Create daysOfWeek variable
daysOfWeek <- weekdays(activity2$FormattedDate)
# Create weekends variable
activity2$dayType<-ifelse(daysOfWeek %in% c("Saturday", "Sunday"), 
                                                          "Weekend", "Weekday")
#     Checkout the dataset
# View(activity2)
# add the plyr library
library(plyr)
# create the dayTypeDf
dayTypeDf <- ddply(activity2, .(interval, dayType), summarize, Avg = mean(steps))
# add the library lattice
library(lattice) 
# create the graph in lattice
xyplot(Avg~interval|dayType, data=dayTypeDf, type="l",  layout = c(1,2),
       main="Average Steps per Interval Based on Type of Day", 
       ylab="Average Number of Steps", xlab="Interval")

################################################################################