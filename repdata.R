
##Load the data (i.e. read.csv())

setwd("C:/DataScience/repdata-010/p1/RepData_PeerAssessment1")
fileURL <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
download.file(fileURL, destfile="activityDATA.zip", quiet=TRUE)
unzip("activityDATA.zip")
activityCSV<-read.csv("activity.csv")

## Process/transform the data (if necessary) into a format suitable for your analysis

library(data.table)
activityDT<-data.table(activityCSV)

## Make a histogram of the total number of steps taken each day

stepsSUM<-aggregate(steps ~ date, data=activityDT, sum)
hist(stepsSUM$steps, breaks=20, col="#7851a9", border="black", main="Histogram of Steps Per Day", xlab="Steps Per Day", ylab="Frequency")
## royal blue 4169E1, cerulean 2a52be, royal purple 7851a9

## Calculate and report the mean and median total number of steps taken per day

stepsMEAN<-mean(stepsSUM$steps)
stepsMEDIAN<-median(stepsSUM$steps)

## Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

stepsINTERVAL<-aggregate(steps ~ interval, data=activityDT, FUN=mean)
plot(stepsINTERVAL, type="l", col="#2a52be")

## Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

stepsORDER <- stepsINTERVAL[order(stepsINTERVAL$steps, decreasing=TRUE), ]
stepsORDER[1,]

## Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

missingSUM<-sum(is.na(activityDT))
missingSUM

## Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

stepsVECTOR<-activityDT$steps
strategyNA<-mean(stepsVECTOR, na.rm=TRUE)

## Create a new dataset that is equal to the original dataset but with the missing data filled in.

new_activityDT <- activityDT
new_activityDT$steps[is.na(new_activityDT$steps)] <- mean(new_activityDT$steps, na.rm = T)

## Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

new_stepsSUM<-aggregate(steps ~ date, data=new_activityDT, sum)
hist(new_stepsSUM$steps, breaks=20, col="#4169E1", border="black", main="Histogram of Steps Per Day", xlab="Steps Per Day", ylab="Frequency")
new_stepsMEAN<-mean(new_stepsSUM$steps)
new_stepsMEDIAN<-median(new_stepsSUM$steps)

## Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

new_activityDT$day<-weekdays(as.Date(new_activityDT$date))
new_activityDT$day_type<-ifelse(new_activityDT$day=="Saturday" | new_activityDT$day=="Sunday", "Weekend", "Weekday")

## Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.

daystepsMEAN<-aggregate(new_activityDT$steps, by=list(new_activityDT$interval, new_activityDT$day_type), mean)
names(daystepsMEAN)<-c("interval", "day_type", "steps")
library(lattice) 
xyplot(steps ~ interval | day_type, daystepsMEAN, type="l", layout=c(1,2), xlab="Interval", ylab="Number of steps")

knit2html("PA1_template.Rmd")
