opts_chunk$set(echo = TRUE, fig.height = 4)
#1Loading and preprocessing the data
activity<- read.csv("activity.csv",header = TRUE)
activity$date<-as.Date(activity$date)
par(mfrow=c(1,1))

#2What is mean total number of steps taken per day?
actdate<-split(activity,activity$date)
actdatemean<-sapply(actdate, function(x) sum(x$steps,na.rm = TRUE))
summary(actdatemean)
hist(actdatemean,main = "total steps taken each day",xlab = "steps each day")

#3What is the average daily activity pattern?
actint<-split(activity,activity$interval)
actintmean<-sapply(actint, function(x) mean(x$steps,na.rm = TRUE))
interval<-names(actint)
plot(actintmean~interval,type="l",main="average daily activity pattern")
maxsteps<-interval[which.max(actintmean)]
maxsteps

#4Imputing missing values
#getting the total number of  missing value
missvalue<- sum(is.na(activity$steps))

#fill the missing data
activity1<-activity
#figure out where is the na,turns out ,there are somedays have no data all
#so I just filling the missing value with the mean fo that 5-minute interval
nas<-sapply(actdate,function(x) sum(is.na(x$steps)))
for (i in 1:17568) {
    if (is.na(activity$steps[i])){
    activity1$steps[i]<- actintmean[which(as.integer(interval)==activity$interval[i])]
    }
}
actdate1<-split(activity1,activity1$date)
actdatemean1<-sapply(actdate1, function(x) sum(x$steps,na.rm = TRUE))
summary(actdatemean1)
par(mfrow=c(2,1))
hist(actdatemean,main = "total steps taken each day",xlab = "steps each day")
hist(actdatemean1,main = "total steps taken each day without missingvalue",xlab = "steps each day")

#it differ from the estimates from the first part of the assignment.After that ,
##the midlevel value grow up

#5Are there differences in activity patterns between weekdays and weekends?
par(mfrow=c(2,1))
activity1$weekday<- weekdays(activity1$date)
activity1$weekday<- with(activity1, sapply(weekday,function(x) if (sum(x== "ÐÇÆÚÁù",x== "ÐÇÆÚÈÕ")==1) {x <- "weekend"} else {x<- "weekday"}))
actweek<-split(activity1,activity1$weekday)
actweekday<-split(actweek$weekday,actweek$weekday$interval)
actweekend<-split(actweek$weekend,actweek$weekend$interval)
actweekdaymean<-sapply(actweekday, function(x) mean(x$steps,na.rm = TRUE))
intervalweekday<-names(actweekday)
plot(actweekdaymean~intervalweekday,type="l",main="average daily activity pattern weekday",xlab = "")
actweekendmean<-sapply(actweekend, function(x) mean(x$steps,na.rm = TRUE))
intervalweekend<-names(actweekend)
plot(actweekendmean~intervalweekend,type="l",main="average daily activity pattern weekend",xlab = "")
