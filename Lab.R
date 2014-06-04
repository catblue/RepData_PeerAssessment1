# Loading and preprocessing the data
# The variables included in this dataset are:
# 1. steps: Number of steps taking in a 5-minute interval (missing values are coded as NA)
# 2. date: The date on which the measurement was taken in YYYY-MM-DD format
# 3. interval: Identifier for the 5-minute interval in which measurement was taken
# there are a total of 17,568 observations in this dataset.

if(!file.exists("./data/activity.csv")){
  if(!file.exists("./data")){dir.create("./data")}
  fileUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip?accessType=DOWNLOAD"
  download.file(fileUrl, destfile="./data/Factivity.zip", method="curl")
  unzip("./data/Factivity.zip",exdir="./data")
}

dt <- read.csv("./data/activity.csv", header = TRUE)
stps <- tapply(dt$steps,dt$date,sum)
# What is mean total number of steps taken per day?
# 1. Make a histogram of the total number of steps taken each day
# 2. Calculate and report the mean and median total number of steps taken per day
#hist(dt$steps)
#table(dt$date)

#mean(dt$steps,na.rm=T)

#table(tapply(dt$steps,INDEX=dt$date))

#hist(as.vector(lapply(unique(dt$date), function(x) sum(dt$steps[dt$date == x],na.rm=T)),mode="numeric"),breaks=15,col="red",xlab="Steps", main="Total number of steps taken per day")
#hist(sapply(unique(dt$date), function(x) sum(dt$steps[dt$date == x],na.rm=T)),breaks=15,col="red",xlab="Steps", main="Total number of steps taken per day")


hist(stps,col="red",xlab="Steps per day", main="Total number of steps taken each day")
abline(v=median(stps,na.rm=T),col="green", lwd=2)
legend("topright", col="green", lwd=2, title="NAs included", legend="mediane",bty="n")
rug(stps)

summary(stps)
#summary(tapply(dt$steps,dt$date,sum,na.rm=T))

# Now find the mean and median, ignoring NAs
summarise(totalSteps, meanSteps = mean(totalSteps, na.rm = TRUE), medianSteps = median(totalSteps, 
                                                                                       na.rm = TRUE))

median(stps, na.rm=T)

round(mean(stps, na.rm=T),digits=5)


stps <- tapply(dt$steps,dt$date,sum,na.rm=T)
hist(stps,col="red",xlab="Steps per day", main="Total number of steps taken each day")
rug(stps)
abline(v=median(stps),col="green", lwd=2)
legend("topright", col="green", lwd=2, title="NAs dropped", legend="mediane",bty="n")



#What is the average daily activity pattern?
# 1. Make a time series plot (i.e. type = "l") of the 5min interval (x-axis) 
# and the average number of steps taken, averaged across all days (y-axis)
# 2. Which 5min interval, on average across all the days in the dataset, contains the max no of steps?

##dt$fdate <- as.Date(dt$date)
#dtw <- split(dt,as.factor(weekdays(dt$fdate)))



par(mfrow = c(1, 1), mar = c(4, 4, 2, 1), oma = c(2, 0, 2, 0))
pattrns <- tapply(dt$steps,dt$interval,mean, na.rm=T)
plot(as.numeric(names(pattrns)),pats,col="red",type="l",ylab="avg. steps/5min",xlab="time (hhmm)")
title("Average daily activity pattern")

maxActivityTime <- as.integer(names(which.max(pattrns)))
sprintf("%d:%d",maxActivityTime%/%100,maxActivityTime%%100)
abline(v=maxActivityTime,col="green",lwd=2)
legend("topright", col="green", lwd=2, legend="max activity",bty="n")

tail(dt$date)

#Imputing missing values
# 1. Calculate and report the total number of rows with NAs
# 2. Devise a strategy for filling in all of the missing values in the dataset. 
# simple e.g. the mean/median for that day, or the mean for that 5-minute interval, etc.
# 3. Create a new dataset that is equal to the original dataset but with the missing data filled in.
# 4. a) Make a histogram of the total number of steps taken each day 
#    b) Calculate and report the mean and median total number of steps taken per day. 
#    c) Do these values differ from the estimates from the first part of the assignment?
#    d) What is the impact of imputing missing data on the estimates of the total daily number of steps?  

nulls <- is.na(dt$steps)
sum(is.na(dt$steps))
sum(is.na(dt$date))
sum(is.na(dt$interval))

dtNew <- dt
dtNew$steps <- ifelse(nulls, as.integer(pattrns[as.character(dt$interval)]),dt$steps )
sum(is.na(dtNew$steps))
sum(is.na(dt$steps))


stpsNew <- tapply(dtNew$steps,dtNew$date,sum)
hist(stps,col="red",xlab="Steps per day", main="Total number of steps taken each day")
rug(stps)
summary(dtNew$steps)


#Are there differences in activity patterns between weekdays and weekends?
# weekdays() function may be of some help here; Use the dataset with the filled-in missing values
# 1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” 
# 2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5min interval (x-axis) 
# and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). 


stps <- tapply(dt$steps,dt$date,sum,na.rm=T)
stpw <- tapply(stps,as.factor(weekdays(as.Date(names(stps)))), mean,na.rm=T)
barplot(c(stpw["Monday"],stpw["Tuesday"],stpw["Wednesday"],stpw["Thursday"],
          stpw["Friday"],stpw["Saturday"],stpw["Sunday"]))



dtNew$wday <- ifelse(weekdays(as.Date(dtNew$date))!="Sunday", "weekday","weekend")
tail(dtNew$wday)
dtNew$wday <-as.factor(dtNew$wday)




pattrnsW <- tapply(dtNew[dtNew$wday=="weekday",]$steps,dtNew[dtNew$wday=="weekday",]$interval,mean)
pattrnsS <- tapply(dtNew[dtNew$wday=="weekend",]$steps,dtNew[dtNew$wday=="weekend",]$interval,mean)

par(mfrow = c(2, 1), mar = c(0, 4, 0, 1), oma = c(2, 0, 2, 0))
plot((names(pattrnsW)),pattrnsW,col="red",type="l",lab=c(1,5,7),ylab="avg. steps/5min",xlab="")
legend("bottomleft", legend="weekday",bty="n")
plot(as.numeric(names(pattrnsS)),pattrnsS,col="red",type="l",lab=c(5,5,7),ylab="avg. steps/5min",xlab="time (5 min intervals)")
legend("bottomleft", legend="Sunday",bty="n")
mtext("  Comparison of day activity patterns", outer = TRUE)










dtNew$wday[weekdays(as.Date(dtNew$date))=="Saturday"] <- "weekend"


pattrnsW <- tapply(dtNew[dtNew$wday=="weekday",]$steps,dtNew[dtNew$wday=="weekday",]$interval,mean)
pattrnsS <- tapply(dtNew[dtNew$wday=="weekend",]$steps,dtNew[dtNew$wday=="weekend",]$interval,mean)

par(mfrow = c(2, 1), mar = c(0, 4, 0, 1), oma = c(2, 0, 2, 0))
plot((names(pattrnsW)),pattrnsW,col="red",type="l",lab=c(1,5,7),ylab="avg. steps/5min",xlab="")
legend("bottomleft", legend="weekday",bty="n")
plot(as.numeric(names(pattrnsS)),pattrnsS,col="red",type="l",lab=c(5,5,7),ylab="avg. steps/5min",xlab="time (5 min intervals)")
legend("bottomleft", legend="Sunday",bty="n")
mtext("  Comparison of day activity patterns", outer = TRUE)










# pats1 <- tapply(dt[wdys,]$steps,dt[wdys,]$interval,mean, na.rm=T)
# pats <- tapply(dt[!wdys,]$steps,dt[!wdys,]$interval,mean, na.rm=T)
# par(mfrow = c(2, 1), mar = c(0, 4, 0, 1), oma = c(2, 0, 2, 0))
# plot((names(pats1)),pats1,col="red",type="l",lab=c(1,5,7),ylab="avg. steps/5min",xlab="")
# legend("bottomleft", legend="weekday",bty="n")
# plot(as.numeric(names(pats)),pats,col="red",type="l",lab=c(5,5,7),ylab="avg. steps/5min",xlab="time (5 min intervals)")
# legend("bottomleft", legend="Sunday",bty="n")
# mtext("  Comparison of day activity patterns", outer = TRUE)
#points(as.numeric(names(pats1)),pats1,col="blue",pch=46)

#title("Average daily activity pattern on weekdays")
#hist(pats,col="red")



#Submitting the Assignment:
# Commit the your completed PA1_template.Rmd
# Commit your PA1_template.md and PA1_template.html
# figures included should have been placed in the figures/ 
#knit2html()