## Loading the data
data1  <- read.csv("activity.csv",na.string="NA")

## Mean total number of steps taken per day
### The total number of steps taken per day
dailySum <- tapply(data1$steps,data1$date,sum,na.rm=TRUE)

### Histogram of the total number of steps taken each day
par(mfrow=c(1,1))
hist(dailySum,breaks=20,ylab="Frequency",xlab="Daily Sum",main="Total Number of Steps Taken per Day")
dev.copy(png,"plot1.png")
dev.off()
### Mean and median of the total number of steps taken per day
#### Mean
dailyMean<- mean(data1$steps,na.rm=TRUE)
#### Median
dailyMedian <- median(data1$steps,na.rm=TRUE)

## The time series plot
crossDayMean <- tapply(data1$steps,data1$interval,mean,na.rm=TRUE)
plot(data1[1:288,]$interval,crossDayMean,type="l",xlab="interval",ylab="Cross Day Mean")
dev.copy(png,"plot2.png")
dev.off()
### Find the 5-minute interval containing the maximum number of steps
which.max(crossDayMean)

## Imputing missing values
### Report the total number of missing value
numberNA <- sum(is.na(data1$steps))
data2 <- data1
### Filling strategy is mean for that 5 minute interval
#a <- tapply(data2$steps,data2$interval,function(x){x<-crossDayMean})
a <- data.frame(data1[1:288,]$interval,crossDayMean)
colnames(a)<-c("interval","crossDayMean")
for(i in 1:length(data2$steps)){
        if(is.na(data2[i,]$steps)==TRUE){
                data2[i,]$steps <- a[which(data2$interval[i] == a$interval),]$crossDayMean
        }
}
sum(is.na(data2$steps))
### Make histogram of total number of steps taken each day
dailySum <- tapply(data2$steps,data2$date,sum,na.rm=TRUE)
hist(dailySum,breaks=20,xlab="Daily Sum")
dev.copy(png,"plot3.png")
dev.off()
### Calculate mean and median total number of steps taken per day
#### Mean
dailyMeanAfter <- mean(data2$steps,na.rm=TRUE)
#### Median
dailyMedianAfter <- median(data2$steps,na.rm=TRUE)

## Are there difference in activity patterns between weekdays and weekends
data2$date <- as.Date(data2$date)
### Create a new variable differentiating weekday and weekend
data2$week <- weekdays(data2$date)
data2$weekday <- weekdays(data2$date)
for(i in 1:length(data2$steps)){
        if(data2[i,]$week==c("Saturday","Sunday")){
                data2[i,]$weekday <- "weekend"
        } else {
                data2[i,]$weekday <- "weekday"
                }
}
### Make the plot
library(ggplot2)
par(mfrow=c(1,2))
crossWeekdayMean <- aggregate(data2$steps,list(data2$weekday,data2$interval),mean)
colnames(crossWeekdayMean) <- c("weekday","interval","crossWeekMean")
xyplot(crossWeekdayMean$crossWeekMean ~ crossWeekdayMean$interval | crossWeekdayMean$weekday,type="l"
       ,layout=c(1,2),xlab="weekday or weekend",ylab="Cross Weekday Mean",
       main="Average Number of Steps Across All weekdays and weekends")
dev.copy(png,"plot4.png")
dev.off()