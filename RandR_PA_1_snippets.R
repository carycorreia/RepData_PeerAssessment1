##################################################################################
##
##      Program:        RandR_PA_1_snippets.R
##      Programmer:     Cary Correia
##
##      Date:           July 17th, 2014
##
##      Purpose:        Use this program to code up and experiment for assignment
##################################################################################
##
##      Prep the workspace
setwd('/Users/carycorreia/Documents/Exploratory_Data_Project1/RepData_PeerAssessment1')
##
##      Loading and preprocessing the data
##      1.  Loading data
data<-          read.csv('activity.csv', header=TRUE, sep="," , na.strings="NA",)
##
##      2. Process/transform the data              
data$date<-     as.Date(data$date, format="%Y-%m-%d")
data$interval<- as.integer(data$interval)
##
##      What is the mean total number of steps taken per day
##      1. Make a histogram of the total number of steps taken each day
##      Create a table to count the number of steps each day
library(ggplot2)
steps.per.day<-aggregate(steps~date, data, sum)
ggplot(steps.per.day, aes(x=steps)) +
        geom_histogram(fill="red")+
        theme_bw()+
        labs(title="Histogram of Steps per day")+
        ylab("Frequency of Steps")+
        xlab("Steps Taken Per Day")+
        theme(plot.title=element_text(lineheight=2, face="bold"))+
        theme(axis.title.x=element_text(lineheight=1, face="bold"))+
        theme(axis.title.y=element_text(lineheight=1, face="bold"))

##
##      2. Calculate and report the mean and median total number of steps taken per day
mean.steps<-mean(steps.per.day$steps, na.rm=TRUE)
median.steps<-median(steps.per.day$steps, na.rm=TRUE)
##
##      What is the average daily activity pattern?
##      1. Make a time series plot (ie: type = "l") of Interval amd avg # of Steps
##        
##      Prep the data for the plot
data.clean<-data[!is.na(data$steps),]
mean.col<-as.data.frame(tapply(data.clean$steps, data.clean$interval, mean))
mean.matrix<-cbind(unique(data.clean$interval), mean.col)
names(mean.matrix)<-c("interval","avgSteps")
##      Draw the plot
        with(mean.matrix, plot(interval, avgSteps, type="l", ann=FALSE))          
        title(main="Average Steps vs Interval", 
        sub=NULL, xlab="Interval", ylab="Average Steps")
## Determine the 5minute interval where we had the highest number of average steps
        H.interval<-    mean.matrix[mean.matrix$avgSteps==max(mean.matrix$avgSteps),]
## Determine the interval where that maximum occurred
        max.interval<-  H.interval$interval
##
##      Imputing missing values
##      1. Calculate and Report the total number of missing values
na.total<-nrow(data)-nrow(data.clean)

##      2. Devise a strategy for filling in all of the missing values in the dataset
##      Create a data quality field to track good steps and bad steps
data$step.bad<-is.na(data$steps)
##      Copy the full steps column into a new column....this will be used to overwrite the NAs
data$Impute<-data$steps
##      Create a "replace" index...ie: this will be a vector to identify the 2,304 replacements
replace<-which(is.na(data$steps))
##      Replace the NA values with the average of each interval
data2<-merge(data, mean.matrix, by="interval", sort=FALSE)
data2<-data2[with(data2, order(date,interval)),]
data[replace, 5]<-data2[replace,6]
##
##      3. Create a new dataset that is equal to the original dataset but with the missing data filled in
data.new<-data
drops<-c("steps","step.bad")
data.new<-data.new[,!(names(data.new)%in% drops)]
data.new<-data.new[c("Impute","date","interval")]
names(data.new)<-c("steps", "date", "interval")


##      4. Make a histogram of the total number of steps taken each day and calcualte and report mean and median
steps.per.day2<-aggregate(steps~date, data.new, sum)
ggplot(steps.per.day2, aes(x=steps)) +
        geom_histogram(fill="blue")+
        theme_bw()+
        labs(title="Histogram of Steps per day- Using Imputed Steps Methodology")+
        ylab("Frequency of Steps")+
        xlab("Steps Taken Per Day")+
        theme(plot.title=element_text(lineheight=2, face="bold"))+
        theme(axis.title.x=element_text(lineheight=1, face="bold"))+
        theme(axis.title.y=element_text(lineheight=1, face="bold"))

##
##     Calculate and report the mean and median total number of steps taken per day
mean.steps.2<-mean(steps.per.day2$steps)
median.steps.2<-median(steps.per.day2$steps)        

impact.mean<-mean.steps - mean.steps.2
impact.median<-median.steps - median.steps.2

##      Are there difference in activity patterns between weekdays and weekends ?
##      Add in day of the week
day<-as.data.frame(weekdays(data.new$date))
names(day)<-"day"
data.withDay<-cbind(data.new$steps, data.new$date, data$interval, day); names(data.withDay)<-c("steps", "date", "interval", "day")

##      Add in Weekend or Weekday
weekDay<-c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")
Type<-c("Weekend", rep("Weekday", 5), "Weekend")
DayType<-as.data.frame(cbind(weekDay, Type))
data.withDay<-merge(x=data.withDay, y=DayType, by.x="day", by.y="weekDay")
data.withDay<-data.withDay[with(data.withDay, order(date,interval)),]
data.withDay$Type<-as.factor(data.withDay$Type)
##
##      Plot the data
##      Prep the data (calculate the means)
answer<-tapply(data.withDay$steps, list(weektype=data.withDay$Type, interval=data.withDay$interval), mean)

library(reshape)
df<-melt(answer, varnames=c("Type", "Interval"), value.name="AvgSteps")

##      Create the graph
 library("lattice")
with(df, xyplot(value~Interval|Type, type="l", ann=FALSE, ylab="Number of steps", layout=c(1,2)))

##      Fin







