###Loading and preprocessing the data
###===================================
# Download archive file, if it does not exist
ActivityFile <- "data/activity.zip"
if(!file.exists(ActivityFile)) {
  FileURL <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
  download.file(url=FileURL,destfile=ActivityFile)
}
unzip(zipfile=ActivityFile, overwrite=T, exdir="data")
ActivityFile <- "data/activity.csv"
activity <- read.csv("data/activity.csv", header=T, sep=",")

#View(activity)
#head(activity)

#Make the date field a Date data type
activity$date<-as.Date(activity$date,format="%Y-%m-%d")
activity.na.omit <- na.omit(activity) 

###What is mean total number of steps taken per day?
###===================================
##1
#View(df)
df<- aggregate(activity.na.omit$steps,by=list(activity.na.omit$date),FUN=sum)
names(df)=c("Date","DailySteps")
hist(df$DailySteps,xlab="Steps", main="Daily Steps Summary")

##2
m<-as.data.frame(mean(df$DailySteps,na.rm=T))
m<-cbind(m,median(df$DailySteps,na.rm=T))
names(m)<-c("Mean","Median")


###What is the average daily activity pattern?
###===================================
##1
df<- aggregate(activity.na.omit$steps,by=list(activity.na.omit$interval),FUN=sum)
names(df)=c("Interval","Steps")
#View(df)
plot(x=df$Interval,y=df$Steps,type="l",xlab="Intervals",ylab="Steps")

##2
subset(df,Steps==max(df$Steps))

###Imputing missing values
###===================================
##1
na.Count<-nrow(activity)-nrow(activity.na.omit)

##2
#use the rounded mean of the time interval as the NA filler
df<- aggregate(activity.na.omit$steps,by=list(activity.na.omit$interval),FUN=mean)
names(df)=c("interval","FillerSteps")
df$FillerSteps<-round(df$FillerSteps,0)
#View(df)

##3
activity.na.refill<-merge(activity,df)
activity.na.refill$steps<-with(activity.na.refill,
    ifelse(is.na(steps),FillerSteps,steps)
    )
activity.na.refill$FillerSteps<-NULL #drop the temp column
#View(activity.na.refill)

##4
df<- aggregate(activity.na.refill$steps,by=list(activity.na.refill$date),FUN=sum)
names(df)=c("Date","DailySteps")
hist(df$DailySteps,xlab="Steps", main="Daily Steps Summary")

##5
m2<-as.data.frame(mean(df$DailySteps,na.rm=T))
m2<-cbind(m2,median(df$DailySteps,na.rm=T))
names(m2)<-c("Mean","Median")

###Are there differences in activity patterns between weekdays and weekends?
###===================================
##1
activity.days<-activity.na.refill
activity.days$strWD<-weekdays(activity.days$date)
activity.days$WD<-as.POSIXlt(activity.days$date)$wday
activity.days$DayType<-as.factor(with(activity.days, 
                          ifelse(WD==0|WD==6,"Weekend","Weekday")
                      ))
#View(activity.days)
##2
old_mfrow=par()$mfrow
old_mar=par()$mar
par(mfrow = c(2, 1), mar=c(3,3,1,1))


dfWD<-subset(activity.days, DayType=="Weekday")
dfWE<-subset(activity.days, DayType=="Weekend")
dfWD<- aggregate(dfWD$steps,by=list(dfWD$interval),FUN=sum)
dfWE<- aggregate(dfWE$steps,by=list(dfWE$interval),FUN=sum)
names(dfWD)=c("Interval","Steps")
names(dfWE)=names(dfWD)
plot(x=dfWD$Interval,y=dfWD$Steps,type="l",xlab="Intervals",ylab="Steps", main="Weekdays")
plot(x=dfWE$Interval,y=dfWE$Steps,type="l",xlab="Intervals",ylab="Steps", main="Weekends")

#resetting the parameters back
par(mfrow = old_mfrow, mar=old_mar)




