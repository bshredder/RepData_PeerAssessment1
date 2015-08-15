# Reproducible Research: Peer Assessment 1
Bill Schroeder  




## Loading and preprocessing the data
  
  

*Check for and insall required libraries*


```r
# check for required packages
if("lubridate" %in% rownames(installed.packages() ) == FALSE){
  install.packages("lubridate")}
library(lubridate)
```

*Load input file*


```r
# read in the data
unzip("activity.zip")
data <- read.csv( "activity.csv", header=TRUE, sep=",")
```


*Process/transform the data into a format suitable for your analysis*


```r
# convert the dates (y/m/d) from factors to dates
data$date <- ymd(as.character(data$date))
```


## What is mean total number of steps taken per day? 
  
  

*Make a histogram of the total number of steps taken each day*

![](PA1_template_files/figure-html/unnamed-chunk-1-1.png) 


*Calculate the mean and median total number of steps taken per day*

*mean total number of steps taken per day*


```r
mean( data$steps, na.rm = TRUE )
```

```
## [1] 37.3826
```

*median total number of steps taken per day*


```r
median( data$steps, na.rm = TRUE )
```

```
## [1] 0
```


## What is the average daily activity pattern?
  
  
*Make a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)*  


```r
# calculate the mean number of steps for each interval
meanStepsPerInterval <- tapply (data$steps, data$interval, mean, na.rm=TRUE)

# find the maximum mean value across all intervals
maxSteps <- max(meanStepsPerInterval) 

# find the interval that contains the maximum mean value
maxStepsInterval <- match( maxSteps, meanStepsPerInterval )

# create a time series plot of steps for all intervals 
plot(meanStepsPerInterval , type="l", xlab="interval", ylab="ave/steps")
abline( v=maxStepsInterval, lty=2, col="red", lwd="5")
text(maxStepsInterval,10,maxStepsInterval) 
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png) 


*Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?*


```
## [1] "Interval: 104 contains the maximum number of steps, which is (206.169811320755)"
```


## Imputing missing values
  
  
*Calculate and report the total number of missing values in the dataset*  
  

```r
# function to calculate total number of NAs
numNAs <- function( data ) {
	isNA <- is.na(data$steps)
	return (sum(isNA))
}

# calculate and display the total number of NAs in ORIGINAL dataset
print(sprintf("Total number of NAs = %s Which is %s percent", numNAs( data ), mean(is.na(data$steps))*100 ) )
```

```
## [1] "Total number of NAs = 2304 Which is 13.1147540983607 percent"
```


*Devise a strategy for filling in all of the missing values in the dataset. Strategy is to replace NAs with ave/day* 
**AND**
*Create a new dataset that is equal to the original dataset but with the missing data filled in*


```r
# clean the original data - replace NAs with average for day
cleansedData <- data
cleansedData$steps[ is.na(cleansedData$steps)] <- mean(cleansedData$steps, rm.na=TRUE)
```


*Make a histogram of the total number of steps taken each day*

![](PA1_template_files/figure-html/unnamed-chunk-8-1.png) 


*Calculate and report the mean and median total number of steps taken per day*

**Mean steps per day of cleansed data*


```r
# calculate the mean across all dates
mean(cleansedData$steps, na.rm=TRUE)
```

```
## [1] 37.3826
```

**Median steps per day of cleansed data*


```r
# calculate median across all dates
median(cleansedData$steps, na.rm=TRUE)
```

```
## [1] 0
```




*Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?*


```
## [1] "Cleaning the data did NOT impact the mean"
```

```
## [1] "Cleaning the data did NOT impact the median"
```


## Are there differences in activity patterns between weekdays and weekends?

*Create a new factor variable in the dataset with two levels -- "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.*


```r
# check if package "chron" is installed
if("chron" %in% rownames(installed.packages() ) == FALSE){
  install.packages("chron")
}
library("chron")
```

```
## 
## Attaching package: 'chron'
## 
## The following objects are masked from 'package:lubridate':
## 
##     days, hours, minutes, seconds, years
```

```r
# function returns if date is a weekend or weekday
dayOfWeekFactor <- function( date ){
  if( TRUE == factor(is.weekend(date) )){ 
    "weekend"
      } else {
        "weekday"
    }
}

# Create a new factor variable in the dataset with two levels# "weekday" and "weekend" 
cleansedData$DoW <- as.factor(sapply(cleansedData$date, dayOfWeekFactor))
```

*Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). The plot should look something like the following, which was created using simulated data:*

![](PA1_template_files/figure-html/unnamed-chunk-13-1.png) 

