# Reproducible Research: Peer Assessment 1
Ernst  
August 7,2016  
## functions

```r
  matrix2vector<-function(M)
  {  
    c<-dim(M)
    MB<-NULL
    
    for (k in 1:c[2]){
       MB<-c(MB,M[1:c[1], k])
      }
    return(MB)
  }
```

## Loading and preprocessing the data

```r
  hlp<- getwd()
  setwd("activity")
  p<-read.csv("activity.csv")
  setwd(hlp)
  summary(p)
```

```
##      steps                date          interval     
##  Min.   :  0.00   2012-10-01:  288   Min.   :   0.0  
##  1st Qu.:  0.00   2012-10-02:  288   1st Qu.: 588.8  
##  Median :  0.00   2012-10-03:  288   Median :1177.5  
##  Mean   : 37.38   2012-10-04:  288   Mean   :1177.5  
##  3rd Qu.: 12.00   2012-10-05:  288   3rd Qu.:1766.2  
##  Max.   :806.00   2012-10-06:  288   Max.   :2355.0  
##  NA's   :2304     (Other)   :15840
```

## What is mean total number of steps taken per day?

```r
  TotalSteps<-with(p,tapply(steps, as.factor(date),sum,na.rm=TRUE))
  hist(TotalSteps)
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

```r
  MEAN<-mean(TotalSteps)
  MEAN
```

```
## [1] 9354.23
```

```r
  MEDIAN<-median(TotalSteps)
  MEDIAN
```

```
## [1] 10395
```

## What is the average daily activity pattern?

```r
  AverageStep<-with(p,tapply(steps, as.factor(interval),mean,na.rm=TRUE))
  plot(AverageStep,type="l", ylab = "Average steps per day",  xlab = "Daytime in 5 minute   steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

```r
  wt<-which.max(AverageStep)
  t<-wt[[1]]
  t
```

```
## [1] 104
```
## Imputing missing values  
## Number of missing values c

```r
  ste<-p$steps
  nac<-is.na(ste)
  stet<-ste[nac[TRUE]]
  c<-length(stet)
  c
```

```
## [1] 2304
```
## Imputation of missing values

```r
  AStep<-with(p,tapply(steps, as.factor(interval),median,na.rm=TRUE))
  ctest<-replicate(61, AStep, simplify = TRUE)
  ja<-matrix2vector(ctest)
  p$replace<-ja
  pnew<-p
  pnew$steps[nac[TRUE]]<-pnew$replace[nac[TRUE]]
```
  
## calculate number of steps per day with replaced missing values

```r
  TotalStepsRep<-with(pnew,tapply(steps, as.factor(date),sum,na.rm=FALSE))
  hist(TotalStepsRep)
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

```r
  MEANrep<-mean(TotalStepsRep)
  MEANrep
```

```
## [1] 9503.869
```

```r
  MEDIANrep<-median(TotalStepsRep)
  MEDIANrep
```

```
## [1] 10395
```
## Are there differences in activity patterns between weekdays and weekends?
## create factor variable with two levels weekend or not

```r
  pnew$weekday<-as.factor((weekdays(as.Date(pnew$date))=="Sonntag")|(weekdays(as.Date(pnew$date))=="Samstag"))
  levels(pnew$weekday) <- c('weekday', 'weekend')
  weekend<-subset(pnew,weekday=="weekend")
  weekday<-subset(pnew,weekday=="weekday")
  AverageStepweekend<-with(weekend,tapply(steps, as.factor(interval),mean))
  AverageStepweekday<-with(weekday,tapply(steps, as.factor(interval),mean))
  par(mfrow = c(2, 1), mar = c(4, 4, 2, 1))
  plot(AverageStepweekday,type="l", ylab = "Average steps per day",  xlab = "Daytime in 5 minute   steps", main="weekday")
  plot(AverageStepweekend,type="l", ylab = "Average steps per day",  xlab = "Daytime in 5 minute   steps", main="weekend")
```

![](PA1_template_files/figure-html/unnamed-chunk-8-1.png)<!-- -->
  

