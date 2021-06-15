---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---
#### Submitted by Syed Abdullah Hasan


## Loading and pre-processing the data

```r
unzip ("./activity.zip")
data <- read.csv("./activity.csv")
unlink ("./activity.csv")

data$date <- ymd(data$date)
```
  
## What is the mean total number of steps taken per day?

```r
sum <- data %>% group_by(date) %>% summarize("steps" = sum (steps))
steps_mean <- mean(sum$steps, na.rm=T)
steps_median <- median(sum$steps,na.rm=T)
hist(sum$steps, 
     xlab = "Daily Steps", 
     main = "Histogram of Daily No. of Steps",
     col = "steelblue",
     breaks = 10)
```

![](PA1_template_files/figure-html/Mean-1.png)<!-- -->
  
The *average* number of steps taken per day is **10,766.19** and the *median* number of steps is **10,765.00**.  

## What is the average daily activity pattern?  


```r
interval <- data%>%group_by(interval)%>%
        summarize(steps=mean(steps, na.rm=T))
with(interval, plot(interval,steps,
                    type="l",
                    col="steelblue",
                    lwd=2,
                    main = "Daily Activity Pattern",
                    xlab = "Time Interval",
                    ylab = "Average Number of Steps")
)
```

![](PA1_template_files/figure-html/Average Daily Activity Pattern-1.png)<!-- -->
  
The figure above illustrates the average daily activity pattern based on the original data set.  

From the figure and data, the five minute interval starting at **835** shows the highest number of **206.1698113** steps taken on average across all days recorded.  

## Imputing missing values  

```r
data_na <- sum(is.na(data$steps))

aggr_plot <- aggr(data, 
                  col = c("steelblue", "yellow"),
                  numbers = T,
                  softVars = T)
```

![](PA1_template_files/figure-html/Imputing Missing Values-1.png)<!-- -->


```r
data_im <- mice (data, m=5, method = "pmm")
data_im <- complete (data_im, 1)
```


```r
sum_im <- data_im %>% group_by(date) %>%
        summarize("steps" = sum(steps))
steps_mean_im <- mean(sum_im$steps, na.rm=T)
steps_median_im <- median(sum_im$steps,na.rm=T)
mean_change <- steps_mean_im - steps_mean
median_change <- steps_median_im - steps_median
```
There are a total of **2,304** missing values in the data set. These have been imputed using Multivariate Imputations by Chained Equations based on the mean function.  

After imputing missing values, the *average* number of steps taken per day is **10,527.46** - indicating a shift of *-238.73* as compared with the original data set.  

The *median* number of steps taken is **10,439.00** - which represents a shift of *-326.00* as compared with the original data set.  

The histogram showing number of steps taken per day for the imputed data set is illustrated below.  


```r
hist(sum_im$steps, 
     xlab = "Daily Steps", 
     main = "Histogram of Daily No. of Steps - Imputed Data",
     col = "lightblue",
     breaks = 10)
```

![](PA1_template_files/figure-html/Histogram-1.png)<!-- -->
  
## Are there differences in activity patterns between weekdays and weekends?  

```r
data_im$day <- ifelse (weekdays(data_im$date) %in% c("Saturday","Sunday"),  "weekend", "weekday")
data_im$day <-factor(data_im$day)
plot_data <- data_im%>%group_by(day,interval)%>%summarize("steps" = mean(steps))
g <- ggplot (plot_data, aes( x= interval, y=steps))+
            facet_wrap(~day, nrow=2)+
            geom_line(aes(y=steps,
                          col = day))+
            labs (title = "Average Activity Patterns",
                  y = "Average no. of steps taken",
                  x = "Interval")+
            theme (legend.position = "none")
```
As observed in the panel plot shown below, the activity pattern is significantly different over weekends as compared with weekdays.  

The main difference can be characterized in terms of a higher number of steps taken during the morning interval periods and a much higher number of steps overall throughout the day. 

In contrast, activity steps tend to be much higher during the early morning hours on weekdays as compared with weekends, possibly when the subject is commuting to work.  

![](PA1_template_files/figure-html/Comparison plot-1.png)<!-- -->
