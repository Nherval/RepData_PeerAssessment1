---
title: "Personal Activity Monitoring Data Analysis"
author: "Natalia Hernández Valenciano"
date: "2025-03-18"
output: html_document
---



## Loading and Preprocessing the Data


``` r
data <- read.csv("activity.csv", stringsAsFactors = FALSE)
data$date <- as.Date(data$date)
```

## Total Number of Steps Taken per Day 


``` r
library(ggplot2)
total_steps <- aggregate(steps ~ date, data, sum, na.rm = TRUE)

#Histogram 

ggplot(total_steps, aes(x = steps)) +
  geom_histogram(binwidth = 1000, fill = "blue", alpha = 0.7) +
  labs(title = "Histogram of Total Steps per Day", x = "Total Steps per Day", y = "Frequency")
```

![plot of chunk total-steps](figure/total-steps-1.png)

``` r
#Mean and Median 
mean_steps <- mean(total_steps$steps)
median_steps <- median(total_steps$steps)
mean_steps
```

```
## [1] 10766.19
```

``` r
median_steps
```

```
## [1] 10765
```

##Average Daily Activity Pattern


``` r
avg_steps_interval <- aggregate (steps~interval, data, mean, na.rm = TRUE)

#Time Series Plot 
plot(avg_steps_interval$interval, avg_steps_interval$steps, type = "l", xlab = "5- Minute Interval", ylab = "Average Steps", main = "Average Daily Activity Pattern")
```

![plot of chunk daily-patern](figure/daily-patern-1.png)

``` r
#Interval with Maximum Steps 
max_interval <- avg_steps_interval[which.max(avg_steps_interval$steps),]
max_interval
```

```
##     interval    steps
## 104      835 206.1698
```

## Imputing Missing Values 


``` r
missing_values <- sum(is.na(data$steps))
missing_values
```

```
## [1] 2304
```

``` r
#Imputation using mean per interval 
data_imputed <- data
data_imputed$steps[is.na(data_imputed$steps)] <- ave(data$steps, data$interval, FUN = function(x) mean(x, na.rm = TRUE))[is.na(data$steps)]

#Histogram After Imputation 
total_steps_imputed <- aggregate(steps~date, data_imputed, sum)
ggplot(total_steps_imputed, aes(x = steps)) + geom_histogram(binwidth = 1000, fill = "green", alpha = 0.7) + labs (title = "Histogram After Imputation", x = "Total Steps per Day", y = "Frecuency")
```

![plot of chunk impute-missing](figure/impute-missing-1.png)

``` r
#Mean and Median After Imputation 
mean_steps_imputed <- mean (total_steps_imputed$steps)
median_steps_imputed <- median(total_steps_imputed$steps)
mean_steps_imputed
```

```
## [1] 10766.19
```

``` r
median_steps_imputed
```

```
## [1] 10766.19
```

##Activity Patterns: Weekday vs. Weekend 

``` r
data_imputed$day_type <- ifelse(weekdays(data_imputed$date) %in% c("Saturday", "Sunday"), "Weekend", "Weekday")

avg_steps_week <- aggregate(steps ~interval + day_type, data_imputed, mean)

ggplot(avg_steps_week, aes(x=interval, y = steps, color = day_type)) + geom_line() + facet_wrap(~day_type, ncol = 1) + labs (title = "Activity Patterns: Weekday vs. Weekend", x = "Interval", y = "Average")
```

![plot of chunk weekday-weekend](figure/weekday-weekend-1.png)






