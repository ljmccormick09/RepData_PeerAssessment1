---
title: "Reproducible Research Course Project 1"
author: "Lisa McCormick"
date: "2024-01-09"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## Loading and preprocessing the data


```{r}

activity_df <- read.csv("C:/Users/lisa.mccormick/Documents/Coursera materials/Reproducible Research/activity.csv", header = TRUE)

```

## What is mean total number of steps taken per day?

1. Make a histogram of the total number of steps taken each day

2. Calculate and report the mean and median total number of steps taken per day

  (I'm not sure if the instructions are asking for the mean and median of total steps each day or over all the days, so I calculated both.)


```{r}
# Histogram of total steps per day

library(ggplot2)
library(dplyr)

total_steps <- activity_df %>% 
    na.omit() %>% 
    group_by(date) %>%
    summarize(Total = sum(steps))

ggplot(total_steps, aes(Total)) +
  geom_histogram(color = "black", fill = "red") +
  labs(title = "Histogram of Total Steps per day", x= "Total Steps per day", y="Frequency")

# Mean and Median Steps per day

mean_med_steps <- activity_df %>% 
    na.omit() %>% 
    group_by(date) %>%
    summarize(Mean = mean(steps), Median = median(steps))

View(mean_med_steps)

# Mean and Median of Total Steps per day

mean_med_total <- total_steps %>% 
    summarize(Mean = mean(Total), Median = median(Total))

View(mean_med_total)

```

## What is the average daily activity pattern?

1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```{r}

# Time series plot of 5-minute interval vs. avg. number of steps taken (avg. across all days)

mean_steps_int <- activity_df %>% 
    na.omit() %>% 
    group_by(interval) %>%
    summarize(Mean = mean(steps))

plot(mean_steps_int$Mean ~ mean_steps_int$interval, type = "l",
     ylab = "Average Steps taken", xlab = "5-minute interval", main = "Average Number of Steps taken per day at each 5 minute interval")

# Which interval contains the maximum number of steps

mean_steps_int[row(mean_steps_int)[mean_steps_int==max(mean_steps_int$Mean)],]

```

## Imputing missing values

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```{r}

# How many missing values in the original dataset

sum(is.na(activity_df))

# Make new table with all activity data and the mean steps per interval

new_activity <- merge(activity_df, mean_steps_int, by = "interval")

# Fill in average steps per interval for missing values, the dataset new_activity has all the missing values filled in.

new_activity$steps <- ifelse(is.na(new_activity$steps), new_activity$Mean, new_activity$steps)

# NEW Histogram of total steps per day

library(ggplot2)
library(dplyr)

new_total_steps <- new_activity %>% 
    group_by(date) %>%
    summarize(NewTotal = sum(steps))

ggplot(new_total_steps, aes(NewTotal)) +
  geom_histogram(color = "black", fill = "green") +
  labs(title = "New Histogram of Total Steps per day", x= "New Total Steps per day", y="Frequency")

# NEW Mean and Median of Total Steps per day

new_mean_med_total <- new_total_steps %>% 
    summarize(NewMean = mean(NewTotal), NewMedian = median(NewTotal))

View(new_mean_med_total)


```

Do these values differ from the estimates from the first part of the assignment? 

The mean and median are now the same value. In the first part of the assignment the mean was slightly higher than the median.


What is the impact of imputing missing data on the estimates of the total daily number of steps?

The impact of imputing missing data is that the data move towards the mean (since I used the mean to impute the missing values.) This is why the median and the mean are the same.



## Are there differences in activity patterns between weekdays and weekends?

Use the dataset with the missing values filled in- this is the new_activity dataset.

1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

2. Make a panel plot containing a time series plot (i.e. type = "l"
type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). 

```{r}

# Create a factor variable indicating if the date is a weekday or weekend.

new_activity$date <- as.Date(new_activity$date)
new_activity$weekday <- weekdays(new_activity$date)
new_activity$Category <- ifelse(new_activity$weekday %in% c("Saturday", "Sunday"), "Weekend", "Weekday")
new_activity$Category <- as.factor(new_activity$Category)

# Time series panel plot of steps taken over 5-minute intervals, averaged across weekdays or weekend days.

new_mean_steps_int <- new_activity %>% 
    group_by(interval, Category) %>%
    summarize(NewMean = mean(steps))

library(lattice)

xyplot(new_mean_steps_int$NewMean ~ new_mean_steps_int$interval | new_mean_steps_int$Category, type = "l", layout = c(1,2), ylab = "Number of Steps", xlab = "Interval", main = "Ave. Steps taken per day at each 5 minute interval")

```

