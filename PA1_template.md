## R Markdown

# What is mean total number of steps taken per day?

``` r
activity <- all_activity %>% filter(!is.na(steps))

# 1-Calculate the total number of steps taken per day
steps_per_day <- activity %>% group_by(date) %>% summarise(total_steps = sum(steps))

# 2-Make a histogram for the total number of steps taken each day

ggplot(steps_per_day, aes(x = total_steps)) +
  geom_histogram(binwidth = 1000, fill = "purple", color = "black") +
  labs(title = "Total Steps Taken Each Day", x = "Total Steps", y = "Frequency")
```

![](PA1_template_files/figure-markdown_github/activity-1.png)

``` r
# 3-Calculate and report the mean and median of the total number of steps taken per day
mean_steps <- mean(steps_per_day$total_steps)
median_steps <- median(steps_per_day$total_steps)
```

## What is the average daily activity pattern?

``` r
interval <- activity %>% group_by(interval) %>% summarise(avg_steps = mean(steps))

# 1-Make a time series plot (i.e. type = "l) of the 5-minute interval (x-axis) and the average number of 
#   steps taken, averaged across all days (y-axis)
ggplot(interval, aes(x = interval, y = avg_steps)) +
  geom_line(color = "red") +
  labs(title = "Average Number of Steps Taken per 5-Minute Interval",
       x = "5-Minute Interval",
       y = "Average Number of Steps")
```

![](PA1_template_files/figure-markdown_github/unnamed-chunk-1-1.png)

``` r
# 2- Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
max_interval <- interval %>% filter(avg_steps == max(avg_steps))
```

# Imputing missing values

``` r
total_missing <- sum(is.na(all_activity$steps))
cat("Total number of missing values in the dataset:", total_missing, "\n")
```

    ## Total number of missing values in the dataset: 2304

``` r
# 1-Calculate the mean steps per 5-minute interval across all days
interval_mean <- all_activity %>% group_by(interval) %>% summarise(mean_steps = mean(steps, na.rm = TRUE))

# 2-Fill in missing values with the mean for the 5-minute interval/3-create a new dataset that is equal 
# to the original dataset but with the missing data filled in.
data_filled <- all_activity %>% 
  left_join(interval_mean, by = "interval") %>% 
  mutate(steps = ifelse(is.na(steps), mean_steps, steps)) %>% 
  select(-mean_steps)

# Find the total number of steps taken per day
steps_per_day_fil <- data_filled %>% group_by(date) %>% summarise(total_steps = sum(steps))

# 4-Create a histogram of the total number of steps taken each day
ggplot(steps_per_day_fil, aes(x = total_steps)) +
  geom_histogram(binwidth = 1000, fill = "orange", color = "black") +
  labs(title = "Total Number of Steps Taken Each Day (Filled Data)",
       x = "Total Steps",
       y = "Frequency")
```

![](PA1_template_files/figure-markdown_github/unnamed-chunk-2-1.png)

``` r
# Find and report the mean and median of the total number of steps taken per day
mean_total_steps<- mean(steps_per_day_fil$total_steps)
median_total_steps <- median(steps_per_day_fil$total_steps)
```

## Are there differences in activity patterns between weekdays and weekends?

``` r
# 1-Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating 
#   whether a given date is a weekday or weekend day.

# Convert the 'date' column to Date type 
activity$date <- as.Date(activity$date)

# Create a factor with two levels: 'weekday' and 'weekend'
activity$day_type <- ifelse(weekdays(activity$date) %in% c("Saturday", "Sunday"), "weekend", "weekday")

# Convert 'day_type' to a factor
activity$day_type <- as.factor(activity$day_type)

# 2- Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and
#   the average number of steps taken, averaged across all weekday days or weekend days (y-axis). 

# Find the average number of steps for each 5-minute interval for weekdays and weekends
avg_steps <- activity %>%
  group_by(interval, day_type) %>%
  summarise(avg_steps = mean(steps, na.rm = TRUE))
```

    ## `summarise()` has grouped output by 'interval'. You can override using the
    ## `.groups` argument.

``` r
# Create the panel plot using ggplot2
ggplot(avg_steps, aes(x = interval, y = avg_steps, color = day_type)) +
  geom_line() +
  facet_wrap(~ day_type, ncol = 1, scales = "free_y") +
  labs(title = "Average Number of Steps per 5-Minute Interval",
       x = "5-Minute Interval",
       y = "Average Number of Steps") +
  theme_minimal()
```

![](PA1_template_files/figure-markdown_github/unnamed-chunk-3-1.png)
