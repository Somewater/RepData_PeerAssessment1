library(ggplot2)

activity <- read.csv(unz("activity.zip", "activity.csv"))
activity$date = as.POSIXct(activity$date)

activity.per_day = aggregate(steps ~ date, activity, FUN = sum)
activity.per_day.mean = mean(activity.per_day$steps)
activity.per_day.median = median(activity.per_day$steps)
qplot(activity.per_day$steps, xlab = "Steps per day")

activity.per_interval = aggregate(steps ~ interval, activity, FUN = mean)
max_steps = max(activity.per_interval$steps)
most_active_row = with(activity.per_interval, activity.per_interval[steps == max_steps,])
most_active_interval = most_active_row$interval
qplot(x = interval, y = steps, data = activity.per_interval, geom = ("line")) + 
  geom_vline(xintercept = most_active_interval, col = "red", linetype = "longdash")

number_of_NA = sum(is.na(activity$steps))
rows_with_NA = activity[is.na(activity$steps),]

interval_to_mean_steps <- function(interval) {
  idx <- which(activity.per_interval[,'interval'] == interval)
  activity.per_interval[idx,]$steps
}

activity.imputing_steps = activity
activity.imputing_steps[is.na(activity.imputing_steps$steps),]$steps = 
  sapply(rows_with_NA$interval, interval_to_mean_steps)

activity.imputing_steps.per_day = aggregate(steps ~ date, activity.imputing_steps, FUN = sum)
activity.imputing_steps.per_day.mean = mean(activity.imputing_steps.per_day$steps)
activity.imputing_steps.per_day.median = median(activity.imputing_steps.per_day$steps)

is.weekend <- function(day_name) {
  if (day_name == "Saturday" || day_name == "Sunday")
    "weekend"
  else    
    "weekday"
}
activity.imputing_steps$weekday <- factor(sapply(weekdays(activity.imputing_steps$date), is.weekend))
activity.imputing_steps.per_interval = aggregate(steps ~ interval + weekday, activity.imputing_steps, FUN = mean)

