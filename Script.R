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
activity[is.na(activity$steps),]