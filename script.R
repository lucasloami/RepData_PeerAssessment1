
# source('script.R')
# init()

# library('knitr')
# knit2html('PA1_template.Rmd')
# browseURL('PA1_template.html')


library(dplyr)
library(lattice)
Sys.setlocale("LC_TIME", "English")

get_data <- function() {
  activity <- read.csv(file="./data/activity.csv", head=TRUE, sep=",")
  return(activity)
}

clean_data <- function(data) {
  data <- na.omit(data)
  return(data)
}

build_histogram <- function(data) {
  hist(data)
}

fill_NA <- function(data) {


}

question_01 <- function(data) {
  # build_histogram(data$steps)
  steps <- aggregate(data$steps, by=list(data$date), FUN=sum, na.rm=TRUE)
  print(head(steps))
  print(mean(steps$x))
  print(median(steps$x))
}

question_02 <- function(data) {
  activity <- aggregate(steps ~ interval, data, mean, na.rm=TRUE)
  print(head(activity))

  #first answer
  plot(activity$interval, activity$steps, type='l', col=1,
     main="Average number of steps averaged over all days", xlab="Interval",
     ylab="Average number of steps")

  #second answer
  max_steps_in_interval_row <- which.max(activity$steps)
  print(activity[max_steps_in_interval_row, ])

}

question_03 <- function(data) {
  #first answer
  NA_counter <- sum(is.na(data))
  print(NA_counter)

  #fill NA with mean of steps per interval in day
  mean_activity <- aggregate(steps ~ interval, data, mean, na.rm=TRUE)
  new_data <- data
  for (i in 1:nrow(data)) {
    if(is.na(data$steps[i])) {
      interval <- data$interval[i]
      interval_row <- which(mean_activity$interval == interval)
      step <- mean_activity$steps[interval_row]
      new_data$steps[i] <- step
    }
  }

  new_activity <- aggregate(steps ~ date, data=new_data, FUN=sum, na.rm=TRUE)
  hist(new_activity$steps, main = "Total steps by day")

  #third answer
  print(mean(new_activity$steps))
  print(median(new_activity$steps))

  # the average value remains the sam after filling NA values, but the median value
  # changed a little bit
}

question_04 <- function(data) {
  #fill NA with mean of steps per interval in day
  mean_activity <- aggregate(steps ~ interval, data, mean, na.rm=TRUE)
  new_data <- data
  for (i in 1:nrow(data)) {
    if(is.na(data$steps[i])) {
      interval <- data$interval[i]
      interval_row <- which(mean_activity$interval == interval)
      step <- mean_activity$steps[interval_row]
      new_data$steps[i] <- step
    }
  }

  new_data$date <- as.Date(new_data$date, "%Y-%m-%d")
  day <- weekdays(new_data$date)
  day_type <- vector()
  for (i in 1:nrow(new_data)) {
    if(day[i] == "Saturday" || day[i] == "Sunday") {
      day_type[i] <- 'weekend'
    } else {
      day_type[i] <- 'weekday'
    }
  }

  new_data$day_type <- day_type
  new_data$day_type <- factor(new_data$day_type)

  steps_per_day <- aggregate(steps ~ interval + day_type, data=new_data, na.rm=TRUE, FUN=mean)
  xyplot(steps ~ interval | day_type, steps_per_day, type = "l", layout = c(1, 2),
    xlab = "Interval", ylab = "Number of steps")
}


init <- function() {
  data <- get_data()
  data <- clean_data(data)
  question_01(data)
  # question_02(data)
  # question_03(data)
  # question_04(data)
}