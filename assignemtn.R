library(ggplot2)

inputdata <- read.csv('activity.csv')
avail_dates <- levels(factor(inputdata$date))
steps <- data.frame("Date" = avail_dates, "SumSteps" = c(1:length(avail_dates)))

for ( i in avail_dates){
  steps$SumSteps[steps$Date == i] <- sum(inputdata[inputdata$date == i,1])
}

hist1 <- qplot(na.omit(steps$SumSteps))
print(hist1)

# Mean steps
meansteps <- mean(na.omit(steps$SumSteps))
# Median steps
medisteps <- median(na.omit(steps$SumSteps))

intervals <- aggregate(steps ~ interval, data = inputdata, FUN = mean)
PlotInterval <- ggplot(data = intervals, aes(x = interval, y = steps)) + 
  geom_line() +
  xlab("Time Intervals (5 Minutes is an unit)") + 
  ylab("Total Number of Steps") +
  ggtitle("Average Number of Steps Taken of the 5-Minute Interval")
print(PlotInterval)

max5min <- intervals$interval[intervals$steps == max(intervals$steps)]

missing_val <- sapply(X = inputdata, FUN = function(x) sum(is.na(x)))
replaced_input <- inputdata

for ( i in c(1:dim(replaced_input)[1])){
  if (is.na(replaced_input$steps[i])){
    replaced_input$steps[i] <- intervals$steps[intervals$interval == replaced_input$interval[i]]
  }
}

replaced_steps <- data.frame("Date" = avail_dates, "SumSteps" = c(1:length(avail_dates)))

for ( i in avail_dates){
  replaced_steps$SumSteps[steps$Date == i] <- sum(replaced_input[replaced_input$date == i,1])
}

hist2 <- qplot(replaced_steps$SumSteps)
print(hist2)

mean_replace <- mean(replaced_steps$SumSteps)
med_replace <- median(replaced_steps$SumSteps)

replaced_input$Weekday <- weekdays(as.Date(replaced_input$date))
replaced_input$Weekend <- as.factor(replaced_input$Weekday == "Saturday" | replaced_input$Weekday == "Sunday")
levels(replaced_input$Weekend) <- c("Weekday", "Weekend")

repl_weekday <- replaced_input[replaced_input$Weekend == "Weekday",]
repl_weekend <- replaced_input[replaced_input$Weekend == "Weekend",]

int_weekday <- aggregate(steps ~ interval + Weekend, data = repl_weekday, FUN = mean)
int_weekend <- aggregate(steps ~ interval + Weekend, data = repl_weekend, FUN = mean)

int_week <- rbind(int_weekday, int_weekend)

PlotInterval2 <- ggplot(data = int_week, aes(x = interval, y = steps)) + facet_grid(Weekend ~ .) + 
  geom_line() +
  xlab("Time Intervals (5 Minutes is an unit)") + 
  ylab("Total Number of Steps") +
  ggtitle("Average Number of Steps Taken of the 5-Minute Interval")
print(PlotInterval2)
