##################
# Imputing missing values

# Are there differences in activity patterns between weekdays and weekends?

# For this part the weekdays() function may be of some help here. Use the 
# dataset with the filled-in missing values for this part.

# Create a new factor variable in the dataset with two levels - "weekday" 
# and "weekend" indicating whether a given date is a weekday or weekend 
# day.

# Make a panel plot containing a time series plot (i.e. type = "l") of the 
# 5-minute interval (x-axis) and the average number of steps taken, 
# averaged across all weekday days or weekend days (y-axis). See the README 
# file in the GitHub repository to see an example of what this plot should 
# look like using simulated data.

##################
# The variables included in this dataset are:
  
# steps: Number of steps taking in a 5-minute interval (missing values are 
# coded as NA)
# date: The date on which the measurement was taken in YYYY-MM-DD format
# interval: Identifier for the 5-minute interval in which measurement was 
# taken

dataset <- read.csv("./repdata_data_activity/activity.csv")
print(paste("names(dataset) =", names(dataset)))
print(paste("ncol(dataset) =", ncol(dataset)))
print(paste("nrow(dataset) =", nrow(dataset)))
#View(dataset)
#print(unique(dataset$date))
print(paste("length(unique(dataset$date)) =", length(unique(dataset$date))))
print(paste("length(unique(dataset$interval)) =", length(unique(dataset$interval))))

DF <- dataset
DF$date <- as.Date(DF$date)
DF$day <- weekdays(DF$date)
for(i in 1:nrow(DF)){
  if(DF$day[i] == "Saturday" | DF$day[i] == "Sunday") DF$day[i] <- "weekend"
  else DF$day[i] <- "weekday"
  
}
View(DF)

DF_weekend <- subset(DF, day == "weekend")
DF_weekday <- subset(DF, day == "weekday")
print(paste("nrow(DF_weekend) =", nrow(DF_weekend)))
print(paste("nrow(DF_weekday) =", nrow(DF_weekday)))


# Create data table showing the average number of steps in a given interval
library(data.table)
DT__weekend <- data.table(DF_weekend)
DT__weekend_2 <- DT__weekend[ , mean(steps, na.rm = TRUE), by = interval]
print(paste("nrow(DT__weekend_2) =", nrow(DT__weekend_2)))
DT__weekend_2$day <- c(rep("weekend", nrow(DT__weekend_2)))
colnames(DT__weekend_2) <- c("interval", "mean.steps", "day")
View(DT__weekend_2)

# Create data table showing the average number of steps in a given interval
library(data.table)
DT__weekday <- data.table(DF_weekday)
DT__weekday_2 <- DT__weekday[ , mean(steps, na.rm = TRUE), by = interval]
print(paste("nrow(DT__weekday_2) =", nrow(DT__weekday_2)))
DT__weekday_2$day <- c(rep("weekday", nrow(DT__weekday_2)))
colnames(DT__weekday_2) <- c("interval", "mean.steps", "day")
View(DT__weekday_2)

# Combine these two data tables 
DT__combined <- rbind(DT__weekend_2, DT__weekday_2)
DT__combined$interval   <- as.numeric(as.character(DT__combined$interval))
DT__combined$mean.steps <- as.numeric(as.character(DT__combined$mean.steps))
View(DT__combined)

print(paste("typeof(DT__combined$interval) =", typeof(DT__combined$interval)))
print(paste("typeof(DT__combined$mean.steps) =", typeof(DT__combined$mean.steps)))

print(names(DT__combined))


library(ggplot2)
g <- ggplot(DT__combined, aes(x = interval, y = mean.steps, col = "red"))
g <- g + geom_line()
g <- g + facet_grid(day ~ .)
g <- g + xlab("Interval") + ylab("Mean steps")
g <- g + theme(legend.position="none")
print(g)

ggsave(filename = "Plot4.png", plot = last_plot())



