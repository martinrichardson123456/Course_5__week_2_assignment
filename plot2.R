##################
# What is the average daily activity pattern?

# Make a time series plot (i.e. type = "l") of the 5-minute interval 
# (x-axis) and the average number of steps taken, averaged across all 
# days (y-axis).
# Which 5-minute interval, on average across all the days in the dataset, 
# contains the maximum number of steps?

# --> So find the average number of steps for each interval?

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


library(data.table)
DT <- data.table(dataset)
DT2 <- DT[ , mean(steps, na.rm = TRUE), by = interval]
print(paste("nrow(DT2) =", nrow(DT2)))
View(DT2)

max_steps <- max(DT2$V1)
print(paste("max_steps =", max_steps))
max_interval <- NULL
for(i in 1:nrow(DT2)){
  if(DT2$V1[i] == max_steps){
    max_interval <- DT2$interval[i]
    break
  }
}
print(paste("max_interval =", max_interval))

png(filename = "Plot2.png", width = 480, height = 480, units = "px")

plot(x = DT2$interval, y = DT2$V1, type = "l", col = "blue",
     xlab = "Interval", ylab = "Mean number of steps",
     main = "Time series plot of the average number of steps taken")
abline(v = max_interval, col = "black", lwd = 2) 

dev.off()
