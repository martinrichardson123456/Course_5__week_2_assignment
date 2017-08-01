##################
# What is mean total number of steps taken per day?

# For this part of the assignment, you can ignore the  missing values in the 
# dataset.
# Make a histogram of the total number of steps taken each day
# Calculate and report the mean and median total number of steps taken per day

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


library(data.table)
DT <- data.table(dataset)
DT2 <- DT[ , sum(steps), by = date]
print(paste("nrow(DT2) =", nrow(DT2)))
#View(DT2)

mean_total_steps_per_day <- mean(DT2$V1, na.rm = TRUE)
print(paste("mean_total_steps_per_day =", mean_total_steps_per_day))
median_total_steps_per_day <- median(DT2$V1, na.rm = TRUE)
print(paste("median_total_steps_per_day =", median_total_steps_per_day))

png(filename = "Plot1.png", width = 480, height = 480, units = "px")

hist(x = DT2$V1, col = "red", breaks = 30, xlab = "Sum of steps in a day",
     main = "Total number of steps taken each day")
abline(v = mean_total_steps_per_day, col = "black", lwd = 2)

dev.off()