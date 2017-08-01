##################
# Imputing missing values

# Note that there are a number of days/intervals where there are missing
# values (coded as NA). The presence of missing days may introduce bias into 
# some calculations or summaries of the data.

# Calculate and report the total number of missing values in the dataset 
# (i.e. the total number of rows with NAs)
# Devise a strategy for filling in all of the missing values in the dataset. 
# The strategy does not need to be sophisticated. For example, you could use 
# the mean/median for that day, or the mean for that 5-minute interval, etc.
# Create a new dataset that is equal to the original dataset but with the 
# missing data filled in.
# Make a histogram of the total number of steps taken each day and Calculate 
# and report the mean and median total number of steps taken per day. Do 
# these values differ from the estimates from the first part of the 
# assignment? What is the impact of imputing missing data on the estimates 
# of the total daily number of steps?

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


# Pick out the rows containing NA values for the number of steps
rows_with_NA <- subset(dataset, is.na(steps))
print(paste("nrow(rows_with_NA) =", nrow(rows_with_NA)))
#View(rows_with_NA)
rows_without_NA <- subset(dataset, !is.na(steps))
print(paste("nrow(rows_without_NA) =", nrow(rows_without_NA)))
#View(rows_without_NA)

# Make a data table containing the mean number of steps in each interval
library(data.table)
DT <- data.table(dataset)
DT2 <- DT[ , mean(steps, na.rm = TRUE), by = interval]
print(paste("nrow(DT2) =", nrow(DT2)))
colnames(DT2) <- c("interval", "mean steps")
#View(DT2)

# Merge the table containing NA step values with the table containing mean
# number of steps in each interval, by the 'interval' column, so we end up
# with a table that has 4 columns: interval, steps, date and mean steps
a <- merge(rows_with_NA, DT2, by = "interval") 
View(a)

# Create a new table called b and replace the steps column with the mean 
# steps column, before re-naming this column
b <- a
b$steps <- b$mean_steps
colnames(b) <- c("interval", "date", "steps")
View(b)

# Replicate table b, but change the order of the columns to match original
# dataset
c <- b
colnames(c) <- c("steps", "date", "interval")
c$steps     <- b$steps
c$date      <- b$date
c$interval  <- b$interval
View(c)
print(paste("nrow(c) =", nrow(c)))

# Recombine the rows with NA values updated to mean of appropriate interval
# with the rows without NA values
imputed_dataset <- rbind(c, rows_without_NA)
print(paste("nrow(imputed_dataset) =", nrow(imputed_dataset)))

# Pick out the rows containing NA values for the number of steps
rows_with_NA__imputed <- subset(imputed_dataset, is.na(steps))
print(paste("nrow(rows_with_NA__imputed) =", nrow(rows_with_NA__imputed)))
#View(rows_with_NA__imputed)

# Using the original dataset that has now had its missing values imputed,
# create a new table that contains the sum of the steps taken on each day
DT_imputed_dataset <- data.table(imputed_dataset)
DT3 <- DT_imputed_dataset[ , sum(steps), by = date]
print(paste("nrow(DT3) =", nrow(DT3)))
View(DT3)



# Calculate the mean and median value of daily steps taken
mean_total_steps_per_day <- mean(DT3$V1, na.rm = FALSE)
print(paste("mean_total_steps_per_day =", mean_total_steps_per_day))
median_total_steps_per_day <- median(DT3$V1, na.rm = FALSE)
print(paste("median_total_steps_per_day =", median_total_steps_per_day))

# Plot histogram of sum of steps in each day

png(filename = "Plot3.png", width = 480, height = 480, units = "px")

hist(x = DT3$V1, col = "green", breaks = 30, xlab = "Sum of steps in a day", 
     main = "Total number of steps taken each day")
abline(v = mean_total_steps_per_day, col = "black", lwd = 2)

dev.off()