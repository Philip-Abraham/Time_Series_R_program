library(xts)

# Create a
data_a <- as.matrix(c(1,1,1,1))
colnames(data_a) <- c("a")
dates_a <- seq(as.Date("2015-01-24"), length = 4, by = "days")
a <- xts(x = data_a, order.by = dates_a)
# Create b
data_b <- as.matrix(c(2,2,2,2,2,2,2))
colnames(data_b) <- c("b")
dates_b <- seq(as.Date("2015-01-26"), length = 7, by = "days")
b <- xts(x = data_b, order.by = dates_b)

# Perform an inner join of a and b
merge(a, b, join = "inner")

# Perform a left-join of a and b, fill missing values with 0
merge(a, b, join = "left", fill = 0)


## Combining xts by row with rbind

# temps, a time series of summer temperature data from Chicago, IL, USA.
data <- read.csv("chicagotemp.csv")
# Create dates as a Date class object starting from 2016-01-01
dates <- seq(as.Date("2016-07-01"), length = 16, by = "days")
# Use xts() to create temps
library(xts)
temps <- xts(x = data, order.by = dates)

# additional data
data2 <- read.csv("chicagotemp2.csv")
dates2 <- seq(as.Date("2016-06-30"), length = 1, by = "days")
temps_june30 <- xts(x = data2, order.by = dates2)

data3 <- read.csv("chicagotemp3.csv")
dates3 <- seq(as.Date("2016-07-17"), length = 1, by = "days")
temps_july17 <- xts(x = data3, order.by = dates3)

data4 <- read.csv("chicagotemp4.csv")
dates4 <- seq(as.Date("2016-07-18"), length = 1, by = "days")
temps_july18 <- xts(x = data4, order.by = dates4)

# For this exercise you will update your temps data with three new observations. 
# One will be before the series started and two will be after. Pay attention to 
# your function call, does order matter?

# Row bind temps_june30 to temps, assign this to temps2
temps2 <- rbind(temps_june30, temps)

# Row bind temps_july17 and temps_july18 to temps2, call this temps3
temps3 <- rbind(temps2, temps_july17, temps_july18)

# Because xts objects are ordered by their time index, the order of arguments in 
# xts's rbind() command is unimportant.


## Fill missing values using last or previous observation
# temps, a time series of summer temperature data from Chicago, IL, USA.
data <- read.csv("chicagotemp.csv")
# Create dates as a Date class object starting from 2016-01-01
dates <- seq(as.Date("2016-07-01"), length = 16, by = "days")
# Use xts() to create temps
library(xts)
temps <- xts(x = data, order.by = dates)
temps <- temps[1:5,] # just need the the first five observations
temps$Temp.Mean[2:3] <- NA # fill 2n and 3rd observ of temp. means with NA's

# Fill missing values in temps using the last observation
temps_last <- na.locf(temps, na.rm = TRUE, fromLast = FALSE, maxgap = Inf)

# Fill missing values in temps using the next observation
temps_next <- na.locf(temps, na.rm = TRUE, fromLast = TRUE, maxgap = Inf)


## NA interpolation using na.approx()
# For this exercise, you'll use a smaller xts version of the Box and Jenkin's 
# AirPassengers data set that ships with R. We've removed a few months of data to 
# illustrate various fill techniques.

data <- AirPassengers[133:144]
data <- as.data.frame(data)
colnames(data) <- c("missing")
data$original <- data$missing
data[3:4,1] <- NA

# Create dates as a Date class object starting from 1960-01-01
dates <- seq(as.Date("1960-01-01"), length = 12, by = "months")

# Use xts() to create x
library(xts)
AirPass <- xts(x = data, order.by = dates)

# Interpolate NAs using linear approximation
na.approx(AirPass)


## Combine a leading and lagging time series
# In this exercise, you will construct a single xts object with three columns. 
# The first column is data one day ahead, the second column is the original data, 
# and the third column is the one day behind - all using xts.

# Create a simple XTS object
# Create the object data 
data <- as.data.frame(5:9)
colnames(data) <- c("x")

# Create dates as a Date class object starting from 2017-09-25
dates <- seq(as.Date("2017-09-25"), length = 5, by = "days")

# Use xts() to create x
library(xts)
x <- xts(x = data, order.by = dates)

# Generating leads and lags can help you visualize trends in your time series 
# data over time.

# negative values indicate leads(future) and positive values indicate lags(past).

# Create a leading object called lead_x
lead_x <- lag(x, k = -1)

# Create a lagging object called lag_x
lag_x <- lag(x, k = 1)

# Merge your three series together and assign to z
z <- cbind(lead_x, x, lag_x)


## Calculate a difference of a series using diff()
# A simple way to view a single (or "first order") difference is to see it as 
# x(t) - x(t-k) where k is the number of lags to go back. 
# These are the same:
# diff(x, differences = 2)
# diff(diff(x))

# In this exercise, you will reuse the AirPass data from earlier, 
# though this time you will use the full series from 1948 to 1960.
data <- AirPassengers[1:144]
data <- as.data.frame(data)
colnames(data) <- c("passengers")
# Create dates as a Date class object starting from 1949-01-01
dates <- seq(as.Date("1949-01-01"), length = 144, by = "months")
# Use xts() to create x
library(xts)
AirPass <- xts(x = data, order.by = dates)

# Calculate the first difference of AirPass and assign to diff_by_hand
diff_by_hand <- AirPass - lag(AirPass)

# Use merge to compare the first parts of diff_by_hand and diff(AirPass)
merge(head(diff_by_hand), head(diff(AirPass)))

# Calculate the first order 12 month difference of AirPass
diff(AirPass, lag = 12, differences = 1)

