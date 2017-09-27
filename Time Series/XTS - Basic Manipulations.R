## Querying for dates
# Date ranges can be extracted from xts objects by simply specifying the period(s) 
# you want using special character strings in your subset.
# A["20090825"]       ## Aug 25, 2009
# A["201203/201212"]  ## Mar to Dec 2012
# A["/201601"]        ## Up to and including January 2016

# For this exercise you will create a simple but very common query. Extract a 
# range of dates using the ISO-8601 feature of xts. After successfully extracting 
# a full year, you will then create a subset of your new object with specific start 
# and end dates using this same notation.

# Create the object data using 306 random numbers
data <- rnorm(306)

# Create dates as a Date class object starting from 2016-01-01
dates <- seq(as.Date("2015-06-01"), length = 306, by = "days")

# Use xts() to create x
library(xts)
x <- xts(x = data, order.by = dates)

# Select all of 2016 from x
x_2016 <- x["2016"]

# Select January 1, 2016 to March 22, 2016
jan_march <- x["2016/2016-03-22"]

# Verify that jan_march contains 82 rows
82 == length(jan_march)


## Use the special T/T notation designed for intraday repeating intervals.

# Intraday times for all days
NYSE["T09:30/T16:00"] 


## Row selection with time objects

# Create the object data using 306 random numbers
data <- rnorm(306)

# Create dates as a Date class object starting from 2016-01-01
dates <- seq(as.Date("2015-06-01"), length = 306, by = "days")

# Use xts() to create x
x <- xts(x = data, order.by = dates)

# Subset x using the vector dates
x[dates]

# Subset x using dates as POSIXct
x[as.POSIXct(dates)]


## Update and replace elements

# Replace the values in x contained in the dates vector with NA
x[dates] <- NA

# Replace all values in x for dates starting March 9, 2016 with 0
x["2016-03-09/"] <- 0

# Verify that the value in x for March 11, 2016 is now indeed 0
x["2016-03-11"]


## Find the first or last period of time
# For this exercise, you'll extract relative observations from a data set called 
# temps, a time series of summer temperature data from Chicago, IL, USA.

data <- read.csv("chicagotemp.csv")
# Create dates as a Date class object starting from 2016-01-01
dates <- seq(as.Date("2016-07-01"), length = 16, by = "days")
# Use xts() to create temps
library(xts)
temps <- xts(x = data, order.by = dates)

# Create lastweek using the last 1 week of temps
lastweek <- last(temps, "1 week")

# Print the last 2 observations in lastweek
last(lastweek, 2)

# Extract all but the first two days of lastweek
first(lastweek, "-2 days")

# Combining first and last
# Extract the first three days of the second week of temps
# The best way to think about this is to work from the inside out. You can get the 
# largest interval of time, then the next largest at the beginning or end of the 
# interval. You can then call first() or last() again, using a finer time resolution, 
# e.g. last(first(last(x))) but with the proper ranges specified.
first(last(first(temps, "2 weeks"), "1 week"), "3 days")


## Matrix arithmetic - add, subtract, multiply, and divide in time!
# xts respects time and will only return the intersection of times when doing 
# various mathematical operations.

# Create a
data_a <- as.matrix(c(1,1,1))
colnames(data_a) <- c("a")
dates_a <- seq(as.Date("2015-01-24"), length = 3, by = "days")
a <- xts(x = data_a, order.by = dates_a)
# Create b
data_b <- as.matrix(c(2))
colnames(data_b) <- c("b")
dates_b <- seq(as.Date("2015-01-24"), length = 1, by = "days")
b <- xts(x = data_b, order.by = dates_b)

# Add a and b. Notice the behavior of the dates, which ones remain?
a + b
# Adding two xts objects returns only the dates common to both. 
# Adding a numeric to an xts object is a bit more intuitive.
# Add a with the numeric value of b
a + as.numeric(b)


## Math with non-overlapping indexes

# Using a and b from the previous exercise, get the value of a + b for each date in a. 
# fill all missing rows of b with 0
# If no b is available on a given date, the answer should be a on that date.
a + merge(b, index(a), fill = 0)

# Now add a to b, but this time make sure all values of a are added to the last 
# known value of b in time.
# If you set fill = na.locf, NAs are filled with the most recent observation.
a + merge(b, index(a), fill = na.locf)
