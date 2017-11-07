## Class attributes - tclass, tzone, and tformat
# xts accessor functions detailed here:
# The index class using indexClass() (e.g. from Date to chron)
# The time zone using indexTZ() (e.g. from America/Chicago to Europe/London)
# The time format to be displayed via indexFormat() (e.g. YYYY-MM-DD)

# In this exercise, you will practice each of these functions and view the results 
# of your changes. To do so, you'll once again use the temps data

# temps, a time series of summer temperature data from Chicago, IL, USA.
data <- read.csv("chicagotemp.csv")
# Create dates as a Date class object starting from 2016-01-01
dates <- seq(as.Date("2016-07-01"), length = 16, by = "days")
dates <- as.POSIXct(dates+1) # added one for dates to on 7-1-2016 instead of 6-30-2016
# Use xts() to create temps
library(xts)
temps <- xts(x = data, order.by = dates)

# Set time zone of temps data to Chicago time
indexTZ(temps) <- "America/Chicago"
indexTZ(temps)

# View the first three indexes of temps
index(temps)[1:3]

# Get the index class of temps
indexClass(temps)

# Get the timezone of temps
indexTZ(temps)

# Change the index format of temps to "%b-%d-%Y"
indexFormat(temps) <- "%b-%d-%Y"

# View the new format
head(temps)


## Time Zones (and why you should care!)
# xts provides a simple way to leverage time zones on a per-series basis. 
# While R provides time zone support in native classes POSIXct and POSIXlt, xts 
# extends this power to the entire object, allowing you to have multiple time zones 
# across various objects.
# Some internal operation system functions require a time zone to do date math. 
# If a time zone isn't explicitly set, one is chosen for you! Be careful to always 
# set a time zone in your environment to prevent errors when working with dates and times.
library(xts)
dates <- seq(as.Date("2016-07-01"), length = 16, by = "days")
dates <- as.POSIXct(dates+1) # added one for dates to on 7-1-2016 instead of 6-30-2016

# Construct an xts time series of the numbers 1 through 16 called times_xts, 
# with tzone set to "America/America/Newyork", and indexed by the dates object.
times_xts <- xts(1:16, order.by = dates, tzone = "America/New_York")

# Change the time zone of times_xts to Asia/Hong_Kong
tzone(times_xts) <- "Asia/Hong_Kong"

# Extract the current time zone of times_xts
tzone(times_xts)


## Determining periodicity
# xts provides a handy tool to discover this regularity in your data by estimating 
# the frequency of the observations - what we are referring to as periodicity - 
# using the periodicity() command

# temps, a time series of summer temperature data from Chicago, IL, USA.
data <- read.csv("chicagotemp.csv")
# Create dates as a Date class object starting from 2016-01-01
dates <- seq(as.Date("2016-07-01"), length = 16, by = "days")
dates <- as.POSIXct(dates+1) # added one for dates to on 7-1-2016 instead of 6-30-2016
# Use xts() to create temps
library(xts)
temps <- xts(x = data, order.by = dates)
# Calculate the periodicity of the temps data set
periodicity(temps)

library(PerformanceAnalytics)
data(edhec)
# Calculate the periodicity of edhec
periodicity(edhec)

# Convert edhec to yearly
edhec_yearly <- to.yearly(edhec)

# Calculate the periodicity of edhec_yearly
periodicity(edhec_yearly)


## Find the number of periods in your data
# Often it is handy to know not just the range of your time series index, but also 
# how many discrete irregular periods your time series data covers. 
library(xts)
# Count the months
nmonths(edhec)

# Count the quarters
nquarters(edhec)

# Count the years
nyears(edhec)


## index tools
# In this exercise, you'll take a look at the weekend weather in temps data using 
# the .indexwday() command. Note that the values range from 0-6, with Sunday equal 
# to 0. Recall that you can use a logical vector to extract elements of an xts object
# temps, a time series of summer temperature data from Chicago, IL, USA.
data <- read.csv("chicagotemp.csv")
# Create dates as a Date class object starting from 2016-01-01
dates <- seq(as.Date("2016-07-01"), length = 16, by = "days")
dates <- as.POSIXct(dates+1) # added one for dates to on 7-1-2016 instead of 6-30-2016
# Use xts() to create temps
library(xts)
temps <- xts(x = data, order.by = dates)

# Explore underlying units of temps in two commands: .index() and .indexwday()
.index(temps)
.indexwday(temps)

# Create an index of weekend days using which()
index <- which(.indexwday(temps) == 0 | .indexwday(temps) == 6)

# Select the index
temps[index]


## Modifying timestamps
# make.index.unique(x, eps = 1e-4)  # Perturb
# make.index.unique(x, drop = TRUE) # Drop duplicates
# align.time(x, n = 60) # Round to the minute

# In this exercise, you'll try the three use cases on an xts object called z
# Create the object data using 306 random numbers
data <- rnorm(12) 
# Create 12 random posixct date objects starting from 2017-10-05
dates <- as.POSIXct(strptime(c("2017-10-05 01:30:00","2017-10-05 02:33:03",
                               "2017-10-05 02:33:03","2017-10-06 13:45:50",
                               "2017-10-06 18:20:32","2017-10-07 01:15:23",
                               "2017-10-08 09:35:10","2017-10-08 09:35:10",
                               "2017-10-08 21:34:09","2017-10-09 09:35:10",
                               "2017-10-10 11:34:09","2017-10-10 11:36:08"), 
                             "%Y-%m-%d %H:%M:%S"))

# Use xts() to create z
library(xts)
z <- xts(x = data, order.by = dates)

# Make z have unique timestamps. eps determines how much identical times should be perturbed,
z_unique <- make.index.unique(z,eps = 1e-4)

# Remove duplicate times in z
z_dup <- make.index.unique(z, drop = TRUE)

# Round observations in z to the next hour - using the number of seconds in one hour for n
z_round <- align.time(z, n = 3600)
