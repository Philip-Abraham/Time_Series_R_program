## Flight Data
# Your task is to understand the travel patterns of tourists visiting Boston
flights <- readRDS('flights.RData')

#View the structure of the flights data
str(flights)

#Examine the first five rows of the flights data
head(flights, n = 5)

#Identify class of the column containing date information
class(flights$date)
# The date column is currently a character string. You'll need to convert this 
# into a time-based object before moving forward with time series analysis

# Xts objects have the functionality of a simple matrix while containing an index 
# allowing easy manipulation over time


## Encoding your flight data
# flights is a data frame containing four columns of flight data and one column of dates
# To convert to an xts object, you'll need to ensure that your date column is in 
# a time-based format

# Load the xts package
library(xts)

# Convert date column to a time-based class
flights$date <- as.Date(flights$date)

# Convert flights to an xts object using as.xts
flights_xts <- as.xts(flights[ , -5], order.by = flights$date)

# Check the class of flights_xts
class(flights_xts)

# Examine the first five lines of flights_xts
head(flights_xts, n=5)
# Your new xts object contains four columns of information about flights indexed 
# on a series of months and years


## Exploring your flight data
# Before any analysis can be done, it is critical to explore the basic qualities 
# of your data, including periodicity, scope, and comprehensiveness

# Identify the periodicity of flights_xts
periodicity(flights_xts)

# Identify the number of periods in flights_xts
nmonths(flights_xts)

# Find data on flights arriving in BOS in June 2014
flights_xts['2014-06']


## Visualize flight data

# Use plot.xts() to view total monthly flights into BOS over time
plot.xts(flights_xts$total_flights)

# Use plot.xts() to view monthly delayed flights into BOS over time
plot.xts(flights_xts$delay_flights)

# For more complicated plots, you may want to use plot.zoo(), which allows you to 
# include multiple columns of data
# Use plot.zoo() to view all four columns of data in their own panels
labels <- c("Total", "Delay","Cancel", "Divert")
plot.zoo(flights_xts, plot.type = "multiple", ylab = labels)

# Use plot.zoo() to view all four columns of data in one panel
lty <- c(1,2,3,4)
plot.zoo(flights_xts, plot.type = "single", lty = lty)
legend("right", lty = lty, legend = labels)
#  It looks like only a small percentage of flights are delayed, cancelled, or diverted


## Calculate time series trends

# Calculate percentage of flights delayed each month: pct_delay
flights_xts$pct_delay <- (flights_xts$delay_flights / flights_xts$total_flights) * 100

# Use plot.xts() to view pct_delay over time
plot.xts(flights_xts$pct_delay)

# Calculate percentage of flights cancelled each month: pct_cancel
flights_xts$pct_cancel <- (flights_xts$cancel_flights / flights_xts$total_flights) * 100

# Calculate percentage of flights diverted each month: pct_divert
flights_xts$pct_divert <-  (flights_xts$divert_flights / flights_xts$total_flights) * 100

# Use plot.zoo() to view all three trends over time
plot.zoo(x = flights_xts[ , c("pct_delay", "pct_cancel", "pct_divert")])
# plot suggests that the percentage of flight cancellations spikes around December 
# and January each year, with the exception of 2012


## Saving time series
# As a first step, you'll want to save your xts object as a rds file for your own use
# Save your xts object to rds file using saveRDS
# As you'll see in this exercise, this method maintains the class of your xts object
saveRDS(object = flights_xts, file = "flights_xts.rds")

# Read your flights_xts data from the rds file
flights_xts2 <- readRDS("flights_xts.rds")

# Check the class of your new flights_xts2 object
class(flights_xts2)

# Examine the first five rows of your new flights_xts2 object
head(flights_xts2, n=5)
# As you can see, saving to rds files allows you to maintain the class of your 
# data object. However, rds files are difficult to read into programs other than R
# A second option for saving xts objects is to convert them to shareable formats 
# beyond the R environment, including comma-separated values (CSV) files.
# Once you've succesfully exported your xts object to a csv file, you can load the 
# data back into R using the read.zoo() command. Unlike readRDS, however, you will 
# need to re-encode your data to an xts object (using as.xts)

# Export your xts object to a csv file using write.zoo
write.zoo(flights_xts, file = "flights_xts.csv", sep = ",")

# Open your saved object using read.zoo
flights2 <- read.zoo("flights_xts.csv", sep = ",", FUN = as.Date, header = TRUE, index.column = 1)

# Encode your new object back into xts
flights_xts2 <- as.xts(flights2)

# Examine the first five rows of your new flights_xts2 object
head(flights_xts2, n=5)

# save xts object locally on your pc
save(flights_xts, file = "flights_xts.RData")
