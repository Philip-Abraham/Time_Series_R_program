## XTS - More than a matrix
# It is best to think of xts objects as normal R matrices, but with special powers. 
# These powers let you manipulate your data as a function of time, as your data is 
# now self-aware of when it exists in time. 

library(xts)

# An xts example
# xts = matrix + index
# xts is a matrix with associated times for each observation

x <- matrix(1:4, ncol = 2, nrow = 2) # x must be a vector or matrix.
x

# order.by is a vector which must be the same length or number of rows as x, be a 
# proper time or date object (very important!), and be in increasing order.
idx <- as.Date(c("2015-01-01", "2015-02-01")) 

X <- xts(x, order.by = idx)
X
str(X)


# xts also allows you to bind arbitrary key-value attributes to your data. This 
# lets you keep metadata about your object inside your object. To add these at 
# creation, you simply pass additional name = value arguments to the xts() function.

# Create the object data using 5 random numbers
data <- rnorm(5)

# Create dates as a Date class object starting from 2016-01-01
dates <- seq(as.Date("2016-01-01"), length = 5, by = "days")

# Use xts() to create smith
smith <- xts(x = data, order.by = dates)

# Create bday (1899-05-08) using a POSIXct date class object
bday <- as.POSIXct("1899-05-08")

# Create hayek and add a new attribute called born
hayek <- xts(x = data, order.by = dates, born = bday)


## Deconstructing xts
# Now that you know how to create a very simple xts object, you are ready to see 
# what the object looks like inside.
# When working with time series, it will sometimes be necessary to separate your 
# time series into its core data and index attributes for additional analysis and 
# manipulation. 

# Extract the core data of hayek
hayek_core <- coredata(hayek)

# View the class of hayek_core
class(hayek_core)

# Extract the index of hayek
hayek_index <- index(hayek)

# View the class of hayek_index
class(hayek_index)


## Time based indices
# For this exercise you'll create two time series using two different time classes. 
# You will then subset each object using the other object's index.

# Create an object of 5 dates called dates starting at "2016-01-01".
dates <- as.Date("2016-01-01") + 0:4

# Create a time series ts_a using the numbers 1 through 5 as your data, and dates as your order.by index.
ts_a <- xts(x = 1:5, order.by = dates)

# Create a time series ts_b using the numbers 1 through 5 as your data, and the same dates, but as POSIXct objects.
ts_b <- xts(x = 1:5, order.by = as.POSIXct(dates))

# Use the index from ts_b to extract the dates from ts_a.
ts_a[index(ts_b)]

# Now do the reverse, indexing ts_b using the times from ts_a.
ts_b[index(ts_a)]


## Converting xts objects
# To get a feel for moving data between classes, let's try a few examples using 
# the Australian population ts object from R named austres

# Convert austres to an xts object called au
au <- as.xts(austres)

# Then convert your xts object (au) into a matrix am
am <- as.matrix(au)

# Inspect the head of am
head(am)

# Convert the original austres into a matrix am2
am2 <- as.matrix(austres)

# Inspect the head of am2
head(am2)


## Importing data

# Create dat by reading tmp_file
tmp_file <- "http://s3.amazonaws.com/assets.datacamp.com/production/course_1127/datasets/tmp_file.csv"
dat <- read.csv(tmp_file)

# Convert dat into xts
xts(dat, order.by = as.Date(rownames(dat), "%m/%d/%Y"))

# Read tmp_file using read.zoo
dat_zoo <- read.zoo(tmp_file, index.column = 0, sep = ",", format = "%m/%d/%Y")

# Convert dat_zoo to xts
dat_xts <- as.xts(dat_zoo)


## Exporting xts objects

# Convert sunspots to xts using as.xts().
sunspots_xts <- as.xts(sunspots)

# Get the temporary file name
tmp <- tempfile()

# Write the xts object using zoo to tmp 
write.zoo(sunspots_xts, sep = ",", file = tmp)

# Read the tmp file. FUN = as.yearmon converts strings such as Jan 1749 into a proper time class
sun <- read.zoo(tmp, sep = ",", FUN = as.yearmon)

# Convert sun into xts. Save this as sun_xts
sun_xts <- as.xts(sun)
