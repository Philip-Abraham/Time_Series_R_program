data("Nile")
print(Nile)

# List the number of observations in the Nile dataset
length(Nile)

# Display the first 10 elements of the Nile dataset
head(Nile, 10)

# Display the last 12 elements of the Nile dataset
tail(Nile, 12)

# The start() and end() functions return the time index of the first and last 
# observations, respectively. The time() function calculates a vector of time 
# indices, with one element for each time index on which the series was observed.
# 
# The deltat() function returns the fixed time interval between observations and 
# the frequency() function returns the number of observations per unit time. 
# Finally, the cycle() function returns the position in the cycle of each observation.

# Gives info on the first observation value of the time series
start(Nile)

# Gives info on the last observation value of the time series
end(Nile)

# frequency info
frequency(Nile)

# delta
deltat(Nile)

# cycle
cycle(Nile)

# Plot the Nile data
plot(Nile, xlab = "Year", ylab = "River Volume (1e9 m^{3})",
     main = "Annual River Nile Volume at Aswan, 1871-1970", type ="b")


################################################################################

data("AirPassengers")

# Plot AirPassengers
plot(AirPassengers)

# The start() and end() functions return the time index of the first and last 
# observations, respectively. The time() function calculates a vector of time 
# indices, with one element for each time index on which the series was observed.
# 
# The deltat() function returns the fixed time interval between observations and 
# the frequency() function returns the number of observations per unit time. 
# Finally, the cycle() function returns the position in the cycle of each observation.

# View the start and end dates of AirPassengers
start(AirPassengers)
end(AirPassengers)

# Use time(), deltat(), frequency(), and cycle() with AirPassengers 
time(AirPassengers)
deltat(AirPassengers)
frequency(AirPassengers)
cycle(AirPassengers)

################################################################################

## Building ts() Objects
# Why create and use time series objects of the ts() class?
# Improved plotting
# Access to time index information
# Model estimation and forecasting

# Start with a vector of data
data_vector <- c(10, 6, 11, 8, 10, 3, 6, 9)

# Apply the ts() function
time_series <- ts(data_vector)
plot(time_series)

# Check whether data_vector and time_series are ts objects
is.ts(data_vector)
is.ts(time_series)

# Specify the start date and observation frequency = 1 observations/year
time_series1 <- ts(data_vector, start = 2001, frequency = 1)
print(time_series1)
plot(time_series1)

# Specify the start date and observation frequency = 4 observations/year
time_series4 <- ts(data_vector, start = 2001, frequency = 4)
print(time_series4)
plot(time_series4)
# As you can see, ts objects are treated differently by commands such as print() 
# and plot(). For example, automatic use of the time-index in your calls to plot() 
# requires a ts object

################################################################################

## Plotting a time series object

data(EuStockMarkets)
# This dataset contains daily closing prices of major European stock indices from 
# 1991-1998, specifically, from Germany (DAX), Switzerland (SMI), France (CAC), 
# and the UK (FTSE). The data were observed when the markets were open, so there 
# are no observations on weekends and holidays. We will proceed with the 
# approximation that this dataset has evenly spaced observations and is a four 
# dimensional time series.

# Check whether eu_stocks is a ts object
is.ts(EuStockMarkets)

# View the start, end, and frequency of eu_stocks
start(EuStockMarkets)
end(EuStockMarkets)
frequency(EuStockMarkets)

# Generate a simple plot of eu_stocks
plot(EuStockMarkets)

# Use ts.plot with eu_stocks
ts.plot(EuStockMarkets, col = 1:4, xlab = "Year", ylab = "Index Value", main = "Major European Stock Indices, 1991-1998")

# Add a legend to your ts.plot
legend("topleft", colnames(EuStockMarkets), lty = 1, col = 1:4, bty = "n")
