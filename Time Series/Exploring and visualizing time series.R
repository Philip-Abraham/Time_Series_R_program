## Time series plots
library(forecast)
library(ggplot2)

# Plot the gold, woolyrnq, and gas time series in separate plots
# Plot the three series
autoplot(gold)
autoplot(woolyrnq)
autoplot(gas)

# Find the outlier in the gold series
goldoutlier <- which.max(gold)

# Look at the seasonal frequencies of the three series
frequency(gold)
frequency(woolyrnq)
frequency(gas)


## Seasonal plots

# a10 contains monthly sales volumes for anti-diabetic drugs in Australia. In the 
# plots, can you see which month has the highest sales volume each year? What is 
# unusual about the results in March and April 2008?

# Load the fpp2 package
library(fpp2)

# Create plots of the a10 data
autoplot(a10)
ggseasonplot(a10)

# Produce a polar coordinate season plot for the a10 data
ggseasonplot(a10, polar = TRUE)

# ausbeer contains quarterly beer production for Australia. 
# Restrict the ausbeer data to start in 1992
# One way of splitting a time series is by using window() function, which extracts 
# a subset from the object x observed between the times start and end.
beer <- window(ausbeer, start = 1992, end = NULL)

# Make plots of the beer data
autoplot(beer)
ggsubseriesplot(beer)


## Autocorrelation of non-seasonal time series
# Oil data, which contains the annual oil production in Saudi Arabia from 
# 1965-2013 (measured in millions of tons).

# # Create an autoplot of the oil data
autoplot(oil)

# Create a lag plot of the oil data
gglagplot(oil)

# Create an ACF plot of the oil data
ggAcf(oil)


## Autocorrelation of seasonal and cyclic time series

# annual sunspot series (which follows the solar cycle of approximately 10-11 
# years) in sunspot.year
# Plot the annual sunspot numbers
autoplot(sunspot.year)
ggAcf(sunspot.year)
# The sunspot data are annual, so they cannot be seasonal. The cycles are aperiodic 
# of variable length.
# The ACF plots show peaks at seasonal lags and at the average cycle length.
# The ACF plots show positive and decreasing spikes when the series is trended.

# Daily traffic to the Hyndsight blog (which follows a 7-day weekly pattern) in hyndsight
# Plot the annual sunspot numbers
autoplot(hyndsight)
ggAcf(hyndsight)


## Stock prices and white noise
# goog series, contains the closing stock price for Google over 1000 trading 
# days ending on February 13, 2017.

# White noise is a term that describes purely random data. You can conduct a 
# Ljung-Box test using the function below to confirm the randomness of a series; 
# a p-value greater than 0.05 suggests that the data are not significantly 
# different from white noise.

# Plot the original series
autoplot(goog)
# Plot the differenced series
autoplot(diff(goog))

# ACF of the differenced series
ggAcf(diff(goog))

# Ljung-Box test of the differenced series
Box.test(diff(goog), lag = 10, type = "Ljung")
# Ljung-Box test was not significant so daily changes in Google stock prices look like white noise


