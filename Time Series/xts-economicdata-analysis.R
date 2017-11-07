## How Boston's tourism industry is affected by economic trends?

# Load gross domestic product (GDP) data for US
gdp <- readRDS('us_gdp.RData')

# Get a summary of your GDP data
summary(gdp)
# Data contains 80 missing data points

# Convert GDP date column to time object
library(xts)
gdp$date <- as.yearqtr(gdp$date)

# Convert GDP data to xts
gdp_xts <- as.xts(gdp[, -1], order.by =gdp$date)

# Plot GDP data over time
plot.xts(gdp_xts)
# Plot shows fairly consistent GDP growth in the United States. However, it looks 
# like it is missing quite a bit of data!


## Replace missing data

# Fill NAs in gdp_xts with the last observation carried forward
gdp_locf <- na.locf(gdp_xts)

# Fill NAs in gdp_xts with the next observation carried backward 
gdp_nocb <- na.locf(gdp_xts, fromLast = TRUE)

# Produce a plot for each of your new xts objects
par(mfrow = c(2,1))
plot.xts(gdp_locf, major.format = "%Y")
plot.xts(gdp_nocb, major.format = "%Y")

# Query for GDP in 1993 in both gdp_locf and gdp_nocb
gdp_locf["1993"]
gdp_nocb["1993"]
# As you can see, the locf and nocb techniques sometimes produce drastically 
# different values, especially when you have long spells of missing data
# locf is more conservative and nocb is a more aggressive, both generate step-wise 
# growth from missing data

# If you have reason to expect linear growth in your data? In this case, it may be 
# more useful to use linear interpolation, which generates new values between the 
# data on either end of the missing value weighted according to time

# Fill NAs in gdp_xts using linear approximation
gdp_approx <- na.approx(gdp_xts)

# Plot your new xts object
plot.xts(gdp_approx, major.format = "%Y")

# Query for GDP in 1993 in gdp_approx
gdp_approx["1993"]
# Although the values you generate may not be 100% accurate, linear interpolation 
# provides a realistic overall picture of GDP growth from the 1940s through the 
# 2010s. Ultimately, which technique you should use depends on the trends you see 
# in the data you have as well as your preconceived notions about the data
# Linear interpolation method will not detect sudden shifts in GDP from quarter to 
# quarter, but can provide a general approximation of trends


## Exploring unemployment data
# In this exercise, you'll gain a bit more practice by exploring, cleaning, and 
# plotting data on unemployment, both in the United States in general and in 
# Massachusetts (MA) in particular

# Load unemployment xts data for MA
unemployment <- readRDS('unemployment.RData')

# View a summary of your unemployment data
summary(unemployment)

# Use na.approx(linear interpolation) to remove missing values in unemployment data
library(xts)
unemployment <- na.approx(unemployment)

# Plot new unemployment data
lty <- c(1,2)
plot.zoo(unemployment, plot.type = "single", lty = lty)
labels <- c("US Unemployment (%)", "MA Unemployment (%)")
legend("topright", lty = lty, legend = labels, bg = "white")


## Lagging unemployment
# Given that economic trends may take some time to influence tourism, it may be 
# helpful to lag your unemployment data before proceeding with analysis
# The xts package specifies lags using a positive value, so that a lag of 1 is 
# expressed using "1" (and a lead of 1 is expressed using "-1")

library(xts)
# Create a one month lag of US unemployment
us_monthlag <- lag(unemployment$us, k = 1)

# Create a one year lag of US unemployment
us_yearlag <- lag(unemployment$us, k = 12)

# Merge your original data with your new lags 
unemployment_lags <- merge(unemployment, us_monthlag,us_yearlag)

# View the first 15 rows of unemployment_lags
head(unemployment_lags, n=15)
# lags generated through this process are not intuitively labelled in your data


## Differencing unemployment
# Differencing provides a very intuitive way to visualize growth trends over time

# Generate monthly difference in unemployment
unemployment$us_monthlydiff <- diff(unemployment$us, lag = 1, differences = 1)

# Generate yearly difference in unemployment
unemployment$us_yearlydiff <- diff(unemployment$us, lag = 12, differences = 1)

# Plot US unemployment and annual difference
par(mfrow = c(2,1))
plot.xts(unemployment$us)
plot.xts(unemployment$us_yearlydiff, type = "h")


## Add a discrete rolling sum to GDP data
# While it helps to know the amount of change from one period to the next, you may 
# want to know the total change since the beginning of the year.
# In this exercise, you'll return to the gdp data. In addition to static GDP values
# in each quarter, you'd like to generate a measure of GDP change from one quarter 
# to the next.
# Load gross domestic product (GDP) data for US
gdp <- readRDS('us_gdp.RData')
# Convert GDP date column to time object
library(xts)
gdp$date <- as.yearqtr(gdp$date)
# Convert GDP data to xts
gdp_xts <- as.xts(gdp[, -1], order.by =gdp$date)
# Fill NAs in gdp_xts using linear approximation
gdp <- na.approx(gdp_xts)
colnames(gdp) <- c("gdp")
# Plot your new xts object
plot.xts(gdp, major.format = "%Y")

# Add a quarterly difference in gdp
gdp$quarterly_diff <- diff(gdp$gdp, lag = 1, differences = 1)

# Split gdp$quarterly_diff into years
gdpchange_years <- split(gdp$quarterly_diff, f = "years")

# Use lapply to calculate the cumsum each year
gdpchange_ytd <- lapply(gdpchange_years, FUN = cumsum)

# Use do.call to rbind the results
gdpchange_xts <- do.call(rbind, gdpchange_ytd)

# Plot cumulative year-to-date change in GDP
plot.xts(gdpchange_xts, type = "h")
# In the plot, each bar shows cumulative GDP growth since the beginning of that year


## Add a continuous rolling average to unemployment data
# In addition to discrete measures such as year-to-date sums, you may be interested 
# in adding a rolling sum or average to your time series data.
# To do so, let's return to your monthly unemployment data. While you may be 
# interested in static levels of unemployment in any given month, a broader picture 
# of the economic environment might call for rolling indicators over several months.

# Load unemployment xts data for MA
unemployment <- readRDS('unemployment.RData')
# Use na.approx(linear interpolation) to remove missing values in unemployment data
library(xts)
unemployment <- na.approx(unemployment)

# Use rollapply to calculate the rolling yearly average US unemployment
unemployment$year_avg <- rollapply(unemployment$us, width = 12, FUN = mean)

# Plot all columns of US unemployment data
lty <- c(2,1)
lwd <- c(1,2)
plot.zoo(unemployment[, c("us", "year_avg")], plot.type = "single", lty = lty, lwd = lwd)
# Your rolling average helps smooth out some of the short term changes in unemployment 
# from month to month and provides a broader picture of the health of the US economy.


## Manipulating MA unemployment data
# In this exercise, you'll to generate: a one-year lag, a six-month first order 
# difference, a six-month rolling average, and a one-year rolling maximum in the 
# MA unemployment rate. 

library(xts)
# Add a one-year lag of MA unemployment
unemployment$ma_yearlag <- lag(unemployment$ma, k=12)

# Add a six-month difference of MA unemployment
unemployment$ma_sixmonthdiff <- diff(unemployment$ma, lag = 6, differences = 1)

# Add a six-month rolling average of MA unemployment
unemployment$ma_sixmonthavg <- rollapply(unemployment$ma, width = 6, FUN = mean)

# Add a yearly rolling maximum of MA unemployment
unemployment$ma_yearmax <- rollapply(unemployment$ma, width = 12, FUN = max)

# View the last year of unemployment data
last(unemployment,"1 year")
