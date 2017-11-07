library(xts)

# Apply a function by time period(s)
# In this exercise you'll practice using period.apply() by taking the weekly mean 
# of your temps data. 
# temps, a time series of summer temperature data from Chicago, IL, USA.
data <- read.csv("chicagotemp.csv")
# Create dates as a Date class object starting from 2016-01-01
dates <- seq(as.Date("2016-07-01"), length = 16, by = "days")
# Use xts() to create temps
library(xts)
temps <- xts(x = data, order.by = dates)

# Calculate the weekly endpoints
ep <- endpoints(temps, on = "weeks")

# Now calculate the weekly mean and display the results
period.apply(temps[, 2], INDEX = ep, FUN = mean)


## period.apply() is similar to using a combination of split() and lapply()

# Split temps by week
temps_weekly <- split(temps, f = "weeks")

# Create a list of weekly means, temps_avg, and print this list
temps_avg <- lapply(X = temps_weekly, FUN = mean)
temps_avg


## Selection by endpoints vs. split-lapply-rbind
# It never hurts to know multiple methods for selecting certain points in your time series.

# temps, a time series of summer temperature data from Chicago, IL, USA.
data <- read.csv("chicagotemp.csv")
# Create dates as a Date class object starting from 2016-01-01
dates <- seq(as.Date("2016-07-01"), length = 16, by = "days")
# Use xts() to create temps
library(xts)
temps <- xts(x = data, order.by = dates)

# Method-1
# Use the split()-lapply()-rbind() paradigm, given for you in the script, to find 
# the last observation in each week in temps. It is stored in temps_1
temps_1 <- do.call(rbind, lapply(split(temps, "weeks"), function(w) last(w, n = "1 day")))

# Method-2
# Create last_day_of_weeks using endpoints()
last_day_of_weeks <- endpoints(temps, on = "weeks")

# Subset temps using last_day_of_weeks 
temps_2 <- temps[last_day_of_weeks]


## Convert univariate series to OHLC data
# In financial series it is common to find Open-High-Low-Close data (or OHLC) 
# calculated over some repeating and regular interval.
# Also known as range bars, aggregating a series based on some regular window can 
# make analysis easier amongst series that have varying frequencies. A weekly 
# economic series and a daily stock series can be compared more easily if the daily 
# is converted to weekly.
# In this exercise, you'll convert from a univariate series into OHLC series, and 
# then convert your final OHLC series back into a univariate series using the xts 
# function to.period(). 

# usd_eur, a daily USD/EUR exchange rate from 1999 to August 2016
df<- read.csv("usdeur.csv", header = FALSE)
df[,1] <- as.POSIXct(df[,1]  ,format ="%d-%b-%y")
dates <- df$V1
data <- df[,2]
library(xts)
usd_eur <- xts(x = data, order.by = dates)

# Note that by default OHLC = TRUE
# Convert usd_eur to weekly and assign to usd_eur_weekly
usd_eur_weekly <- to.period(usd_eur, period = "weeks")

# Convert usd_eur to monthly and assign to usd_eur_monthly
usd_eur_monthly <- to.period(usd_eur, period = "months")

# Convert usd_eur to yearly univariate and assign to usd_eur_yearly
# No OHLC bars
usd_eur_yearly <- to.period(usd_eur, period = "years", OHLC = FALSE)


## Convert a series to a lower frequency
# Besides converting univariate time series to OHLC series, to.period() also lets 
# you convert OHLC to lower regularized frequency - something like subsampling your data.

# For this exercise we'll introduce a new dataset, the edhec hedge fund index data 
# from the PerformanceAnalytics package.
# In this exercise you will use the Equity Market Neutral time series from the 
# edhec data, which we've assigned to eq_mkt
library(PerformanceAnalytics)
data(edhec)
chart.Histogram(edhec[,5,drop=FALSE])
# version with more breaks and the standard close fit density distribution
chart.Histogram(edhec[,5,drop=FALSE], breaks=40, methods = c("add.density", "add.rug") )
chart.Histogram(edhec[,5,drop=FALSE], methods = c( "add.centered") )
# version with just the histogram and normal distribution centered on 0
chart.Histogram(edhec[,5,drop=FALSE], methods = c( "add.centered") )
# version with histogram, normal, and close fit distribtuion
chart.Histogram(edhec[,5,drop=FALSE], methods = c( "add.centered", "add.density") )
# add a rug to the previous plot for more granularity on precisely where the distribution fell
chart.Histogram(edhec[,5,drop=FALSE], methods = c( "add.centered", "add.density", "add.rug") )
# now show a qqplot to give us another view on how normal the data are
chart.Histogram(edhec[,5,drop=FALSE], methods = c( "add.centered", "add.density", "add.rug", "add.qqplot") )
# add risk measure(s) to show where those are in relation to observed returns
chart.Histogram(edhec[,5,drop=FALSE], methods = c("add.density", "add.centered", "add.rug", "add.risk") )

eq_mkt <- edhec[,5]

# Convert eq_mkt to quarterly OHLC using the base to.period()
mkt_quarterly <- to.period(eq_mkt, period = "quarters")

# Depending on the chosen frequency, the index class of your data may be coerced 
# to something more appropriate to the new data. For example, when using the 
# shortcut function to.quarterly(), xts will convert your index to the yearqtr 
# class to make periods more obvious.
# We can override this behavior by using the indexAt argument. Specifically, using 
# firstof would give you the time from the beginning of the period. In addition, 
# you can change the base name of each column by supplying a string to the argument name.

# Convert the original eq_mkt again, this time using to.quarterly() directly. 
# Change the base name of each OHLC column to edhec_equity and change the index to "firstof"
mkt_quarterly2 <- to.quarterly(eq_mkt, name = "edhec_equity", indexAt = "firstof")


## Calculate basic rolling value of series by month
# One common aggregation you may want to apply involves doing a calculation within 
# the context of a period, but returning the interim results for each observation 
# of the period.
# For example, you may want to calculate a running month-to-date cumulative sum of a 
# series. This would be relevant when looking at monthly performance of a mutual fund 
# you are interested in investing in.

# For this exercise, you'll calculate the cumulative annual return using the edhec fund data 
library(PerformanceAnalytics)
data(edhec)

# Split edhec into years
edhec_years <- split(edhec , f = "years")

# Use lapply to calculate the cumsum for each year in edhec_years
edhec_ytd <- lapply(edhec_years, FUN = cumsum)

# Use do.call to to convert your previous list output to a single xts object
edhec_xts <- do.call(rbind, edhec_ytd)


## Calculate the rolling standard deviation of a time series
# Using rollapply(), calculate the 3-month standard deviation of the eq_mkt series. 
# Note that eq_mkt has monthly observations. 
# Call your 3-month rolling standard deviation eq_sd.
library(PerformanceAnalytics)
data(edhec)
eq_mkt <- edhec[,5]
eq_sd <- rollapply(eq_mkt, width = 3, FUN = sd)
