## Representing a univariate time series
# The very first step in the analysis of any time series is to address if the time 
# series have the right mathematical properties to apply the standard statistical 
# framework. If not, you must transform the time series first.
# In finance, price series are often transformed to differenced data, making it a 
# return series. 
# In R, the ROC() (which stands for "Rate of Change") function from the TTR package 
# does this automatically to a price or volume series x

# In this exercise, you will compare plots of the Apple daily prices and Apple 
# daily returns

# load csv
df <- read.csv("dataset_2_1.csv", header = TRUE, sep = " ")

# Create date index
dates <- as.Date(df$Index)

library(xts)
# Create xts object
data <- as.xts(df[,-1], order.by = dates)
names(data) <- c("apple")

# Plot Apple's stock price 
plot(data, main ="Apple stock price")

# Create a time series called rtn containing Apple's daily returns
library(TTR)
rtn <- ROC(data)

# Plot Apple daily price and daily returns 
par(mfrow = c(2, 1))
plot(data)
plot(rtn)


## Histogram of returns
# The density function, represented by the histogram of returns, indicates the 
# most common returns in a time series without taking time into account.

# Draw the histogram of rtn titled "Apple stock return distribution" and with 
# probability = TRUE to scale the histogram to a probability density
par(mfrow = c(1, 1)) # reset plot window
hist(rtn,probability = TRUE, main = "Apple stock return distribution")

# Add a density line
lines(density(rtn[-1,]),col="red", lwd=2) # removed first row of rtn because of NA value
# It looks like Apple might have some extreme returns!


## Box and whisker plot
# A box and whisker plot gives information regarding the shape, variability, and 
# center (or median) of a data set. It is particularly useful for displaying skewed data.
# By comparing the data set to a standard normal distribution, you can identify 
# departure from normality (asymmetry, skewness, etc).

# Draw box and whisker plot for the Apple returns, and
# Draw a box and whisker plot of a normal distribution
# Redraw both plots horizontally on the same graphical window
rtn <- as.numeric(rtn[-1,])
par(mfrow = c(2, 1))
boxplot(rtn, horizontal = TRUE)
boxplot(rnorm(1000), horizontal = TRUE)


## Autocorrelation
# Another important piece of information is the relationship between one point in 
# the time series and points that come before it. This is called autocorrelation 
# and it can be displayed as a chart which indicates the correlation between points 
# separated by various time lags.

# In this exercise, you will create an autocorrelation plot of the Apple stock 
# price return data 

# Draw autocorrelation plot
# In R, the autocorrelation function displays the first 30 lag by default
acf(rtn,main = "Apple return autocorrelation")

# Redraw with a maximum lag of 10
acf(rtn,main = "Apple return autocorrelation", lag.max = 10)


## q-q plot
# A q-q plot is a plot of the quantiles of one dataset against the quantiles of a 
# second dataset. This is often used to understand if the data matches the standard 
# statistical framework, or a normal distribution.
# This is useful to check for normality at a glance but note that it is not an accurate statistical test.

# In the context of this exercise, the first dataset is Apple stock return and the 
# second dataset is a standard normal distribution. In this exercise, you will 
# check how Apple stock returns in rtn deviate from a normal distribution.

# Create q-q plot
qqnorm(rtn, main = "Apple return QQ-plot")

# Add a red line showing normality
qqline(rtn, col = "red")
# It does not look like Apple returns fit a normal distribution very well in the tails

