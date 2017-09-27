# The ses() function produces forecasts obtained using simple exponential 
# smoothing (SES). The parameters are estimated using least squares estimation. 
# All you need to specify is the time series and the forecast horizon; the default 
# forecast time is h = 10 years.

## Simple exponential smoothing
# Here, you will apply these functions to marathon, the annual winning times in 
# the Boston marathon from 1897-2016.
library(fpp2)
autoplot(marathon)

# Use ses() to forecast the next 10 years of winning times
fc <- ses(marathon, h = 10)

# Use summary() to see the model parameters
summary(fc)

# Use autoplot() to plot the forecasts
autoplot(fc)

# Add the one-step forecasts for the training data to the plot
autoplot(fc) + autolayer(fitted(fc))


## SES vs naive
# Create a training set using subset() comprising all but the last 20 years of 
# the data which you will reserve for testing.
train <- subset(marathon, end = length(marathon) - 20)

# Compute SES and naive forecasts, save to fcses and fcnaive
fcses <- ses(train, h = 20)
fcnaive <- naive(train, h = 20)

# Calculate forecast accuracy measures
accuracy(fcses, marathon)
accuracy(fcnaive, marathon)

# Save the best forecasts as fcbest
fcbest <- fcnaive
# More complex methods aren't always better


## Holt's trend methods
# Here, you will apply it to the austa series, which contains annual counts of 
# international visitors to Australia from 1980-2015 (in millions). 

# Produce 10 year forecasts of austa using holt()
fcholt <- holt(austa, h = 10)

# Look at fitted model using summary()
summary(fcholt)

# Plot the forecasts
autoplot(fcholt)

# Check that the residuals look like white noise
checkresiduals(fcholt)


## Holt-Winters with monthly data
# hw() function produces forecasts using the Holt-Winters method specific to 
# whatever you set equal to the seasonal argument.
# Here, you will apply hw() to a10, the monthly sales of anti-diabetic drugs in 
# Australia from 1991 to 2008.

# Plot the data
autoplot(a10)

# Produce 3 year forecasts
fc <- hw(a10, seasonal = "multiplicative", h = 36)

# Check if residuals look like white noise
checkresiduals(fc)
whitenoise <- FALSE # p-value significant (below 0.05)

# Plot forecasts
autoplot(fc)


## Holt-Winters method with daily data
# The Holt-Winters method can also be used for daily type of data, where the 
# seasonal pattern is of length 7, and the appropriate unit of time for h is in days.
# 
# Here, you will compare an additive Holt-Winters method and a seasonal naive() 
# method for the hyndsight data, which contains the daily pageviews on the 
# Hyndsight blog for one year starting April 30, 2014. 

# Using subset(), set up a training set where the last 4 weeks of the available 
# data in hyndsight have been omitted.
train <- subset(hyndsight, end = length(hyndsight)-28)

# Holt-Winters additive forecasts as fchw
fchw <- hw(train, seasonal = "additive", h = 28)

# Seasonal naive forecasts as fcsn
fcsn <- snaive(train, h = 28)

# Find better forecasts with accuracy()
accuracy(fchw, hyndsight)
accuracy(fcsn, hyndsight)

# Plot the better forecasts
autoplot(fchw)


## Automatic forecasting with exponential smoothing
# The namesake function for finding errors, trend, and seasonality (ETS) provides 
# a completely automatic way of producing forecasts for a wide range of time series.

#  You will now test it on two series, austa and hyndsight

# Fit ETS model to austa in fitaus
fitaus <- ets(austa)

# Check residuals
checkresiduals(fitaus)

# Plot forecasts
fitaus %>% forecast() %>% autoplot()

# Repeat for hyndsight data in fiths
fiths <- ets(hyndsight)
checkresiduals(fiths)
fiths %>% forecast() %>% autoplot()

# Which model(s) fails test? (TRUE or FALSE)
fitausfail <- FALSE
fithsfail <- TRUE


## ETS vs seasonal naive
# Here, you will compare ETS forecasts against seasonal naive forecasting for 20 
# years of cement, which contains quarterly cement production using time series 
# cross-validation for 4 steps ahead.
autoplot(fpp2::qcement)

# A function to return ETS forecasts, fets(), has been written for you.
fets <- function(y, h) {
  forecast(ets(y), h = h)
}

# Apply tsCV() for both ETS and seasonal naive methods to the cement data for a 
# forecast horizon of 4. Use the newly created fets and the existing snaive 
# functions as your forecast function argument for e1 and e2, respectively.
e1 <- tsCV(qcement, fets, h = 4)
e2 <- tsCV(qcement, snaive, h = 4)

# Compute the MSE of the resulting 4-step errors and remove missing values. 
mean(e1^2, na.rm = TRUE)
mean(e2^2, na.rm = TRUE)

# Copy the best forecast MSE
bestmse <- mean(e2^2, na.rm = TRUE)


## When does ETS fail?
# Computing the ETS does not work well for all series.
# Here, you will observe why it does not work well for the annual Canadian lynx population

# Plot the lynx series
autoplot(lynx)

# Use ets() to model the lynx series
fit <- ets(lynx)

# Use summary() to look at model and parameters
summary(fit)

# Plot 20-year forecasts of the lynx series
fit %>% forecast(h=20) %>% autoplot()
# It is important to realize that ETS does not work for all cases
