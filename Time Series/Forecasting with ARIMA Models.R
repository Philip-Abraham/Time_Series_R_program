# Box-Cox transformations for time series
# Here, you will use a Box-Cox transformation to stabilize the variance of the 
# pre-loaded a10 series, which contains monthly anti-diabetic drug sales in 
# Australia from 1991-2008.


library(fpp2)

# In this exercise, you will need to experiment to see the effect of the lambda (??) 
# argument on the transformation. Notice that small changes in ?? (???1????????1) make little 
# difference to the resulting series. You want to find a value of ?? that makes 
# the seasonal fluctuations of roughly the same size across the series.

# Plot the series
autoplot(a10)

# Try some values of lambda in Box-Cox transformations
a10 %>% BoxCox(lambda =-1) %>% autoplot()
a10 %>% BoxCox(lambda =1) %>% autoplot()
a10 %>% BoxCox(lambda =-0.5) %>% autoplot()
a10 %>% BoxCox(lambda =0.5) %>% autoplot()
a10 %>% BoxCox(lambda =-0.3) %>% autoplot()
a10 %>% BoxCox(lambda =-0.2) %>% autoplot()

# Compare with BoxCox.lambda()
BoxCox.lambda(a10)


## Non-seasonal differencing for stationarity
# Differencing is a way of making a time series stationary; this means that you 
# remove any systematic patterns such as trend and seasonality from the data. 
# A white noise series is considered a special case of a stationary time series.

# wmurders data, which contains the annual female murder rate in the US from 1950-2004.

# Plot the US female murder rate
autoplot(wmurders)

# Plot the differenced murder rate
autoplot(diff(wmurders))

# Plot the ACF of the differenced murder rate
ggAcf(diff(wmurders))
# the data looks like white noise after differencing


## Seasonal differencing for stationarity
# The data set here is h02, which contains 17 years of monthly corticosteroid 
# drug sales in Australia.

# Plot the data
autoplot(h02)

# Take logs and seasonal differences of h02
difflogh02 <- diff(log(h02), lag = 12)

# Plot difflogh02
autoplot(difflogh02)

# Because difflogh02 still looks non-stationary, take another lag-1 difference 
# by applying diff() to itself and save this to ddifflogh02. 
# Plot the resulting series.
ddifflogh02 <- diff(difflogh02)
autoplot(ddifflogh02)

# Plot ACF of ddifflogh02
ggAcf(ddifflogh02)
# The data does not look white noise after transformation, but you could develop
# an ARIMA model for it


## Automatic ARIMA models for non-seasonal time series
# In this exercise, you will automatically choose an ARIMA model for the pre-loaded 
# austa series, which contains the annual number of international visitors to 
# Australia from 1980-2015. You will then check the residuals (recall that a p-value 
# greater than 0.05 indicates that the data resembles white noise) and produce some forecasts. 

# Fit an automatic ARIMA model to the austa series
fit <- auto.arima(austa)

# Check that the residuals look like white noise
checkresiduals(fit)

# Summarize the model
summary(fit)

# Find the AICc value and the number of differences used
AICc <- -14.46
d <- 1

# Plot forecasts of the next 10 periods from the chosen model.
fit %>% forecast(h = 10) %>% autoplot()


## Forecasting with ARIMA models
# In the examples here, watch for how the different models affect the forecasts 
# and the prediction intervals on the austa data

# Plot forecasts from an ARIMA(0,1,1) model with no drift
austa %>% Arima(order = c(0, 1, 1), include.constant = FALSE) %>% forecast() %>% autoplot()

# Plot forecasts from an ARIMA(2,1,3) model with drift
austa %>% Arima(order = c(2, 1, 3), include.constant = TRUE) %>% forecast() %>% autoplot()

# Plot forecasts from an ARIMA(0,0,1) model with a constant.
austa %>% Arima(order = c(0, 0, 1), include.constant = TRUE) %>% forecast() %>% autoplot()

# Plot forecasts from an ARIMA(0,2,1) model with no constant.
austa %>% Arima(order = c(0, 2, 1), include.constant = FALSE) %>% forecast() %>% autoplot()


## Comparing auto.arima() and ets() on non-seasonal data
# In this exercise, you will compare the MSE of two forecast functions applied to 
# austa, and plot forecasts of the function that computes the best forecasts.

# Set up forecast functions for ETS and ARIMA models
fets <- function(x, h) {
        forecast(ets(x), h = h)
}
farima <- function(x, h) {
        forecast(auto.arima(x), h = h)
}

# Compute CV errors for ETS as e1
e1 <- tsCV(austa, fets, h=1)

# Compute CV errors for ARIMA as e2
e2 <- tsCV(austa, farima, h=1)

# Find MSE of each model class
mean(e1^2, na.rm=TRUE)
mean(e2^2, na.rm=TRUE)

# Plot 10-year forecasts using the best model class which in this case is based on the ARIMA model
austa %>% farima(h = 10) %>% forecast() %>% autoplot()


## Automatic ARIMA models for seasonal time series
# In this exercise, you will use these functions to model and forecast the h02 
# data, which contains monthly sales of cortecosteroid drugs in Australia.

# Check that the logged h02 data have stable variance
h02 %>% log() %>% autoplot()

# Fit a seasonal ARIMA model to h02 with lambda = 0
fit <- auto.arima(h02, lambda = 0)

# Summarize the fitted model
summary(fit)

# Record the amount of lag-1 differencing and seasonal differencing used
d <- 1
D <- 1

# Plot 2-year forecasts
fit %>% forecast(h=24) %>% autoplot()


## Exploring auto.arima() options
# To make auto.arima() work harder to find a good model, add the optional argument 
# stepwise = FALSE to look at a much larger collection of models.
# Here, you will try finding an ARIMA model for the euretail data, which contains 
# quarterly retail trade in the Euro area from 1996-2011
autoplot(euretail)

# Use the default options in auto.arima() to find an ARIMA model for euretail
fit1 <- auto.arima(euretail)

# Don't use a stepwise search
fit2 <- auto.arima(euretail,stepwise = FALSE)

# AICc of better model
AICc <- 68.39

# Compute 2-year forecasts from better model
fit2 %>% forecast(h=8) %>% autoplot()


## Comparing auto.arima() and ets() on seasonal data
# You will compare seasonal ARIMA and ETS models applied to the quarterly cement 
# production data qcement. Because the series is very long, you can afford to use 
# a training and test set rather than time series cross-validation. This is much faster.

autoplot(qcement)

# Use 20 years of the qcement data beginning in 1988
train <- window(qcement, start = 1988, end = c(2007, 4))

# Fit an ARIMA and an ETS model to the training data
fit1 <- auto.arima(train)
fit2 <- ets(train)

# Check that both models have white noise residuals
checkresiduals(fit1)
checkresiduals(fit2)

# Produce forecasts for each model
# The last data point in qcement is in the first quarter of 2014, and the last 
# data point in the training set is in the fourth quarter of 2007. Therefore, h in 
# forecast() is equal to 1+(4???(2013???2007))
fc1 <- forecast(fit1, h = 25)
fc2 <- forecast(fit2, h = 25)

# Use accuracy() to find best model based on RMSE
# You do not need to set up a test set. Just pass the whole of qcement as the test 
# set to accuracy() and it will find the relevant part to use in comparing with the 
# forecasts.
accuracy(fc1, qcement)
accuracy(fc2, qcement)
bettermodel <- fc2


## Forecasting Global Temperatures
# Here, you will forecast the annual global temperature deviations globtemp to 2050.
# The data in globtemp (from astsa) are the annual global temperature deviations to 2015.
# Fit an ARIMA model to the data
library(astsa)
library(fpp2)
autoplot(globtemp)

# create training and test set from globtemp data
train <- window(globtemp, end=1980) # training data
test <- window(globtemp, start=1981,end=2015)

#Create ARIMA model
fit <- auto.arima(train)

# Check that the residuals look like white noise
checkresiduals(fit)

# Summarize the model
summary(fit)

# Forecast for test data
forc <- forecast(fit, h=35)

# Plot
autoplot(train) +autolayer(fitted(forc)) + autolayer(forecast(fit,h=35))+autolayer(test,series="Test Data")
accuracy(forc,test)

# recombine train and test set and rerun model on complete data
fit_full <- auto.arima(globtemp)

# Plot forecasts
fit_full %>% forecast(h=35) %>% autoplot()
