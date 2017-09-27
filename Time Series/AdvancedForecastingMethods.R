library(fpp2)

## Forecasting sales allowing for advertising expenditure
# In this exercise, you will model sales data regressed against advertising 
# expenditure, with an ARMA error to account for any serial correlation in the 
# regression errors. The data are available in your workspace as advert and 
# comprise 24 months of sales and advertising expenditure for an automotive 
# parts company. 

ggplot(aes(x = advert, y = sales),
       data = as.data.frame(advert)) +
        geom_point() +
        ggtitle("Monthly Changes in Sales data and Advertising Expenditure")

# Plot the data in advert. The variables are on different scales, so use facets = TRUE
autoplot(advert, facets = TRUE)

# Fit ARIMA model
fit <- auto.arima(advert[, "sales"], xreg = advert[, "advert"], stationary = TRUE)

# Residuals from dynamic regression model
checkresiduals(fit)

# Check model. Increase in sales for each unit increase in advertising
salesincrease <- coefficients(fit)[3]

# Forecasts from dynamic regression model specifying the next 6 months of advertising 
# expenditure as 10 units per month as fc. To repeat 10 six times
fc <- forecast(fit, xreg = rep(10, 6))

# Plot fc with x and y labels
autoplot(fc) + xlab("Month") + ylab("Sales")


## Forecasting electricity demand
# In this exercise, you will fit a quadratic regression model(daily electricity 
# demand as a function of temperature) with an ARMA error.
ggplot(aes(x = Temperature, y = Demand),
       data = as.data.frame(elecdemand)) +
        geom_point() +
        ggtitle("Daily Electricity Demand as a Function of Temperature")

# Time plots of demand and temperatures
autoplot(elecdemand[, c("Demand", "Temperature")], facets = TRUE)

# # Matrix of regressors
xreg <- cbind(MaxTemp = elecdemand[, "Temperature"], 
              MaxTempSq = elecdemand[, "Temperature"]^2, 
              Workday = elecdemand[, "WorkDay"])

# Fit a dynamic regression model of the demand column with ARIMA errors 
fit <- auto.arima(elecdemand[, "Demand"], xreg = xreg)

# If the next day is a working day (indicator is 1) with maximum temperature 
# forecast to be 20°C, what is the forecast demand?
forecast(fit, xreg = cbind(20, 20^2, 1))


## Dynamic harmonic regression
# Forecasting weekly data
# With weekly data, it is difficult to handle seasonality using ETS or ARIMA models 
# as the seasonal length is too large (approximately 52). Instead, you can use 
# harmonic regression which uses sines and cosines to model the seasonality.
# The fourier() function makes it easy to generate the required harmonics. The 
# higher the order (K), the more "wiggly" the seasonal pattern is allowed to be. 
# With K=1, it is a simple sine curve. You can select the value of K by 
# minimizing the AICc value. 

# gasoline data comprises weekly data on US finished motor gasoline products. 
# In this exercise, you will fit a harmonic regression to this data set and 
# forecast the next 3 years
autoplot(gasoline)

# Set up an xreg matrix called harmonics using the fourier() method on gasoline 
# with order K=13 which has been chosen to minimize the AICc.
harmonics <- fourier(gasoline, K = 13)

# Fit regression model with ARIMA errors
# Set seasonal to FALSE because seasonality is handled by the regressors
fit <- auto.arima(gasoline, xreg = harmonics, seasonal = FALSE)

# Forecasts next 3 years
newharmonics <- fourier(gasoline, K = 13, h = 156)
fc <- forecast(fit, xreg = newharmonics)

# Plot forecasts fc
autoplot(fc)
# The point predictions look a bit low


## Harmonic regression for multiple seasonality
# Harmonic regressions are also useful when time series have multiple seasonal 
# patterns. For example, taylor contains half-hourly electricity demand in England 
# and Wales over a few months in the year 2000. The seasonal periods are 48 (daily 
# seasonality) and 7 x 48 = 336 (weekly seasonality). There is not enough data to 
# consider annual seasonality.
# 
# auto.arima() would take a long time to fit a long time series such as this one, 
# so instead you will fit a standard regression model with Fourier terms using the 
# tslm() function. This is very similar to lm() but is designed to handle time series. 
# With multiple seasonality, you need to specify the order K for each of the seasonal periods.

autoplot(taylor)

# Fit a harmonic regression using order 10 for each type of seasonality
fit <- tslm(taylor ~ fourier(taylor, K = c(10, 10)))

# Forecast 20 working days ahead as fc. Remember that the data are half-hourly 
# in order to set the correct value for h
fc <- forecast(fit, newdata = data.frame(fourier(taylor, K = c(10, 10), h = 960)))

# Plot the forecasts
autoplot(fc)

# Check the residuals of your fitted model. 
# As you can see, auto.arima() would have done a better job.
checkresiduals(fit)
# The residuals from the fitted model fails the tests badly, yet the forecasts are quite good


## Forecasting call bookings
# calls, which contains 20 consecutive days of 5-minute call volume data for a 
# large North American bank. There are 169 5-minute periods in a working day, and 
# so the weekly seasonal frequency is 5 x 169 = 845. The weekly seasonality is 
# relatively weak, so here you will just model daily seasonality

# Plot the calls data to see the strong daily seasonality and weak weekly seasonality
autoplot(calls)

# Set up the xreg matrix using order 10 for daily seasonality and 0 for weekly seasonality
xreg <- fourier(calls, K = c(10,0))

# Fit a dynamic regression model
fit <- auto.arima(calls, xreg = xreg, seasonal = FALSE, stationary = TRUE)

# Check the residuals
checkresiduals(fit)
# The residuals in this case still fail the white noise tests, but their autocorrelations 
# are tiny, even though they are significant. This is because the series is so long. 
# It is often unrealistic to have residuals that pass the tests for such long series. 
# The effect of the remaining correlations on the forecasts will be negligible.

# Plot forecasts for 10 working days ahead
fc <- forecast(fit, xreg =  fourier(calls, c(10, 0), h = 1690))
autoplot(fc)


## TBATS models for electricity demand

# The gas data contains Australian monthly gas production. A plot of the data shows 
# the variance has changed a lot over time, so it needs a transformation. 
# The seasonality has also changed shape over time, and there is a strong trend. 
# This makes it an ideal series to test the tbats() function which is designed to 
# handle these features.

# Plot the gas data
autoplot(gas)

# Fit a TBATS model to the gas data
fit <- tbats(gas)

# Forecast the series for the next 5 years
fc <- forecast(fit,h=60)

# Plot the forecasts
autoplot(fc)

# Record the Box-Cox parameter and the order of the Fourier terms
# TBATS(0.082, {0,0}, 0.992, {<12,5>})
lambda <- 0.082
K <- 5
