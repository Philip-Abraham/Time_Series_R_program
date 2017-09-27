library(astsa)

## Identifying ARIMA
# A time series exhibits ARIMA behavior if the differenced data has ARMA behavior
# In this case, the model is an ARIMA(1,1,0) because the differenced data are an 
# autoregression of order one

# The simulated time series is in x
x <- arima.sim(model = list(order = c(1, 1, 0), ar = .9), n = 200)

# Plot x
plot(x, main = "ARIMA(p = 1, d = 1, q = 0)")

# ACF and PCF of an Integrated ARMA
acf2(x)

# Plot the differenced data
plot(diff(x), main = "ARMA(p = 1, d = 0, q = 0)")

# ACF and PCF of a Differenced ARIMA
acf2(diff(x))


## Simulated ARIMA
# Here, we generated 250 observations from the ARIMA(2,1,0) model with drift
# The simulated time series is in x - Set the mean argument to 0.3 to produce a drift.
x <- arima.sim(model = list(order = c(2, 1, 0), ar = c(1.5,-0.75)), n = 250, mean=0.3)

# Plot x
plot(x)

# detrended series
y <- diff(x)

# Plot y
plot(y)

# Plot sample P/ACF of differenced data and determine model
acf2(y)

# Fit an ARIMA(2,1,0) model using sarima() to the generated data. 
# Examine the t-table and other output information to assess the model fit.
sarima(x, p = 2, d = 1, q = 0)


## Global Warming
# The data in globtemp (from astsa) are the annual global temperature deviations to 2015.
# Fit an ARIMA model to the data

x <- globtemp
plot(x)

# A plot of the data shows random walk behavior, which suggests you should work 
# with the differenced data
y <- diff(x)
plot(y)

# Plot the sample P/ACF pair of the differenced data 
acf2(y)

# Fit an ARIMA(1,1,1) model to globtemp
sarima(globtemp, p = 1, d = 1, q = 1)

# Fit an ARIMA(0,1,2) model to globtemp
sarima(globtemp, p = 0, d = 1, q = 2)

# Fit an ARIMA(3,1,0) model to globtemp
sarima(globtemp, p = 3, d = 1, q = 0)
# The ACF is tailing off and the PACF cuts off at lag 3, implying an ARIMA(3,1,0) model. 
# Although the ARIMA(3,1,0)model fits reasonably well, it is the worst of the three 
# models (below) because it uses too many parameters for such small autocorrelations
# The model diagnostics suggest that both the ARIMA(0,1,2) and the ARIMA(1,1,1) 
# are reasonable models. However, the AIC and BIC suggest that the ARIMA(0,1,2) 
# performs slightly better on the globtemp data.


## Diagnostics - Simulated Overfitting
# Here, we generated 250 observations from the ARIMA(0,1,1) model with MA parameter 0.9.
# The simulated time series is in x 
x <- arima.sim(model = list(order = c(0, 1, 1), ma = .9), n = 250)
plot(x)
# differenced data
plot(diff(x))

# Plot the sample ACF and PACF of the differenced data using acf2() and note that 
# the model is easily identified.
acf2(diff(x))

# Fit an ARIMA(0,1,1) model to the simulated data using sarima(). Compare the MA 
# parameter estimate to the actual value of .9, and examine the residual plots.
sarima(x, p = 0, d = 1, q = 1)

# Overfit the model by adding an additional MA parameter. That is, fit an 
# ARIMA(0,1,2) to the data and compare it to the ARIMA(0,1,1) run.
sarima(x, p = 0, d = 1, q = 2)
# As you can see from the t-table, the second MA parameter is not significantly 
# different from zero and the first MA parameter is approximately the same in each 
# run. Also, the AIC and BIC both increase when the parameter is added. 
# In addition, the residual analysis of your ARIMA(0,1,1) model is fine. 
# All of these facts together indicate that you have a successful model fit.


## Forecasting Simulated ARIMA
# 120 observations from an ARIMA(1,1,0) model with AR parameter .9. 
# The data are in y and the first 100 observations are in x.
y <- arima.sim(model = list(order = c(1, 1, 0), ar = .9), n = 119)
x <- window(y, end=100)

plot(x)
plot(diff(x))

# Plot P/ACF pair of differenced data 
acf2(diff(x))

# Fit model - check t-table and diagnostics
sarima(x, p = 1, d = 1, q = 0)

# Forecast the data 20 time periods ahead
sarima.for(x, n.ahead = 20, p = 1, d = 1, q = 0) 
lines(y)  


## Forecasting Global Temperatures
# Here, you will forecast the annual global temperature deviations globtemp to 2050.
# The data in globtemp (from astsa) are the annual global temperature deviations to 2015.
# Fit an ARIMA model to the data
y <- globtemp
x <- window(y, end=1980) # training data

plot(x)

# A plot of the data shows random walk behavior, which suggests you should work 
# with the differenced data
plot(diff(x))

# Plot the sample P/ACF pair of the differenced data 
acf2(diff(x))

# Based on your previous analysis ARIMA(0,1,2) model  was the best fit for the globtemp data. 
# Recheck the parameter significance in the t-table output and check the residuals 
# for any departures from the model assumptions.
sarima(x, p = 0, d = 1, q = 2) 

# Forecast the test data
sarima.for(x, n.ahead = 35, p = 0, d = 1, q = 2) 
lines(y)  

# Forecast data 35 years into the future
sarima.for(globtemp, n.ahead = 35, p = 0, d = 1, q = 2) 
