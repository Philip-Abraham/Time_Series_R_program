library(astsa)

ARMA = arima.sim(n=100, list(order=c(1,0,1), ma=.9, ar=0.6)) #ARMA(1,1)
AR = arima.sim(n=100, list(order=c(1,0,0) , ar=0.6)) # ARMA(1,0) = AR(1)
MA = arima.sim(n=100, list(order=c(0,0,1), ma=.9)) # ARMA(0,1) = MA(1)
par(mfrow=c(2,3))
acf(AR, lag.max = 20)
acf(MA, lag.max = 20)
acf(ARMA, lag.max = 20)
pacf(AR, lag.max = 20)
pacf(MA, lag.max = 20)
pacf(ARMA, lag.max = 20)

## Fitting an AR(1) Model
# Generate 100 observations from the AR(1) model
x <- arima.sim(model = list(order = c(1, 0, 0), ar = .9), n = 100) 

# Plot the generated data 
plot(x)

# Plot the sample P/ACF pair
acf2(x)

# Fit an AR(1) to the data and examine the t-table
sarima(x, p = 1, d = 0, q = 0)


## Fitting an AR(2) Model
# Generate 200 observations from the AR(2) model
x <- arima.sim(model = list(order = c(2, 0, 0), ar = c(1.5, -.75)), n = 200) 

# Plot the generated data 
plot(x)

# Plot the sample P/ACF pair
acf2(x)

# Fit an AR(2) to the data and examine the t-table
sarima(x, p = 2, d = 0, q = 0)


## Fitting an MA(1) Model
# Generate 100 observations from the MA(1) model
x <- arima.sim(model = list(order = c(0, 0, 1), ma = -.8), n = 100)

# Plot the generated data 
plot(x)

# Plot the sample P/ACF pair
acf2(x)

# Fit an MA(1) to the data and examine the t-table
sarima(x, p = 0, d = 0, q = 1)


## Fitting an ARMA model
# Generate 250 observations from the ARMA model
x <- arima.sim(model = list(order = c(2, 0, 1), ar = c(1, -.9), ma = .8), n = 250)

# Plot x
plot(x)

# Plot the sample P/ACF of x
acf2(x)

# Fit an ARMA(2,1) to the data and examine the t-table
sarima(x, p = 2, d = 0, q = 1)


## Model Choice - I
# The best approach to fitting ARMA is to start with a low order model, and then 
# try to add a parameter at a time to see if the results change.

# In this exercise, you will fit various models to the varve data and note the 
# AIC and BIC for each model. AIC and BIC helps in finding the model with the smallest error.
# AIC and BIC measure the error and penalize (differently) for adding parameters

library(astsa)
# Sedimentary deposits from one location in Massachusetts for 634 years, beginning 
# nearly 12,000 years ago
ts.plot(varve)

# The varve series has been logged and differenced as:
dl_varve <- diff(log(varve))
ts.plot(dl_varve)

# sarima() run includes a residual analysis graphic. Specifically, the output 
# shows (1) the standardized residuals, (2) the sample ACF of the residuals, (3) a 
# normal Q-Q plot, and (4) the p-values corresponding to the Box-Ljung-Pierce Q-statistic
# In each run, check the four residual plots as follows:
# The standardized residuals should behave as a white noise sequence with mean 
# zero and variance one. Examime the residual plot for departures from this behavior.
# The sample ACF of the residuals should look like that of white noise. 
# Examine the ACF for departures from this behavior.
# Normality is an essential assumption when fitting ARMA models. Examine the Q-Q plot for departures from normality and to identify outliers.
# Use the Q-statistic plot to help test for departures from whiteness of the residuals.

# Fit an MA(1) to dl_varve.   
sarima(dl_varve, p = 0, d = 0, q = 1)

# Fit an MA(2) to dl_varve. Improvement?
sarima(dl_varve, p = 0, d = 0, q = 2)

# Fit an ARMA(1,1) to dl_varve. Improvement?
sarima(dl_varve, p = 1, d = 0, q = 1)


## ARMA
# The data in oil are crude oil, WTI spot price FOB (in dollars per barrel), weekly 
# data from 2000 to 2008. 
# Fit an ARMA model to the returns.
ts.plot(oil)

# Calculate approximate oil returns
oil_returns <- diff(log(oil))
  
# Plot oil_returns and notice that there are a couple of outliers prior to 2004. 
# Convince yourself that the returns are stationary
ts.plot(oil_returns)
  
# Plot the P/ACF pair for oil_returns
acf2(oil_returns)
  
# From the P/ACF pair, it is apparent that the correlations are small and the 
# returns are nearly noise. But it could be that both the ACF and PACF are tailing off. 
# If this is the case, then an ARMA(1,1) is suggested. Fit this model to the oil 
# returns using sarima(). Does the model fit well? 
# Can you see the outliers in the residual plot?
sarima(oil_returns, p = 1, d = 0, q = 1)
