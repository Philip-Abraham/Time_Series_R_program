## Simulate the simple moving average model
# Generate MA model with slope 0.5
x <- arima.sim(model = list(ma = 0.5), n = 100)

# Generate MA model with slope 0.9
y <- arima.sim(model = list(ma = 0.9), n = 100)

# Generate MA model with slope -0.5
z <- arima.sim(model = list(ma = -0.5), n = 100)

# Plot all three models together
plot.ts(cbind(x, y, z))


## Estimate the autocorrelation function (ACF) for a moving average
# Calculate ACF for x
acf(x)

# Calculate ACF for y
acf(y)

# Calculate ACF for z
acf(z)


## Estimate the simple moving average model
data(Nile)

# Fit the MA model to Nile
MA <- arima(Nile, order = c(0, 0, 1))
print(MA)

# Plot Nile and MA_fit 
ts.plot(Nile)
MA_fit <- Nile - resid(MA)
points(MA_fit, type = "l", col = 2, lty = 2)


## Simple forecasts for River Nile flow levels from an estimated MA model
# note that except for the 1-step forecast, all forecasts from the MA model are 
# equal to the estimated mean (intercept)

# Make a 1-step forecast based on MA
predict_MA <- predict(MA)

# Obtain the 1-step forecast using $pred[1]
predict_MA$pred[1]

# Make a 1-step through 10-step forecast based on MA
predict(MA, n.ahead = 10)

# Plot the Nile series plus the forecast and 95% prediction intervals
ts.plot(Nile, xlim = c(1871, 1980))
MA_forecasts <- predict(MA, n.ahead = 10)$pred
MA_forecast_se <- predict(MA, n.ahead = 10)$se
points(MA_forecasts, type = "l", col = 2)
points(MA_forecasts - 2*MA_forecast_se, type = "l", col = 2, lty = 2)
points(MA_forecasts + 2*MA_forecast_se, type = "l", col = 2, lty = 2)


##  AR vs MA models
# All factors being equal, a model that produces a lower AIC or BIC than another 
# model is considered a better fit.

# Find correlation between AR_fit and MA_fit
cor(AR_fit, MA_fit)

# Find AIC of AR
AIC(AR)

# Find AIC of MA
AIC(MA)

# Find BIC of AR
BIC(AR)

# Find BIC of MA
BIC(MA)

# Although the correlation between AR & MA fitted data is high (0.94), the AR model is
# a better fit than the MA model based on the AIC and BIC values for each model.

# Remember that the MA ACF plot should show strong autocorrelation at a lag of 1 but virtually no autocorrelation at all other lags.
# Remember that the RW and AR models typically show large autocorrelation for many lags, but the ACF of an AR delays to zero more quickly than that of the RW. The MA ACF should have approximately zero autocorrelation at all lags greater than 1. The WN ACF should have approximately zero autocorrelation at all lags. Which plots match these expectations?
# Remember that the RW ACF plot is likely to show large autocorrelation for many lags without quick decay to zero.
# Remember that the WN ACF plot should show virtually no autocorrelation at all lags.


