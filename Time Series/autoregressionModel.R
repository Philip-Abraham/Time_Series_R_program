## Simulate the autoregressive model
# Use arima.sim() to simulate 100 observations of an AR model with slope equal to 0.5
x <- arima.sim(model = list(ar = 0.5), n = 100)

# Simulate an AR model with 0.9 slope
y <- arima.sim(model = list(ar = 0.9), n = 100)
        
# Simulate an AR model with -0.75 slope
z <- arima.sim(model = list(ar = -0.75), n = 100)
        
# Plot your simulated data
plot.ts(cbind(x, y, z))

## Estimate the autocorrelation function (ACF) for an autoregression
# Calculate the ACF for x
acf(x)

# Calculate the ACF for y
acf(y)

# Calculate the ACF for z
acf(z)


## Compare the random walk (RW) and autoregressive (AR) models
# The AR model represented by series y below exhibits greater persistencethan series x,
# but the ACF continues to decay to zero. By contrast, the RW model represented 
# by series z shows considerable persistence and relatively little decay in the ACF.

# Use arima.sim() to simulate 200 observations from an AR model with slope 0.9 
x <- arima.sim(model =list(ar = 0.9), n = 200)
ts.plot(x)
acf(x)

# Simulate and plot AR model with slope 0.98
y <- arima.sim(model =list(ar = 0.98), n = 200)
ts.plot(y)
acf(y)

# Simulate and plot RW model
z <- arima.sim(model =list(order = c(0, 1, 0)), n = 200)
ts.plot(z)
acf(z)


## Estimate the autoregressive (AR) model
data("AirPassengers")

# Plot AirPassengers
plot(AirPassengers)

# Fit the AR model to AirPassengers
AR <- arima(AirPassengers, order =c(1, 0, 0))
print(AR)

# Run the following commands to plot the series and fitted values
ts.plot(AirPassengers)
AR_fitted <- AirPassengers - residuals(AR)
points(AR_fitted, type = "l", col = 2, lty = 2)
legend("topleft", c("Observed","Fitted"), lty = 1:2, lwd = 3, col = 1:2, bty = "n")

# 1-step ahead forecasts
predict(AR)
# h-step ahead forecasts
predict(AR, n.ahead = 6)


## Simple forecasts from an estimated AR model for Nile flow  data series
data(Nile)
ts.plot(Nile)

# Fit an AR model to Nile
AR_fit <-arima(Nile, order =c(1, 0, 0))
print(AR_fit)

# Use predict() to make a 1-step forecast
predict_AR <- predict(AR_fit,n.ahead = 1)

# Obtain the 1-step forecast using $pred[1]
predict_AR$pred[1]

# Use predict to make 1-step through 10-step forecasts
predict(AR_fit, n.ahead = 10)

# Run to plot the Nile series plus the forecast and 95% prediction intervals
ts.plot(Nile, xlim = c(1871, 1980))
AR_forecast <- predict(AR_fit, n.ahead = 10)$pred
AR_forecast_se <- predict(AR_fit, n.ahead = 10)$se
points(AR_forecast, type = "l", col = 2)
points(AR_forecast - qnorm(.975)*AR_forecast_se, type = "l", col = 2, lty = 2)
points(AR_forecast + qnorm(.975)*AR_forecast_se, type = "l", col = 2, lty = 2)

