library(astsa)

## Fit a 250 observations from a pure seasonal model with ar = .9, ma=.5
# Three years of data and the model ACF and PACF are plotted for you.
set.seed(666)
phi = c(rep(0,11),.9)
phi2 = c(rep(0,11),.5)
sARMA = arima.sim(list(order=c(12,0,12), ar=phi, ma=phi2), n=37)
sARMA = ts(sARMA, freq=12)
layout(matrix(c(1,2, 1,3), nc=2))
par(mar=c(3,3,2,1), mgp=c(1.6,.6,0))
plot(sARMA, axes=FALSE, main='seasonal AR(1) & MA(1)', xlab="year", type='c')
Months = c("J","F","M","A","M","J","J","A","S","O","N","D")
points(sARMA, pch=Months, cex=1.25, font=4, col=1:4)
axis(1, 1:4); abline(v=1:4, lty=2, col='#cccccc')
axis(2); box()
ACF = ARMAacf(ar=phi, ma=phi2, 100)
PACF = ARMAacf(ar=phi, ma=phi2, 100, pacf=TRUE)
plot(ACF,type="h", xlab="lag", ylim=c(-.1,1)); abline(h=0)
plot(PACF, type="h", xlab="lag", ylim=c(-1,1)); abline(h=0)

## Generate 250 observations from a pure seasonal model with ar = .9, ma=.5
set.seed(666)
x = arima.sim(list(order=c(12,0,12), ar=phi, ma=phi2), n=250)

# Plot sample P/ACF to lag 60 and compare to the true values
acf2(x, max.lag = 60)

# Fit the seasonal model to sARMA
sarima(x, p = 0, d = 0, q = 0, P = 1, D = 0, Q = 1, S = 12)


## Mixed Seasonal Model
# Mixed model: SARIMA(p, d, q) x (P, D, Q)s model
# Time series data collected on a seasonal basis typcially have mixed dependence. 
# For example, what happens in June is often related to what happend in May as well 
# as what happened in June of last year.

# Air Passengers - Monthly totals of international airline passengers, 1949-1960
x = AirPassengers
# Note that x is the original series, which shows trend plus increasing variance. The
# logged data are in lx, and the transformation stabilizes the variance. The logged data
# are then differenced to remove trend, and are stored in dlx. It is clear the there is still
# persistence in the seasons (i.e., dlxt ~ dlxt????12), so that a twelfth-order difference
# is applied and stored in ddlx. The transformed data appears to be stationary and we
# are now ready to fit a model.
lx = log(x); dlx = diff(lx); ddlx = diff(dlx, 12)
plot.ts(cbind(x,lx,dlx,ddlx), main="")
# below of interest for showing seasonal RW (not shown here):
par(mfrow=c(2,1))
monthplot(dlx); monthplot(ddlx)

# The sample ACF and PACF of ddlx are shown
acf2(ddlx,50)
# Between Seasons: It appears that at the seasons, the ACF is cutting off a lag 1s
# (s = 12), whereas the PACF is tailing off at lags 1s, 2s, 3s, 4s; .. . These results
# implies an SMA(1), P = 0, Q = 1, in the season (s = 12).
# Within Seasons: Inspecting the sample ACF and PACF at the lower lags, it appears
# as though both are tailing off. This suggests an ARMA(1; 1) within the seasons,
# p = q = 1.

# Thus, we first try an ARIMA(1, 1, 1) X (0; 1, 1)12 on the logged data:
airpass_fit1 <- sarima(log(AirPassengers), p = 1, d = 1, q = 1, P = 0, D = 1, Q = 1, S = 12)
airpass_fit1$ttable
# However, the AR parameter is not significant, so we should try dropping one
# parameter from the within seasons part. In this case,we try both an ARIMA(0, 1, 1) X
# (0, 1, 1)12 and an ARIMA(1, 1, 0) X (0, 1, 1)12 model
airpass_fit2 <- sarima(log(AirPassengers), p = 0, d = 1, q = 1, P = 0, D = 1, Q = 1, S = 12)
airpass_fit2$ttable
airpass_fit3 <- sarima(log(AirPassengers), p = 1, d = 1, q = 0, P = 0, D = 1, Q = 1, S = 12)
airpass_fit3$ttable
# All information criteria prefer the ARIMA(0, 1, 1) X (0, 1, 1)12 model

# Finally, we forecast the logged data out twelve months
sarima.for(lx, 12, 0,1,1, 0,1,1,12)


## Simulate Mixed Seasonal Model
## Fit a 250 observations from a mixed seasonal model with ma1=-0.6, sma1 = 0.8
# Three years of data are plotted for you.
set.seed(666)
phi2  = c(-0.6,0,0,0,0,0,0,0,0,0,0,0.8,-0.6)
sARMA = arima.sim(list(order=c(0,0,13), ma=phi2), n=37)
sARMA = ts(sARMA, freq=12)
layout(matrix(c(1,2, 1,3), nc=2))
par(mar=c(3,3,2,1), mgp=c(1.6,.6,0))
plot(sARMA, axes=FALSE, main='mixed seasonal MA(1)', xlab="year", type='c')
Months = c("J","F","M","A","M","J","J","A","S","O","N","D")
points(sARMA, pch=Months, cex=1.25, font=4 , col=1:4)
axis(1, 1:4); abline(v=1:4, lty=2, col='#cccccc')
axis(2); box()
ACF = ARMAacf(ar=0, ma=phi2, 60)
PACF = ARMAacf(ar=0, ma=phi2, 60, pacf=TRUE)
plot(ACF,type="h", xlab="lag", ylim=c(-.1,1)); abline(h=0)
plot(PACF, type="h", xlab="lag", ylim=c(-1,1)); abline(h=0)


## Generate 250 observations from a pure seasonal model with ma1=-0.6, sma1 = 0.8
set.seed(666)
phi2  = c(-0.6,0,0,0,0,0,0,0,0,0,0,0.8,-0.6)
x = arima.sim(list(order=c(0,0,13), ma=phi2), n=250)

# Plot sample P/ACF to lag 60 and compare to the true values
acf2(x, max.lag = 60)

# Fit the seasonal model to x
sarima(x, p = 0, d = 0, q = 1, P = 0, D = 0, Q = 1, S = 12)


## Data Analysis - Unemployment I
# You will now start to fit a seasonal ARIMA model to the monthly US unemployment data
library(astsa)

# Plot the monthly US unemployment (unemp) time series from astsa. 
# Note trend and seasonality.
unemp_stl <- stl(unemp,s.window="periodic")
plot(unemp_stl)

# Detrend and plot the data. Save this as d_unemp. 
# Notice the seasonal persistence.
d_unemp <- diff(unemp)
plot(d_unemp)

# Seasonally difference d_unemp and plot it
# notice that it looks stationary now.
dd_unemp <- diff(d_unemp, lag = 12) 
plot(dd_unemp)

# The STL method decomposes a time series into seasonal, trend, and error components 
# using Loess(non-linear regression technique)
unemp_stl <- stl(unemp,s.window="periodic")
plot(unemp_stl)


## Data Analysis - Unemployment II
# Now, you will continue fitting an SARIMA model to the monthly US unemployment unemp 
# time series by looking at the sample ACF and PACF of the fully differenced series.
# Plot P/ACF pair of fully differenced data to lag 60
acf2(dd_unemp, max.lag = 60)
# the nonseaonal component: the PACF cuts off at lag 2 and the ACF tails off.
# the seasonal component: the ACF cuts off at lag 12 and the PACF tails off at lags 12, 24, 36, ...

# Fit an appropriate model. Check the residuals to ensure appropriate model fit.
sarima(unemp, p = 2, d = 1, q = 0, P = 0, D = 1, Q = 1, S = 12)


## Forecasting Monthly Unemployment
# Forecast the data 3 years into the future
sarima.for(unemp, n.ahead = 36, 2, 1, 0, 0, 1, 1, 12)


## Data Analysis - Commodity Prices
# Data set chicken, which is the monthly whole bird spot price, Georgia docks, 
# US cents per pound, from August, 2001 to July, 2016
chick_stl <- stl(chicken,s.window="periodic")
plot(chick_stl)

# Plot the differenced (d = 1) data diff(chicken). 
# Note that the trend is removed and note the seasonal behavior
plot(diff(chicken))

# Plot the sample ACF and PACF of the differenced data to lag 60 (5 years). 
# Notice that an AR(2) seems appropriate but there is a small but significant 
# seasonal component remaining in the detrended data
acf2(diff(chicken), max.lag = 60)

# Fit an ARIMA(2,1,0) to the chicken data to see that there is correlation 
# remaining in the residuals.
sarima(chicken, p = 2, d = 1, q = 0)

# Fit an SARIMA(2,1,0)x(1,0,0)12 and notice the model fits well.
sarima(chicken, p = 2, d = 1, q = 0, P = 1, D = 0, Q = 0, S = 12)


## How Hard is it to Forecast Commodity Prices?
# To see a difficulty in predicting a commodity, you will forecast the price of 
# chicken to five years in the future. When you complete your forecasts, you will 
# note that even just a few years out, the acceptable range of prices is very large. 
# This is because commodities are subject to many sources of variation.
sarima.for(chicken, n.ahead = 60, 2, 1, 0, 1, 0, 0, 12)


## Data Analysis - Birth Rate
# The data are monthly live births (adjusted) in thousands for the United States, 
# 1948-1979, and includes the baby boom after WWII.

# The birth data are plotted. Note the long-term trend (random walk) and the 
# seasonal component of the data.
birth_stl <- stl(birth,s.window="periodic")
plot(birth_stl)

# Plot P/ACF to lag 60 of differenced data
d_birth <- diff(birth)
acf2(d_birth, max.lag = 60)

# Plot P/ACF to lag 60 of seasonal differenced data
dd_birth <- diff(d_birth, lag = 12)
acf2(dd_birth, max.lag = 60)
# SARIMA(0,1,1)x(0,1,1)12 model seems reasonable?

# Fit SARIMA(0,1,1)x(0,1,1)_12. What happens?
sarima(birth, p = 0, d = 1, q = 1, P = 0, D = 1, Q = 1, S = 12)

# The residual analysis from the first fit indicated that the residuals were not 
# white noise. Hence, it was necessary to include an additional nonseasonal AR term 
# to account for the extra correlation.
# Add an additional AR (nonseasonal, p = 1) parameter to account for additional 
# correlation. Does the model fit well?
sarima(birth, p = 1, d = 1, q = 1, P = 0, D = 1, Q = 1, S = 12)
