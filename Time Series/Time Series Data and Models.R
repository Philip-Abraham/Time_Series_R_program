install.packages("devtools")     #  Latest package- only need to do this once
devtools::install_github("nickpoison/astsa")
library(astsa)


library(xts)

# View a detailed description of AirPassengers
help(AirPassengers)

# Plot AirPassengers
plot(AirPassengers)

# Plot the DJIA daily closings
plot(djia$Close)

# SOI measures changes in air pressure related to sea surface
# temperatures in the central Pacific Ocean. The central Pacific warms every
# three to seven years due to the El Niño effect, which has been blamed for
# various global extreme weather events. The series show two basic oscillations
# types, an obvious annual cycle (hot in the summer, cold in the winter), and a
# slower frequency that seems to repeat about every 4 years.

# Plot the Southern Oscillation Index
plot(soi)


## Differencing isway to remove trends from data

# Generate a multifigure plot comparing the global temperature data (globtemp) 
# with the detrended series
par(mfrow = c(2,1))
plot(globtemp) 
plot(diff(globtemp))

# Generate another multifigure plot comparing the weekly cardiovascular mortality 
# in Los Angeles County (cmort) with the detrended series
par(mfrow = c(2,1))
plot(cmort)
plot(diff(cmort))


## Dealing with Trend and Heteroscedasticity

# Generate a multifigure plot to (1) plot the quarterly US GNP (gnp) data and 
# notice it is not stationary, and (2) plot the approximate growth rate of the US 
# GNP using diff() and log()
par(mfrow = c(2,1))
plot(gnp)
plot(diff(log(gnp)))

# Use a multifigure plot to (1) plot the daily DJIA closings (djia$Close) and 
# notice that it is not stationary. The data are an xts object. Then (2) plot the 
# approximate DJIA returns using diff() and log()
par(mfrow = c(2,1))
plot(djia[,"Close"])
plot(diff(log(djia[,"Close"])))


## Simulating ARMA Models
# Any stationary time series can be written as a linear combination of white noise.
# In addition, any ARMA model has this form, so it is a good choice for modeling 
# stationary time series.

# You will generate data from various ARMA models. For each command, generate 200 
# observations and plot the result.

# Generate and plot white noise
WN <- arima.sim(model = list(order = c(0, 0, 0)), n = 200)
plot(WN)

# AR and MA Models
x <- arima.sim(list(order = c(1, 0, 0), ar = -.7), n = 200)
y <- arima.sim(list(order = c(0, 0, 1), ma = -.7), n = 200)
par(mfrow = c(1, 2))
plot(x, main = "AR(1)")
plot(y, main = "MA(1)")
# Both Ar & Ma plots look similar


# AR(2) with mean 50
z <- arima.sim(list(order = c(2, 0, 0), ar = c(1.5, -.75)), n = 200) + 50
plot(z)

# Non-Zero Mean (mean=11.79) White Noise
plot(arima.sim(model = list(order = c(0, 0, 0)), n = 200) + 11.79)

# Generate and plot an MA(1) with parameter .9 
MA <- arima.sim(model = list(order = c(0, 0, 1), ma = .9), n = 200)
plot(MA)

# Generate and plot an AR(2) with parameters 1.5 and -.75
AR <- arima.sim(model = list(order = c(2, 0, 0), ar = c(1.5,-0.75)), n = 200)
plot(AR)

# A Random Walk is a special caseof an AR(1) model, where the slope coefficient=1


# some test MA models
MA <- arima.sim(model = list(order = c(0, 0, 1), ma = .9), n = 200)
plot(MA)
MA2 <- arima.sim(model = list(order = c(0, 0, 2), ma = c(.9,.9)), n = 200)
plot(MA2)
MA3 <- arima.sim(model = list(order = c(0, 0, 3), ma = c(.9,.9,.9)), n = 200)
plot(MA3)
MAt1 <- arima.sim(model = list(order = c(0, 0, 3), ma = c(.09,.09,.09)), n = 200)
plot(MAt1)
MAt2 <- arima.sim(model = list(order = c(0, 0, 3), ma = c(9,9,9)), n = 200)
plot(MAt2)
MAt3 <- arima.sim(model = list(order = c(0, 0, 3), ma = c(0,0,0)), n = 200)
plot(MAt3)

