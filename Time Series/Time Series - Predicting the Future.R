# Load Monthly Los Angeles Precip Data
LA_dat_oct = read.csv("./LA_Precip_Oct.csv")
LA_dat_oct <- LA_dat_oct[,c(1,6,8,9)]
LA_dat_nov = read.csv("./LA_Precip_Nov.csv")
LA_dat_nov <- LA_dat_nov[,c(1,6,8,9)]
LA_dat_dec = read.csv("./LA_Precip_Dec.csv")
LA_dat_dec <- LA_dat_dec[,c(1,6,8,9)]
LA_dat_jan = read.csv("./LA_Precip_Jan.csv")
LA_dat_jan <- LA_dat_jan[,c(1,6,8,9)]
LA_dat_feb = read.csv("./LA_Precip_Feb.csv")
LA_dat_feb <- LA_dat_feb[,c(1,6,8,9)]
LA_dat_mar = read.csv("./LA_Precip_Mar.csv")
LA_dat_mar <- LA_dat_mar[,c(1,6,8,9)]
LA_dat_apr = read.csv("./LA_Precip_Apr.csv")
LA_dat_apr <- LA_dat_apr[,c(1,6,8,9)]

# Removing trends in level by differencing
dz_oct <- diff(LA_dat_oct$TotalPrecip)
dz_nov <- diff(LA_dat_nov$TotalPrecip)
dz_dec <- diff(LA_dat_dec$TotalPrecip)
dz_jan <- diff( LA_dat_jan$TotalPrecip)
dz_feb <- diff(LA_dat_feb$TotalPrecip)
dz_mar <- diff(LA_dat_mar$TotalPrecip)
dz_apr <- diff(LA_dat_apr$TotalPrecip)

# Make diff data into time series data
oct <- ts(dz_oct, start = 1945, frequency = 1)
nov <- ts(dz_nov, start = 1945, frequency = 1)
dec <- ts(dz_dec, start = 1945, frequency = 1)
jan <- ts(dz_jan, start = 1945, frequency = 1)
feb <- ts(dz_feb, start = 1945, frequency = 1)
mar <- ts(dz_mar, start = 1945, frequency = 1)
apr <- ts(dz_apr, start = 1945, frequency = 1)

# Plot dz
ts.plot(oct,
        nov,
        dec,
        jan,
        feb,
        mar,
        apr,
        gpars=list(xlab="Years", ylab="PrecipDifference(in)",col = 22:28, lwd = 3))
legend("bottomleft", c("oct","nov","dec","jan","feb","mar","apr"), lty = 1, lwd = 3, col = 22:28, bty = "n")


# Los Angeles Annual Precip
LA_dat_annual <- LA_dat_oct[,4]+LA_dat_nov[,4]+LA_dat_dec[,4]+LA_dat_jan[,4]+LA_dat_feb[,4]+LA_dat_mar[,4]+LA_dat_apr[,4]
# Time series of Los Angeles Annual Precip
LA_dat_annual_ts <- ts(LA_dat_annual, start = 1945, frequency = 1)
# plot of ts
ts.plot(LA_dat_annual_ts)
# This will fit in a line
abline(reg=lm(LA_dat_annual_ts~time(LA_dat_annual_ts)))
# This will print the cycle across years.
cycle(LA_dat_annual_ts)
# This will aggregate the cycles and display a year on year trend
ts.plot(aggregate(LA_dat_annual_ts, FUN=mean))
# Box plot
boxplot(LA_dat_annual_ts~cycle(LA_dat_annual_ts))

# Removing trends in level by differencing
dz_annual <- diff(LA_dat_annual_ts)
# plot of dz
ts.plot(dz_annual)

# Removing seasonal trends with seasonal/cycles differencing
dz_annual_sea <- diff(LA_dat_annual_ts, lag=3)
# plot of dz after removing 3 year trends
ts.plot(dz_annual_sea)

# Removing seasonal trends with seasonal/cycles differencing
dz_annual_sea <- diff(LA_dat_annual_ts, lag=49)
# plot of dz after removing 49 year trends
ts.plot(dz_annual_sea)


# Simulate the white noise model - ARIMA(AutoRegressive Integrated Moving Average) 
# ARIMA(0, 0, 0) model
# Simulate a WN model with list(order = c(0, 0, 0)), n = 100 observations from the WN model
white_noise <- arima.sim(model =list(order = c(0, 0, 0)), n = 100)

# Plot your white_noise data 
ts.plot(white_noise)

# Simulate from the WN model with: mean = 100, sd = 10
white_noise_2 <- arima.sim(model = list(order = c(0, 0, 0)), n = 100, mean = 100, sd = 10)

# Plot your white_noise_2 data
ts.plot(white_noise_2)

# Estimate the white noise model of Los Angeles Annual Precip time series data
# Fit the WN model to LA_dat_annual_ts using the arima command
arima(LA_dat_annual_ts, order = c(0, 0, 0))

# Calculate the mean and variance of LA_dat_annual_ts using mean() and var(), respectively. 
# Compare the results with the output of your arima() command.
mean(LA_dat_annual_ts)
var(LA_dat_annual_ts)

# Simulate the random walk model
# ARIMA(0, 1, 0) model
# Generate a RW model using arima.sim to produce 100 observations
random_walk <- arima.sim(model = list(order = c(0, 1, 0)), n =100)

# Plot random_walk
ts.plot(random_walk)

# Calculate the first difference series
random_walk_diff <- diff(random_walk)

# Plot random_walk_diff
ts.plot(random_walk_diff)

# Generate a RW model with a drift uing arima.sim
rw_drift <- arima.sim(model = list(order = c(0, 1, 0)), n = 100, mean = 1)

# Plot rw_drift
ts.plot(rw_drift)

# Calculate the first difference series
rw_drift_diff <- diff(rw_drift)

# Plot rw_drift_diff
ts.plot(rw_drift_diff)

# Estimate the random walk model
# Difference your random_walk data
rw_diff <- diff(random_walk)

# Plot rw_diff
ts.plot(rw_diff)

# Now fit the WN model to the differenced data
model_wn <- arima(rw_diff, order = c(0, 0, 0))

# Store the value of the estimated time trend (intercept)
int_wn <- model_wn$coef

# Plot the original random_walk data
ts.plot(random_walk)

# Use abline(0, ...) to add time trend to the figure
abline(0, int_wn)


# Are the white noise model or the random walk model stationary?
# Use arima.sim() to generate WN data
white_noise <- arima.sim(model =list(order = c(0, 0, 0)), n = 100)

# Use cumsum() to convert your WN data to RW
random_walk <- cumsum(white_noise)

# Use arima.sim() to generate WN drift data
wn_drift <- arima.sim(model =list(order = c(0, 0, 0)), mean=0.4, n = 100)

# Use cumsum() to convert your WN drift data to RW
rw_drift <- cumsum(wn_drift)

# Plot all four data objects
plot.ts(cbind(white_noise, random_walk, wn_drift, rw_drift))
