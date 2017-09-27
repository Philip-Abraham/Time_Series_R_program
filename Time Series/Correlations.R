data(EuStockMarkets)
# This dataset contains daily closing prices of major European stock indices from 
# 1991-1998, specifically, from Germany (DAX), Switzerland (SMI), France (CAC), 
# and the UK (FTSE). The data were observed when the markets were open, so there 
# are no observations on weekends and holidays. We will proceed with the 
# approximation that this dataset has evenly spaced observations and is a four 
# dimensional time series.


## Asset prices vs. asset returns

# Plot eu_stocks
plot(EuStockMarkets)

# Use this code to  convert daily prices in the eu_stocks data to daily net returns
returns <- EuStockMarkets[-1,] / EuStockMarkets[-1860,] - 1

# Convert returns to ts
returns <- ts(returns, start = c(1991, 130), frequency = 260)

# Plot returns
plot(returns)

# Use this code to convert prices to log returns
logreturns <- diff(log(EuStockMarkets))

# Plot logreturns
plot(logreturns)

# eu_percentreturns dataset, which is the percentage returns calculated from your EuStockMarkets data
eu_percentreturns <- 100*returns

# Generate means from eu_percentreturns
colMeans(eu_percentreturns)

# Use apply to calculate sample variance from eu_percentreturns
apply(eu_percentreturns, MARGIN = 2, FUN = var)

# Use apply to calculate standard deviation from eu_percentreturns
apply(eu_percentreturns, MARGIN = 2, FUN = sd)

# Display a histogram of percent returns for each index
par(mfrow = c(2,2))
apply(eu_percentreturns, MARGIN = 2, FUN = hist, main = "", xlab = "Percentage Return")

# Display normal quantile plots of percent returns for each index
par(mfrow = c(2,2))
apply(eu_percentreturns, MARGIN = 2, FUN = qqnorm, main = "")
qqline(eu_percentreturns)

# Covariance & Correlation
# Use cov() with DAX_logreturns and FTSE_logreturns
DAX_logreturns <- logreturns[,1]
FTSE_logreturns <- logreturns[,4]
cov(DAX_logreturns, FTSE_logreturns)

# Use cov() with logreturns
cov(logreturns)

# Use cor() with DAX_logreturns and FTSE_logreturns
cor(DAX_logreturns, FTSE_logreturns)

# Use cor() with logreturns
cor(logreturns)


## Autocorrelations
# Load Monthly Los Angeles Precip Data
LA_dat_oct = read.csv("./LA_Precip_Oct.csv")
LA_dat_oct <- LA_dat_oct[,c(1,9)]
LA_dat_nov = read.csv("./LA_Precip_Nov.csv")
LA_dat_nov <- LA_dat_nov[,c(1,9)]
LA_dat_dec = read.csv("./LA_Precip_Dec.csv")
LA_dat_dec <- LA_dat_dec[,c(1,9)]
LA_dat_jan = read.csv("./LA_Precip_Jan.csv")
LA_dat_jan <- LA_dat_jan[,c(1,9)]
LA_dat_feb = read.csv("./LA_Precip_Feb.csv")
LA_dat_feb <- LA_dat_feb[,c(1,9)]
LA_dat_mar = read.csv("./LA_Precip_Mar.csv")
LA_dat_mar <- LA_dat_mar[,c(1,9)]
LA_dat_apr = read.csv("./LA_Precip_Apr.csv")
LA_dat_apr <- LA_dat_apr[,c(1,9)]

# combined monthly precip dataset
LA_dat_comb <- rbind(LA_dat_oct,LA_dat_nov,LA_dat_dec,LA_dat_jan,LA_dat_feb,LA_dat_mar,LA_dat_apr)

# Time series of Los Angeles monthly precip.
LA_dat_oct_ts <- ts(LA_dat_oct[,2], start = 1945, frequency = 1)
LA_dat_nov_ts <- ts(LA_dat_nov[,2], start = 1945, frequency = 1)
LA_dat_dec_ts <- ts(LA_dat_dec[,2], start = 1945, frequency = 1)
LA_dat_jan_ts <- ts(LA_dat_jan[,2], start = 1945, frequency = 1)
LA_dat_feb_ts <- ts(LA_dat_feb[,2], start = 1945, frequency = 1)
LA_dat_mar_ts <- ts(LA_dat_mar[,2], start = 1945, frequency = 1)
LA_dat_apr_ts <- ts(LA_dat_apr[,2], start = 1945, frequency = 1)

# Combined monthly precip time series
LA_dat_comb_ts <- cbind(LA_dat_oct_ts,LA_dat_nov_ts,LA_dat_dec_ts,LA_dat_jan_ts,LA_dat_feb_ts,LA_dat_mar_ts,LA_dat_apr_ts)

# plot of combined monthly precip time series
ts.plot(LA_dat_comb_ts, col=8:14)
legend("topleft", c("oct","nov","dec","jan","feb","mar","apr"), lty = 1, lwd = 3, col = 8:14, bty = "n")
abline(reg=lm(LA_dat_oct_ts~time(LA_dat_oct_ts)),col="8",lwd = 5)
abline(reg=lm(LA_dat_nov_ts~time(LA_dat_nov_ts)),col="9",lwd = 5)
abline(reg=lm(LA_dat_dec_ts~time(LA_dat_dec_ts)),col="10",lwd = 5)
abline(reg=lm(LA_dat_jan_ts~time(LA_dat_jan_ts)),col="11",lwd = 5)
abline(reg=lm(LA_dat_feb_ts~time(LA_dat_feb_ts)),col="12",lwd = 5)
abline(reg=lm(LA_dat_mar_ts~time(LA_dat_mar_ts)),col="13",lwd = 5)
abline(reg=lm(LA_dat_apr_ts~time(LA_dat_apr_ts)),col="14",lwd = 5)

# Monthly precip correlation
cor(LA_dat_comb_ts)

# plot april and november TS, because they correlate at 0.34
ts.plot(LA_dat_apr_ts,LA_dat_nov_ts,col=1:2)
legend("topleft", c("apr","nov"), lty = 1, lwd = 3, col = 1:2, bty = "n")

plot(LA_dat_nov$TotalPrecip~LA_dat_apr$TotalPrecip, xlab="April Precip(in)",ylab="November Precip(in)")
abline(lm(LA_dat_nov$TotalPrecip ~LA_dat_apr$TotalPrecip))
# regression
lm(LA_dat_nov$TotalPrecip ~LA_dat_apr$TotalPrecip)

# plot october and december TS, because they correlate at 0.39
ts.plot(LA_dat_oct_ts,LA_dat_dec_ts,col=3:4)
legend("topleft", c("oct","dec"), lty = 1, lwd = 3, col = 3:4, bty = "n")


plot(LA_dat_oct$TotalPrecip~LA_dat_dec$TotalPrecip, xlab="October Precip(in)",ylab="December Precip(in)")
abline(lm(LA_dat_oct$TotalPrecip ~LA_dat_dec$TotalPrecip))
# regression
lm(LA_dat_oct$TotalPrecip ~LA_dat_dec$TotalPrecip)


LA_dat_totalPrecip_ts <- LA_dat_oct_ts+LA_dat_nov_ts+LA_dat_dec_ts+LA_dat_jan_ts+LA_dat_feb_ts+LA_dat_mar_ts+LA_dat_apr_ts
# Autocorrelations at lag 1 and 2
# the acf() command provides a shortcut. Applying acf(..., lag.max = 1, plot = FALSE) 
# to a series x automatically calculates the lag-1 autocorrelation.

# Autocorrelations of series 'combined precip', by lag 1
acf(LA_dat_totalPrecip_ts, lag.max = 1, plot = FALSE)

# Autocorrelation by lag: "The Autocorrelation Function" (ACF) - combined precip
acf(LA_dat_totalPrecip_ts, plot = FALSE)
acf(LA_dat_totalPrecip_ts, plot = TRUE)

# autocorrelations for each mont oct-apr
# Pair of blue, horizontal, dashed lines representing lag-wise 95% confidence 
# intervals centered at zero. These are used for determining the statistical 
# significance of an individual autocorrelation estimate at a given lag versus a 
# null value of zero, i.e., no autocorrelation at that lag.
acf(LA_dat_oct_ts, plot = TRUE)
acf(LA_dat_nov_ts, plot = TRUE)
acf(LA_dat_dec_ts, plot = TRUE)
acf(LA_dat_jan_ts, plot = TRUE)
acf(LA_dat_feb_ts, plot = TRUE)
acf(LA_dat_mar_ts, plot = TRUE)
acf(LA_dat_apr_ts, plot = TRUE)
