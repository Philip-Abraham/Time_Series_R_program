## Control graphic parameters
# The function to tailor the chart parameters is par(), the option to change 
# character size is cex, to display more than one chart on a single window you 
# use mfrow

# load csv
df <- read.csv("dataset_1_1.csv", header = TRUE, sep = " ")

# Create date index
dates <- as.Date(df$Index)

# Create xts object
data <- as.xts(df[,-1], order.by = dates)

# Create 2x1 graphical window
par(mfrow = c(2, 1))
plot(data$yahoo, main="yahoo")
plot(data$microsoft, main="microsoft")

# Reduce margin size to 60% and character size to 80% of their normal sizes and 
# and replot with same graphical window and titles
par(mfrow = c(2, 1), mex = 0.6, cex = 0.8)
plot(data$yahoo, main="yahoo")
plot(data$microsoft, main="microsoft")


## Adding an extra series to an existing chart
# A great way to visually compare two times series is to display them on the same 
# chart with different scales

# Plot the "microsoft" series
plot(data$microsoft, main="Stock prices since 2015")
# Add the "dow_chemical" series in red
lines(data$dow_chemical, col = "red")
# Add a Y axis on the right side of the chart
axis(side = 4, at = pretty(data$dow_chemical))
# Add a legend in the bottom right corner
legend(x = "bottomright", legend = c("microsoft", "dow_chemical"),
       col = c("black", "red"), lty = c(1, 1))


## Highlighting events in a time series
# In this exercise, you will visually compare the average of the Citigroup stock 
# market prices to its price on January 4, 2016, after it was affected by 
# turbulence in the Chinese stock market

# Plot the "citigroup" time series
plot(data$citigroup, main = "Citigroup")

# Create vert_line to identify January 4th, 2016 in citigroup
vert_line <- which(index(data$citigroup) == as.Date("2016-01-04"))

# Add a red vertical line using vert_line
abline(v = .index(data)[vert_line], col = "red")

# Create hori_line to identify average price of citigroup
hori_line <- mean(data$citigroup)

# Add a blue horizontal line using hori_line
abline(h = hori_line, col = "blue")


## Highlighting a specific period in a time series
# In this exercise, you will highlight a single period in a chart of the 
# Citigroup time series 

# Create period to hold the 3 months of 2015
period <- c("2015-01/2015-03")

# Highlight the first three months of 2015 
library(PerformanceAnalytics)
chart.TimeSeries(data$citigroup, period.areas=period)

# Highlight the first three months of 2015 in light grey
chart.TimeSeries(data$citigroup, period.areas=period, period.color="lightgrey")


## Fancy Stock Chart

# Plot the microsoft series
plot(data$microsoft, main="Dividend date and amount")

# Add the citigroup series
lines(data$citigroup,col="orange", lwd=2)

# Add a new y axis for the citigroup series
axis(side = 4, at = pretty(data$citigroup), col="orange")

# Create the two legend strings
micro <- paste0("Microsoft div. of ", "$0.39"," on ", "15 Nov. 2016")
citi <- paste0("Citigroup div. of ", "$0.16"," on ", "13 Nov. 2016")

# Create the legend in the bottom right corner
legend(x = "bottomright", legend = c(micro, citi), col = c("black", "orange"), lty = c(1, 1))

