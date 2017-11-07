# Imagine you already own a portfolio of stocks and you have some spare cash to 
# invest, how can you wisely select a new stock to invest your additional cash? 
# Analyzing the statistical properties of individual stocks vs. an existing 
# portfolio is a good way of approaching the problem.

# Correlation to your existing portfolio to assess diversification, return 
# histogram to assess risk and box and whisker plot to assess average return

## Current portfolio description
# Your savings are invested in a portfolio made of 3 stocks: Yahoo, Apple and 
# Microsoft. Each stocks has the same weight in the portfolio at 33%. You have 
# some extra cash to invest, but before going any further, you want to gather 
# some information on your existing portfolio.

# load csv
df <- read.csv("data_4_1.csv", header = TRUE, sep = ",")
x <- df[,-1]
dates <- as.Date(df$Index)
library(xts)
data <- as.xts(x,order.by = dates)

# Plot the portfolio value
plot(data$value, main="Portfolio Value")

# Plot the portfolio return
plot(data$return, main="Portfolio Return")

# Plot a histogram of portfolio return 
hist(data$return, probability = TRUE)

# Add a density line
lines(density(data$return),col="red", lwd=2)

## Conclusions of existing portfolio
# Good performance, return positive on average, and some extreme returns
# The value increased by 100% over the course of 7 years
# Really good performance between 2010 and 2014
# No performance since end of 2014

# Rule number one in investment: capital protection
# Low correlation = protection from severe losses

## Goal: 
# Choose one stock to invest your spare cash
# A new dataset
# Choose only one stock
# Compare old and new portfolio
# Pick one new stock to add to existing portfolio from the four stocks in data_new:
# Goldman Sachs (GS)
# Coca-Cola (KO)
# Walt Disney (DIS)
# Caterpillar (CAT)

# load csv
df_new <- read.csv("data_4_3.csv", header = TRUE, sep = ",")
x_new <- df_new[,-1]
dates_new <- as.Date(df_new$Index)
library(xts)
data_new <- as.xts(x_new,order.by = dates_new)

# Plot the four stocks on the same graphical window
par(mfrow = c(2, 2), mex = 0.8, cex = 0.8) # character size and the margin size 80% of their normal sizes
plot(data_new$GS)
plot(data_new$KO)
plot(data_new$DIS)
plot(data_new$CAT)

# Now that you know what the new stocks look like, you want to find out if any of 
# them provide diversification benefits to your existing portfolio. 
# You can do this by looking at the correlation of each stock to our portfolio, 
# visualized through regression lines.

# The return of your existing portfolio
portfolio <- df$return
# The returns of available new stocks
library(TTR)
gs <- ROC(df_new$GS); gs <- gs[-1]
ko <- ROC(df_new$KO); ko <- ko[-1]
dis <- ROC(df_new$DIS); dis <- dis[-1]
cat <- ROC(df_new$CAT); cat <- cat[-1]

# On a single graphical window, draw the scatterplots of the four stocks against the 
# portfolio returns, and add the regression lines of the four stock returns 
# against the portfolio returns
par(mfrow = c(2, 2))

plot(x = portfolio, y = gs)
abline(reg = lm(gs ~ portfolio), col = "red", lwd = 2)

plot(x = portfolio, y = ko)
abline(reg = lm(ko ~ portfolio), col = "red", lwd = 2)

plot(x = portfolio, y = dis)
abline(reg = lm(dis ~ portfolio), col = "red", lwd = 2)

plot(x = portfolio, y = cat)
abline(reg = lm(cat ~ portfolio), col = "red", lwd = 2)

# Per slope of regression line, Coca-Cola stock is the least correlated with the 
# existing portfolio. Therefore, Coca-Cola stock is best for you to buy 
# to add diversification to your portfolio.


# You decide to buy stocks in Coca-Cola, and now your portfolio is made of equal 
# proportions of four stocks: Yahoo, Microsoft, Apple and Coca-Cola.

# load csv
df <- read.csv("old.vs.new.portfolio.csv", header = TRUE, sep = ",")
x <- df[,-1]
dates <- as.Date(df$Index)
library(xts)
old.vs.new.portfolio <- as.xts(x,order.by = dates)

# Plot new and old portfolio values on same chart
plot(old.vs.new.portfolio$old.portfolio.value)
lines(old.vs.new.portfolio$new.portfolio.value, col="red")

# Plot density of the new and old portfolio returns on same chart
plot(density(old.vs.new.portfolio$old.portfolio.rtn))
lines(density(old.vs.new.portfolio$new.portfolio.rtn),col="red")
# The new portfolio seems to have less variation based on the density lines


## A more accurate comparison of portfolios
# Looking at the value and distribution of returns of your portfolio is a good 
# start, but it doesn't necessarily tell the whole story. You could obviously look 
# at many other charts and metrics, but ultimately what matters is performance and 
# specifically periods of poor performance.

# Any time the cumulative returns dips below the maximum cumulative returns, 
# it's a drawdown. Drawdowns are measured as a percentage of that maximum cumulative return, in effect, measured from peak equity.

library(PerformanceAnalytics)
# Draw value, return, drawdowns of old portfolio
charts.PerformanceSummary(old.vs.new.portfolio$old.portfolio.rtn)

# Draw value, return, drawdowns of new portfolio
charts.PerformanceSummary(old.vs.new.portfolio$new.portfolio.rtn)

# Draw both portfolios on same chart
charts.PerformanceSummary(old.vs.new.portfolio[, c(3, 4)])
# The new portfolio looks to have a higher cumulative return and lower drawdown 
# for this period of time

