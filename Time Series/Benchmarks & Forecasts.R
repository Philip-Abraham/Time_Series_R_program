## Naive forecasting methods

# Forecast is the mean or median of simulated futures of a time series
# Load the fpp2 package
library(fpp2)

# Use naive() to forecast the goog series
fcgoog <- naive(goog, 20)

# Plot and summarize the forecasts
autoplot(fcgoog)
summary(fcgoog)

# Use snaive() to forecast the ausbeer series
fcbeer <- snaive(ausbeer,h=16)

# Plot and summarize the forecasts
autoplot(fcbeer)
summary(fcbeer)


## Naive forecasting methods
# The very simplest forecasting method is to use the most recent observation; 
# this is called a naive forecast. For seasonal data, a related idea is to use 
# the corresponding season from the last year of data. For example, if you want to 
# forecast the sales volume for next March, you would use the sales volume from the 
# previous March. This is implemented in the snaive() function, meaning, seasonal naive.
# For both forecasting methods, you can set the second argument to h which specifies 
# the number of values you want to forecast;

# Use naive() to forecast the goog series
fcgoog <- naive(goog, 20)

# Plot and summarize the forecasts
autoplot(fcgoog)
summary(fcgoog)

# Use snaive() to forecast the ausbeer series
fcbeer <- snaive(ausbeer,h=16)

# Plot and summarize the forecasts
autoplot(fcbeer)
summary(fcbeer)


## Checking time series residuals
# Residuals should look like white noise
# They should be uncorrelated
# They should have mean zero
# They should have constant variance
# They should be normally distributed

# Check the residuals from the naive forecasts applied to the goog series
goog %>% naive() %>% checkresiduals()
# Do they look like white noise (TRUE or FALSE)
googwn <-TRUE

# Check the residuals from the seasonal naive forecasts applied to the ausbeer series
ausbeer %>% snaive() %>% checkresiduals()

# Do they look like white noise (TRUE or FALSE)
beerwn <- FALSE
#  Ljung-Box test shows a small p-value indicating that data does not resemble white noise.


## Evaluating forecast accuracy of non-seasonal methods
# Example: Saudi Arabian oil production
autoplot(oil)
gghistogram(oil)
training <- window(oil, end = 2003)
test <- window(oil, start = 2004)
fc <- naive(training, h = 10)
autoplot(fc) + autolayer(fitted(fc)) + autolayer(test, series = "Test data")
accuracy(fc, test)

# Example: gold series comprises daily gold prices over 1108 trading days. 
autoplot(gold)

library(forecast)
# Create the training data as train  
# for gold comprising the first 1000 observations.
train <- window(gold, end = 1000)

# Create test set
test <- window(gold, start = 1001)

# Compute naive forecasts and save to naive_fc
naive_fc <- naive(train, h = length(gold)-length(train))

# Plot naive forecasts
autoplot(naive_fc) + autolayer(fitted(naive_fc)) + autolayer(test, series = "Test data")

# Compute mean forecasts and save to mean_fc
mean_fc <- meanf(train, h = length(gold)-length(train))

# Plot mean forecasts
autoplot(mean_fc) + autolayer(fitted(mean_fc)) + autolayer(test, series = "Test data")

# Use accuracy() to compute RMSE statistics
accuracy(naive_fc, gold)
accuracy(mean_fc, gold)

# Assign one of the two forecasts as bestforecasts based on their RMSE
bestforecasts <- naive_fc


## Evaluating forecast accuracy of seasonal methods
# Here, you will use the Melbourne quarterly visitor numbers (vn[, "Melbourne"]) 
# to create three different training sets, omitting the last 1, 2 and 3 years, respectively. 

# Create three training series omitting the last 1, 2, and 3 years
train1 <- window(vn[, "Melbourne"], end = c(2014, 4))
train2 <- window(vn[, "Melbourne"], end = c(2013, 4))
train3 <- window(vn[, "Melbourne"], end = c(2012, 4))

# Compute one year (4 quarters/year) of forecasts using snaive() 
fc1 <- snaive(train1, h = 4)
fc2 <- snaive(train2, h = 4)
fc3 <- snaive(train3, h = 4)

# Use accuracy() to compare the MAPE of each series
accuracy(fc1, vn[, "Melbourne"])["Test set", "MAPE"]
accuracy(fc2, vn[, "Melbourne"])["Test set", "MAPE"]
accuracy(fc3, vn[, "Melbourne"])["Test set", "MAPE"]


## time series cross-validation
# Here, you will use tsCV() to compute and plot the MSE values for up to 8 steps 
# ahead, along with the naive() method applied to the goog data. 

# Compute cross-validated errors for up to 8 steps ahead
e <- matrix(NA_real_, nrow = 1000, ncol = 8)
for (h in 1:8){
        e[, h] <- tsCV(goog, forecastfunction = naive, h = h)}

# Compute the MSE values and remove missing values
mse <- colMeans(e^2, na.rm = TRUE)

# Plot the MSE values against the forecast horizon
data.frame(h = 1:8, MSE = mse) %>%
        ggplot(aes(x = h, y = MSE)) + geom_point()
