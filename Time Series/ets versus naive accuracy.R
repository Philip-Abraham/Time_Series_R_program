## Evaluating forecast accuracy of non-seasonal methods
# Example: Saudi Arabian oil production

# naive
autoplot(oil)
gghistogram(oil)
training <- window(oil, end = 2003)
test <- window(oil, start = 2004)
fc <- naive(training, h = 10)
autoplot(fc) + autolayer(fitted(fc)) + autolayer(test, series = "Test data")


# ets
etsmod <- ets(training)
ets_fc <- forecast(etsmod)
autoplot(ets_fc) + autolayer(fitted(ets_fc)) + autolayer(test, series = "Test data")


# accuracy
accuracy(fc, test)
accuracy(ets_fc, test)
