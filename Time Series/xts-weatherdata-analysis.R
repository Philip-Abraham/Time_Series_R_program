## Weather Data
# explore weather patterns in the Boston area to understand what might be 
# affecting flight delays and cancellations

## Generate a single Boston climate data (data from Weather Underground) xts object
weatherDat_Bos2007 <- readRDS("weatherDat_Bos2007.RData") # for some reason i can't use "load" on 2007 data
load("weatherDat_Bos2008.RData")
load("weatherDat_Bos2009.RData")
load("weatherDat_Bos2010.RData")
load("weatherDat_Bos2011.RData")
load("weatherDat_Bos2012.RData")
load("weatherDat_Bos2013.RData")
load("weatherDat_Bos2014.RData")
load("weatherDat_Bos2015.RData")

# combine 2007-2015 data
df <- rbind(weatherDat_Bos2007,weatherDat_Bos2008,weatherDat_Bos2009,
            weatherDat_Bos2010,weatherDat_Bos2011,weatherDat_Bos2012,
            weatherDat_Bos2013,weatherDat_Bos2014,weatherDat_Bos2015)
# extract just temperatures
temps <-df[,2]
dates <- df[,1]

# Use xts() to create time series
library(xts)
temps_xts_1 <- xts(x = temps, order.by = dates)

tempdays<- split(temps_xts_1[,1], f = "days")

tempdays_max <- lapply(tempdays, max) # find max temp for each day
tempdays_max.ytd <- do.call(rbind, tempdays_max)

tempdays_avg <- lapply(tempdays, mean) # find mean temp for each day
tempdays_avg.ytd <- do.call(rbind, tempdays_avg)

tempdays_min <- lapply(tempdays, min) # find min temp for each day
tempdays_min.ytd <- do.call(rbind, tempdays_min)

tempdata <- as.data.frame(cbind(tempdays_max.ytd,tempdays_avg.ytd,tempdays_min.ytd))
names(tempdata) <- c("max","mean","min")

# Create dates as a Date class object starting from 2007-01-01
dates <- seq(as.Date("2007-01-01"), length = length(tempdays_max.ytd), by = "days")
# Use xts() to create temps_xts
library(xts)
temps_xts <- xts(x = tempdata, order.by = dates)

# View data for the first 3 days of the last month of the first year in temps_xts
first(last(first(temps_xts, "1 year"), "1 month"), "3 days")

## Visualizing Boston winters
# You discovered in flight data analysis that a much higher percentage of flights 
# are delayed or cancelled in Boston during the winter. It seems logical that 
# temperature is an important factor here. Perhaps colder temperatures are 
# associated with a higher percentage of flight delays or cancellations?
# In this exercise, you'll probe the plausibility of this hypothesis by plotting 
# temperature trends over time and generating a visual overview of Boston winters.

# Identify the periodicity of temps_xts
periodicity(temps_xts)

# Generate a plot of mean Boston temperature for the duration of your data
plot.xts(temps_xts$mean)

# Generate a plot of mean Boston temperature from November 2010 through April 2011
plot.xts(temps_xts$mean["2010-11/2011-04"])

# Use plot.zoo to generate a single plot showing mean, max, and min temperatures during the same period 
lty<- c(3,1,3)
plot.zoo(temps_xts["2010-11/2011-04"], plot.type = "single", lty = lty)


## Subsetting and adjusting periodicity
# Next step is to merge your temperature data with the flight data
# Flight data stretches from 2010 through 2015 in monthly periods. By contrast, 
# your temperature data ranges from 2007 through 2015 in daily periods. Before you 
# merge, you should subset your data and adjust the periodicity to monthly

# load flight xts object
load("flights_xts.RData")

# Subset your temperature data to include only 2010 through 2015: temps_xts_2
temps_xts_2 <- temps_xts["2010/2015"]

# Use to.period to convert temps_xts_2 to monthly periodicity
# the data from the first day of each month will be representatie data for  that month
temps_monthly <- to.period(temps_xts_2, period = "months", OHLC = FALSE, indexAt = "firstof")

# Compare the periodicity and duration of temps_monthly and flights_xts 
periodicity(temps_monthly)
periodicity(flights_xts)

# Is the value selected by the to.period() call (in this case, the first of the month) 
# appropriate for this context?
# It may not be useful to select a single row as representative of the entire month
# Instead, it makes more sense to generate average temperature values per month


## Generating a monthly average
# The three key steps here involve encoding your new data to xts objects, 
# generating monthly averages from those objects, and checking the periodicity 
# and duration before you attempt a merge

# Split temps_xts_2 into separate lists per month
monthly_split <- split(temps_xts_2$mean , f = "months")

# Use lapply to generate the monthly mean of mean temperatures
mean_of_means <- lapply(monthly_split, FUN = mean)

# Use as.xts to generate an xts object of average monthly temperature data
library(xts)
index <- seq(as.Date("2010-01-01"), length = nmonths(temps_monthly), by = "months")
temps_monthly <- as.xts(as.numeric(mean_of_means), order.by = index)

# Compare the periodicity and duration of your new temps_monthly and flights_xts 
periodicity(temps_monthly)
periodicity(flights_xts)


## Using merge() and plotting over time
# Now that you have temperature data covering the same time period (2010-2015) at 
# the same frequency (monthly) as your flights data, you are ready to merge
# In this exercise, you'll merge your two xts objects by column and generate new 
# plots exploring how flight delays and cancellations relate to temperature

# Use merge to combine your flights and temperature objects
temps_monthly <- readRDS('temps_monthly.RData') # temperory until i get full data
flights_temps <- merge(flights_xts, temps_monthly)

# Examine the first few rows of your combined xts object
head(flights_temps)

# Plot these three columns in a single panel to easily visualize the relationship 
# between flight delays, cancellations and temperature
lty <- c(1,2,3)
plot.zoo(flights_temps[,c("pct_cancel", "pct_delay", "temps_monthly")], plot.type = "single", lty = lty)
labels <- c("Pct. Cancel", "Pct. Delay", "Temperature")
legend("topright", lty = lty, legend = labels, bg = "white")
# The relationship between flight cancellations and temperature is strong, while 
# the relationship between flight delays and temperature is less obvious


## Are flight delays related to visibility or wind?
# Assess the hypothesis that flight delays are a function of visibility and wind

# load monthly visibility and wind data for Boston for the 2010-2015 period
vis <- readRDS('vis.RData')
wind <- readRDS('wind.RData')

# Confirm the periodicity and duration of the vis and wind data
periodicity(vis)
periodicity(wind)

# Merge vis and wind with your existing flights_temps data
flights_weather <- merge(flights_temps,vis,wind)

# View the first few rows of your flights_weather data
head(flights_weather)

# Plot these three columns in a single panel to easily visualize the relationship 
# between flight delays, visibility and wind
lty <- c(1,2,3)
plot.zoo(flights_weather[,c("pct_delay", "vis", "wind")], plot.type = "multiple", lty = lty)
labels <- c("Pct. Delay", "Visibility", "Wind")
# Although this plot doesn't depict any strong relationships between visibility or 
# wind and delayed flights, it tells you quite a bit about your data. Wind speed 
# sometimes corresponds to flight delays, while visibility does not appear related 
# to delays. More importantly, the flatline data on visibility prior to 2012 should 
# raise some eyebrows. Before proceeding with your analysis, you may want to 
# reassess the quality of your data