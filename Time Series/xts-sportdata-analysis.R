## Encoding and plotting Red Sox data
# As a start, you've compiled data on games played by the Boston Red Sox from 
# 2010 through 2015. In this exercise, you'll explore the data, encode it to xts, 
# and plot some trends over time. 

library(xts)
# Load sports data for Boston
sports_xts <- readRDS('sports.RData')

# subset Redsox data
redsox_xts <- sports_xts[sports_xts$mlb==1,]

# Plot the Red Sox score and the opponent score over time
plot.zoo(redsox_xts[, c("boston_score", "opponent_score")])


## Calculate a closing average
# Now that you've explored some trends in your Red Sox data, you want to produce 
# some useful indicators. In this exercise, you'll calculate the team's win/loss 
# average at the end of each season. In financial terms, you can think of this as 
# the team's value at the close of the season.

# Identify the date of the last game each season. Because baseball seasons are 
# contained in a single year, you can specify the on argument to "years" to give 
# you the final game each year.
close <- endpoints(redsox_xts, on = "years")

# Calculate average win/loss record at the end of each season
period.apply(redsox_xts[, "win_loss"], INDEX = close, FUN = mean)
# In this case, you can see the Red Sox had a very strong season in 2013 (avg = 0.599) 
# and a weak season in 2014 (avg = 0.438).


## Calculate and plot a seasonal average

# Split redsox_xts win_loss data into years 
redsox_seasons <- split(redsox_xts$win_loss, f = "years")

# Use lapply to calculate the cumulative mean for each season
cummean <- function(x){ # Function calculates the sum, and divides by the number of entries in the sum 
  cumsum(x)/seq_along(x)
}
redsox_ytd <- lapply(redsox_seasons, cummean)

# Use do.call to rbind the results. (convert your list output to a single xts 
# object (redsox_winloss) which contains the win/loss average throughout each season.)
redsox_winloss <- do.call(rbind, redsox_ytd)

# Plot the win_loss average for the 2013 season
plot.xts(redsox_winloss["2013"], ylim = c(0, 1))
# The plot shows the cumulative win/loss average after each game in the 2013 season.
# Because each value is a cumulative average within the season, it makes sense that 
# the value is extremely volatile at the beginning of the season, when each win or 
# loss makes a bigger difference.


## Calculate and plot a rolling average
# The final baseball indicator you'd like to generate is the L10, or the moving 
# win/loss average from the previous ten games. While the cumulative win/loss average 
# tells you how the team is doing overall, the L10 indicator provides a more 
# specific picture of the team's recent performance. Beyond the world of sports, 
# this measure is comparable to a financial indicator focused on recent portfolio 
# performance.

# Select only the 2013 season
redsox_2013 <- redsox_xts["2013"]

# Use rollapply to generate the last ten average
lastten_2013 <- rollapply(redsox_2013$win_loss, width = 10, FUN = mean)

# Plot the last ten average during the 2013 season
plot.xts(lastten_2013, ylim = c(0, 1))


## Extract weekend games
library(xts)
# Load sports data for Boston
# data on all games played by Boston-area sports teams from 2010 through 2015
sports <- readRDS('sports.RData')

# Extract the day of the week of each observation
weekday <- .indexwday(sports)
head(weekday) # These values range from 0-6, with Sunday equal to 0 and Saturday equal to 6

# Generate an index of weekend dates
weekend <- which(.indexwday(sports) == 0 | .indexwday(sports) == 6)

# Subset only weekend games
weekend_games <- sports[weekend] # xts object containing data on all weekend games involving Boston sports teams from 2010 through 2015
head(weekend_games)


## Calculate a rolling average across all sports
# You are tasked with generating a rolling win/loss average focused on games played in Boston

# Generate a subset of sports data with only homegames
homegames <- sports[sports$homegame == 1]

# Calculate the win/loss average of the last 20 home games
homegames$win_loss_20 <- rollapply(homegames$win_loss, width = 20, FUN = mean)

# Calculate the win/loss average of the last 100 home games
homegames$win_loss_100 <- rollapply(homegames$win_loss, width = 100, FUN = mean)

# Use plot.zoo() to visualize both indicators - win_loss_20 and win_loss_100
lty <- c(3,1)
lwd <- c(1,2)
plot.zoo(homegames[, c("win_loss_20", "win_loss_100")], plot.type = "single", lty = lty, lwd = lwd)
# Plot shows the general success of Boston sports teams when playing at home both 
# in the short term (20 games) and the long term (100 games). It looks like Boston 
# teams did very well at home in 2014!

