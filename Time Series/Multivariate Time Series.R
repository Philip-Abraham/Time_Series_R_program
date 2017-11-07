## Two time series grouped or stacked

# load csv
df <- read.csv("Book1.csv", header = TRUE, sep = ",")

# Create date index
dates <- seq(as.Date("2016-01-01"), length = 12, by = "months")

# Use xts() to create portfolio
library(xts)
portfolio <- xts(x = df, order.by = dates)

# Stacked bar chart: for each period, there is a single bar, and each time series 
# is represented by a portion of the bar proportional to the value of the time 
# series at this date (i.e. the total at each period adds up to 100%)

# Plot stacked barplot
barplot(portfolio)

# Grouped barchart: for a single period, there are as many bars as time series

# Plot grouped barplot
barplot(portfolio, beside=TRUE)


## Visualizing bivariate relationships
# In this exercise, you will draw a scatterplot and regression line for the return 
# series for the SP500 (sp500) and Citigroup (citi) 

# load csv
df <- read.csv("data_3_2.csv", header = TRUE, sep = ",")
# SP500 returns
sp500 <- df$sp500
# Citigroup returns
citi <- df$citigroup

# Draw the scatterplot
plot(x = sp500, y = citi)

# Add a regression line of citi against sp500
lm(citi ~ sp500)
abline(reg=lm(citi ~ sp500),col="red", lwd=2)
# It looks there is definitely a positive linear relationship


## Correlation matrix
# Pearson correlation: measures the linear relationship between 2 variables.
# Spearman rank correlation: measures the statistical dependency between the ranking 
# of 2 variables (not necessarily linear).
# The latter is used when there is no assumption made on the distribution of the data.

# In this exercise, you will calculate the correlation matrix of the data provided 
# in the dataset my_data containing the returns for six stocks
# load csv
df <- read.csv("data_3_2.csv", header = TRUE, sep = ",")
my_data <- df[,-1]

# Notice how the two methods calculate different correlation values.

# Create correlation matrix using Pearson method
round(cor(my_data), digit = 4)

# Create correlation matrix using Spearman method
round(cor(my_data,method="spearman"), digit = 4)


## Scatterplots for multiple pairs of data
# A scatterplot matrix can be useful to visualize everything at once
# Create upper panel scatterplot matrix
pairs(my_data,lower.panel = NULL)


## Correlation plot
cor_mat <- cor(my_data)

library(corrplot)
# Plot the correlation matrix of cor_mat using default settings
corrplot(cor_mat)

# Create correlation matrix with numbers
corrplot(cor_mat, method="number")

# Create correlation matrix with colors
corrplot(cor_mat, method="color")

# Create upper triangle correlation matrix
corrplot(cor_mat, method="number", type = "upper")


## Correlation matrix as heatmap
# Should you want to check correlations betweens hundreds of time series, 
# representing correlations with numbers is not really helpful - for a dataset of 
# 100 elements, you would have to analyze 10,000 (100 x 100) correlation numbers!
# In this case, a heatmap is a better suited tool. A heatmap is a map or diagram 
# in which data values are represented as colors

# Create a correlation matrix
a <- rnorm(13, mean=.1, sd=.3)
b <- rnorm(13, mean=.1, sd=.3)
c <- rnorm(13, mean=.1, sd=.3)
d <- rnorm(13, mean=.1, sd=.3)
e <- rnorm(13, mean=.1, sd=.3)
f <- rnorm(13, mean=.1, sd=.3)
g <- rnorm(13, mean=.1, sd=.3)
h <- rnorm(13, mean=.1, sd=.3)
i <- rnorm(13, mean=.1, sd=.3)
j <- rnorm(13, mean=.1, sd=.3)
k <- rnorm(13, mean=.1, sd=.3)
l <- rnorm(13, mean=.1, sd=.3)
m <- rnorm(13, mean=.1, sd=.3)

cor_mat <- matrix(c(a,b,c,d,e,f,g,h,i,j,k,l,m),nrow=13,ncol=13)
for(i in 1:dim(cor_mat)[2]){
  cor_mat[i,i] <- 1
}  

# Draw heatmap of cor_mat
corrplot(cor_mat, method = "color")

# Draw upper heatmap
corrplot(cor_mat, method = "color", type = "upper")

# Draw the upper heatmap ordering the matrix using hclust in the order argument
corrplot(cor_mat, method = "color", type = "upper", order="hclust")


