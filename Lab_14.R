#~~~~~~~ Step 1 – Install and Load Packages
setwd("~/Desktop/AGE333_Lab14")
install.packages("cowplot")
library("ggplot2")
library("gridExtra")
library(cowplot)

#~~~~~~~ Step 2 – Data Import

WASDE <- read.csv('WASDE.csv')

#~~~~~~~ Step 3 – Visualize Corn Price, Demand, and Supply Trends Over Time
g_price <- ggplot(data=WASDE, aes(x=year, y=corn_price)) +
      geom_line(color="red") +
      ggtitle("Corn Prices over Time (1973-2019)") +
      labs(y="Corn Price ($)", x="Year")

g_demand <- ggplot(data=WASDE, aes(x=year, y=total_use)) +
      geom_line(color="blue") +
      ggtitle("Corn Demand over Time (1973-2019)") +
      labs(y="Use (Mil bu.)", x="Year")

g_supply <- ggplot(data=WASDE, aes(x=year, y=total_supply)) +
      geom_line(color="green") +
      ggtitle("Corn Supply over Time (1973-2019)") +
      labs(y="Supply (Mil bu.)", x="Year")

plot_grid(g_price, g_demand, g_supply, ncol = 1)


#~~~~~~~ Step 4 – Relating Price to Corn Stock-to-Use Ratio

# Create a new variable for stock-to-use ratio and create a scatterplot of corn prices on stock-to-use ratio
WASDE$SUR <- WASDE$end_stocks / WASDE$total_use


# Plot corn price vs. stock-to-use ratio with a regression line
  ggplot(data=WASDE, aes(x=SUR, y=corn_price)) +
      geom_point(shape=1) +
      geom_smooth(method=lm, color="red") +
      ggtitle("Corn Prices vs. Stock-to-Use Ratio (1973–2019)") +
      labs(y="Corn Price ($)", x="Stock-to-Use Ratio") + theme_minimal()

# Add a watermark to the combined plot

#~~~~~~~ Step 5 - Linear Regression and Diagnostics

# Estimate linear regression model
reg1 <- lm(corn_price ~ SUR, data=WASDE)

library(gtsummary)
tbl_regression(reg1, intercept = TRUE) %>%
      add_glance_source_note(include = c(r.squared, nobs))

## Calculating Price Elasticity

# Calculate averages
mean_sur <- mean(WASDE$SUR)
mean_price <- mean(WASDE$corn_price)


# Residual Analysis
# Summary statistics of residuals
summary(resid(reg1))

# Histogram of residuals
hist(resid(reg1), 
     main = "Histogram of Linear Regression Errors",
     xlab = "Linear Model Residuals")


# Residuals vs. SUR Plot

# Scatterplot of errors vs SUR
ggplot(data=WASDE, aes(x=SUR, y=resid(reg1))) +
      geom_point(shape=1) +
      ggtitle("Linear Regression Errors vs. Stock-to-Use Ratio") +
      labs(y="Errors", x="Stock-to-Use Ratio")



#~~~~~~~ Step 6 - Non-linear Regression and Diagnostics

# Estimating the Inverse SUR Model

# Create the inverse of stock-to-use ratio, run the regression, and examine the error terms
WASDE$SUR_Inv <- 1 / WASDE$SUR
reg2 <- lm(corn_price ~ SUR_Inv, data=WASDE)

summary(reg2)

# Residual Diagnostics (Non-linear Model)
# Residual analysis
summary(resid(reg2))
hist(resid(reg2), main="Histogram of Non-linear Regression Errors", xlab="Non-linear Model Residuals")

# Residuals vs SUR plot
ggplot(data=WASDE, aes(x=SUR, y=resid(reg2))) +
      geom_point(shape=1) +
      ggtitle("Non-linear Regression Errors vs. Stock-to-Use Ratio") +
      labs(y="Errors", x="Stock-to-Use Ratio")


#~~~~~~~ Step 7 - Estimating separate demand curves for different time periods

# Create a character variable denoting the two time periods, create a dummy variable for the post-2006 period, graph a scatterplot of price on SUR with unique colors and regression lines for each period
WASDE$period <- ifelse(WASDE$year >= 2006, "2006-2019", "1973-2005")
WASDE$P2006 <- as.numeric(WASDE$year >= 2006)

ggplot(data=WASDE, aes(x=SUR, y=corn_price, color=period)) +
      geom_point(shape=1) +
      geom_smooth(method=lm, se=FALSE) +
      ggtitle("Corn Prices vs. Stock-to-Use Ratio (1973–2019)") +
      labs(y="Corn Price ($)", x="Stock-to-Use Ratio")


# Run a linear regression with time period specific
reg3 <- lm(corn_price ~ SUR + P2006 + SUR:P2006, data=WASDE)

summary(reg3)


#~~~~~~~ Step 8 - Check for auto-correlation/serial correlation

# Collect the residuals from the last regression, create a time series of the errors with a one-year lag of the error, then regress the error terms on the lagged error terms
error <- ts(resid(reg3), start=1973, end=2019, frequency=1)   # the ts() function tells R to set the errors as a time-series 
lag_error <- lag(error, -1)                                   # the lag() function creates a one-period lag of the error term
error <- cbind(error, lag_error)                              # cbind() binds the specified vectors together as columns to create a new data frame

reg4 <- lm(error ~ lag_error, data=error)

summary(reg4)




