###Exponential smoothing and holts###

# Load necessary libraries
install.packages("forecast")
library(forecast)

# Prepare the time series data
years <- 1996:2019
obs<- c(150.3, 150.9, 151.4, 151.9, 152.5, 152.9, 153.2, 153.7, 153.6, 153.5, 154.4, 154.9, 155.7, 156.3, 156.6, 156.7, 157, 157.3, 157.8, 158.3, 158.6, 158.6, 159.1, 159.3
)
ts_data <- ts(obs, start = min(years), frequency = 1)

# Simple Exponential Smoothing
alpha <- 0.3
ses_model <- ses(ts_data, alpha = alpha)
ses_model

# Holt’s Exponential Smoothing
beta <- 0.2
holt_model <- holt(ts_data, alpha = alpha, beta = beta)
holt_model

# Plot the original data and both smoothed models on the same graph
plot(ts_data, type = "o", col = "blue", xlab = "Year", ylab = "Observation", main = "Time Series with Smoothing")
lines(ses_model$fitted, col = "red", lty = 1)  # SES smoothed values
lines(holt_model$fitted, col = "green", lty = 2)  # Holt's smoothed values

# Add a legend
legend("topleft", legend = c("Original Data", "SES Smoothed", "Holt's Smoothed"),
       col = c("blue", "red", "green"), lty = 1:2, pch = 1)





###Atmospheric CO2 Concentration - forecast###

# Load necessary libraries
install.packages("ggplot2")
library(ggplot2)  # For enhanced plotting

# Input the data
years <- 1991:2003
co2_concentration <- c(355.62, 356.36, 357.1, 358.86, 360.9, 362.58, 363.84, 366.58, 
                       368.3, 369.47, 371.03, 373.61, 357.61)

# Create a time series object
ts_co2 <- ts(co2_concentration, start=1991, end=2003)

# Plot the time series data
plot(ts_co2, type="o", col="blue", lwd=2, ylab="Average CO2 Concentration", 
     xlab="Year", main="Atmospheric CO2 Concentration at Mauna Loa (1991-2003)")

# Calculate the 3-year moving average (sides=1 for future forecasting)
moving_avg <- filter(ts_co2, filter=rep(1/3, 3), sides=1)

# Extract the last moving average value as the forecast for 2004
forecast_2004 <- tail(moving_avg, 1)

# Print the forecasted value for 2004
cat("Forecasted CO2 Concentration for 2004:", round(forecast_2004, 2), "\n")

# Combine the forecasted value with the original data
co2_concentration_forecast <- c(co2_concentration, forecast_2004)
ts_co2_forecast <- ts(co2_concentration_forecast, start=1991, end=2004)

# Plot the time series including the forecasted value for 2004
plot(ts_co2_forecast, type="o", col="blue", lwd=2, ylab="Average CO2 Concentration", 
     xlab="Year", main="Atmospheric CO2 Concentration with 2004 Forecast")

# Highlight the forecasted value for 2004
points(2004, forecast_2004, col="red", pch=19)
text(2004, forecast_2004, labels=paste("Forecasted 2004:", round(forecast_2004, 2)), 
     pos=4, col="red")




###holts on airpassenger###

# Load necessary libraries
install.packages("forecast")
library(forecast)

# Load the AirPassengers data
data("AirPassengers")

# Plot the original data
plot(AirPassengers, main = "AirPassengers Data", ylab = "Number of Passengers", xlab = "Year")

# Apply Holt-Winters Multiplicative Model
holt_winters_model <- HoltWinters(AirPassengers, gamma = TRUE, seasonal = "multiplicative")

# Forecast the next 12 months
forecasted_values <- forecast(holt_winters_model, h = 12)
forecasted_values

# Plot the forecasts
plot(forecasted_values, main = "Holt-Winters Forecast for AirPassengers", ylab = "Number of Passengers", xlab = "Year")
