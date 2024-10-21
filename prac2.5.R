###SARIMA###


#1) DATA PREPARATION
# A]
# Load the necessary libraries
library(forecast)

# Create the sales data
sales_data <- c(200, 220, 240, 210, 230, 250, 300, 270, 260, 280, 310, 330, 
                220, 230, 250, 240, 260, 280, 320, 300, 290, 310, 330, 350, 
                240, 250, 270, 260, 280, 300, 340, 320, 310, 330, 350, 370, 
                260, 270, 290, 280, 300, 320, 360, 340, 330, 350, 370, 390, 
                280, 290, 310, 300, 320, 340, 380, 360, 350, 370, 390, 410)

# Convert to time series object
sales_ts <- ts(sales_data, start = c(2019, 1), frequency = 12)

# Plot the sales data
plot(sales_ts, main = "Monthly Sales Data", xlab = "Time", ylab = "Sales", col = "blue", lwd = 2)

# Observations: Check for seasonality and trend by inspecting the plot

#B]
# Differencing to remove trend
diff_sales_ts <- diff(sales_ts, differences = 1)
plot(diff_sales_ts, main = "Differenced Sales Data", xlab = "Time", ylab = "Differenced Sales")

#2) MODEL IDETIFICATION
#A)
# Perform Augmented Dickey-Fuller test for stationarity
library(tseries)
adf_test <- adf.test(sales_ts)
print(adf_test)
# If p-value > 0.05, differencing is needed (non-stationary).

#B)
# Seasonal differencing can be checked by examining the ACF plot
Acf(sales_ts)
# Look for seasonal lags in the ACF plot (e.g., significant peaks at 12 months).

#c)
# ACF and PACF plots
Acf(diff_sales_ts, main="ACF of Differenced Sales Data")
Pacf(diff_sales_ts, main="PACF of Differenced Sales Data")

#3) SARIMA MODEL FITTING
#a)
# Fit a SARIMA model using auto.arima
sarima_model <- auto.arima(sales_ts, seasonal = TRUE)
summary(sarima_model)

#b)
# Plot residuals to check for white noise
checkresiduals(sarima_model)

#4) Forecasting with SARIMA
#a)
# Forecast for the next 12 months
forecast_sales <- forecast(sarima_model, h = 12)
plot(forecast_sales, main = "Sales Forecast for Next 12 Months", xlab = "Time", ylab = "Sales")

#b)

#5) Model Comparison

#a)
# Fit ARIMA without seasonality
arima_model <- auto.arima(sales_ts, seasonal = FALSE)
summary(arima_model)

#b)
# Compare models using AIC, BIC, and RMSE
sarima_aic <- AIC(sarima_model)
arima_aic <- AIC(arima_model)

sarima_bic <- BIC(sarima_model)
arima_bic <- BIC(arima_model)

sarima_rmse <- sqrt(mean(residuals(sarima_model)^2))
arima_rmse <- sqrt(mean(residuals(arima_model)^2))

print(paste("SARIMA AIC:", sarima_aic, "BIC:", sarima_bic, "RMSE:", sarima_rmse))
print(paste("ARIMA AIC:", arima_aic, "BIC:", arima_bic, "RMSE:", arima_rmse))

#c)

#6)

#a)
# Suppose you have a holiday variable (1 for holiday, 0 for non-holiday)
# Add holiday variable as external regressor
holiday_effect <- c(rep(0, 11), 1, rep(0, 11), 1, rep(0, 11), 1, rep(0, 11), 1, rep(0, 11), 1)

# Fit SARIMA with external regressors
sarima_with_holiday <- auto.arima(sales_ts, xreg = holiday_effect, seasonal = TRUE)
summary(sarima_with_holiday)


#b)
# Compare the SARIMA with and without holiday adjustments using AIC, BIC, and RMSE
sarima_holiday_aic <- AIC(sarima_with_holiday)
sarima_holiday_bic <- BIC(sarima_with_holiday)
sarima_holiday_rmse <- sqrt(mean(residuals(sarima_with_holiday)^2))

print(paste("SARIMA with holiday AIC:", sarima_holiday_aic, "BIC:", sarima_holiday_bic, "RMSE:", sarima_holiday_rmse))

