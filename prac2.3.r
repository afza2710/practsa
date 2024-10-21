###AR process###

# Load necessary libraries
library(forecast)
library(tseries)
#Q-1))
# 1 (a) Simulate 100 observations from AR(1) process
set.seed(42)
n <- 100
phi <- 0.5
ar1_process <- arima.sim(model = list(ar = phi), n = n)



# 1 (b) Plot the time series
plot.ts(ar1_process, main = "Simulated AR(1) Process", ylab = "X_t", col = "blue")


# 1 (c) Estimate AR(1) parameter using arima function
ar1_fit <- arima(ar1_process, order = c(1, 0, 0))
phi_estimated <- ar1_fit$coef[1]
phi_estimated  # Print estimated phi


# 2 (i) Plot ACF and PACF of the series
par(mfrow = c(1, 2))  # Set up a 1x2 plotting grid
acf(ar1_process, main = "ACF of AR(1) Process")
pacf(ar1_process, main = "PACF of AR(1) Process")
par(mfrow = c(1, 1))  # Reset plotting grid


# 3 (i) Fit AR(1) or AR(2) model to the data
ar_fit <- arima(ar1_process, order = c(1, 0, 0))  # AR(1)
ar_fit_2 <- arima(ar1_process, order = c(2, 0, 0))  # AR(2)

# 3 (ii) Forecast the next 10 observations and plot the forecast
forecasted_values <- forecast(ar_fit, h = 10)
plot(forecasted_values, main = "Forecast of AR(1) Process", col = "red")





###MA process###

# Load necessary libraries
library(forecast)
library(tseries)

# 1 (a) Simulate 100 observations from MA(1) process
set.seed(42)
n <- 100
theta <- 0.5
ma1_process <- arima.sim(model = list(ma = theta), n = n)

# 1 (b) Plot the time series
plot.ts(ma1_process, main = "Simulated MA(1) Process", ylab = "X_t", col = "blue")

# 1 (c) Estimate MA(1) parameter using arima function
ma1_fit <- arima(ma1_process, order = c(0, 0, 1))
theta_estimated <- ma1_fit$coef[1]
theta_estimated  # Print estimated theta

# 2 (i) Plot ACF and PACF of the series
par(mfrow = c(1, 2))  # Set up a 1x2 plotting grid
acf(ma1_process, main = "ACF of MA(1) Process")
pacf(ma1_process, main = "PACF of MA(1) Process")
par(mfrow = c(1, 1))  # Reset plotting grid

# 3 (i) Fit MA(1) or MA(2) model to the data
ma_fit <- arima(ma1_process, order = c(0, 0, 1))  # MA(1) model
ma_fit_2 <- arima(ma1_process, order = c(0, 0, 2))  # MA(2) model

# 3 (ii) Forecast the next 10 observations and plot the forecast
forecasted_values <- forecast(ma_fit, h = 10)
plot(forecasted_values, main = "Forecast of MA(1) Process", col = "red")





###ARMA2,2###

# Load necessary libraries
library(forecast)
library(tseries)

# Step 0: Set up the Data
data <- data.frame(
  Time = 1:15,
  Values = c(100, 110, 120, 130, 140, 150, 160, 170, 180, 190, 200, 210, 220, 230, 240)
)

# Convert the data to a time series object
ts_data <- ts(data$Values)

# Step 1: Plot the ACF and PACF
par(mfrow = c(1, 2))  # Plot ACF and PACF side by side
acf(ts_data, main = "ACF of Original Series")
pacf(ts_data, main = "PACF of Original Series")
par(mfrow = c(1, 1))  # Reset plotting layout

# Step 2: Perform Stationarity Test (ADF test)
adf_test <- adf.test(ts_data)
print(adf_test)

# Step 3: Fit an ARMA(2,2) Model to the original data
model_arma <- arima(ts_data, order = c(2, 0, 2))#pval>0.05 nonstationary
model_arma <- arima(ts_data, order = c(1, 0, 1))# so make it 1,0,1
summary(model_arma)


### COMMENT ###

# this data is non stationary thats why it will not fit in arima(2,2). To make this work first convert this data into stationary and then proceed

# Step 4: Check the Residual Diagnostics
checkresiduals(model_arma)

# Step 5: Forecast the Next 10 Values
forecasted_values <- forecast(model_arma, h = 10)

# Step 6: Plot the Original Time Series, Fitted Values, and Forecasted Values
plot(forecasted_values, main = "Original and Forecasted Values", 
     xlab = "Time", ylab = "Values", ylim = c(100, 300))
lines(ts_data, col = "blue", lwd = 2)  # Original values
lines(fitted(model_arma), col = "red", lwd = 2)  # Fitted values
legend("topleft", legend = c("Original", "Fitted", "Forecast"),
       col = c("blue", "red", "black"), lty = 1, bty = "n")






###ARMA1,1###

# Load necessary libraries
library(forecast)
library(tseries)

# Step 1: Set up the Data
data <- data.frame(
  Quarter = 1:12,
  Sales = c(500, 520, 540, 560, 580, 600, 620, 640, 660, 680, 700, 720)
)

# Convert the sales data to a time series object
ts_sales <- ts(data$Sales, start = 1, frequency = 1)

# Step 2: Plot ACF and PACF
par(mfrow = c(1, 2))  # Set up the plotting area
acf(ts_sales, main = "ACF of Sales Data")
pacf(ts_sales, main = "PACF of Sales Data")

# Step 3: Fit an ARMA(1,1) Model
model_arma <- arima(ts_sales, order = c(1, 0, 1))
summary(model_arma)

# Step 4: Check the Residual Diagnostics
checkresiduals(model_arma)

# Step 5: Forecast the Next 12 Values
forecasted_values <- forecast(model_arma, h = 12)

# Step 6: Plot the Original Time Series, Fitted Values, and Forecasted Values
plot(forecasted_values, main = "Original and Forecasted Sales Values", 
     xlab = "Quarter", ylab = "Sales", ylim = c(480, 800))
lines(ts_sales, col = "blue", lwd = 2)  # Original values
lines(fitted(model_arma), col = "red", lwd = 2)  # Fitted values
legend("topleft", legend = c("Original", "Fitted", "Forecast"),
       col = c("blue", "red", "black"), lty = 1, bty = "n")

