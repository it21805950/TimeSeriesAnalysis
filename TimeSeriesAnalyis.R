
set.seed(15794)
y = arima.sim(list(order = c(1,1,0), ar = 0.7), n = 300)

# Plot the time series
plot(y, type = "l", col = "blue", lwd = 2, xlab = "Time", ylab = "Value", main = " Time Series Simulation")


# Plot the ACF
acf(y, main = "Sample Autocorrelation Function (ACF) ", xlab = "Lag", ylab = "ACF")

# Plot the PACF
pacf(y, main = "Sample Partial Autocorrelation Function (PACF) ", xlab = "Lag", ylab = "PACF")


# Apply first differencing to the data
y_diff = diff(y)

# Plot the differenced data
plot(y_diff, type = "l", col = "blue", lwd = 2, xlab = "Time", ylab = "Differenced Value", main = "Differenced Time Series")

# Plot the ACF of the differenced data
acf(y_diff, main = "ACF of Differenced Time Series", xlab = "Lag", ylab = "ACF")

# Plot the PACF of the differenced data
pacf(y_diff, main = "PACF of Differenced Time Series", xlab = "Lag", ylab = "PACF")




# install.packages("tseries")
library(tseries)

# Perform the Augmented Dickey-Fuller test
adf_test_result = adf.test(y_diff)

# Print the ADF test results
print(adf_test_result)

acf(y_diff, main = "Sample Autocorrelation Function (ACF) ", xlab = "Lag", ylab = "ACF")

# Plot the PACF
pacf(y_diff, main = "Sample Partial Autocorrelation Function (PACF)", xlab = "Lag", ylab = "PACF")



# Fit the ARIMA(1,1,0) model
model = arima(y, order = c(1, 1, 6))

# Summary of the model
summary(model)





model_110 = arima(y, order = c(1, 1, 0))
model_111 = arima(y, order = c(1, 1, 1))
model_011 = arima(y, order = c(0, 1, 1))
model_116 = arima(y, order = c(1, 1, 6))

aic_110 = AIC(model_110)
aic_111 = AIC(model_111)
aic_011 = AIC(model_011)
aic_116 = AIC(model_116)

bic_110 = BIC(model_110)
bic_111 = BIC(model_111)
bic_011 = BIC(model_011)
bic_116 = BIC(model_116)

cat("AIC for ARIMA(1,1,0):", aic_110, "\n")
cat("AIC for ARIMA(1,1,1):", aic_111, "\n")
cat("AIC for ARIMA(0,1,1):", aic_011, "\n")
cat("AIC for ARIMA(1,1,6):", aic_116, "\n")

cat("BIC for ARIMA(1,1,0):", bic_110, "\n")
cat("BIC for ARIMA(1,1,1):", bic_111, "\n")
cat("BIC for ARIMA(0,1,1):", bic_011, "\n")
cat("BIC for ARIMA(1,16):", bic_116, "\n")

# Check residuals
tsdiag(model)

# Plot residuals ACF to check for white noise
acf(residuals(model), main = "ACF of Residuals")

Box.test(residuals(model), type = "Ljung-Box", lag = 20)



# Forecast the next 6 periods
forecast_result = forecast::forecast(model, h = 6)

# Print forecast
print(forecast_result)

# Plot the actual series and the forecast
plot(forecast_result, main = "Actual and Forecasted Data", xlab = "Time", ylab = "Value")

# Add the original data to the plot for comparison
lines(y, col = "blue")

