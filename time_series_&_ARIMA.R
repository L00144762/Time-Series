# simple example of a time series

# vecot contains monthly information

sales <- c(18,22,20,30,24,29,33,14,12,23, 20,12,11,
           12,15,16,25,40,14,20,36, 39, 10, 29)
ts_sales <- ts(sales, start = c(2003,1), 
               frequency = 12) #montly data, would be 4 if it was quartely )
ts_sales
plot(ts_sales)

start(ts_sales)
end(ts_sales)
frequency(ts_sales)




##Using NIlr time series
# that contain annual flow of the Nile
# between 1871-1970

# Plot the data first
# and smooth it to resolve erro components
install.packages("forecast")
library(forecast)
opar <- par(no.readonly = TRUE)
par(mfrow = c(2,2))

plot(Nile, main =  "Raw time series")


y_range <- c(min(Nile), max(Nile))
#ma() function is used to smooth the Nile time series
plot(ma(Nile, 3), main = "Simple moving average (k=3", ylim = y_range)
plot(ma(Nile, 7), main = "simple moving avergae (k=7)", ylim = y_range)
plot(ma(Nile, 15), main = "simple moving avergae (k=15)", ylim = y_range)

# as k increases, the plot becomes increasingly smooth

# plot the ACF chart - measure of how the observations
# in the time series relate to each other
# If the autocorrelation crosses the dashed blue line, it means
# specific las is definitely correlated with current time series.
# A stationary time series witll havea autocorrelation 
# fall quickly to 0, with non-stationarity sereis it drops quickly

#ACF() plot
acf_result <- Acf(Nile)

# partial auto-correlation plot
pacf_result <- Pacf(Nile)

# Test if the time series is stationary
library(tseries)
# p-values < 0.05 then the TS is stationary
adf.test(Nile)

# Assess the presence of a trend in the data
# result = 1 ; need to do something to the data once to make it stationary

ndiffs(Nile)

#There is a trend in the data, so the series is differenced once (lag = 1)
#  zero is good for AREMA
diff_Nile <- diff(Nile, lag = 1)
ndiffs(diff_Nile)

# Show both charts side-by-side for comparison
default_settings <- par(no.readonly = TRUE)
par(mfrow = c(1,1))
plot(Nile)
plot(diff_Nile)
par(default_settings)

adf.test(diff_Nile)


# Identifying one or more models, we need to examine auto-correlation 
# and partial auto-correlation plotsfor the differenced Nile time series


Acf(diff_Nile, main =  "auto-correlation plot for differenced Nile time series",
    )
#partial auto-correlation plot
Pacf(diff_Nile, main =  "partial auto-correlation plot for differenced Nile time series",
)

# We use the original datast for the ARIMA model
# and modify the d value to suit our earlier findings
# and d = 1
arima_model <- Arima(Nile, order = c(0,1,1))
arima_model
# Accuracy measures using the MAPE
# measures prediction of accuracy

accuracy(arima_model)

qqnorm(arima_model$residuals)
qqline(arima_model$residuals)

# box test function provides a tetst that autocorrelates
# are all 0 (null hypothesis)
Box.test(arima_model$residuals, type = "Ljung-Box")

# Forecast 3 years ahead for the Nile time series
forecast(arima_model, 3)

plot(forecast(arima_model, 3), 
     xlab = "Year", 
     ylab = "Annual Flow")

# doesnt seem to be much, should maybe change model (in notes - e.g. Arima(1,1,1)) or use auto arima and see

auto_arima <- auto.arima(Nile)
auto_arima
accuracy(auto_arima)

qqnorm(auto_arima$residuals)
qqline(auto_arima$residuals)

plot(forecast(auto_arima, 3),
     xlab = "Year",
     ylab = "Annual Flow")

