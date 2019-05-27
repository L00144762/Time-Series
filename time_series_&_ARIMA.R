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
# value of 1 is returned. need to lag it by 1; there's something in the time series
# that needs to be removed. this number indicates that the data contains a trend
# so the time series is differenced once (lag = 1)
diff_Nile <- diff(Nile, lag = 1)
ndiffs(diff_Nile)

# Show both charts side-by-side for comparison

default_settings <- par(no.readonly = TRUE)
#1 row x 2 column
par(mfrow = c(1,2))
plot(Nile)
plot(diff_Nile)

# we'll asses whether there's a presence of a trend
# in the differenced time series
ndiffs(diff_Nile)

#we're sure there isn't a trend in the time series
# apply the ADF test to the differenced time series


adf.test(diff_Nile)

# p-value is smaller than the cut off
# therefore results show that the time series is now stationary
# so we van proceed to the next step
# H0 =data needs to be stationary; p < 0.05 therefore is doesn't need to be differenced


# Identifying one or more reasonable models; we need to examine auto-correlation 
# and partial auto-correlation plots for the differenced Nile time series
# ACF & PACF plots for forecast


Acf(diff_Nile, main =  "auto-correlation plot for differenced Nile time series")
#partial auto-correlation plot
Pacf(diff_Nile, main =  "partial auto-correlation plot for differenced Nile time series")
# if none of the points cross the blue line by a lot then run auto arima
# acf and pacf are "manual" ways of testing 
# which ever is well past the blue line in acf plot, this is the p-value of arima model
# in this case it's 1 because the value a 1 on the x-axis shoots way over the blue line
# pacf gives q-value which is also 1.

# We use the original datast for the ARIMA model
# and modify the d value to suit our earlier findings
# and d = 1
# we apply the model to our original time series

#using 0,1,1 to see what it's like, even tho we have a strong indication it should be 1,1,1

arima_model <- Arima(Nile, order = c(0,1,1)) #p, d, q

arima_model


# Accuracy measures using the MAPE (mean absolute percentage error)
# measures prediction of accuracy

accuracy(arima_model)

# MAPE has a value of 12.935, meaning there's a chacne that the values this model creates
# could be 13% wrong
# there is a forecast error of 13% in this ARIMA model, with p, d, q values of 0,1,1

#QQnorm produces a normal qq plot of the values in y
# adding a qqline shows us the theoretical quantile-quantile plot
# this line passes through the 1st and 3rd probability quartiles

qqnorm(arima_model$residuals)
qqline(arima_model$residuals)

# To examine whether the data and the model will fit well or not
# Prove that the auto-correlations are zero
# using the box.test() function
# H0 = all values are zero

Box.test(arima_model$residuals, type = "Ljung-Box")

# p > 0.05; shows the model appears to fit the data well 

# Forecast 3 years ahead for the Nile time series
# 3 indicates number of years to forecast
forecast(arima_model, 3)

# Plot the time series prediction
# This shows the forecast and the 80% and 95% confidence bands
plot(forecast(arima_model, 3), 
     xlab = "Year", 
     ylab = "Annual Flow")

# Using the auto arima function

auto_arima_model <- auto.arima(Nile)
auto_arima_model

# auto-arima is saying to use p, d, q values of 1,1,1
# Compare accuracy of the models(manual and auto)
accuracy(arima_model)
accuracy(auto_arima_model)
qqnorm(auto_arima$residuals)
qqline(auto_arima$residuals)

qqnorm(arima_model$residuals)
qqline(arima_model$residuals)

Box.test(auto_arima_model$residuals, type = "Ljung-Box")
# p value is 0.7 which is a lot better thant the 0.2 value of the first model tested

plot(forecast(arima_model, 5), 
     xlab = "Year", 
     ylab = "Annual Flow")

#  this is only based on past information and arima only concentrates on the top 3rd of the data
# look at data trend and decide to use arima or a different time series or even regression
