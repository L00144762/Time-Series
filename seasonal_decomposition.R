# Seasonal decomposition


# Airline passengers dataset contains 
# monthlyn totals (thousands) of international
# airline passengers 1949 - 1970
 plot(AirPassengers)

 #there is multiplicative - the variability is increasing each yeaer by some factor (seasonal)
 
 # before we decompose the time series into trend, seasonal and irregular components,
 # we need to convert from a multipplicative to an additive model
 # using a log transform
 #  additive model means plot shouldn't be increasing over time like in the 
 # multiplicative model
 
log_of_AirPassengers <- log(AirPassengers)
plot(log_of_AirPassengers) 

# variance has now stabilised 

# Now decompose the time series
# We're using "period" so that the seasonal components
# remain the same across each year
# This component describes how quickly the seasonality evolves. 
# If you think the seasonal pattern is constant through time, 
# you should set this parameter to a big value, so that you use 
# the entire data to perform your analysis. If on the other way round
# the seasonal pattern evolves quickly, reduce this parameter to use only 
# the recent data so that your analysis is not affected by old seasonal 
# pattern that are not relevant anymore
# Either set the character string “periodic” or the span (in lags) of 
# the loess window for seasonal extraction, which should be odd
# See https://www.wessa.net/download/stl.pdf for more info

seasonal_decomposition <- stl(log_of_AirPassengers, s.window = "period")
plot(seasonal_decomposition)

#  components are currently in logged format
# we need to convert back to the original metric 
# we do this using the exp function
converted_AirPassengers <- exp(seasonal_decomposition$time.series)
converted_AirPassengers

# for example Nov 1960 had seasonal change of 20% (value of 0.80 in seasonal colum for this year)
# there was a drop by 20%, if it was 1.20 then there would have been an increase of 20%

# seasadj gets rid of the seasonality 
season_adj_AirPassengers <- seasadj(seasonal_decomposition)
seasonplot(AirPassengers, 12, col = rainbow(12),
           year.labels = TRUE, 
           main = "Seasonal plot of AirPassengers")
seasonplot(season_adj_AirPassengers, 12, col = rainbow(12),
           year.labels = TRUE, 
           main = "Seasonal element removed from AirPassengers")

# the chart now appears stationary
# Lets check the data for a trend
# checling for stationarity with ndiffs
ndiffs(season_adj_AirPassengers)
diff_seasonal_adj_AirPassengers <-diff(season_adj_AirPassengers, lag = 1)

ndiffs(diff_seasonal_adj_AirPassengers)
# value of zero is returned therefore the d value of the model is going to be 1
# because we lagged the data by 1

#H0 = data needs to be differenced to make it stationary
# p value < 0.05 meaning the data doesnt need to be differenced anymore
adf.test(diff_seasonal_adj_AirPassengers)

# using the seasonally adjusted model that hasn't been differenced 
auto_arima_model_AP <- auto.arima(season_adj_AirPassengers)
auto_arima_model_AP

qqnorm(auto_arima_model_AP$residuals)
qqline(auto_arima_model_AP$residuals)

accuracy(auto_arima_model_AP)

default_settings
forecast(auto_arima_model_AP)
plot(forecast(auto_arima_model_AP, 3),
     xlab = "Year", ylab = "Annual Passengers")

# plan for testing..
# Extract the last 3 periods out of the original time series first
# and then compare with the predicted time series forecast..IMPORTANT FOR CA4, WILL GIVE EXTRA MARKS
