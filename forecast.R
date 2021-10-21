#forecast.R
#implementing time series forecasting methods.

require(forecast)

#as mean not constant website recommends taking the difference of the timeseries
#https://a-little-book-of-r-for-time-series.readthedocs.io/en/latest/src/timeseries.html
#called stationarity
# d differences means you can call an arima(d) fitting scheme
plot(hospital_data$Inpatients_ICU,col='red')
lines(diff(hospital_data$Inpatients_ICU),type='b',col='blue')
lines(diff(hospital_data$Inpatients_ICU,difference=2),type='b',col='green')
#one difference iteration is sufficient
(fore <- Arima(hospital_data$Inpatients_ICU))
plot(fore)
plot(predict(fore)$pred)

     
