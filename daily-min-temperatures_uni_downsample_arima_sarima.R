#Load file
getwd()
setwd("C:/Users/lloyd/Desktop/simplilearn/pg_program_ds&business_analytics/course/courses/Data Science with R/Machine_Learning_R/Time Series")
data=read.csv("daily-min-temperatures.csv")
#structure of the data frame
str(data)
#Changing dtype of date
library(lubridate)
data$Date=as.Date(data$Date, format='%Y-%m-%d')
head(data)
library(data.table)
data=as.data.table(data)
setkey(data, Date)
#Checking missing values
summary(data)
#Plot the data
plot(data$Date, data$Temp, type='l')
#Downsampling: changing frequency from daily to Monthly
#create indexes for every Month
date.indexes=seq.int(from = 1, to = nrow(data), by=12)
date.indexes
#We will then pass these indexes to our data frame which will drop other data.
data[date.indexes]
#downsampling is to aggreagte data rather than drop
data_new=data[, mean(Temp), by=format(Date, '%Y-%m-01')]
summary(data_new)
data_new$format=as.Date(data_new$format)
head(data_new)
plot(data_new$format, data_new$V1, type='l')
colnames(data_new)=c("Date", "Temp")
#change to time series
ts_data=ts(data_new$Temp, start = c(1981), frequency = 12)
ts_data
#Checking stationary
install.packages("tseries")
library(tseries)
#plot the time series data
install.packages("forecast")
library(forecast)
autoplot(ts_data)
#Dickey-puller test
adf.test(ts_data, k=12)
#Since p-value > 0.05 It is not stationary
#checking varinace in data
library(zoo)
rolling_std=rollapply(ts_data, width=12, FUN=sd)
autoplot(rolling_std)
#Variance is not constant
ts_log=log(ts_data)
rolling_std=rollapply(ts_log, width=12, FUN=sd)
autoplot(rolling_std)
#Dickey-puller test
adf.test(ts_log, k=12)
#Since p-value > 0.05 It is not stationary
#Diffrerencing
ts_Data_d1=diff(ts_log, differences = 1)
adf.test(ts_Data_d1, k=12)
#Since p-value < 0.05 It is stationary
autoplot(ts_Data_d1)
#acf and pacf plot
acf(ts_Data_d1)
#Here q=2
pacf(ts_Data_d1)
#Here p=1
#Arima Model
arima_model=Arima(ts_log, order = c(1, 1, 2))
print(arima_model)
#prediction
autoplot(ts_data)+autolayer(exp(arima_model$fitted), series = "Arima(112)")
#forecast the value
autoplot(forecast(ts_data, h=12, model = arima_model))
#SARIMA MODEL
model=Arima(ts_log, order = c(1, 1, 2), seasonal = c(1, 1, 2))
print(model)
#prediction
autoplot(ts_data)+autolayer(exp(model$fitted), series = "Arima(112)(112)12")
#forecast the value
autoplot(forecast(ts_data, h=12, model = model))
