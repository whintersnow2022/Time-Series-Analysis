#Time Series Data
#Philippines' Quarterly Imports of Medical and Pharmaceutical Products
#From 1998 to 2020 Using SARIMA and ETS

#Shane La Rosa
# 05/22/21

install.packages(csv)
library(csv)
library(tseries)
library(astsa)
library(ggplot2)
library(fpp2)
library(TSA)
library(forecast)
library(plotly)
install.packages("TSstudio")
library(TSstudio)
library(urca)



imports_MedPhar_data <- (read.csv("Import of Philippine Medphar Products.csv",header=TRUE))
names(imports_MedPhar_data)
str(imports_MedPhar_data)
summary(imports_MedPhar_data)

#Model Identification
#Converting data into time series
imports_MedPhar_ts <- ts(imports_MedPhar_data,start=1998, frequency =4)
str(imports_MedPhar_ts)
print(imports_MedPhar_ts)

#Plotting the time series data
ts.plot(imports_MedPhar_ts,col="black", main="Imports of Medicinal & Pharmaceutical 
        Products (1998-2020)" ,xlab= "Years",ylab= "In million Philippine Peso",type="b")

#autocorrelation
acf(imports_MedPhar_ts,lag.max= 36,main=NULL)

pacf(imports_MedPhar_ts,main=NULL, lag.max = 36)


#stationary using Dicker-Fuller Test
adf.test(imports_MedPhar_ts)

#Converting data from non stationary to stationary

imports_data <- diff(imports_MedPhar_ts, differences=1)
plot(imports_data, ylab= "diff (1)", 
     main="Imports of Medical & Pharmaceutical Products,1998-2020")
abline(h=0,col="blue",lty="dashed")
acf(imports_data,lag.max = 36)
pacf(imports_data,lag.max = 36)

#Dicker Fuller Test in First Difference
adf.test(imports_data)

#Log transformation for the equal variance
log_imports <-log(imports_MedPhar_ts)

ts.plot(log_imports, main=NULL, xlab="Years", ylab="Log Imports (MedPhar)")
plot(decompose(log_imports))

# Number of Difference Needed in the series
ndiffs(log_imports)
nsdiffs(log_imports)

#First difference of seasonal and nonseasonal
importsdiff1<-diff(log_imports, lag=4, differences = 1 )
plot.ts(importsdiff1,ylab="Differenced Log (Imports MedPhar)",
        main="Imports of Medical & Pharmaceutical Products,1998-2020")

#ACF and PACF of stationary series
par(mfrow = c(1,2))
Acf(importsdiff1, lag.max = 16, main="Autocorrelation Function of diff1")
Pacf(importsdiff1, lag.max = 16, main="Partial Autocorrelation Function of diff1")


#Dicker Fuller Test of stationary series
adf.test(importsdiff1)


#Parameter Estimation
#Splitting
#SARIMA CANDIDATE MODEL
train1 <- window(imports_MedPhar_ts, start = 1998, end = c(2016, 2))
test1 <- window(imports_MedPhar_ts,start= c(2016,3),end= c(2020,4))

mod1.train <- Arima(train1, order=c(0,1,2),
                    seasonal=list (order = c(0,1,2),period=4))
mod1.train 

mod2.train <- Arima(train1, order=c(2,1,0),
                    seasonal=c(0,1,2))
mod2.train


mod3.train <- Arima(train1, order=c(1,1,2),
                    seasonal=c(1,1,2))
mod3.train

mod4.train <- Arima(train1, order=c(0,1,2),
                    seasonal=c(0,1,0))
mod4.train


mod5.train <- Arima(train1, order=c(0,1,3),
                    seasonal=c(0,1,1))
mod5.train

mod6.train <- Arima(train1, order=c(0,1,4),
                    seasonal=c(0,1,1))
mod6.train

mod7.train <- Arima(train1, order=c(0,1,2),
                    seasonal=c(0,1,1))
mod7.train


----
# Model (SARIMA & ETS)
# Splitting of data beginning in 1988
train1 <- window(imports_MedPhar_ts, start = 1998, end = c(2016, 2))
test1 <- window(imports_MedPhar_ts,start= c(2016,3),end= c(2020,4))
  
ts_info(train1)
ts_info(test1)

# Fit an ARIMA and an ETS model to the training data
fit1 <- Arima(train1, order=c(0,1,2),
              seasonal=c(0,1,1)) 
fit1

fit2 <- ets(train1,opt.crit="mse")
fit2

# Check that both models have white noise residuals
check_res(fit1)
check_res(fit2)

# Produce forecasts for each model
#SARIMA
importforcast1 <- forecast(fit1, h = 38)
importforcast1


test_forecast(forecast.obj = importforcast1, actual = imports_MedPhar_ts,
              test = test1)

accuracy(importforcast1, test)
check_res(importforcast1)

 #ETS
importforcast2 <- forecast(fit1, h = 38)
importforcast2

test_forecast(forecast.obj = importforcast2, actual = imports_MedPhar_ts,
              test = test1)

accuracy(importforcast2, test)
check_res(importforcast2)



#USING AUTO.ARIMA
#Model
imports_df <- ts_to_prophet(imports_MedPhar_ts)
head(imports_df)

library(lubridate)
imports_df$flag <- ifelse(year(imports_df$ds) >= 2010, 1, 0)
print(imports_df)

#Set the sample out and forecast horizon
h1 <- 18 # the length of the testing partition
h2 <- 38 # forecast horizon

import_split <- ts_split(imports_MedPhar_ts, sample.out = h1)
train <- import_split$train
test <- import_split$test

ts_info(train)
ts_info(test)

train_df <- imports_df[1:(nrow(imports_df) - h1), ]
test_df <- imports_df[(nrow(imports_df) - h1 + 1):nrow(imports_df), ]


md1 <- auto.arima(train, stepwise = FALSE, approximation = FALSE,D=1
                  , trace = TRUE)
md1

fc1 <- forecast(md1, h = h1)
accuracy(fc1, test)


test_forecast(forecast.obj = fc1, actual = imports_MedPhar_ts, test = test)


md2 <- ets(train, opt.crit = "mse")
md2

fc2 <- forecast(md2, h = h1)
accuracy(fc2, test)

test_forecast(forecast.obj = fc2, actual = imports_MedPhar_ts, test = test)


check_res(md1)
check_res(md2)

fc_final <- forecast(md1, h = h2)
plot_forecast(fc_final)

#final model

fc_final <- forecast(md1, h
                     = h2)
fc_final




