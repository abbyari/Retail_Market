# import libarary

library(timeSeries)
library(quantmod)
library(forecast)
library(xts)
library(tseries)
library(forecast)
library(tseries)
library(Quandl)
library(tidyverse)
library(tidyquant)
library(timetk)
library(forecast)
library(gridExtra)
library(timeSeries)
library(quantmod)
library(forecast)
library(xts)
library(tseries)
library(MASS)
library(TSPred)
library(ggplot2)
library(plotly)

#Get data

df_main <- read.csv(file = "D:/R - research/casestudyretail/timeseries/Totalcsv.csv",header = TRUE)

df_shop <- df_main[,c("ORDERDATE", "ITEMCODE", "DELIVEREDQUANTITY")]

str(df_shop)

# after performing apriori , we have listed top 10 products. here we are analysing the time series for item no -IT0001012.

top1 = df_shop[df_shop$ITEMCODE == 'IT0001012',][,c("ORDERDATE", "DELIVEREDQUANTITY")]

str(top1)
plot(top1, xlab='Years', ylab = 'Top item Sales')
write.csv(top1,"top1.csv", quote = FALSE, row.names = TRUE)

# read time series data after doing monthly agrregation in excel .

data= read.csv(file="D:/R - research/casestudyretail/timeseries/Time_Datacsv.csv", header=TRUE)

#EDA testing
#creating time series data object

data = ts(data[,2],start = c(2013,10),frequency = 12)

class(data)
plot(data)

# we are checking the start and end time for the data
start(data)

#check end of series
end(data)

#check frequency which should be 12
frequency(data)

#check summary
summary(data)

#check the mean line . this will fit a line.
plot(data)
 abline(reg=lm(data~time(data)))
# mean is not constant within time. We must keep mean constant throughout time.


#check the trend
plot(aggregate(data,FUN=mean))

#check seasonal by boxplot on perticular month the peak
boxplot(data~cycle(data))

#checking for stationarity

#this is Augmented Dickey fuller test for stationaity.The null hypothesis states that large p value indicate non stationaity
#and smaller p value indicate stationaity( thresold :0.05)

adf.test(data)

#p-value = 0.0856
#so data is not stationary
#lets check for log(data)

adf.test(log(data))

#p-value =0.01
#now data is stationary

#check plot for log data 
plot(log(data))
abline(reg=lm(log(data)~time(log(data))))

#though variance and co variance near to constant, the mean is not constant with time.
#so we need to check for differentiation

plot(diff(log(data)))
abline(reg=lm(diff(log(data))~time(diff(log(data)))))

# mean of data is still not constant. Lets do one more differentiation

plot(diff(diff(log(data))))
abline(reg=lm(diff(diff(log(data)))~time(diff(diff(log(data))))))

# now data is looking good.
#lets check the adf test once again.
adf.test(diff(diff(log(data))))

# we are satistifed that p value is 0.01

# lets do diagnosing the ACF and PCAF plot

# Auto corelation on actual data
Acf(data)

#Auto corelation on our required data

Acf(diff(diff(log(data))))

# The first line is way far out of blue region.So we are ingnoring.
Lets take the next line. And the immediate line which got inverted is line no : 2 . So we have to take that value for q (MA) as 1 .

# we already decided to take value of d is 2
# lets check the value for p by Pacf

Pacf(diff(diff(log(data))))

# The immediate line which got inverted is line no : 3 . So we have to take that value for p (AR) as 2 .

# Building the model using Arima

fit<-arima(log(data),c(2,2,1),seasonal=list(order=c(2,2,1),period=10))
# ignore the warning
# for less period
fit_2<-arima(log(data),c(2,2,1),seasonal=list(order=c(2,2,1),period=8))
# ignore the warning

#Model without any seasonal
fit_1<-arima(log(data),c(2,2,1))

#Model of auto.arima

fit_arima=auto.arima(log(data))

# Model for data without converting actual data into log function

plot(diff((data)))

abline(reg=lm(diff(data)~time(diff(data))))

acf(diff(data))

# q is 0

pacf((diff(data)))

# p is 4

fit_3<-arima((data),c(4,1,0))

# lets check the AIC value for 3 model

fit   # aic = 94.35

fit_1 # aic = 101.25

fit_2  # aic = 106.73

fit_arima # AIC=99.29

fit_3  # aic = 1152.97

## if AIC is less the model seems to good. Lets check on predicted data and we shall decide the best model based on predicted data and accuracy


### Forecast and side by side test

## data predict for fit

#to predict for next 1 years
pred<-predict(fit,n.ahead=1*12)

#convert from log to decimal
pred1<-2.718^pred$pred

#check the predicted values
pred1
#plot the model
ts.plot(data,2.718^pred$pred,log="y",lty=c(1,3))

## data is drastically going down which doesnot look good at all.

## data predict for fit_1


#to predict for next 1 years
pred<-predict(fit_1,n.ahead=1*12)

#convert from log to decimal
pred_fit11<-2.718^pred$pred

#check the predicted values
pred_fit11
#plot the model
ts.plot(data,2.718^pred$pred,log="y",lty=c(1,3))

## data is drastically going down which doesnot look good at all.


## data predict for fit_2


#to predict for next 1 years
pred<-predict(fit_2,n.ahead=1*12)

#convert from log to decimal
pred_fit12<-2.718^pred$pred

#check the predicted values
pred_fit12
#plot the model
ts.plot(data,2.718^pred$pred,log="y",lty=c(1,3))

## data prediction is zigzag and not bad. We shall consider fit_2

## data predict for fit_arima


#to predict for next 1 years
pred<-predict(fit_arima,n.ahead=1*12)

#convert from log to decimal
pred_fit13<-2.718^pred$pred

#check the predicted values
pred_fit13
#plot the model
ts.plot(data,2.718^pred$pred,log="y",lty=c(1,3))



## data is drastically going down which doesnot look good at all.




## data predict for fit_3


#to predict for next 1 years
pred<-predict(fit_3,n.ahead=1*12)
#plot the model
ts.plot(data,pred$pred,log="y",lty=c(1,3))

## data is near to straightline but not going down drastically. So we can consider it.


##### so finally we take fit_2 and fit_3 for predection.


# we are checking the start and end time for the data

start(data)
# 2013 10

#check end of series
end(data)

2017   12

# lets take data for 2013 to 2016
datawide<-ts(data,frequency=12,start=c(2013,10),end=c(2016,12))

# create a model fit_new using same concept of fit_2 but on datawide's data

fit2_new<-arima(log(datawide),c(2,2,1),seasonal=list(order=c(2,2,1),period=8))
## ignore the warning

pred<-predict(fit2_new,n.ahead=1*12)
pred_test1<-2.718^pred$pred
#plot the model
ts.plot(data,2.718^pred$pred,log="y",lty=c(1,3))

# data variance is very high in graph. lets check accuracy

round(accuracy(pred$pred,data),3) 

  ME        RMSE         MAE         MPE        MAPE      ACF1       Theil's U
78519.05    80691.71     78519.05    99.984     99.984   -0.045      3.63

# create a model fit_new using same concept of fit_3 but on datawide's data


fit3_new<-arima((datawide),c(4,1,0))


pred<-predict(fit3_new,n.ahead=1*12)

#plot the model
ts.plot(data,pred$pred,log="y",lty=c(1,3))


# data variance is normal. We can take finally fit_3 model for final predection


round(accuracy(pred$pred,data),3) 

 ME           RMSE         MAE        MPE     MAPE      ACF1      Theil's U
 7818.155     20396.69      15196.17   4.126   19.827    0.02      0.84


# we shall consider lowest MAE and MAPE as best model

so we shall consider fit_3 as best model
















