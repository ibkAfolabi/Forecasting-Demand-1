
#INTERNET MODEN LINEAR TREND FORECAST
library(forecast) 
NigeriaTrend <- read.csv("NigeriaTrend.csv")
# create time series
NigeriaIM.ts <- msts(NigeriaTrend$Internet_Modem,seasonal.periods = c(7,366.25),start = decimal_date(as.Date("2020-04-02")))

#Data partition using the last three weeks as validation data
nValid <- 21
nTrain <- length(NigeriaIM.ts) - nValid
train.ts <- window(NigeriaIM.ts,start=decimal_date(as.Date("2020-04-02")),end=c(decimal_date(as.Date("2020-06-08"))))
valid.ts <- window(NigeriaIM.ts, start=c(decimal_date(as.Date("2020-06-09"))), end=c(decimal_date(as.Date("2020-06-29"))))

# fit linear trend model to training set and create forecasts
train.lm <- tslm(train.ts ~ trend)
train.lm.pred <- forecast(train.lm, h = nValid, level = 0)
summary(train.lm)


#plot the forecast model applied on the training data and forecasted in the validation period
windows()
plot(train.lm.pred, ylim = c(0, 100),  ylab = "num of appearance", xlab = "Time",  
     xlim = c(2020.25,2020.50), main = "")

#label the horizontal axis
axis(1, at = seq(2020.25,2020.50), labels = format(seq(2020.25,2020.50)))

#attach the trend line
lines(train.lm.pred$fitted, lwd = 2, col = "blue")
#ploting the 3weeks we forecast for the validation
lines(valid.ts)


#plotting the Forecast Errors against time
windows()
plot(train.lm.pred$residuals, ylim = c(0, 100),  ylab = "Forecast Errors", 
     xlab = "Time",  xlim = c(2020.25,2020.50), main = "")

#label the horizontal axis
axis(1, at = seq(2020.25,2020.50), labels = format(seq(2020.25,2020.50)))
lines(valid.ts - train.lm.pred$mean, lwd = 1)










#INTERNET ROUTER POLYNOMIAL TREND FORECAST
library(forecast) 
NigeriaTrend <- read.csv("NigeriaTrend.csv")
# create time series
NigeriaRouter.ts <- msts(NigeriaTrend$Router,seasonal.periods = c(7,366.25),start = decimal_date(as.Date("2020-04-02")))

#Data partition using the last three weeks as validation data
nValid <- 21
nTrain <- length(NigeriaRouter.ts) - nValid
train.ts <- window(NigeriaRouter.ts,start=decimal_date(as.Date("2020-04-02")),end=c(decimal_date(as.Date("2020-06-08"))))
valid.ts <- window(NigeriaRouter.ts, start=c(decimal_date(as.Date("2020-06-09"))), end=c(decimal_date(as.Date("2020-06-29"))))

# fit linear trend model to training set and create forecasts
train.lm <- tslm(train.ts ~ trend)


# fit quadratic trend using function I(), which treats an object "as is".
train.lm.poly.trend <- tslm(train.ts ~ trend + I(trend^2))

#summary of the developed model
summary(train.lm.poly.trend)

#using it to forecast on the validation data
train.lm.poly.trend.pred <- forecast(train.lm.poly.trend, h = nValid, level = 0)

#ploting the forecast
windows()
plot(train.lm.poly.trend.pred, ylim = c(0, 100),  ylab = "Ridership", 
     xlab = "Time",  xlim = c(2020.25,2020.50), main = "", flty = 2)
#labeling the axis
axis(1, at = seq(2020.25,2020.50), labels = format(seq(2020.25,2020.50))) 

#fitting the poly trend line
lines(train.lm.poly.trend$fitted, lwd = 2)
#adding the validation data to the display
lines(valid.ts)


#ploting the forecast errors
windows()
plot(train.lm.poly.trend$residuals, ylim = c(0, 100),  ylab = "Forecast Errors", 
     xlab = "Time",  xlim = c(2020.25,2020.50), main = "")
#label axis
axis(1, at = seq(2020.25,2020.50), labels = format(seq(2020.25,2020.50)))
#adding the validation data 
lines(valid.ts - train.lm.poly.trend.pred$mean, lwd = 1)



#INTERNET Network_Switch LINEAR TREND FORECAST
library(forecast) 
NigeriaTrend <- read.csv("NigeriaTrend.csv")
# create time series
NigeriaNetwork_Switch.ts <- msts(NigeriaTrend$Network_Switch,seasonal.periods = c(7,366.25),start = decimal_date(as.Date("2020-04-02")))

#Data partition using the last three weeks as validation data
nValid <- 21
nTrain <- length(NigeriaNetwork_Switch.ts) - nValid
train.ts <- window(NigeriaNetwork_Switch.ts,start=decimal_date(as.Date("2020-04-02")),end=c(decimal_date(as.Date("2020-06-08"))))
valid.ts <- window(NigeriaNetwork_Switch.ts, start=c(decimal_date(as.Date("2020-06-09"))), end=c(decimal_date(as.Date("2020-06-29"))))

# fit linear trend model to training set and create forecasts
train.lm <- tslm(train.ts ~ trend)
train.lm.pred <- forecast(train.lm, h = nValid, level = 0)
summary(train.lm)


#plot the forecast model applied on the training data and forecasted in the validation period
windows()
plot(train.lm.pred, ylim = c(0, 100),  ylab = "num of appearance", xlab = "Time",  
     xlim = c(2020.25,2020.50), main = "")

#label the horizontal axis
axis(1, at = seq(2020.25,2020.50), labels = format(seq(2020.25,2020.50)))

#attach the trend line
lines(train.lm.pred$fitted, lwd = 2, col = "blue")
#ploting the 3weeks we forecast for the validation
lines(valid.ts)


#plotting the Forecast Errors against time
windows()
plot(train.lm.pred$residuals, ylim = c(0, 100),  ylab = "Forecast Errors", 
     xlab = "Time",  xlim = c(2020.25,2020.50), main = "")

#label the horizontal axis
axis(1, at = seq(2020.25,2020.50), labels = format(seq(2020.25,2020.50)))
lines(valid.ts - train.lm.pred$mean, lwd = 1)










#INTERNET Uninterruptible_power_supply POLYNOMIAL TREND FORECAST
library(forecast) 
NigeriaTrend <- read.csv("NigeriaTrend.csv")
# create time series
NigeriaUninterruptible_power_supply.ts <- msts(NigeriaTrend$Uninterruptible_power_supply,seasonal.periods = c(7,366.25),start = decimal_date(as.Date("2020-04-02")))

#Data partition using the last three weeks as validation data
nValid <- 21
nTrain <- length(NigeriaUninterruptible_power_supply.ts) - nValid
train.ts <- window(NigeriaUninterruptible_power_supply.ts,start=decimal_date(as.Date("2020-04-02")),end=c(decimal_date(as.Date("2020-06-08"))))
valid.ts <- window(NigeriaUninterruptible_power_supply.ts, start=c(decimal_date(as.Date("2020-06-09"))), end=c(decimal_date(as.Date("2020-06-29"))))

# fit linear trend model to training set and create forecasts
train.lm <- tslm(train.ts ~ trend)


# fit quadratic trend using function I(), which treats an object "as is".
train.lm.poly.trend <- tslm(train.ts ~ trend + I(trend^2))

#summary of the developed model
summary(train.lm.poly.trend)

#using it to forecast on the validation data
train.lm.poly.trend.pred <- forecast(train.lm.poly.trend, h = nValid, level = 0)

#ploting the forecast
windows()
plot(train.lm.poly.trend.pred, ylim = c(0, 100),  ylab = "Ridership", 
     xlab = "Time",  xlim = c(2020.25,2020.50), main = "", flty = 2)
#labeling the axis
axis(1, at = seq(2020.25,2020.50), labels = format(seq(2020.25,2020.50))) 

#fitting the poly trend line
lines(train.lm.poly.trend$fitted, lwd = 2)
#adding the validation data to the display
lines(valid.ts)


#ploting the forecast errors
windows()
plot(train.lm.poly.trend$residuals, ylim = c(0, 100),  ylab = "Forecast Errors", 
     xlab = "Time",  xlim = c(2020.25,2020.50), main = "")
#label axis
axis(1, at = seq(2020.25,2020.50), labels = format(seq(2020.25,2020.50)))
#adding the validation data 
lines(valid.ts - train.lm.poly.trend.pred$mean, lwd = 1)



