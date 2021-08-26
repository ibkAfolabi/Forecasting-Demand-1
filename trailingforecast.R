
install.packages('forecast')
install.packages('lubridate')
library(forecast)
library(lubridate)
library(forecast) 
library(zoo)


#USING TRAILING MOVING AVERAGE FOR PREDICTION IM
NigeriaTrend <- read.csv("NigeriaTrend.csv")
# create time series
NigeriaIM.ts <- msts(NigeriaTrend$Internet_Modem,seasonal.periods = c(7,366.25),start = decimal_date(as.Date("2020-04-02")))

windows()
# centered moving average with window order = 7
ma.centered <- ma(NigeriaIM.ts, order = 7)

# generate a plot of the series 
plot(NigeriaIM.ts, ylim = c(0, 100),  ylab = "num of appearance", 
     xlab = "Time", 
     xlim = c(2020.25,2020.50), main = "")
#label the axis
axis(1, at = seq(2020.25,2020.50, 1), labels = format(seq(2020.25,2020.50, 1)))
#insert the line to show the centered moving average
lines(ma.centered, lwd = 2)

# trailing moving average with window k = 7
# in rollmean(), use argument align = right to calculate a trailing moving average.
ma.trailing <- rollmean(NigeriaIM.ts, k = 7, align = "right")
#insert the line to show the trailing moving average
lines(ma.trailing, lwd = 2, lty = 2)

#attaching the legend showing what the lines represents
legend(2020.25,2020.50, c("num of appearance","Centered Moving Average", "Trailing Moving Average"), 
       lty=c(1,1,2), lwd=c(1,2,2), bty = "n")  

#MAKING PREDICTION USING MOVING AVERAGE
#Data partition using the last three weeks as validation data
nValid <- 21
nTrain <- length(NigeriaIM.ts) - nValid
train.ts <- window(NigeriaIM.ts,start=decimal_date(as.Date("2020-04-02")),end=c(decimal_date(as.Date("2020-06-08"))))
valid.ts <- window(NigeriaIM.ts, start=c(decimal_date(as.Date("2020-06-09"))), end=c(decimal_date(as.Date("2020-06-29"))))


# moving average on training
ma.trailing <- rollmean(train.ts, k = 7, align = "right")

# obtain the last moving average in the training period
last.ma <- tail(ma.trailing, 1)

# create forecast based on last MA
#ma.trailing.pred <- ts(rep(last.ma, nTest), start=c(decimal_date(as.Date("2020-06-09"))), end = c(decimal_date(as.Date("2020-06-29")), freq = 365))
#print(ma.trailing.pred)  

ma.trailing.pred <- ts(rep(last.ma, nValid), start=c(decimal_date(as.Date("2020-06-09"))), 
                       end=c(decimal_date(as.Date("2020-06-29"))), freq = 366)


# plot the series
windows()
#ploting the training data
plot(train.ts, ylim = c(0, 100),  ylab = "num of appearance", xlab = "Time",  
      xlim = c(2020.25,2020.50), main = "")


#show the trailing average line for the training period
#plot the validation data
lines(ma.trailing, lwd = 2, col = "blue") 
#show the trailing line for the validation period
lines(ma.trailing.pred, lwd = 2, col = "blue", lty = 2)
lines(test.ts)


#USING TRAILING MOVING AVERAGE FOR PREDICTION ROUTER
NigeriaTrend <- read.csv("NigeriaTrend.csv")
# create time series
NigeriaRouter.ts <- msts(NigeriaTrend$Router,seasonal.periods = c(7,366.25),start = decimal_date(as.Date("2020-04-02")))

windows()
# centered moving average with window order = 7
ma.centered <- ma(NigeriaRouter.ts, order = 7)

# generate a plot of the series 
plot(NigeriaRouter.ts, ylim = c(0, 100),  ylab = "num of appearance", 
     xlab = "Time", 
     xlim = c(2020.25,2020.50), main = "")
#label the axis
axis(1, at = seq(2020.25,2020.50, 1), labels = format(seq(2020.25,2020.50, 1)))
#insert the line to show the centered moving average
lines(ma.centered, lwd = 2)

# trailing moving average with window k = 7
# in rollmean(), use argument align = right to calculate a trailing moving average.
ma.trailing <- rollmean(NigeriaRouter.ts, k = 7, align = "right")
#insert the line to show the trailing moving average
lines(ma.trailing, lwd = 2, lty = 2)

#attaching the legend showing what the lines represents
legend(2020.25,2020.50, c("num of appearance","Centered Moving Average", "Trailing Moving Average"), 
       lty=c(1,1,2), lwd=c(1,2,2), bty = "n")  

#MAKING PREDICTION USING MOVING AVERAGE
#Data partition using the last three weeks as validation data
nValid <- 21
nTrain <- length(NigeriaRouter.ts) - nValid
train.ts <- window(NigeriaRouter.ts,start=decimal_date(as.Date("2020-04-02")),end=c(decimal_date(as.Date("2020-06-08"))))
valid.ts <- window(NigeriaRouter.ts, start=c(decimal_date(as.Date("2020-06-09"))), end=c(decimal_date(as.Date("2020-06-29"))))


# moving average on training
ma.trailing <- rollmean(train.ts, k = 7, align = "right")

# obtain the last moving average in the training period
last.ma <- tail(ma.trailing, 1)

# create forecast based on last MA
#ma.trailing.pred <- ts(rep(last.ma, nTest), start=c(decimal_date(as.Date("2020-06-09"))), end = c(decimal_date(as.Date("2020-06-29")), freq = 365))
#print(ma.trailing.pred)  

ma.trailing.pred <- ts(rep(last.ma, nValid), start=c(decimal_date(as.Date("2020-06-09"))), 
                       end=c(decimal_date(as.Date("2020-06-29"))), freq = 366)


# plot the series
windows()
#ploting the training data
plot(train.ts, ylim = c(0, 100),  ylab = "num of appearance", xlab = "Time",  
     xlim = c(2020.25,2020.50), main = "")


#show the trailing average line for the training period
#plot the validation data
lines(ma.trailing, lwd = 2, col = "blue") 
#show the trailing line for the validation period
lines(ma.trailing.pred, lwd = 2, col = "blue", lty = 2)
lines(valid.ts)

#USING TRAILING MOVING AVERAGE FOR PREDICTION Network_Switch
NigeriaTrend <- read.csv("NigeriaTrend.csv")
# create time series
NigeriaNetwork_Switch.ts <- msts(NigeriaTrend$Network_Switch,seasonal.periods = c(7,366.25),start = decimal_date(as.Date("2020-04-02")))

windows()
# centered moving average with window order = 7
ma.centered <- ma(NigeriaNetwork_Switch.ts, order = 7)

# generate a plot of the series 
plot(NigeriaNetwork_Switch.ts, ylim = c(0, 100),  ylab = "num of appearance", 
     xlab = "Time", 
     xlim = c(2020.25,2020.50), main = "")
#label the axis
axis(1, at = seq(2020.25,2020.50, 1), labels = format(seq(2020.25,2020.50, 1)))
#insert the line to show the centered moving average
lines(ma.centered, lwd = 2)

# trailing moving average with window k = 7
# in rollmean(), use argument align = right to calculate a trailing moving average.
ma.trailing <- rollmean(NigeriaNetwork_Switch.ts, k = 7, align = "right")
#insert the line to show the trailing moving average
lines(ma.trailing, lwd = 2, lty = 2)

#attaching the legend showing what the lines represents
legend(2020.25,2020.50, c("num of appearance","Centered Moving Average", "Trailing Moving Average"), 
       lty=c(1,1,2), lwd=c(1,2,2), bty = "n")  

#MAKING PREDICTION USING trailing MOVING AVERAGE
#Data partition using the last three weeks as validation data
nValid <- 21
nTrain <- length(NigeriaNetwork_Switch.ts) - nValid
train.ts <- window(NigeriaNetwork_Switch.ts,start=decimal_date(as.Date("2020-04-02")),end=c(decimal_date(as.Date("2020-06-08"))))
valid.ts <- window(NigeriaNetwork_Switch.ts, start=c(decimal_date(as.Date("2020-06-09"))), end=c(decimal_date(as.Date("2020-06-29"))))


# moving average on training
ma.trailing <- rollmean(train.ts, k = 7, align = "right")

# obtain the last moving average in the training period
last.ma <- tail(ma.trailing, 1)

# create forecast based on last MA
#ma.trailing.pred <- ts(rep(last.ma, nTest), start=c(decimal_date(as.Date("2020-06-09"))), end = c(decimal_date(as.Date("2020-06-29")), freq = 365))
#print(ma.trailing.pred)  

ma.trailing.pred <- ts(rep(last.ma, nValid), start=c(decimal_date(as.Date("2020-06-09"))), 
                       end=c(decimal_date(as.Date("2020-06-29"))), freq = 366)


# plot the series
windows()
#ploting the training data
plot(train.ts, ylim = c(0, 100),  ylab = "num of appearance", xlab = "Time",  
     xlim = c(2020.25,2020.50), main = "")


#show the trailing average line for the training period
#plot the validation data
lines(ma.trailing, lwd = 2, col = "blue") 
#show the trailing line for the validation period
lines(ma.trailing.pred, lwd = 2, col = "blue", lty = 2)
lines(valid.ts)

#USING TRAILING MOVING AVERAGE FOR PREDICTION Uninterruptible_power_supply
NigeriaTrend <- read.csv("NigeriaTrend.csv")
# create time series
NigeriaUninterruptible_power_supply.ts <- msts(NigeriaTrend$Uninterruptible_power_supply,seasonal.periods = c(7,366.25),start = decimal_date(as.Date("2020-04-02")))

windows()
# centered moving average with window order = 7
ma.centered <- ma(NigeriaUninterruptible_power_supply.ts, order = 7)

# generate a plot of the series 
plot(NigeriaUninterruptible_power_supply.ts, ylim = c(0, 100),  ylab = "num of appearance", 
     xlab = "Time", 
     xlim = c(2020.25,2020.50), main = "")
#label the axis
axis(1, at = seq(2020.25,2020.50, 1), labels = format(seq(2020.25,2020.50, 1)))
#insert the line to show the centered moving average
lines(ma.centered, lwd = 2)

# trailing moving average with window k = 7
# in rollmean(), use argument align = right to calculate a trailing moving average.
ma.trailing <- rollmean(NigeriaUninterruptible_power_supply.ts, k = 7, align = "right")
#insert the line to show the trailing moving average
lines(ma.trailing, lwd = 2, lty = 2)

#attaching the legend showing what the lines represents
legend(2020.25,2020.50, c("num of appearance","Centered Moving Average", "Trailing Moving Average"), 
       lty=c(1,1,2), lwd=c(1,2,2), bty = "n")  

#MAKING PREDICTION USING trailing MOVING AVERAGE
#Data partition using the last three weeks as validation data
nValid <- 21
nTrain <- length(NigeriaUninterruptible_power_supply.ts) - nValid
train.ts <- window(NigeriaUninterruptible_power_supply.ts,start=decimal_date(as.Date("2020-04-02")),end=c(decimal_date(as.Date("2020-06-08"))))
valid.ts <- window(NigeriaUninterruptible_power_supply.ts, start=c(decimal_date(as.Date("2020-06-09"))), end=c(decimal_date(as.Date("2020-06-29"))))


# moving average on training
ma.trailing <- rollmean(train.ts, k = 7, align = "right")

# obtain the last moving average in the training period
last.ma <- tail(ma.trailing, 1)

# create forecast based on last MA
#ma.trailing.pred <- ts(rep(last.ma, nTest), start=c(decimal_date(as.Date("2020-06-09"))), end = c(decimal_date(as.Date("2020-06-29")), freq = 365))
#print(ma.trailing.pred)  

ma.trailing.pred <- ts(rep(last.ma, nValid), start=c(decimal_date(as.Date("2020-06-09"))), 
                       end=c(decimal_date(as.Date("2020-06-29"))), freq = 366)


# plot the series
windows()
#ploting the training data
plot(train.ts, ylim = c(0, 100),  ylab = "num of appearance", xlab = "Time",  
     xlim = c(2020.25,2020.50), main = "")


#show the trailing average line for the training period
#plot the validation data
lines(ma.trailing, lwd = 2, col = "blue") 
#show the trailing line for the validation period
lines(ma.trailing.pred, lwd = 2, col = "blue", lty = 2)
lines(valid.ts)
