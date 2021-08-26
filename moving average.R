#MOVING AVERAGE Internet_Modem Nigeria
library(forecast) 
library(zoo)
NigeriaTrend <- read.csv("NigeriaTrend.csv")
# create time series
NigeriaIM.ts  <- ts(NigeriaTrend$Internet_Modem,
                    start = c(2020, 91), end = c(2020, 200), freq = 365)

windows()
# centered moving average with window order = 7
ma.centered <- ma(NigeriaIM.ts, order = 7)

# generate a plot of the series 
plot(NigeriaIM.ts, ylim = c(0, 100),  ylab = "num of appearance", 
     xlab = "Time", 
     xlim = c(2020.20,2020.55), main = "")
#label the axis
axis(1, at = seq(2020.20,2020.55, 1), labels = format(seq(2020.20,2020.55, 1)))
#insert the line to show the centered moving average
lines(ma.centered, lwd = 2)

#MOVING AVERAGE Router Nigeria
library(forecast) 
library(zoo)
NigeriaTrend <- read.csv("NigeriaTrend.csv")
# create time series
NigeriaRouter.ts  <- ts(NigeriaTrend$Router,
                        start = c(2020, 91), end = c(2020, 200), freq = 365)

windows()
# centered moving average with window order = 7
ma.centered <- ma(NigeriaRouter.ts, order = 7)

# generate a plot of the series 
plot(NigeriaRouter.ts, ylim = c(0, 100),  ylab = "num of appearance", 
     xlab = "Time", 
     xlim = c(2020.20,2020.55), main = "")
#label the axis
axis(1, at = seq(2020.20,2020.55, 1), labels = format(seq(2020.20,2020.55, 1)))
#insert the line to show the centered moving average
lines(ma.centered, lwd = 2)


#MOVING AVERAGE Network_Switch Nigeria
library(forecast) 
library(zoo)
NigeriaTrend <- read.csv("NigeriaTrend.csv")
# create time series
NigeriaNetwork_Switch.ts  <- ts(NigeriaTrend$Network_Switch,
                        start = c(2020, 91), end = c(2020, 200), freq = 365)

windows()
# centered moving average with window order = 7
ma.centered <- ma(NigeriaNetwork_Switch.ts, order = 7)

# generate a plot of the series 
plot(NigeriaNetwork_Switch.ts, ylim = c(0, 100),  ylab = "num of appearance", 
     xlab = "Time", 
     xlim = c(2020.20,2020.55), main = "")
#label the axis
axis(1, at = seq(2020.20,2020.55, 1), labels = format(seq(2020.20,2020.55, 1)))
#insert the line to show the centered moving average
lines(ma.centered, lwd = 2)

#MOVING AVERAGE Uninterruptible_power_supply Nigeria
library(forecast) 
library(zoo)
NigeriaTrend <- read.csv("NigeriaTrend.csv")
# create time series
NigeriaUninterruptible_power_supply.ts  <- ts(NigeriaTrend$Uninterruptible_power_supply,
                        start = c(2020, 91), end = c(2020, 200), freq = 365)

windows()
# centered moving average with window order = 7
ma.centered <- ma(NigeriaUninterruptible_power_supply.ts, order = 7)

# generate a plot of the series 
plot(NigeriaUninterruptible_power_supply.ts, ylim = c(0, 100),  ylab = "num of appearance", 
     xlab = "Time",  
     xlim = c(2020.20,2020.55), main = "")
#label the axis
axis(1, at = seq(2020.20,2020.55, 1), labels = format(seq(2020.20,2020.55, 1)))
#insert the line to show the centered moving average
lines(ma.centered, lwd = 2)












