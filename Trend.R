

#CREATE TREND FOR NIGERIA - ALL FOUR VARIABLES
library(forecast)
library(ggplot2)
NigeriaTrend <- read.csv("NigeriaTrend.csv")
# create time series object using ts()
# ts() takes three arguments: start, end, and freq. 
# with monthly data, the frequency of periods per season is 12 (per year). 
# arguments start and end are (season number, period number) pairs. 
# here start is Jan 1991: start = c(1991, 1); end is Mar 2004: end = c(2004, 3).
NigeriaIM.ts  <- ts(NigeriaTrend$Internet_Modem,
                   start = c(2020, 91), end = c(2020, 200), freq = 365)
NigeriaRouter.ts  <- ts(NigeriaTrend$Router,
                  start = c(2020, 91), end = c(2020, 200), freq = 365)
NigeriaNetwork_Switch.ts  <- ts(NigeriaTrend$Network_Switch,
                        start = c(2020, 91), end = c(2020, 200), freq = 365)
NigeriaUninterruptible_power_supply.ts  <- ts(NigeriaTrend$Uninterruptible_power_supply,
                                start = c(2020, 91), end = c(2020, 200), freq = 365)
windows()
ts.plot(NigeriaIM.ts,NigeriaRouter.ts,NigeriaNetwork_Switch.ts,NigeriaUninterruptible_power_supply.ts, gpars= list(col=c("black","red","green","blue")))


#CREATE TREND FOR LAGOS - ALL FOUR VARIABLES
library(forecast)
library(ggplot2)
LagosTrend <- read.csv("LagosTrend.csv")
# create time series object using ts()
# ts() takes three arguments: start, end, and freq. 
# with monthly data, the frequency of periods per season is 12 (per year). 
# arguments start and end are (season number, period number) pairs. 
# here start is Jan 1991: start = c(1991, 1); end is Mar 2004: end = c(2004, 3).
LagosIM.ts  <- ts(LagosTrend$Internet_Modem,
                    start = c(2020, 91), end = c(2020, 200), freq = 365)
LagosRouter.ts  <- ts(LagosTrend$Router,
                        start = c(2020, 91), end = c(2020, 200), freq = 365)
LagosNetwork_Switch.ts  <- ts(LagosTrend$Network_Switch,
                                start = c(2020, 91), end = c(2020, 200), freq = 365)
LagosUninterruptible_power_supply.ts  <- ts(LagosTrend$Uninterruptible_power_supply,
                                              start = c(2020, 91), end = c(2020, 200), freq = 365)
windows()
ts.plot(LagosIM.ts,LagosRouter.ts,LagosNetwork_Switch.ts,LagosUninterruptible_power_supply.ts, gpars= list(col=c("black","red","green","blue")))


#CREATE TREND FOR ABUJA - ALL FOUR VARIABLES
library(forecast)
library(ggplot2)
AbujaTrend <- read.csv("AbujaTrend.csv")
# create time series object using ts()
# ts() takes three arguments: start, end, and freq. 
# with monthly data, the frequency of periods per season is 12 (per year). 
# arguments start and end are (season number, period number) pairs. 
# here start is Jan 1991: start = c(1991, 1); end is Mar 2004: end = c(2004, 3).
AbujaIM.ts  <- ts(AbujaTrend$Internet_Modem,
                  start = c(2020, 91), end = c(2020, 200), freq = 365)
AbujaRouter.ts  <- ts(AbujaTrend$Router,
                      start = c(2020, 91), end = c(2020, 200), freq = 365)
AbujaNetwork_Switch.ts  <- ts(AbujaTrend$Network_Switch,
                              start = c(2020, 91), end = c(2020, 200), freq = 365)
AbujaUninterruptible_power_supply.ts  <- ts(AbujaTrend$Uninterruptible_power_supply,
                                            start = c(2020, 91), end = c(2020, 200), freq = 365)
windows()
ts.plot(AbujaIM.ts,AbujaRouter.ts,AbujaNetwork_Switch.ts,AbujaUninterruptible_power_supply.ts, gpars= list(col=c("black","red","green","blue")))



#CREATE TREND FOR KANO - ALL FOUR VARIABLES
library(forecast)
library(ggplot2)
KanoTrend <- read.csv("KanoTrend.csv")
# create time series object using ts()
# ts() takes three arguments: start, end, and freq. 
# with monthly data, the frequency of periods per season is 12 (per year). 
# arguments start and end are (season number, period number) pairs. 
# here start is Jan 1991: start = c(1991, 1); end is Mar 2004: end = c(2004, 3).
KanoIM.ts  <- ts(KanoTrend$Internet_Modem,
                    start = c(2020, 91), end = c(2020, 200), freq = 365)
KanoRouter.ts  <- ts(KanoTrend$Router,
                        start = c(2020, 91), end = c(2020, 200), freq = 365)
KanoNetwork_Switch.ts  <- ts(KanoTrend$Network_Switch,
                                start = c(2020, 91), end = c(2020, 200), freq = 365)
KanoUninterruptible_power_supply.ts  <- ts(KanoTrend$Uninterruptible_power_supply,
                                              start = c(2020, 91), end = c(2020, 200), freq = 365)
windows()
ts.plot(KanoIM.ts,KanoRouter.ts,KanoNetwork_Switch.ts,KanoUninterruptible_power_supply.ts, gpars= list(col=c("black","red","green","blue")))


#CREATE TREND FOR OYO - ALL FOUR VARIABLES
library(forecast)
library(ggplot2)
OyoTrend <- read.csv("OyoTrend.csv")
# create time series object using ts()
# ts() takes three arguments: start, end, and freq. 
# with monthly data, the frequency of periods per season is 12 (per year). 
# arguments start and end are (season number, period number) pairs. 
# here start is Jan 1991: start = c(1991, 1); end is Mar 2004: end = c(2004, 3).
OyoIM.ts  <- ts(OyoTrend$Internet_Modem,
                 start = c(2020, 91), end = c(2020, 200), freq = 365)
OyoRouter.ts  <- ts(OyoTrend$Router,
                     start = c(2020, 91), end = c(2020, 200), freq = 365)
OyoNetwork_Switch.ts  <- ts(OyoTrend$Network_Switch,
                             start = c(2020, 91), end = c(2020, 200), freq = 365)
OyoUninterruptible_power_supply.ts  <- ts(OyoTrend$Uninterruptible_power_supply,
                                           start = c(2020, 91), end = c(2020, 200), freq = 365)
windows()
ts.plot(OyoIM.ts,OyoRouter.ts,OyoNetwork_Switch.ts,OyoUninterruptible_power_supply.ts, gpars= list(col=c("black","red","green","blue")))




