
#COVID AND IM
library(forecast)
library(ggplot2)
NigeriaTrend <- read.csv("NigeriaTrend.csv")
# normalize the numerical vriables using min-max normalization method to [0,1].
#to nomalize Internet_Modem
NigeriaTrend$Internet_Modem<-(NigeriaTrend$Internet_Modem - min(NigeriaTrend$Internet_Modem))/
  (max(NigeriaTrend$Internet_Modem)-min(NigeriaTrend$Internet_Modem))

covidTrend <- read.csv("COVID19.csv")
#to nomalize covid
covidTrend$covid<-(covidTrend $covid - min(covidTrend $covid))/
  (max(covidTrend $covid)-min(covidTrend$covid))


# create time series object using ts()
# ts() takes three arguments: start, end, and freq. 


NigeriaInternet_Modem.ts  <- ts(NigeriaTrend$Internet_Modem,
                                start = c(2020, 91), end = c(2020, 200), freq = 365)
covid.ts  <- ts(covidTrend$covid,
                start = c(2020, 91), end = c(2020, 200), freq = 365)

windows()
ts.plot(NigeriaInternet_Modem.ts,covid.ts, gpars= list(col=c("black","red")))



#COVID AND Router
library(forecast)
library(ggplot2)
NigeriaTrend <- read.csv("NigeriaTrend.csv")
# normalize the numerical vriables using min-max normalization method to [0,1].
#to nomalize Router
NigeriaTrend$Router<-(NigeriaTrend$Router - min(NigeriaTrend$Router))/
  (max(NigeriaTrend$Router)-min(NigeriaTrend$Router))

covidTrend <- read.csv("COVID19.csv")
#to nomalize covid
covidTrend$covid<-(covidTrend $covid - min(covidTrend $covid))/
  (max(covidTrend $covid)-min(covidTrend$covid))


# create time series object using ts()
# ts() takes three arguments: start, end, and freq. 


NigeriaRouter.ts  <- ts(NigeriaTrend$Router,
                                start = c(2020, 91), end = c(2020, 200), freq = 365)
covid.ts  <- ts(covidTrend$covid,
                start = c(2020, 91), end = c(2020, 200), freq = 365)

windows()
ts.plot(NigeriaRouter.ts,covid.ts, gpars= list(col=c("black","red")))

#COVID AND Network_Switch
library(forecast)
library(ggplot2)
NigeriaTrend <- read.csv("NigeriaTrend.csv")
# normalize the numerical vriables using min-max normalization method to [0,1].
#to nomalize Network_Switch
NigeriaTrend$Network_Switch<-(NigeriaTrend$Network_Switch - min(NigeriaTrend$Network_Switch))/
  (max(NigeriaTrend$Network_Switch)-min(NigeriaTrend$Network_Switch))

covidTrend <- read.csv("COVID19.csv")
#to nomalize covid
covidTrend$covid<-(covidTrend $covid - min(covidTrend $covid))/
  (max(covidTrend $covid)-min(covidTrend$covid))


# create time series object using ts()
# ts() takes three arguments: start, end, and freq. 


NigeriaNetwork_Switch.ts  <- ts(NigeriaTrend$Network_Switch,
                        start = c(2020, 91), end = c(2020, 200), freq = 365)
covid.ts  <- ts(covidTrend$covid,
                start = c(2020, 91), end = c(2020, 200), freq = 365)

windows()
ts.plot(NigeriaNetwork_Switch.ts,covid.ts, gpars= list(col=c("black","red")))




#COVID AND Uninterruptible_power_supply
library(forecast)
library(ggplot2)
NigeriaTrend <- read.csv("NigeriaTrend.csv")
# normalize the numerical vriables using min-max normalization method to [0,1].
#to nomalize Uninterruptible_power_supply
NigeriaTrend$Uninterruptible_power_supply<-(NigeriaTrend$Uninterruptible_power_supply - min(NigeriaTrend$Uninterruptible_power_supply))/
  (max(NigeriaTrend$Uninterruptible_power_supply)-min(NigeriaTrend$Uninterruptible_power_supply))

covidTrend <- read.csv("COVID19.csv")
#to nomalize covid
covidTrend$covid<-(covidTrend $covid - min(covidTrend $covid))/
  (max(covidTrend $covid)-min(covidTrend$covid))


# create time series object using ts()
# ts() takes three arguments: start, end, and freq. 


NigeriaUninterruptible_power_supply.ts  <- ts(NigeriaTrend$Uninterruptible_power_supply,
                                start = c(2020, 91), end = c(2020, 200), freq = 365)
covid.ts  <- ts(covidTrend$covid,
                start = c(2020, 91), end = c(2020, 200), freq = 365)

windows()
ts.plot(NigeriaUninterruptible_power_supply.ts,covid.ts, gpars= list(col=c("black","red")))

