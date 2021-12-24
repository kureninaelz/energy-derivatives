#============================================================================
#                        TAKE-HOME EXAM TOPIC 16
#============================================================================

# install libraries ----
library(lubridate)
library(fable)
library(feasts)
library(tsibbledata) 
library(tidyverse) 
library(tsibble)
library(xts)
install.packages("forecast")
library(forecast)
install.packages("dplyr")
library(dplyr)
install.packages("astsa")
library(astsa)

# data uploading ----

energy <- read.csv(file = "ICEBRN.csv", header = TRUE, sep = ";")
glimpse(energy)
head(energy)

# data transformation and plotting ----

energy$X.DATE.<- ymd(energy$X.DATE.)
class(energy$X.DATE.)
      
energyts <- ts(energy$X.CLOSE., start = c(2010,1), frequency = 365.25)
class(energyts)

energytsvol <- ts(energy$X.VOL., start = c(2010,1), frequency = 6)
class(energytsvol)

autoplot(energyts) + ggtitle("Brent prices dynamics") +
  ylab("Daily closing prices") + xlab("Year")

autoplot(energytsvol) + ggtitle("Brent trading volume") +
  ylab("Daily volume") + xlab("Week")

# price ---- 

energyts %>% decompose() %>% plot()

acf(energyts, lag.max = 180) # clearly SAR - differences to be applied
pacf(energyts, lag.max = 180)

ggseasonplot(energyts) + ggtitle("Brent prices: seasonal dynamics")

autoplot(energyts)
entslogd <- diff(log(energyts), lag = 30)
autoplot(entslogd) #looks more stationary

entslogd2 <- diff(entslogd, lag=6)
autoplot(entslogd2) + ggtitle("Brent prices dynamics [transformed]") +
  ylab("Logged Daily closing prices") + xlab("Year") #looks even more stationary

ggseasonplot(entslogd2) + ggtitle("Brent prices: seasonal dynamics [transformed]") #clear conclusion about 'season-out-lier' - relative market environment is captured

# checking AR

acf2(entslogd2, max.lag = 30)

energydif <- (diff (xts(energy$X.CLOSE., energy$X.DATE.), lag = 1))
ggAcf(energydif, lag.max = 30)
Box.test(energydif, lag = 30, type = "Ljung-Box") #lag 1: residuals resemble a white noise


ggAcf(entslogd2, lag.max = 30)
Box.test(entslogd2, lag = 30, type = "Ljung") #transformed data: residuals resemble a white noise

Box.test(energydif, lag = 1, type = "Ljung")
Box.test(energydif, lag = 6, type = "Ljung") #p-value > 0.05 >> seasonality with lag 30 >> weekly seasonality (as we have daily data)
Box.test(energydif, lag = 30, type = "Ljung") #p-value > 0.05 >> seasonality with lag 30 >> monthly seasonality (as we have daily data)
Box.test(energydif, lag = 180, type = "Ljung")

tsdisplay(diff(xts(energy$X.CLOSE., energy$X.DATE.),30))

## price: modeling and forecasting ----

# showing that SES and simple ARIMA do not capture the seasonal trend

fcses <- ses(entslogd2, h=365) 
autoplot(fcses)
checkresiduals(fcses) #do not capture seasonality 

marima <- arima(entslogd2, order = c(1, 0, 1))
fcaam <- forecast(marima)
autoplot(fcaam) #do not capture seasonality 

marimaaut <- auto.arima(entslogd2)
fcar <- forecast(marimaaut)
autoplot(fcar, h=365) #do not capture seasonality 

# sARIMA: partially capture the trend ()

marimass <- arima(entslogd2, order = c (0, 1, 0), seasonal = list(order = c (0, 1, 0), period = 52))
marimass
summary(marimass)
fcarss <- forecast(marimass)
autoplot(fcarss, h=365*10)

marimas <- arima(entslogd2, order = c (0, 1, 0), seasonal = list(order = c (1, 1, 0), period = 52))
marimas
summary(marimas)
fcars <- forecast(marimas)
autoplot(fcars, h = 365*10) #better then previous by RMSE and AIC

sarima(entslogd2, 1,1,0, 1,1,1, 14)
sarima.for(entslogd2, 30, 1,1,0, 1,1,1, 14) #not bad - Box-Ljung test failed predictably

# complex STL + ETS with non-stationary data

fcets <- stlf(energyts) #when ETS does not capture seasonality, STL does / not transformed data
autoplot(fcets)

# Fourier series

harmonic <- fourier(energyts, K = 2)
fit <- auto.arima(energyts, xreg = harmonic, seasonal = FALSE)
fit

harmonic <- fourier(energyts, K = 15)
fit <- auto.arima(energyts, xreg = harmonic, seasonal = FALSE)
fit

newharmonics <- fourier(energyts, K = 15, h = 2*356.25)
fc <- forecast(fit, xreg = newharmonics)
autoplot(fc) 


# snaive

fcnaive <- snaive(energyts, h = 365)
autoplot(fcnaive) 

fcnaivetr <- snaive(entslogd2, h = 365)
autoplot(fcnaivetr) 
checkresiduals(fcnaivetr) #residuals do not resemble the white noise, but this predictable because of high frequency data

# training set + snaive

train <- subset(entslogd2, end = length(entslogd2) - 365)
fcn <- snaive(train, seasonal = "multiplicative", h = 365)
accuracy(fcn, entslogd2)
autoplot(fcn)
checkresiduals(fcn)

## volume ----

energytsvol %>% decompose() %>% plot()

acf(energytsvol, lag = 52) # pure weekly pattern
pacf(energytsvol, lag.max = 52) 

autoplot(energytsvol)
entsvold <- diff(energytsvol, lag = 6)
autoplot(entsvold) + ggtitle("Brent trading volume [transformed]") +
  ylab("Volume") + xlab("Week") #looks more stationary - one step as number of lags is more clear


acf2(entsvold, max.lag = 52)

## volume: modeling and forecasting

# ETS model - failed to capture seasonality

etsvol <- ets(entsvold, model = "ZZZ")
fcetsvol <- forecast(etsvol, h = 6*52)
summary(etsvol)
autoplot(fcetsvol)

# Holt-Winters model - failed to capture seasonality

hwvol <- hw(entsvold, h = 52*6, seasonal = "additive", damped = TRUE)
fchwvol <- forecast(hwvol)
autoplot(fchwvol)

# snaive

fcnaivetr <- snaive(entsvold, h = 52*3)
autoplot(fcnaivetr) 
checkresiduals(fcnaivetr)

# sARIMA

sarima(entsvold, 0,1,0, 1,1,0, 6)
sarima.for(entsvold, 30, 0,1,0, 1,1,0, 6) 

