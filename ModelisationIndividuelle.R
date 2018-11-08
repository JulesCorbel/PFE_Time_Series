setwd("/home/gis3/pguillot/PFE")
library(tseries)
library(forecast)

annuelle <- read.csv("Data_Annuel.csv", sep=";", dec=",", nrows=30)
trim <- read.csv("Data_Trim.csv", sep=";", dec=",")

#Séries temporelles MSE
MSEAnn <- ts(annuelle$MSE, start = 1990, end = 2019)
MSETrim <- ts(trim$MSE, start=c(1990,1), end = c(2017,4), frequency = 4)
plot(MSEAnn)
plot(MSETrim)
acf(MSEAnn, main="Auto-corrélation de la masse salariale annuelle", na.action=na.pass)
pacf(MSEAnn, main="Auto-corrélation partielle de la masse salariale annuelle", na.action=na.pass)
acf(MSETrim, main="Auto-corrélation de la masse salariale trimestrielle", na.action=na.pass)
pacf(MSETrim, main="Auto-corrélation partielle de la masse salariale trimestrielle", na.action=na.pass)

#On sépare les séries : modélisations jusque 2015, prévision à partir de 2016

MSEAnnTrain <- window(MSEAnn, start=1990, end=2015)
MSETrimTrain <- window(MSETrim, start=1990, end=c(2015,4))
MSEAnnTest <- window(MSEAnn, start=2016)
MSETrimTest <- window(MSETrim, start=2016)

##Modélisation par Holt-Winter
LEMSEAnn <- HoltWinters(MSEAnnTrain,alpha=NULL,beta=NULL,gamma=FALSE)
LEMSETrim <- HoltWinters(MSETrimTrain,alpha=NULL,beta=NULL,gamma=NULL, seasonal = "mul")
LEMSEAnnPred <- predict(LEMSEAnn, n.ahead = 2)
LEMSETrimPred <- predict(LEMSETrim, n.ahead = 6)

plot(MSEAnnTest, type='l', ylim=c(5000000000,6000000000))
lines(LEMSEAnnPred, col="red")

plot(MSETrimTest, type='l')
lines(LEMSETrimPred, col='red')

##Modèles SARIMA
MSEAnnTrainStat <- diff(MSEAnnTrain, differences = 2, lag = 1)

#Stationnarisation de la série
temp <- diff(MSETrimTrain, differences = 1, lag = 1)
MSETrimTrainStat <- diff(test, differences = 1, lag = 4)
plot(MSETrimTrainStat)
Box.test(MSETrimTrainStat, type = "Ljung-Box")
acf(MSETrimTrainStat)
pacf(MSETrimTrainStat)

ARIMAMSETrimTrain <- auto.arima(MSETrimTrain, stationary = F)
plot(ARIMAMSETrimTrain$fitted)
lines(MSETrimTrain, col="red")

ARIMAMSEAnnTrain <- auto.arima(MSEAnnTrain, stationary = F)
plot(ARIMAMSEAnnTrain$fitted)
lines(MSEAnnTrain, col="red")
