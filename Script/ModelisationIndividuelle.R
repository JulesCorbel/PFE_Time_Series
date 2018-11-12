rm(list=ls())
setwd("~/PFE_Time_Series")
library(tseries)
library(forecast)

annuelle <- read.csv("Data/Data_Annuel.csv", sep=";", dec=",", nrows=30)
trim <- read.csv("Data/Data_Trim.csv", sep=";", dec=",")

###Séries temporelles MSE
MSEAnn <- ts(annuelle$MSE, start = 1990, end = 2019)
MSETrim <- ts(trim$MSE, start=c(1990,1), end = c(2017,4), frequency = 4)
plot(MSEAnn, main="Evolution annuelle de la masse salariale")
plot(MSETrim, main="Evolution trimestrielle de la masse salariale")
acf(MSEAnn, main="Auto-corrélation de la masse salariale annuelle", na.action=na.pass)
pacf(MSEAnn, main="Auto-corrélation partielle de la masse salariale annuelle", na.action=na.pass)
acf(MSETrim, main="Auto-corrélation de la masse salariale trimestrielle", na.action=na.pass)
pacf(MSETrim, main="Auto-corrélation partielle de la masse salariale trimestrielle", na.action=na.pass)

#On sépare les séries : modélisations jusque 2015, prévision à partir de 2016

MSEAnnTrain <- window(MSEAnn, start=1990, end=2012)
MSETrimTrain <- window(MSETrim, start=1990, end=c(2012,4))
MSEAnnTest <- window(MSEAnn, start=2013)
MSETrimTest <- window(MSETrim, start=2013)

##Modélisation par Holt-Winter
LEMSEAnn <- HoltWinters(MSEAnnTrain,alpha=NULL,beta=NULL,gamma=FALSE)
LEMSETrim <- HoltWinters(MSETrimTrain,alpha=NULL,beta=NULL,gamma=NULL, seasonal = "mul")
LEMSEAnnPred <- forecast(LEMSEAnn, h = 5)
LEMSETrimPred <- forecast(LEMSETrim, h = 18)

plot(MSEAnnTest, type='l', ylim=c(5000000000,6000000000),
     main="Comparaison entre la prédiction du modèle et les valeurs
     réelles pour la masse salariale annuelle")
lines(LEMSEAnnPred$mean, col="red")

plot(MSETrimTest, type='l', ylim=c(1350000000,1650000000),
     main="Comparaison entre la prédiction du modèle et les valeurs
     réelles pour la masse salariale trimestrielle")
lines(LEMSETrimPred$mean, col='red')

##Modèles SARIMA
MSEAnnTrainStat <- diff(MSEAnnTrain, differences = 2, lag = 1)

#Stationnarisation de la série
temp <- diff(MSETrimTrain, differences = 1, lag = 1)
MSETrimTrainStat <- diff(temp, differences = 1, lag = 4)
plot(MSETrimTrainStat, main="Masse salariale trimestrielle avec une différence de 1
     et une saisonnalité de 4")
Box.test(MSETrimTrainStat, type = "Ljung-Box")
acf(MSETrimTrainStat, main="Auto-corrélation de la masse salariale trimestrielle")
pacf(MSETrimTrainStat, main="Auto-corrélation partielle de la masse salariale trimestrielle")

#Train
ARIMAMSETrimTrain <- auto.arima(MSETrimTrain, stationary = F)
plot(MSETrimTrain, main="Comparaison entre le modèle et les données d'apprentissage
     pour la masse salariale trimestrielle")
lines(ARIMAMSETrimTrain$fitted, col="red")

ARIMAMSEAnnTrain <- auto.arima(MSEAnnTrain, stationary = F)
plot(MSEAnnTrain, main="Comparaison entre le modèle et les données d'apprentissage
     pour la masse salariale annuelle")
lines(ARIMAMSEAnnTrain$fitted, col="red")

#Test
ARIMAMSETrimTest <- forecast(ARIMAMSETrimTrain, h=18)
plot(MSETrimTest, main="Comparaison entre le modèle et les données de validation
     pour la masse salariale trimestrielle", ylim=c(1300000000,1700000000))
lines(ARIMAMSETrimTest$mean, col="red")

ARIMAMSEAnnTest <- forecast(ARIMAMSEAnnTrain, h = 5)
plot(MSEAnnTest, main="Comparaison entre le modèle et les données de validation
     pour la masse salariale annuelle", ylim=c(5400000000,7200000000))
lines(ARIMAMSEAnnTest$mean, col="red")

###SMIC
SMICAnn <- ts(annuelle$SMIC, start = 1990, end = 2019)
SMICTrim <- ts(trim$SMIC, start=c(1990,1), end = c(2017,4), frequency = 4)
plot(SMICAnn, main="Evolution annuelle du SMIC")
plot(SMICTrim, main="Evolution trimestrielle du SMIC")
acf(SMICAnn, main="Auto-corrélation du SMIC", na.action=na.pass)
pacf(SMICAnn, main="Auto-corrélation partielle du SMIC", na.action=na.pass)
acf(SMICTrim, main="Auto-corrélation du SMIC", na.action=na.pass)
pacf(SMICTrim, main="Auto-corrélation partielle du SMIC", na.action=na.pass)

#On sépare les séries : modélisations jusque 2015, prévision à partir de 2016

SMICAnnTrain <- window(SMICAnn, start=1990, end=2012)
SMICTrimTrain <- window(SMICTrim, start=1990, end=c(2012,4))
SMICAnnTest <- window(SMICAnn, start=2013)
SMICTrimTest <- window(SMICTrim, start=2013)

##Modélisation par Holt-Winter
LESMICAnn <- HoltWinters(SMICAnnTrain,alpha=NULL,beta=NULL,gamma=FALSE)
LESMICTrim <- HoltWinters(SMICTrimTrain,alpha=NULL,beta=NULL,gamma=NULL, seasonal = "add")
LESMICAnnPred <- forecast(LESMICAnn, h = 7)
LESMICTrimPred <- forecast(LESMICTrim, h = 20)

plot(SMICAnnTest, type='l', ylim = c(9.4,11),
     main="Comparaison entre la prédiction du lissage exponentiel et les valeurs
     réelles pour le SMIC annuel")
lines(LESMICAnnPred$mean, col="red")

plot(SMICTrimTest, type='l', ylim = c(9.4, 10.7),
     main="Comparaison entre la prédiction du lissage exponentiel et les valeurs
     réelles pour le SMIC trimestrielle")
lines(LESMICTrimPred$mean, col='red')

##Modèles SARIMA

#Train
ARIMASMICTrimTrain <- auto.arima(SMICTrimTrain, stationary = F)
plot(SMICTrimTrain, main="Comparaison entre le modèle et les données d'apprentissage
     pour le SMIC trimestrielle")
lines(ARIMASMICTrimTrain$fitted, col="red")

ARIMASMICAnnTrain <- auto.arima(SMICAnnTrain, stationary = F)
plot(SMICAnnTrain, main="Comparaison entre le modèle et les données d'apprentissage
     pour le SMIC annuel")
lines(ARIMASMICAnnTrain$fitted, col="red")

#Test
ARIMASMICTrimTest <- forecast(ARIMASMICTrimTrain, h = 20)
plot(SMICTrimTest, main="Comparaison entre le modèle et les données de validation
     pour le SMIC trimestriel", ylim = c(9.4,10.7))
lines(ARIMASMICTrimTest$mean, col="red")

ARIMASMICAnnTest <- forecast(ARIMASMICAnnTrain, h = 7)
plot(SMICAnnTest, main="Comparaison entre le modèle et les données de validation
     pour le SMIC annuel", ylim=c(9.4,11))
lines(ARIMASMICAnnTest$mean, col="red")

###PIB

PIBAnn <- ts(annuelle$PIB, start = 1990, end = 2019)
PIBTrim <- ts(trim$PIB, start=c(1990,1), end = c(2017,4), frequency = 4)
plot(PIBAnn, main="Evolution annuelle du PIB")
plot(PIBTrim, main="Evolution trimestrielle du PIB")
acf(PIBAnn, main="Auto-corrélation du PIB", na.action=na.pass)
pacf(PIBAnn, main="Auto-corrélation partielle du PIB", na.action=na.pass)
acf(PIBTrim, main="Auto-corrélation du PIB", na.action=na.pass)
pacf(PIBTrim, main="Auto-corrélation partielle du PIB", na.action=na.pass)

#On sépare les séries : modélisations jusque 2015, prévision à partir de 2016

PIBAnnTrain <- window(PIBAnn, start=1990, end=2012)
PIBTrimTrain <- window(PIBTrim, start=1990, end=c(2012,4))
PIBAnnTest <- window(PIBAnn, start=2013)
PIBTrimTest <- window(PIBTrim, start=2013)

##Modélisation par Holt-Winter
LEPIBAnn <- HoltWinters(PIBAnnTrain,alpha=NULL,beta=NULL,gamma=FALSE)
LEPIBTrim <- HoltWinters(PIBTrimTrain,alpha=NULL,beta=NULL,gamma=NULL)
LEPIBAnnPred <- forecast(LEPIBAnn, h = 7)
LEPIBTrimPred <- forecast(LEPIBTrim, h = 17)

plot(PIBAnnTest, type='l',
     main="Comparaison entre la prédiction du modèle et les valeurs
     réelles pour le PIB")
lines(LEPIBAnnPred$mean, col="red")

plot(PIBTrimTrain, type='l', 
     main="Comparaison entre la prédiction du modèle et les valeurs
     réelles pour le PIB")
lines(LEPIBTrim$fitted[,1], col='red')

##Modèles SARIMA

#Train
ARIMAPIBTrimTrain <- auto.arima(PIBTrimTrain, stationary = F)
plot(PIBTrimTrain, main="Comparaison entre le modèle et les données d'apprentissage
     pour le SMIC trimestrielle")
lines(ARIMAPIBTrimTrain$fitted, col="red")

ARIMAPIBAnnTrain <- auto.arima(PIBAnnTrain, stationary = F)
plot(PIBAnnTrain, main="Comparaison entre le modèle et les données d'apprentissage
     pour le PIB annuel")
lines(ARIMAPIBAnnTrain$fitted, col="red")

#Test
ARIMAPIBTrimTest <- forecast(ARIMAPIBTrimTrain, h = 20)
plot(PIBTrimTest, main="Comparaison entre le modèle et les données de validation
     pour le PIB trimestriel")
lines(ARIMAPIBTrimTest$mean, col="red")

ARIMAPIBAnnTest <- forecast(ARIMAPIBAnnTrain, h = 7)
plot(PIBAnnTest, main="Comparaison entre le modèle et les données de validation
     pour le PIB trimestriel")
lines(ARIMAPIBAnnTest$mean, col="red")

###Taux de chômage

TCHOAnn <- ts(annuelle$TCHO, start = 1990, end = 2019)
TCHOTrim <- ts(trim$TCHO, start=c(1990,1), end = c(2017,4), frequency = 4)
plot(TCHOAnn, main="Evolution annuelle du taux de chômage")
plot(TCHOTrim, main="Evolution trimestrielle du taux de chômage")
acf(TCHOAnn, main="Auto-corrélation du taux de chômage", na.action=na.pass)
pacf(TCHOAnn, main="Auto-corrélation partielle du taux de chômage", na.action=na.pass)
acf(TCHOTrim, main="Auto-corrélation du taux de chômage", na.action=na.pass)
pacf(TCHOTrim, main="Auto-corrélation partielle du taux de chômage", na.action=na.pass)

#On sépare les séries : modélisations jusque 2015, prévision à partir de 2016

TCHOAnnTrain <- window(TCHOAnn, start=1990, end=2012)
TCHOTrimTrain <- window(TCHOTrim, start=1990, end=c(2012,4))
TCHOAnnTest <- window(TCHOAnn, start=2013)
TCHOTrimTest <- window(TCHOTrim, start=2013)

##Modélisation par Holt-Winter
LETCHOAnn <- HoltWinters(TCHOAnnTrain,alpha=NULL,beta=NULL,gamma=FALSE)
LETCHOTrim <- HoltWinters(TCHOTrimTrain,alpha=NULL,beta=NULL,gamma=FALSE)
LETCHOAnnPred <- forecast(LESMICAnn, h = 7)
LETCHOTrimPred <- forecast(LESMICTrim, h = 20)

plot(TCHOAnnTest, type='l', ylim=c(8.7,11),
     main="Comparaison entre la prédiction du lissage exponentiel et les valeurs
     réelles pour le taux de chômage annuel")
lines(LETCHOAnnPred$mean, col="red")

plot(TCHOTrimTest, type='l', ylim = c(8.8, 10.6),
     main="Comparaison entre la prédiction du lissage exponentiel et les valeurs
     réelles pour le taux de chômage trimestrielle")
lines(LETCHOTrimPred$mean, col='red')

##Modèles SARIMA

#Train
ARIMATCHOTrimTrain <- auto.arima(TCHOTrimTrain, stationary = F)
plot(TCHOTrimTrain, main="Comparaison entre le modèle et les données d'apprentissage
     pour le taux de chômage trimestriel")
lines(ARIMATCHOTrimTrain$fitted, col="red")

ARIMATCHOAnnTrain <- auto.arima(TCHOAnnTrain, stationary = F)
plot(TCHOAnnTrain, main="Comparaison entre le modèle et les données d'apprentissage
     pour le taux de chômage annuel")
lines(ARIMATCHOAnnTrain$fitted, col="red")

#Test
ARIMATCHOTrimTest <- forecast(ARIMATCHOTrimTrain, h = 20)
plot(TCHOTrimTest, main="Comparaison entre le modèle et les données de validation
     pour le taux de chômage trimestriel", ylim = c(9.4,10.7))
lines(ARIMATCHOTrimTest$mean, col="red")

ARIMATCHOAnnTest <- forecast(ARIMATCHOAnnTrain, h = 7)
plot(TCHOAnnTest, main="Comparaison entre le modèle et les données de validation
     pour le taux de chômage annuel")
lines(ARIMATCHOAnnTest$mean, col="red")
