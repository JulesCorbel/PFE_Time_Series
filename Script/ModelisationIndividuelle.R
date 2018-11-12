setwd("~/Polytech/PFE")
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

MSEAnnTrain <- window(MSEAnn, start=1990, end=2015)
MSETrimTrain <- window(MSETrim, start=1990, end=c(2015,4))
MSEAnnTest <- window(MSEAnn, start=2016)
MSETrimTest <- window(MSETrim, start=2016)

##Modélisation par Holt-Winter
LEMSEAnn <- HoltWinters(MSEAnnTrain,alpha=NULL,beta=NULL,gamma=FALSE)
LEMSETrim <- HoltWinters(MSETrimTrain,alpha=NULL,beta=NULL,gamma=NULL, seasonal = "mul")
LEMSEAnnPred <- forecast(LEMSEAnn, h = 2)
LEMSETrimPred <- forecast(LEMSETrim, h = 6)

plot(MSEAnnTest, type='l', ylim=c(5000000000,6000000000),
     main="Comparaison entre la prédiction du modèle et les valeurs
     réelles pour la masse salariale annuelle")
lines(LEMSEAnnPred$mean, col="red")

plot(MSETrimTest, type='l',
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
ARIMAMSETrimTest <- forecast(ARIMAMSETrimTrain, h=7)
plot(MSETrimTest, main="Comparaison entre le modèle et les données de validation
     pour la masse salariale trimestrielle", ylim=c(1300000000,1570000000))
lines(ARIMAMSETrimTest$mean, col="red")

ARIMAMSEAnnTest <- forecast(ARIMAMSEAnnTrain, h = 2)
plot(MSEAnnTest, main="Comparaison entre le modèle et les données de validation
     pour la masse salariale annuelle", ylim=c(5000000000,6000000000))
lines(ARIMAMSEAnnTest$mean, col="red")

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

plot(PIBTrimTest, type='l', ylim=c(50000, 530000)
     main="Comparaison entre la prédiction du modèle et les valeurs
     réelles pour le PIB")
lines(LEPIBTrimPred$mean, col='red')


