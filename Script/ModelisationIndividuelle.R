#rm(list=ls())
setwd("~/PFE_Time_Series")
library(tseries)
library(forecast)

annuelle <- read.csv("Data/Data_Annuel.csv", sep=";", dec=",", nrows=30)
trim <- read.csv("Data/Data_Trim.csv", sep=";", dec=",")

###Séries temporelles MSE
MSEAnn<-visualisation(annuelle$MSE, 1990, 2017, 1, "de la masse salariale annuelle")
MSETrim<-visualisation(trim$MSE, 1990, 2017, 4, "de la masse salariale trimestrielle")

MSEAnnTrain <- window(MSEAnn, start=1990, end=2012)
MSETrimTrain <- window(MSETrim, start=1990, end=c(2012,4))
MSEAnnTest <- window(MSEAnn, start=2013)
MSETrimTest <- window(MSETrim, start=2013)

##Lissage Exponentiel
MSEAnnPred<-lissage_exponentiel(MSEAnnTrain, 1990, 2012, 1, 5)

plot(MSEAnnTest, type='l', ylim=c(5000000000,6300000000),
     main="Comparaison entre la prédiction du modèle et les valeurs
     réelles pour la masse salariale annuelle")
lines(MSEAnnPred$mean, col="red")

MSETrimPred<-lissage_exponentiel(MSETrimTrain, 1990, 2012, 4, 18)

plot(MSETrimTest, type='l', ylim=c(1350000000,1650000000),
     main="Comparaison entre la prédiction du modèle et les valeurs
     réelles pour la masse salariale trimestrielle")
lines(MSETrimPred$mean, col='red')

##Modèles SARIMA
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
SMICAnn<-visualisation(annuelle$SMIC, 1990, 2019, 1, "du SMIC annuel")
SMICTrim<-visualisation(trim$SMIC, 1990, 2017, 4, "du SMIC trimestriel")

#On sépare les séries : modélisations jusque 2015, prévision à partir de 2016

SMICAnnTrain <- window(SMICAnn, start=1990, end=2012)
SMICTrimTrain <- window(SMICTrim, start=1990, end=c(2012,4))
SMICAnnTest <- window(SMICAnn, start=2013)
SMICTrimTest <- window(SMICTrim, start=2013)

##Lissage exponentiel
SMICAnnPred<-lissage_exponentiel(SMICAnnTrain, 1990, 2012, 1, 7)

plot(SMICAnnTest, type='l', ylim = c(9.4,11),
     main="Comparaison entre la prédiction du lissage exponentiel et les valeurs
     réelles pour le SMIC annuel")
lines(SMICAnnPred$mean, col="red")

SMICTrimPred<-lissage_exponentiel(SMICTrimTrain, 1990, 2012, 1, 20)

plot(SMICTrimTest, type='l', ylim = c(9.4, 11),
     main="Comparaison entre la prédiction du lissage exponentiel et les valeurs
     réelles pour le SMIC trimestrielle")
lines(SMICTrimPred$mean, col='red')

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
lines(SMICTrimPred$mean, col='green')
###PIB

PIBAnn<-visualisation(annuelle$PIB, 1990, 2019, 1, "du PIB annuel")
PIBTrim<-visualisation(trim$PIB, 1990, 2017, 4, "du PIB trimestriel")

#On sépare les séries : modélisations jusque 2015, prévision à partir de 2016

PIBAnnTrain <- window(PIBAnn, start=1990, end=2012)
PIBTrimTrain <- window(PIBTrim, start=1990, end=c(2012,4))
PIBAnnTest <- window(PIBAnn, start=2013)
PIBTrimTest <- window(PIBTrim, start=2013)

##Lissage Exponentiel
PIBAnnPred<-lissage_exponentiel(PIBAnnTrain, 1990, 2012, 1, 5)

plot(PIBAnnTest, type='l',
     main="Comparaison entre la prédiction du lissage exponentiel et les valeurs
     réelles pour le PIB annuel")
lines(PIBAnnPred$mean, col="red")

PIBTrimPred<-lissage_exponentiel(PIBTrimTrain, 1990, 2012, 4, 17)

plot(PIBTrimTest, type='l', ylim=c(502000, 535000),
     main="Comparaison entre la prédiction du lissage exponentiel et les
      valeurs réelles pour le PIB trimestriel")
lines(PIBTrimPred$mean, col='red')

##Modèles SARIMA

#Train
ARIMAPIBTrimTrain <- auto.arima(PIBTrimTrain, stationary = F)
plot(PIBTrimTrain, main="Comparaison entre le modèle et les données d'apprentissage
     pour le PIB trimestriel")
lines(ARIMAPIBTrimTrain$fitted, col="red")

ARIMAPIBAnnTrain <- auto.arima(PIBAnnTrain, stationary = F)
plot(PIBAnnTrain, main="Comparaison entre le modèle et les données d'apprentissage
     pour le PIB annuel")
lines(ARIMAPIBAnnTrain$fitted, col="red")

#Test
ARIMAPIBTrimTest <- forecast(ARIMAPIBTrimTrain, h = 17)
plot(PIBTrimTest, main="Comparaison entre le modèle et les données de validation
     pour le PIB trimestriel", ylim=c(510000, 544000))
lines(ARIMAPIBTrimTest$mean, col="red")

ARIMAPIBAnnTest <- forecast(ARIMAPIBAnnTrain, h = 5)
plot(PIBAnnTest, main="Comparaison entre le modèle et les données de validation
     pour le PIB trimestriel", ylim=c(2050,2200))
lines(ARIMAPIBAnnTest$mean, col="red")

###Taux de chômage

TCHOAnn<-visualisation(annuelle$TCHO, 1990, 2019, 1, "du taux de chômage annuel")
TCHOTrim<-visualisation(trim$TCHO, 1990, 2017, 4, "du taux de chômage trimestriel")

TCHOAnnTrain <- window(TCHOAnn, start=1990, end=2012)
TCHOTrimTrain <- window(TCHOTrim, start=1990, end=c(2012,4))
TCHOAnnTest <- window(TCHOAnn, start=2013)
TCHOTrimTest <- window(TCHOTrim, start=2013)

##Lissage Exponentiel

TCHOAnnPred<-lissage_exponentiel(TCHOAnnTrain, 1990, 2012, 1, 7)

plot(TCHOAnnTest, type='l', ylim=c(8.7,11),
     main="Comparaison entre la prédiction du lissage exponentiel et les valeurs
     réelles pour le taux de chômage annuel")
lines(TCHOAnnPred$mean, col="red")

TCHOTrimPred<-lissage_exponentiel(TCHOTrimTrain, 1990, 2012, 1, 20)

plot(TCHOTrimTest, type='l', ylim = c(8.8, 10.6),
     main="Comparaison entre la prédiction du lissage exponentiel et les valeurs
     réelles pour le taux de chômage trimestriel")
lines(TCHOTrimPred$mean, col='red')
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
     pour le taux de chômage trimestriel", ylim = c(8.7,10))
lines(ARIMATCHOTrimTest$mean, col="red")

ARIMATCHOAnnTest <- forecast(ARIMATCHOAnnTrain, h = 7)
plot(TCHOAnnTest, main="Comparaison entre le modèle et les données de validation
     pour le taux de chômage annuel")
lines(ARIMATCHOAnnTest$mean, col="red")

###Aged

AGEDAnn<-visualisation(annuelle$AGED, 1990, 2019, 1, "aged")

AGEDAnnTrain <- window(AGEDAnn, start=1990, end=2012)
AGEDAnnTest <- window(AGEDAnn, start=2013)


##Lissage Exponentiel
AGEDAnnPred<-lissage_exponentiel(AGEDAnnTrain, 1990, 2012, 1, 7)

plot(AGEDAnnTest, ylim=c(270, 310), type='l',
     main="Comparaison entre la prédiction du lissage exponentiel et les valeurs
     réelles pour le AGED annuel")
lines(AGEDAnnPred$mean, col="red")

##Modèles SARIMA

#Train
ARIMAAGEDAnnTrain <- auto.arima(AGEDAnnTrain, stationary = F)
plot(AGEDAnnTrain, main="Comparaison entre le modèle et les données d'apprentissage
     pour le AGED annuel")
lines(ARIMAAGEDAnnTrain$fitted, col="red")

#Test
ARIMAAGEDAnnTest <- forecast(ARIMAAGEDAnnTrain, h = 7)
plot(AGEDAnnTest, main="Comparaison entre le modèle et les données de validation
     pour le AGED annuel")
lines(ARIMAAGEDAnnTest$mean, col="red")


