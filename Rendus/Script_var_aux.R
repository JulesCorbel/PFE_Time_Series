rm(list=ls())

require(tseries)
require(forecast)
require(corrplot)
require(fUnitRoots)

#Erreur quadratique moyenne
EQM<-function(serie, prediction){
  return(sum((serie - prediction)^2)/length(serie))
}

########################### Importation des données  #############################################
#trim <- read.csv("~/PFE_Time_Series/Data/Data_Trim.csv", sep=";", dec=",")
trim <- read.csv("~/Cygwin/app/home/Jules/PFE_Time_Series/Data/Data_Trim.csv", sep=";", dec=",")
##################################################################################################

########################### Analyse descriptive des séries #######################################

MSE <- ts(trim$MSE, start = 1990, end = c(2017, 2), frequency=4)
PIB <- ts(trim$PIB, start = 1990, end = c(2017, 1), frequency=4)
SMIC <- ts(trim$SMIC, start = c(1990,1), end = c(2017, 4), frequency = 4)
TCHOF <- ts(trim$TCHOF, start = c(1990,1), end = c(2017, 4), frequency = 4)

#Découpage des séries en échantillons d'apprentissage et de test
MSETrain <- window(MSE, start=1990, end=c(2015,4))
MSETest <- window(MSE, start=2016, end=c(2017,2))
PIBTrain <- window(PIB, start=1990, end=c(2015,4))
PIBTest <- window(PIB, start=2016, end=c(2017,1))
SMICTrain <- window(SMIC, start=1990, end=c(2015,4))
SMICTest <- window(SMIC, start=2016, end=c(2017,2))
TCHOFTrain <- window(TCHOF, start=1990, end=c(2015,4))
TCHOFTest <- window(TCHOF, start=2016, end=c(2017,2))

#Prédiction de la valeur du PIB pour 2017Q2
PredARIMAPIB<- forecast(auto.arima(PIBTrain), h=6)
new.value <- PredARIMAPIB$mean[6]
PIBTest<-ts(c(PredARIMAPIB$mean, new.value), start = 2016, end = c(2017, 2), frequency=4)


##################################################################################################

###################### Modélisation ARMA avec variables exogènes de la MSE #######################

#PIB
SARIMAPIB <- auto.arima(MSETrain, xreg = cbind(PIBTrain))
SARIMAPIB
PredPIB <- forecast(SARIMAPIB, xreg = cbind(PIBTest))
plot(PredPIB$mean, col="red",
     ylim=c(min(MSETest,PredPIB$mean), max(MSETest,PredPIB$mean)),
     main = "Masse salariale expliquée par le PIB vs Vraies valeurs")
lines(MSETest)
EQM(PredPIB$mean, MSETest)

#SMIC
SARIMASMIC <- auto.arima(MSETrain, xreg = cbind(SMICTrain))
SARIMASMIC
PredSMIC <- forecast(SARIMASMIC, xreg = cbind(SMICTest))
plot(PredSMIC$mean, col="red",
     ylim=c(min(MSETest,PredSMIC$mean), max(MSETest,PredSMIC$mean)),
     main = "Masse salariale expliqué par le SMIC vs Vraies valeurs")
lines(MSETest)
EQM(PredSMIC$mean, MSETest)

#TCHOF
SARIMATCHOF <- auto.arima(MSETrain, xreg = cbind(TCHOFTrain))
SARIMATCHOF
PredTCHOF <- forecast(SARIMATCHOF, xreg = cbind(TCHOFTest))
plot(PredTCHOF$mean, col="red",
     ylim=c(min(MSETest,PredTCHOF$mean), max(MSETest,PredTCHOF$mean)),
     main = "Masse salariale expliquée par le taux de chômage vs Vraies valeurs")
lines(MSETest)
EQM(PredTCHOF$mean, MSETest)

#PIB & SMIC
SARIMAPIBSMIC <- auto.arima(MSETrain, xreg = cbind(PIBTrain, SMICTrain))
SARIMAPIBSMIC
PredPIBSMIC <- forecast(SARIMAPIBSMIC, xreg = cbind(PIBTest, SMICTest))
plot(PredPIBSMIC$mean, col="red",
     ylim=c(min(MSETest,PredPIBSMIC$mean), max(MSETest,PredPIBSMIC$mean)),
     main = "Masse salariale expliquée par le PIB et le SMIC vs Vraies valeurs")
lines(MSETest)
EQM(PredPIBSMIC$mean, MSETest)

#PIB & TCHOF
SARIMAPIBTCHOF <- auto.arima(MSETrain, xreg = cbind(PIBTrain, TCHOFTrain))
SARIMAPIBTCHOF
PredPIBTCHOF <- forecast(SARIMAPIBTCHOF, xreg = cbind(PIBTest, TCHOFTest))
plot(PredPIBTCHOF$mean, col="red",
     ylim=c(min(MSETest,PredPIBTCHOF$mean), max(MSETest,PredPIBTCHOF$mean)),
     main = "Masse salariale expliquée par le PIB et le taux de chômage vs Vraies valeurs")
lines(MSETest)
EQM(PredPIBTCHOF$mean, MSETest)

#SMIC & TCHOF
SARIMASMICTCHOF <- auto.arima(MSETrain, xreg = cbind(SMICTrain, TCHOFTrain))
SARIMASMICTCHOF
PredSMICTCHOF <- forecast(SARIMASMICTCHOF, xreg = cbind(SMICTest, TCHOFTest))
plot(PredSMICTCHOF$mean, col="red",
     ylim=c(min(MSETest,PredSMICTCHOF$mean), max(MSETest,PredSMICTCHOF$mean)),
     main = "Masse salariale expliquée par le SMIC et le taux de chômage vs Vraies valeurs")
lines(MSETest)
EQM(PredSMICTCHOF$mean, MSETest)

#PIB, SMIC & TCHOF
SARIMACOMPLET <- auto.arima(MSETrain, xreg = cbind(PIBTrain, SMICTrain, TCHOFTrain))
SARIMACOMPLET
PredCOMPLET <- forecast(SARIMACOMPLET, xreg = cbind(PIBTest, SMICTest, TCHOFTest))
plot(PredCOMPLET$mean, col="red",
     ylim=c(min(MSETest,PredCOMPLET$mean), max(MSETest,PredCOMPLET$mean)),
     main = "Masse salariale expliquée par le PIB, le SMIC et le taux de chômage vs Vraies valeurs")
lines(MSETest)
EQM(PredCOMPLET$mean, MSETest)

#Comparaison des résultats
resultats<-matrix(nrow=7, ncol=1, dimnames = list(c("PIB", "SMIC", "TCHOF", "PIB & SMIC", "PIB & SMIC", "PIB & TCHOF", "PIB, SMIC & TCHOF"), 
                                                  c("EQM")))
resultats[1,1] = EQM(MSETest, PredPIB$mean)
resultats[2,1] = EQM(MSETest, PredSMIC$mean)
resultats[3,1] = EQM(MSETest, PredTCHOF$mean)
resultats[4,1] = EQM(MSETest, PredPIBSMIC$mean)
resultats[5,1] = EQM(MSETest, PredPIBTCHOF$mean)
resultats[6,1] = EQM(MSETest, PredSMICTCHOF$mean)
resultats[7,1] = EQM(MSETest, PredCOMPLET$mean)
resultats

##################################################################################################
