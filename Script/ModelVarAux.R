#Ce script correspond à la modélisation des variables MSE en fonction des autres séries temporelles

library(vars)
library(smooth)
library(portes)

###SARIMA

##Annuelle

# 0 Variable

ARIMAMSEAnnTrain <- auto.arima(MSEAnnTrain, stationary = F)
PredVide <- forecast(ARIMAMSEAnnTrain, h = 2)
plot(PredVide$mean, main="Comparaison entre le modèle ARIMA et les données
     d'apprentissage pour la masse salariale annuelle", 
     ylim=c(min(MSEAnnTest,PredVide$mean),max(MSEAnnTest,PredVide$mean)),
     col="red")
lines(MSEAnnTest)
EQM(MSEAnnTest, PredVide$mean)

# 1 VARIABLE

#Aged
SARIMAAged <- auto.arima(MSEAnnTrain, stationary = F, xreg = AGEDAnnTrain)
PredAged <- forecast(SARIMAAged, xreg = AGEDAnnTest[1:2])
plot(PredAged$mean, col="red", 
     ylim=c(min(MSEAnnTest,PredAged$mean), max(MSEAnnTest,PredAged$mean)),
     main = "SARIMA expliqué par 'Aged' vs Vraies valeurs")
lines(MSEAnnTest)
EQM(MSEAnnTest, PredAged$mean)

#PIB
SARIMAPIB <- auto.arima(MSEAnnTrain, stationary = F, xreg = PIBAnnTrain)
PredPIB <- forecast(SARIMAPIB, xreg = PIBAnnTest[1:2])
plot(PredPIB$mean, col="red",
     ylim=c(min(MSEAnnTest,PredPIB$mean), max(MSEAnnTest,PredPIB$mean)),
     main = "SARIMA expliqué par le PIB vs Vraies valeurs")
lines(MSEAnnTest)
EQM(MSEAnnTest, PredPIB$mean)

#SMIC
SARIMASMIC <- auto.arima(MSEAnnTrain, stationary = F, xreg = SMICAnnTrain)
PredSMIC <- forecast(SARIMASMIC, xreg = SMICAnnTest[1:2])
plot(PredSMIC$mean, col="red",
     ylim=c(min(MSEAnnTest,PredSMIC$mean), max(MSEAnnTest,PredSMIC$mean)),
     main = "SARIMA expliqué par le SMIC vs Vraies valeurs")
lines(MSEAnnTest)
EQM(MSEAnnTest, PredSMIC$mean)

#TCHO
SARIMATCHO <- auto.arima(MSEAnnTrain, stationary = F, xreg = TCHOAnnTrain)
PredTCHO <- forecast(SARIMATCHO, xreg = TCHOAnnTest[1:2])
plot(PredTCHO$mean, col="red",
     ylim=c(min(MSEAnnTest,PredTCHO$mean), max(MSEAnnTest,PredTCHO$mean)),
     main = "SARIMA expliqué par le taux chômage vs Vraies valeurs")
lines(MSEAnnTest)
EQM(MSEAnnTest, PredTCHO$mean)

plot(MSEAnnTest, main="Comparaison des modèles avec 1 variable", ylim=c(4788526734,6530930676))
lines(PredAged$mean, col="blue")
lines(PredPIB$mean, col="green")
lines(PredSMIC$mean, col="red")
lines(PredTCHO$mean, col="gold")
legend('topright', legend = c('Série MSE', 'Aged', 'PIB', 'SMIC', 'Taux chômage'),
       col=c('black', 'blue', 'green', 'red', 'gold'), lty=1, cex=0.8)

## 2 VARIABLES

#Aged & PIB
SARIMAAgedPIB <- auto.arima(MSEAnnTrain, xreg = cbind(AGEDAnnTrain, PIBAnnTrain))
PredAgedPIB <- forecast(SARIMAAgedPIB, xreg = cbind(AGEDAnnTest[1:2], PIBAnnTest[1:2]))
plot(PredAgedPIB$mean, col="red",
     ylim=c(min(MSEAnnTest,PredAgedPIB$mean), max(MSEAnnTest,PredAgedPIB$mean)),
     main = "SARIMA expliqué par 'Aged' et le PIB vs Vraies valeurs")
lines(MSEAnnTest)
EQM(MSEAnnTest, PredAgedPIB$mean)

#Aged & SMIC
SARIMAAgedSMIC <- auto.arima(MSEAnnTrain, xreg = cbind(AGEDAnnTrain, SMICAnnTrain))
PredAgedSMIC <- forecast(SARIMAAgedSMIC, xreg = cbind(AGEDAnnTest[1:2], SMICAnnTest[1:2]))
plot(PredAgedSMIC$mean, col="red",
     ylim=c(min(MSEAnnTest,PredAgedSMIC$mean), max(MSEAnnTest,PredAgedSMIC$mean)),
     main = "SARIMA expliqué par 'Aged' et le SMIC vs Vraies valeurs")
lines(MSEAnnTest)
EQM(MSEAnnTest, PredAgedSMIC$mean)

#Aged & TCHO
SARIMAAgedTCHO <- auto.arima(MSEAnnTrain, xreg = cbind(AGEDAnnTrain, TCHOAnnTrain))
PredAgedTCHO <- forecast(SARIMAAgedTCHO, xreg = cbind(AGEDAnnTest[1:2], TCHOAnnTest[1:2]))
plot(PredAgedTCHO$mean, col="red",
     ylim=c(min(MSEAnnTest,PredAgedTCHO$mean), max(MSEAnnTest,PredAgedTCHO$mean)),
     main = "SARIMA expliqué par 'Aged' et le taux de chômage vs Vraies valeurs")
lines(MSEAnnTest)
EQM(MSEAnnTest, PredAgedTCHO$mean)

#PIB & SMIC
SARIMAPIBSMIC <- auto.arima(MSEAnnTrain, xreg = cbind(PIBAnnTrain, SMICAnnTrain))
PredPIBSMIC <- forecast(SARIMAPIBSMIC, xreg = cbind(PIBAnnTest[1:2], SMICAnnTest[1:2]))
plot(PredPIBSMIC$mean, col="red",
     ylim=c(min(MSEAnnTest,PredPIBSMIC$mean), max(MSEAnnTest,PredPIBSMIC$mean)),
     main = "SARIMA expliqué par le PIB et le SMIC vs Vraies valeurs")
lines(MSEAnnTest)
EQM(MSEAnnTest, PredPIBSMIC$mean)
#Pas de modèle ARIMA possible en stationnaire

#PIB & TCHO
SARIMAPIBTCHO <- auto.arima(MSEAnnTrain, xreg = cbind(PIBAnnTrain, TCHOAnnTrain))
PredPIBTCHO <- forecast(SARIMAPIBTCHO, xreg = cbind(PIBAnnTest[1:2], TCHOAnnTest[1:2]))
plot(PredPIBTCHO$mean, col="red",
     ylim=c(min(MSEAnnTest,PredPIBTCHO$mean), max(MSEAnnTest,PredPIBTCHO$mean)),
     main = "SARIMA expliqué par le PIB et le taux de chômage vs Vraies valeurs")
lines(MSEAnnTest)
EQM(PredPIBTCHO$mean, MSEAnnTest)

#SMIC & TCHO
SARIMASMICTCHO <- auto.arima(MSEAnnTrain, xreg = cbind(SMICAnnTrain, TCHOAnnTrain))
PredSMICTCHO <- forecast(SARIMASMICTCHO, xreg = cbind(SMICAnnTest[1:2], TCHOAnnTest[1:2]))
plot(PredSMICTCHO$mean, col="red",
     ylim=c(min(MSEAnnTest,PredSMICTCHO$mean), max(MSEAnnTest,PredSMICTCHO$mean)),
     main = "SARIMA expliqué par le SMIC et le taux de chômage vs Vraies valeurs")
lines(MSEAnnTest)
EQM(PredSMICTCHO$mean, MSEAnnTest)
#Pas de modèle ARIMA possible stationnaire

plot(MSEAnnTest, main="Comparaison des modèles avec 2 variables", ylim=c(5213734262, 6599683445))
lines(PredAgedPIB$mean, col="red")
lines(PredAgedSMIC$mean, col="blue")
lines(PredAgedTCHO$mean, col="green")
lines(PredPIBSMIC$mean, col="gold")
lines(PredPIBTCHO$mean, col="purple")
lines(PredSMICTCHO$mean, col="brown")
legend('topright', 
       legend = c('Série MSE', 'Aged & PIB', 'Aged & SMIC', 'Aged & Taux chômage',
                  'PIB & SMIC', 'PIB & Taux chômage', 'SMIC & Taux chômage'),
       col=c('black', 'red', 'blue', 'green', 'gold', 'purple', 'brown'), lty=1, cex=0.8)

## 3 VARIABLES

#Aged & PIB & SMIC
SARIMAAgedPIBSMIC <- auto.arima(MSEAnnTrain, xreg = cbind(AGEDAnnTrain, PIBAnnTrain, SMICAnnTrain))
PredAgedPIBSMIC <- forecast(SARIMAAgedPIBSMIC, xreg = cbind(AGEDAnnTest[1:2], PIBAnnTest[1:2], SMICAnnTest[1:2]))
plot(PredAgedPIBSMIC$mean, col="red",
     ylim=c(min(MSEAnnTest,PredAgedPIBSMIC$mean), max(MSEAnnTest,PredAgedPIBSMIC$mean)),
     main = "Lissage exponentiel expliqué par 'Aged', le PIB et le SMIC vs Vraies valeurs")
lines(MSEAnnTest)
EQM(PredAgedPIBSMIC$mean, MSEAnnTest)

#Aged & PIB & TCHO
SARIMAAgedPIBTCHO <- auto.arima(MSEAnnTrain, xreg = cbind(AGEDAnnTrain, PIBAnnTrain, TCHOAnnTrain))
PredAgedPIBTCHO <- forecast(SARIMAAgedPIBTCHO, xreg = cbind(AGEDAnnTest[1:2], PIBAnnTest[1:2], TCHOAnnTest[1:2]))
plot(PredAgedPIBTCHO$mean, col="red",
     ylim=c(min(MSEAnnTest,PredAgedPIBTCHO$mean), max(MSEAnnTest,PredAgedPIBTCHO$mean)),
     main = "Lissage exponentiel expliqué par 'Aged', le PIB et le taux de chômage vs Vraies valeurs")
lines(MSEAnnTest)
EQM(PredAgedPIBTCHO$mean, MSEAnnTest)

#Aged & SMIC & TCHO
SARIMAAgedSMICTCHO <- auto.arima(MSEAnnTrain, xreg = cbind(AGEDAnnTrain, SMICAnnTrain, TCHOAnnTrain))
PredAgedSMICTCHO <- forecast(SARIMAAgedSMICTCHO, xreg = cbind(AGEDAnnTest[1:2], SMICAnnTest[1:2], TCHOAnnTest[1:2]))
plot(PredAgedSMICTCHO$mean, col="red",
     ylim=c(min(MSEAnnTest,PredAgedSMICTCHO$mean), max(MSEAnnTest,PredAgedSMICTCHO$mean)),
     main = "Lissage exponentiel expliqué par 'Aged', le SMIC et le taux de chômage vs Vraies valeurs")
lines(MSEAnnTest)
EQM(PredAgedSMICTCHO$mean, MSEAnnTest)


#PIB & SMIC & TCHO
SARIMAPIBSMICTCHO <- auto.arima(MSEAnnTrain, xreg = cbind(PIBAnnTrain, SMICAnnTrain, TCHOAnnTrain))
PredPIBSMICTCHO <- forecast(SARIMAPIBSMICTCHO, xreg = cbind(PIBAnnTest[1:2], SMICAnnTest[1:2], TCHOAnnTest[1:2]))
plot(PredPIBSMICTCHO$mean, col="red",
     ylim=c(min(MSEAnnTest,PredPIBSMICTCHO$mean), max(MSEAnnTest,PredPIBSMICTCHO$mean)),
     main = "Lissage exponentiel expliqué par le PIB, le SMIC et le taux de chômage vs Vraies valeurs")
lines(MSEAnnTest)
EQM(PredPIBSMICTCHO$mean, MSEAnnTest)

plot(MSEAnnTest, main="Comparaison des modèles avec 3 variables", ylim=c(5474429602, 6545732819))
lines(PredAgedPIBSMIC$mean, col="blue")
lines(PredAgedPIBTCHO$mean, col="green")
lines(PredAgedSMICTCHO$mean, col="red")
lines(PredPIBSMICTCHO$mean, col="gold")
legend('right', legend = c('Série MSE', 'Aged & PIB & SMIC', 'Aged & PIB & Taux chômage',
                             'Aged & SMIC & Taux chômage', 'PIB & SMIC & Taux chômage'),
       col=c('black', 'blue', 'green', 'red', 'gold'), lty=1, cex=0.8)

## 4 VARIABLES

SARIMACOMPLET <- auto.arima(MSEAnnTrain, xreg = cbind(PIBAnnTrain, SMICAnnTrain, TCHOAnnTrain, AGEDAnnTrain))
PredCOMPLET <- forecast(SARIMACOMPLET, xreg = cbind(PIBAnnTest[1:2], SMICAnnTest[1:2], TCHOAnnTest[1:2], AGEDAnnTest[1:2]))
plot(PredCOMPLET$mean, col="red",
     ylim=c(min(MSEAnnTest,PredCOMPLET$mean), max(MSEAnnTest,PredCOMPLET$mean)),
     main = "Lissage exponentiel expliqué par le PIB, le SMIC et le taux de chômage vs Vraies valeurs")
lines(MSEAnnTest)

#Comparaison des 5 modèles

plot(MSEAnnTest, main="Comparaison des 4 modèles", ylim=c(4788526734, 6460708016))
lines(PredVide$mean, col='brown')
lines(PredPIB$mean, col="blue")
lines(PredSMICTCHO$mean, col="green")
lines(PredAgedPIBTCHO$mean, col="red")
lines(PredCOMPLET$mean, col="gold")
legend('topright', legend = c('Série MSE', 'Pas de variable explicative', 'PIB', 'SMIC + Taux chômage',
                             'Aged + PIB + Taux chômage', 'Complet'),
       col=c('black', 'brown', 'blue', 'green', 'red', 'gold'), lty=1, cex=0.8)

##Trimestrielle

# 0 Variable
ARIMAMSETrimTrain <- auto.arima(MSETrimTrain)
PredVide <- forecast(ARIMAMSETrimTrain, h = 5)
plot(PredVide$mean, main="Comparaison entre le modèle ARIMA et les données
     d'apprentissage pour la masse salariale annuelle", 
     ylim=c(min(MSETrimTest,PredVide$mean),max(MSETrimTest,PredVide$mean)),
     col="red")
lines(MSETrimTest)
EQM(PredVide$mean, MSETrimTest)

# 1 VARIABLE

#PIB
SARIMAPIB <- auto.arima(MSETrimTrain, xreg = PIBTrimTrain)
PredPIB <- forecast(SARIMAPIB, xreg = PIBTrimTest[1:5])
plot(PredPIB$mean, col="red",
     ylim=c(min(MSETrimTest,PredPIB$mean), max(MSETrimTest,PredPIB$mean)),
     main = "SARIMA expliqué par le PIB vs Vraies valeurs")
lines(MSETrimTest)
EQM(PredPIB$mean, MSETrimTest)

#SMIC
SARIMASMIC <- auto.arima(MSETrimTrain, xreg = SMICTrimTrain)
PredSMIC <- forecast(SARIMASMIC, xreg = SMICTrimTest[1:5])
plot(PredSMIC$mean, col="red",
     ylim=c(min(MSETrimTest,PredSMIC$mean), max(MSETrimTest,PredSMIC$mean)),
     main = "SARIMA expliqué par le SMIC vs Vraies valeurs")
lines(MSETrimTest)
EQM(PredSMIC$mean, MSETrimTest)

#TCHO
SARIMATCHO <- auto.arima(MSETrimTrain, xreg = TCHOTrimTrain)
PredTCHO <- forecast(SARIMATCHO, h=5, xreg = TCHOTrimTest[1:5])
plot(PredTCHO$mean, col="red",
     ylim=c(min(MSETrimTest,PredTCHO$mean), max(MSETrimTest,PredTCHO$mean)),
     main = "SARIMA expliqué par le taux chômage vs Vraies valeurs")
lines(MSETrimTest)
EQM(PredTCHO$mean, MSETrimTest)

plot(MSETrimTest, main="Comparaison des modèles avec 1 variable", ylim=c(1.35e+9, 1.65e+9))
lines(PredPIB$mean, col="green")
lines(PredSMIC$mean, col="red")
lines(PredTCHO$mean, col="gold")
legend('bottomright', legend = c('Série MSE', 'PIB', 'SMIC', 'Taux chômage'),
       col=c('black', 'green', 'red', 'gold'), lty=1, cex=0.8)

## 2 VARIABLES

#PIB & SMIC
SARIMAPIBSMIC <- auto.arima(MSETrimTrain, xreg = cbind(PIBTrimTrain, SMICTrimTrain))
PredPIBSMIC <- forecast(SARIMAPIBSMIC, xreg = cbind(PIBTrimTest[1:5], SMICTrimTest[1:5]))
plot(PredPIBSMIC$mean, col="red",
     ylim=c(min(MSETrimTest,PredPIBSMIC$mean), max(MSETrimTest,PredPIBSMIC$mean)),
     main = "SARIMA expliqué par le PIB et le SMIC vs Vraies valeurs")
lines(MSETrimTest)
EQM(PredPIBSMIC$mean, MSETrimTest)

#PIB & TCHO
SARIMAPIBTCHO <- auto.arima(MSETrimTrain, xreg = cbind(PIBTrimTrain, TCHOTrimTrain))
PredPIBTCHO <- forecast(SARIMAPIBTCHO, xreg = cbind(PIBTrimTest[1:5], TCHOTrimTest[1:5]))
plot(PredPIBTCHO$mean, col="red",
     ylim=c(min(MSETrimTest,PredPIBTCHO$mean), max(MSETrimTest,PredPIBTCHO$mean)),
     main = "SARIMA expliqué par le PIB et le taux de chômage vs Vraies valeurs")
lines(MSETrimTest)
EQM(PredPIBTCHO$mean, MSETrimTest)

#SMIC & TCHO
SARIMASMICTCHO <- auto.arima(MSETrimTrain, xreg = cbind(SMICTrimTrain, TCHOTrimTrain))
PredSMICTCHO <- forecast(SARIMASMICTCHO, xreg = cbind(SMICTrimTest[1:5], TCHOTrimTest[1:5]))
plot(PredSMICTCHO$mean, col="red",
     ylim=c(min(MSETrimTest,PredSMICTCHO$mean), max(MSETrimTest,PredSMICTCHO$mean)),
     main = "SARIMA expliqué par le SMIC et le taux de chômage vs Vraies valeurs")
lines(MSETrimTest)
EQM(PredSMICTCHO$mean, MSETrimTest)

plot(MSETrimTest, main="Comparaison des modèles avec 2 variables")
lines(PredPIBSMIC$mean, col="gold")
lines(PredPIBTCHO$mean, col="purple")
lines(PredSMICTCHO$mean, col="brown")
legend('bottomright', 
       legend = c('Série MSE', 'PIB & SMIC', 'PIB & Taux chômage', 'SMIC & Taux chômage'),
       col=c('black', 'gold', 'purple', 'brown'), lty=1, cex=0.8)

## 3 VARIABLES

#PIB & SMIC & TCHO
SARIMAPIBSMICTCHO <- auto.arima(MSETrimTrain, xreg = cbind(PIBTrimTrain, SMICTrimTrain, TCHOTrimTrain))
PredPIBSMICTCHO <- forecast(SARIMAPIBSMICTCHO, xreg = cbind(PIBTrimTest[1:5], SMICTrimTest[1:5], TCHOTrimTest[1:5]))
plot(PredPIBSMICTCHO$mean, col="red",
     ylim=c(min(MSETrimTest,PredPIBSMICTCHO$mean), max(MSETrimTest,PredPIBSMICTCHO$mean)),
     main = "Lissage exponentiel expliqué par le PIB, le SMIC et le taux de chômage vs Vraies valeurs")
lines(MSETrimTest)
EQM(PredPIBSMICTCHO$mean, MSETrimTest)

#Comparaison des 4 modèles

plot(MSETrimTest, main="Comparaison des 4 modèles", ylim=c(1350308319, 1573075485))
lines(PredVide$mean, col='brown')
lines(PredPIB$mean, col="blue")
lines(PredSMICTCHO$mean, col="green")
lines(PredPIBSMICTCHO$mean, col="red")
legend('left', legend = c('Série MSE', 'Pas de variable explicative', 'PIB', 'SMIC + Taux chômage',
                              'Aged + PIB + Taux chômage'),
       col=c('black', 'brown', 'blue', 'green', 'red'), lty=1, cex=0.8)

