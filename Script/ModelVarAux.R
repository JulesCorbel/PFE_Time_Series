#Ce script correspond à la modélisation des variables MSE en fonction des autres séries temporelles

library(smooth)

###Lissage Exponentiel
##MSE annuelle

#0 VARIABLE
LE <- es(MSEAnn, model = "ZZZ", ic="AICc", h = 2, holdout = T)
plot(LE$forecast, col="red",
     main = "Lissage exponentiel de MSE vs Vraies valeurs",
     ylim=c(min(LE$forecast,MSEAnnTest),max(LE$forecast,MSEAnnTest)))
lines(MSEAnnTest)
EQM(MSEAnnTest, LE$forecast)

## 1 VARIABLE

#Aged
LEAged <- es(MSEAnn, model = "ZZZ", ic="AICc", xreg = AGEDAnn[1:28], h = 2, holdout = T)
plot(LEAged$forecast, col="red", 
     ylim=c(min(LEAged$forecast,MSEAnnTest),max(LEAged$forecast,MSEAnnTest)),
     main = "Lissage exponentiel expliqué par 'Aged' vs Vraies valeurs")
lines(MSEAnnTest)
EQM(MSEAnnTest, LEAged$forecast)

#PIB
LEPIB <- es(MSEAnn, model = "ZZZ", ic="AICc", xreg = PIBAnn[1:28], h = 2, holdout = T)
plot(LEPIB$forecast, col="red",
     main = "Lissage exponentiel expliqué par le PIB vs Vraies valeurs",
     ylim=c(min(LEPIB$forecast,MSEAnnTest),max(LEPIB$forecast,MSEAnnTest)))
lines(MSEAnnTest)
EQM(MSEAnnTest, LEPIB$forecast)

#SMIC
LESMIC <- es(MSEAnn, model = "ZZZ", ic="AICc", xreg = SMICAnn[1:28], h = 2, holdout = T)
plot(LESMIC$forecast, col="red",
     main = "Lissage exponentiel expliqué par le SMIC vs Vraies valeurs",
     ylim=c(min(LESMIC$forecast,MSEAnnTest),max(LESMIC$forecast,MSEAnnTest)))
lines(MSEAnnTest)
EQM(MSEAnnTest, LESMIC$forecast)

#TCHO
LETCHO <- es(MSEAnn, model = "ZZZ", ic="AICc", xreg = TCHOAnn[1:28], h = 2, holdout = T)
plot(LETCHO$forecast, col="red",
     main = "Lissage exponentiel expliqué par le taux de chômage 
     vs Vraies valeurs",
     ylim=c(min(LETCHO$forecast,MSEAnnTest),max(LETCHO$forecast,MSEAnnTest)))
lines(MSEAnnTest)
EQM(MSEAnnTest, LETCHO$forecast)

plot(MSEAnnTest, main="Comparaison des modèles avec 1 variable", ylim=c(5168766285,5503220125))
lines(LEAged$forecast, col="blue")
lines(LEPIB$forecast, col="green")
lines(LESMIC$forecast, col="red")
lines(LETCHO$forecast, col="gold")
legend('bottomleft', legend = c('Série MSE', 'Aged', 'PIB', 'SMIC', 'Taux chômage'),
       col=c('black', 'blue', 'green', 'red', 'gold'), lty=1, cex=0.8)

## 2 VARIABLES

#Aged & PIB
LEAgedPIB <- es(MSEAnn, model = "ZZZ", ic="AICc", xreg = cbind(AGEDAnn[1:28], PIBAnn[1:28]), h = 2, holdout = T)
plot(LEAgedPIB$forecast, col="red",
     main = "Lissage exponentiel expliqué par 'Aged' et le PIB vs Vraies valeurs",
     ylim=c(min(LEAgedPIB$forecast,MSEAnnTest),max(LEAgedPIB$forecast,MSEAnnTest)))
lines(MSEAnnTest)
EQM(MSEAnnTest, LEAgedPIB$forecast)

#Aged & SMIC
LEAgedSMIC <- es(MSEAnn, model = "ZZZ", ic="AICc", xreg = cbind(AGEDAnn[1:28], SMICAnn[1:28]), h = 2, holdout = T)
plot(LEAgedSMIC$forecast, col="red",
     main = "Lissage exponentiel expliqué par 'Aged' et le SMIC vs Vraies valeurs",
     ylim=c(min(LEAgedSMIC$forecast,MSEAnnTest),max(LEAgedSMIC$forecast,MSEAnnTest)))
lines(MSEAnnTest)
EQM(MSEAnnTest, LEAgedSMIC$forecast)

#Aged & TCHO
LEAgedTCHO <- es(MSEAnn, model = "ZZZ", ic="AICc", xreg = cbind(AGEDAnn[1:28], TCHOAnn[1:28]), h = 2, holdout = T)
plot(LEAgedTCHO$forecast, col="red",
     main = "Lissage exponentiel expliqué par 'Aged' et le taux de chômage vs Vraies valeurs",
     ylim=c(min(LEAgedTCHO$forecast,MSEAnnTest),max(LEAgedTCHO$forecast,MSEAnnTest)))
lines(MSEAnnTest)
EQM(MSEAnnTest, LEAgedTCHO$forecast)

#PIB & SMIC
LEPIBSMIC <- es(MSEAnn, model = "ZZZ", ic="AICc", xreg = cbind(PIBAnn[1:28], SMICAnn[1:28]), h = 2, holdout = T)
plot(LEPIBSMIC$forecast, col="red",
     main = "Lissage exponentiel expliqué par le PIB et le SMIC vs Vraies valeurs",
     ylim=c(min(LEPIBSMIC$forecast,MSEAnnTest),max(LEPIBSMIC$forecast,MSEAnnTest)))
lines(MSEAnnTest)
EQM(MSEAnnTest, LEPIBSMIC$forecast)

#PIB & TCHO
LEPIBTCHO <- es(MSEAnn, model = "ZZZ", ic="AICc", xreg = cbind(PIBAnn[1:28], TCHOAnn[1:28]), h = 2, holdout = T)
plot(LEPIBTCHO$forecast, col="red",
     main = "Lissage exponentiel expliqué par le PIB et le taux de chômage vs Vraies valeurs",
     ylim=c(min(LEPIBTCHO$forecast,MSEAnnTest),max(LEPIBTCHO$forecast,MSEAnnTest)))
lines(MSEAnnTest)
EQM(MSEAnnTest, LEPIBTCHO$forecast)

#SMIC & TCHO
LESMICTCHO <- es(MSEAnn, model = "ZZZ", ic="AICc", xreg = cbind(SMICAnn[1:28], TCHOAnn[1:28]), h = 2, holdout = T)
plot(LESMICTCHO$forecast, col="red",
     main = "Lissage exponentiel expliqué par le SMIC et le taux de chômage vs Vraies valeurs",
     ylim=c(min(LESMICTCHO$forecast,MSEAnnTest),max(LESMICTCHO$forecast,MSEAnnTest)))
lines(MSEAnnTest)
EQM(MSEAnnTest, LESMICTCHO$forecast)

plot(MSEAnnTest, main="Comparaison des modèles avec 2 variables", ylim=c(5178326823, 5503220125))
lines(LEAgedPIB$forecast, col="red")
lines(LEAgedSMIC$forecast, col="blue")
lines(LEAgedTCHO$forecast, col="green")
lines(LEPIBSMIC$forecast, col="gold")
lines(LEPIBTCHO$forecast, col="purple")
lines(LESMICTCHO$forecast, col="brown")
legend('right', 
       legend = c('Série MSE', 'Aged & PIB', 'Aged & SMIC', 'Aged & Taux chômage',
                  'PIB & SMIC', 'PIB & Taux chômage', 'SMIC & Taux chômage'),
       col=c('black', 'red', 'blue', 'green', 'gold', 'purple', 'brown'), lty=1, cex=0.8)

## 3 VARIABLES

#Aged & PIB & SMIC
LEAgedPIBSMIC <- es(MSEAnn, model = "ZZZ", ic="AICc", xreg = cbind(AGEDAnn[1:28], PIBAnn[1:28], SMICAnn[1:28]), h = 2, holdout = T)
plot(LEAgedPIBSMIC$forecast, col="red",
     main = "Lissage exponentiel expliqué par 'Aged', le PIB et le SMIC vs Vraies valeurs",
     ylim=c(min(LEAgedPIBSMIC$forecast,MSEAnnTest),max(LEAgedPIBSMIC$forecast,MSEAnnTest)))
lines(MSEAnnTest)
EQM(MSEAnnTest, LEAgedPIBSMIC$forecast)

#Aged & PIB & TCHO
LEAgedPIBTCHO <- es(MSEAnn, model = "ZZZ", ic="AICc", xreg = cbind(AGEDAnn[1:28], PIBAnn[1:28], TCHOAnn[1:28]), h = 2, holdout = T)
plot(LEAgedPIBTCHO$forecast, col="red",
     main = "Lissage exponentiel expliqué par 'Aged', le PIB et le taux de chômage vs Vraies valeurs",
     ylim=c(min(LEAgedPIBTCHO$forecast,MSEAnnTest),max(LEAgedPIBTCHO$forecast,MSEAnnTest)))
lines(MSEAnnTest)
EQM(MSEAnnTest, LEAgedPIBTCHO$forecast)

#Aged & SMIC & TCHO
LEAgedSMICTCHO <- es(MSEAnn, model = "ZZZ", ic="AICc", xreg = cbind(AGEDAnn[1:28], SMICAnn[1:28], TCHOAnn[1:28]), h = 2, holdout = T)
plot(LEAgedSMICTCHO$forecast, col="red",
     main = "Lissage exponentiel expliqué par 'Aged', le SMIC et le taux de chômage vs Vraies valeurs",
     ylim=c(min(LEAgedSMICTCHO$forecast,MSEAnnTest),max(LEAgedSMICTCHO$forecast,MSEAnnTest)))
lines(MSEAnnTest)
EQM(MSEAnnTest, LEAgedSMICTCHO$forecast)

#PIB & SMIC & TCHO
LEPIBSMICTCHO <- es(MSEAnn, model = "ZZZ", ic="AICc", xreg = cbind(PIBAnn[1:28], SMICAnn[1:28], TCHOAnn[1:28]), h = 2, holdout = T)
plot(LEPIBSMICTCHO$forecast, col="red",
     main = "Lissage exponentiel expliqué par le PIB, le SMIC et le taux de chômage vs Vraies valeurs",
     ylim=c(min(LEPIBSMICTCHO$forecast,MSEAnnTest),max(LEPIBSMICTCHO$forecast,MSEAnnTest)))
lines(MSEAnnTest)
EQM(MSEAnnTest, LEPIBSMICTCHO$forecast)

plot(MSEAnnTest, main="Comparaison des modèles avec 3 variables", ylim=c(5124549325, 5503220125))
lines(LEAgedPIBSMIC$forecast, col="blue")
lines(LEAgedPIBTCHO$forecast, col="green")
lines(LEAgedSMICTCHO$forecast, col="red")
lines(LEPIBSMICTCHO$forecast, col="gold")
legend('right', legend = c('Série MSE', 'Aged & PIB & SMIC', 'Aged & PIB & Taux chômage',
                             'Aged & SMIC & Taux chômage', 'PIB & SMIC & Taux chômage'),
       col=c('black', 'blue', 'green', 'red', 'gold'), lty=1, cex=0.8)

## 4 VARIABLES

LECOMPLET <- es(MSEAnn, model = "ZZZ", ic="AICc", xreg = cbind(PIBAnn[1:28], SMICAnn[1:28], TCHOAnn[1:28], AGEDAnn[1:28]), h = 2, holdout = T)
plot(LECOMPLET$forecast, col="red",
     main = "Lissage exponentiel expliqué par le PIB, le SMIC et le taux de chômage vs Vraies valeurs",
     ylim=c(min(LECOMPLET$forecast,MSEAnnTest),max(LECOMPLET$forecast,MSEAnnTest)))
lines(MSEAnnTest)
EQM(MSEAnnTest, LECOMPLET$forecast)

#Comparaison des 5 modèles

plot(MSEAnnTest, main="Comparaison des 5 modèles", ylim=c(5189848053, 5681371752))
lines(MSEAnnPred$mean, col='brown')
lines(LEPIB$forecast, col="blue")
lines(LEPIBTCHO$forecast, col="green")
lines(LEPIBSMICTCHO$forecast, col="red")
lines(LECOMPLET$forecast, col="gold")
legend('right', legend = c('Série MSE', 'Pas de variable explicative', 'PIB', 'PIB + Taux chômage',
                             'SMIC + PIB + Taux chômage', 'Complet'),
       col=c('black', 'brown', 'blue', 'green', 'red', 'gold'), lty=1, cex=0.5)

##MSE trimestrielle

#0 VARIABLE
LET <- es(MSETrim, model = "ZZZ", ic="AICc", h = 5, holdout = T)
plot(LET$forecast, col="red",
     main = "Lissage exponentiel expliqué par 'Aged' vs Vraies valeurs",
     ylim=c(min(LET$forecast,MSETrimTest),max(LET$forecast,MSETrimTest)))
lines(MSETrimTest)
EQM(MSETrimTest, LET$forecast)

## 1 VARIABLE

#PIB
LETPIB <- es(MSETrim, model = "ZZZ", ic="AICc", xreg = PIBTrim[1:109], h = 5, holdout = T)
plot(LETPIB$forecast, col="red",
     main = "Lissage exponentiel expliqué par le PIB vs Vraies valeurs",
     ylim=c(min(LETPIB$forecast,MSETrimTest),max(LETPIB$forecast,MSETrimTest)))
lines(MSETrimTest)
EQM(MSETrimTest, LETPIB$forecast)

#SMIC
LETSMIC <- es(MSETrim, model = "ZZZ", ic="AICc", xreg = SMICTrim[1:109], h = 5, holdout = T)
plot(LETSMIC$forecast, col="red",
     main = "Lissage exponentiel expliqué par le SMIC vs Vraies valeurs",
     ylim=c(min(LETSMIC$forecast,MSETrimTest),max(LETSMIC$forecast,MSETrimTest)))
lines(MSETrimTest)
EQM(MSETrimTest, LETSMIC$forecast)

#TCHO
LETTCHO <- es(MSETrim, model = "ZZZ", ic="AICc", xreg = TCHOTrim[1:109], h = 5, holdout = T)
plot(LETTCHO$forecast, col="red",
     main = "Lissage exponentiel expliqué par le taux de chômage 
     vs Vraies valeurs",
     ylim=c(min(LETTCHO$forecast,MSETrimTest),max(LETTCHO$forecast,MSETrimTest)))
lines(MSETrimTest)
EQM(MSETrimTest, LETTCHO$forecast)

plot(MSETrimTest, main="Comparaison des modèles avec 1 variable")
lines(LETPIB$forecast, col="green") #La meilleure
lines(LETSMIC$forecast, col="red")
lines(LETTCHO$forecast, col="blue")
legend('bottomleft', legend = c('Série MSE', 'PIB', 'SMIC', 'Taux chômage'),
       col=c('black', 'green', 'red', 'gold'), lty=1, cex=0.8)

## 2 VARIABLES

#PIB & SMIC
LETPIBSMIC <- es(MSETrim, model = "ZZZ", ic="AICc", xreg = cbind(PIBTrim[1:109], SMICTrim[1:109]), h = 5, holdout = T)
plot(LETPIBSMIC$forecast, col="red",
     main = "Lissage exponentiel expliqué par le PIB et le SMIC vs Vraies valeurs",
     ylim=c(min(LETPIBSMIC$forecast,MSETrimTest),max(LETPIBSMIC$forecast,MSETrimTest)))
lines(MSETrimTest)
EQM(MSETrimTest, LETPIBSMIC$forecast)

#PIB & TCHO
LETPIBTCHO <- es(MSETrim, model = "ZZZ", ic="AICc", xreg = cbind(PIBTrim[1:109], TCHOTrim[1:109]), h = 5, holdout = T)
plot(LETPIBTCHO$forecast, col="red",
     main = "Lissage exponentiel expliqué par le PIB et le taux de chômage vs Vraies valeurs",
     ylim=c(min(LETPIBTCHO$forecast,MSETrimTest),max(LETPIBTCHO$forecast,MSETrimTest)))
lines(MSETrimTest)
EQM(MSETrimTest, LETPIBTCHO$forecast)

#SMIC & TCHO
LETSMICTCHO <- es(MSETrim, model = "ZZZ", ic="AICc", xreg = cbind(SMICTrim[1:109], TCHOTrim[1:109]), h = 5, holdout = T)
plot(LETSMICTCHO$forecast, col="red",
     main = "Lissage exponentiel expliqué par le SMIC et le taux de chômage vs Vraies valeurs",
     ylim=c(min(LETSMICTCHO$forecast,MSETrimTest),max(LETSMICTCHO$forecast,MSETrimTest)))
lines(MSETrimTest)
EQM(MSETrimTest, LETSMICTCHO$forecast)

plot(MSETrimTest, main="Comparaison des modèles avec 2 variables", ylim=c(1362716118, 1593222641))
lines(LETPIBSMIC$forecast, col="gold")
lines(LETPIBTCHO$forecast, col="purple") #Meilleure
lines(LETSMICTCHO$forecast, col="red")
legend('right', 
       legend = c('Série MSE', 'PIB & SMIC', 'PIB & Taux chômage', 'SMIC & Taux chômage'),
       col=c('black', 'gold', 'purple', 'brown'), lty=1, cex=0.8)

## 3 VARIABLES

#PIB & SMIC & TCHO
LETCOMPLET <- es(MSETrim, model = "ZZZ", ic="AICc", xreg = cbind(PIBTrim[1:109], SMICTrim[1:109], TCHOTrim[1:109]), h = 5, holdout = T)
plot(LETCOMPLET$forecast, col="red",
     main = "Lissage exponentiel expliqué par le PIB, le SMIC et le taux de chômage vs Vraies valeurs",
     ylim=c(min(LETCOMPLET$forecast,MSETrimTest),max(LETCOMPLET$forecast,MSETrimTest)))
lines(MSETrimTest)
EQM(MSETrimTest, LETCOMPLET$forecast)

#Comparaison des 4 modèles

plot(MSETrimTest, main="Comparaison des 4 modèles")
lines(LE$forecast, col='brown')
lines(LETPIB$forecast, col="blue")
lines(LETPIBTCHO$forecast, col="green")
lines(LETCOMPLET$forecast, col="red")
legend('right', legend = c('Série MSE', 'Pas de variable explicative', 'PIB', 'PIB + Taux chômage',
                           'Complet'),
       col=c('black', 'brown', 'blue', 'green', 'red'), lty=1, cex=0.5)

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
PredVide <- forecast(ARIMAMSETrimTrain, h = 6)
plot(PredVide$mean, main="Comparaison entre le modèle ARIMA et les données
     d'apprentissage pour la masse salariale annuelle", 
     ylim=c(min(MSETrimTest,PredVide$mean),max(MSETrimTest,PredVide$mean)),
     col="red")
lines(MSETrimTest)
EQM(PredVide$mean, MSETrimTest)

# 1 VARIABLE

#PIB
SARIMAPIB <- auto.arima(MSETrimTrain, xreg = PIBTrimTrain)
PredPIB <- forecast(SARIMAPIB, xreg = PIBTest)
plot(PredPIB$mean, col="red",
     ylim=c(min(MSETest,PredPIB$mean), max(MSETest,PredPIB$mean)),
     main = "SARIMA expliqué par le PIB vs Vraies valeurs")
lines(MSETest)
EQM(PredPIB$mean, MSETrimTest)

#SMIC
SARIMASMIC <- auto.arima(MSETrimTrain, xreg = SMICTrimTrain)
PredSMIC <- forecast(SARIMASMIC, xreg = SMICTest)
plot(PredSMIC$mean, col="red",
     ylim=c(min(MSETest,PredSMIC$mean), max(MSETest,PredSMIC$mean)),
     main = "SARIMA expliqué par le SMIC vs Vraies valeurs")
lines(MSETest)
EQM(PredSMIC$mean, MSETest)

#TCHO
SARIMATCHO <- auto.arima(MSETrimTrain, xreg = TCHOTrimTrain)
PredTCHO <- forecast(SARIMATCHO, xreg = TCHOFTest)
plot(PredTCHO$mean, col="red",
     ylim=c(min(MSETest,PredTCHO$mean), max(MSETest,PredTCHO$mean)),
     main = "SARIMA expliqué par le taux chômage vs Vraies valeurs")
lines(MSETest)
EQM(PredTCHO$mean, MSETest)

plot(MSETrimTest, main="Comparaison des modèles avec 1 variable", ylim=c(1.35e+9, 1.65e+9))
lines(PredPIB$mean, col="green")
lines(PredSMIC$mean, col="red")
lines(PredTCHO$mean, col="gold")
legend('bottomright', legend = c('Série MSE', 'PIB', 'SMIC', 'Taux chômage'),
       col=c('black', 'green', 'red', 'gold'), lty=1, cex=0.8)

## 2 VARIABLES

#PIB & SMIC
SARIMAPIBSMIC <- auto.arima(MSETrain, xreg = cbind(PIBTrain, SMICTrain))
PredPIBSMIC <- forecast(SARIMAPIBSMIC, xreg = cbind(PIBTest, SMICTest))
plot(PredPIBSMIC$mean, col="red",
     ylim=c(min(MSETest,PredPIBSMIC$mean), max(MSETest,PredPIBSMIC$mean)),
     main = "SARIMA expliqué par le PIB et le SMIC vs Vraies valeurs")
lines(MSETest)
EQM(PredPIBSMIC$mean, MSETest)

#PIB & TCHO
SARIMAPIBTCHO <- auto.arima(MSETrain, xreg = cbind(PIBTrain, TCHOFTrain))
PredPIBTCHO <- forecast(SARIMAPIBTCHO, xreg = cbind(PIBTest, TCHOFTest))
plot(PredPIBTCHO$mean, col="red",
     ylim=c(min(MSETest,PredPIBTCHO$mean), max(MSETest,PredPIBTCHO$mean)),
     main = "SARIMA expliqué par le PIB et le taux de chômage vs Vraies valeurs")
lines(MSETest)
EQM(PredPIBTCHO$mean, MSETest)

#SMIC & TCHO
SARIMASMICTCHO <- auto.arima(MSETrain, xreg = cbind(SMICTrain, TCHOFTrain))
PredSMICTCHO <- forecast(SARIMASMICTCHO, xreg = cbind(SMICTest, TCHOFTest))
plot(PredSMICTCHO$mean, col="red",
     ylim=c(min(MSETest,PredSMICTCHO$mean), max(MSETest,PredSMICTCHO$mean)),
     main = "SARIMA expliqué par le SMIC et le taux de chômage vs Vraies valeurs")
lines(MSETest)
EQM(PredSMICTCHO$mean, MSETest)

plot(MSETrimTest, main="Comparaison des modèles avec 2 variables")
lines(PredPIBSMIC$mean, col="gold")
lines(PredPIBTCHO$mean, col="purple")
lines(PredSMICTCHO$mean, col="brown")
legend('bottomright', 
       legend = c('Série MSE', 'PIB & SMIC', 'PIB & Taux chômage', 'SMIC & Taux chômage'),
       col=c('black', 'gold', 'purple', 'brown'), lty=1, cex=0.8)

## 3 VARIABLES

#PIB & SMIC & TCHO
SARIMAPIBSMICTCHO <- auto.arima(MSETrain, xreg = cbind(PIBTrain, SMICTrain, TCHOFTrain))
PredPIBSMICTCHO <- forecast(SARIMAPIBSMICTCHO, xreg = cbind(PIBTest, SMICTest, TCHOFTest))
plot(PredPIBSMICTCHO$mean, col="red",
     ylim=c(min(MSETest,PredPIBSMICTCHO$mean), max(MSETest,PredPIBSMICTCHO$mean)),
     main = "Lissage exponentiel expliqué par le PIB, le SMIC et le taux de chômage vs Vraies valeurs")
lines(MSETest)
EQM(PredPIBSMICTCHO$mean, MSETest)

#Comparaison des 4 modèles

plot(MSETrimTest, main="Comparaison des 4 modèles", ylim=c(1350308319, 1573075485))
lines(PredVide$mean, col='brown')
lines(PredPIB$mean, col="blue")
lines(PredSMICTCHO$mean, col="green")
lines(PredPIBSMICTCHO$mean, col="red")
legend('left', legend = c('Série MSE', 'Pas de variable explicative', 'PIB', 'SMIC + Taux chômage',
                              'Aged + PIB + Taux chômage'),
       col=c('black', 'brown', 'blue', 'green', 'red'), lty=1, cex=0.8)

