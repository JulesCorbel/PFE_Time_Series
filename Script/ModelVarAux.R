#Ce script correspond à la modélisation des variables MSE en fonction des autres séries temporelles


library(smooth)
###Lissage Exponentiel
##MSE annuelle

#0 VARIABLE
LE <- es(MSEAnn, model = "ZZZ", ic="AICc", h = 2, holdout = T)
plot(LE$forecast, col="red",
     main = "Lissage exponentiel expliqué par 'Aged' vs Vraies valeurs",
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

MSETrim <- window(MSETrim, start=1990, end=c(2017,1))
MSETrimTest <- window(MSETrimTest, start=2016, end=c(2017,1))

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

# 1 VARIABLE

#Aged
AGEDAnnSta <- diff(AGEDAnn, differences = 2)
acf(AGEDAnnSta)
pacf(AGEDAnnSta)
Box.test(AGEDAnnSta)
plot(AGEDAnnSta)
AGEDAnnStaTrain <- window(AGEDAnnSta, end=2015)
AGEDAnnStaTest <- window(AGEDAnnSta, start=2016)

MSEAnnSta <- diff(MSEAnn, differences = 2)
acf(MSEAnnSta)
pacf(MSEAnnSta)
Box.test(MSEAnnSta)
plot(MSEAnnSta)
MSEAnnStaTrain <- window(MSEAnnSta, end=2015)
MSEAnnStaTest <- window(MSEAnnSta, start=2016)

SARIMAAged <- auto.arima(MSEAnnStaTrain, stationary = F, xreg = AGEDAnnStaTrain)
PredAged <- forecast(SARIMAAged, xreg = AGEDAnnTest[1:2])
plot(PredAged$mean, col="red", 
     ylim=c(min(MSEAnnTest,PredAged$mean), max(MSEAnnTest,PredAged$mean)),
     main = "SARIMA expliqué par 'Aged' vs Vraies valeurs")
lines(MSEAnnTest)

#PIB
SARIMAPIB <- auto.arima(MSEAnnTrain, stationary = F, xreg = PIBAnnTrain)
PredPIB <- forecast(SARIMAPIB, xreg = PIBAnnTest[1:28])
plot(PredPIB$mean, col="red", ylim=c(5474429602, 6712539781),
     main = "SARIMA expliqué par le PIB vs Vraies valeurs")
lines(MSEAnnTest)

#SMIC
SARIMASMIC <- auto.arima(MSEAnnTrain, stationary = F, xreg = SMICAnnTrain)
PredSMIC <- forecast(SARIMASMIC, xreg = SMICAnnTest[1:28])
plot(PredSMIC$mean, col="red", ylim=c(5474429602, 6928283434),
     main = "SARIMA expliqué par le SMIC vs Vraies valeurs")
lines(MSEAnnTest)

#TCHO
SARIMATCHO <- auto.arima(MSEAnnTrain, stationary = F, xreg = TCHOAnnTrain)
PredTCHO <- forecast(SARIMATCHO, xreg = TCHOAnnTest[1:28])
plot(PredTCHO$mean, col="red", ylim=c(5474429602, 7167531139),
     main = "SARIMA expliqué par le taux chômage vs Vraies valeurs")
lines(MSEAnnTest)

plot(MSEAnnTest, main="Comparaison des modèles avec 1 variable", ylim=c(5474429602, 7167531139))
lines(PredAged$mean, col="blue")
lines(PredPIB$mean, col="green")
lines(PredSMIC$mean, col="red")
lines(PredTCHO$mean, col="gold")
legend('topleft', legend = c('Série MSE', 'Aged', 'PIB', 'SMIC', 'Taux chômage'),
       col=c('black', 'blue', 'green', 'red', 'gold'), lty=1, cex=0.8)

## 2 VARIABLES

#Aged & PIB
SARIMAAgedPIB <- auto.arima(MSEAnnTrain, xreg = cbind(AGEDAnnTrain, PIBAnnTrain))
PredAgedPIB <- forecast(SARIMAAgedPIB, xreg = cbind(AGEDAnnTest[1:28], PIBAnnTest[1:28]))
plot(PredAgedPIB$mean, col="red", ylim=c(4273400654, 5963269156),
     main = "SARIMA expliqué par 'Aged' et le PIB vs Vraies valeurs")
lines(MSEAnnTest)

#Aged & SMIC
SARIMAAgedSMIC <- auto.arima(MSEAnnTrain, xreg = cbind(AGEDAnnTrain, SMICAnnTrain))
PredAgedSMIC <- forecast(SARIMAAgedSMIC, xreg = cbind(AGEDAnnTest[1:28], SMICAnnTest[1:28]))
plot(PredAgedSMIC$mean, col="red", ylim=c(5474429602, 6889864072),
     main = "SARIMA expliqué par 'Aged' et le SMIC vs Vraies valeurs")
lines(MSEAnnTest)

#Aged & TCHO
SARIMAAgedTCHO <- auto.arima(MSEAnnTrain, xreg = cbind(AGEDAnnTrain, TCHOAnnTrain))
PredAgedTCHO <- forecast(SARIMAAgedTCHO, xreg = cbind(AGEDAnnTest[1:28], TCHOAnnTest[1:28]))
plot(PredAgedTCHO$mean, col="red", ylim=c(5474429602, 7054008367),
     main = "SARIMA expliqué par 'Aged' et le taux de chômage vs Vraies valeurs")
lines(MSEAnnTest)

#PIB & SMIC
SARIMAPIBSMIC <- auto.arima(MSEAnnTrain, xreg = cbind(PIBAnnTrain, SMICAnnTrain))
PredPIBSMIC <- forecast(SARIMAPIBSMIC, xreg = cbind(PIBAnnTest[1:28], SMICAnnTest[1:28]))
plot(PredPIBSMIC$mean, col="red", ylim=c(5474429602, 6916290440),
     main = "SARIMA expliqué par le PIB et le SMIC vs Vraies valeurs")
lines(MSEAnnTest)

#PIB & TCHO
SARIMAPIBTCHO <- auto.arima(MSEAnnTrain, xreg = cbind(PIBAnnTrain, TCHOAnnTrain))
PredPIBTCHO <- forecast(SARIMAPIBTCHO, xreg = cbind(PIBAnnTest[1:28], TCHOAnnTest[1:28]))
plot(PredPIBTCHO$mean, col="red", ylim=c(4568330820, 5773709052),
     main = "SARIMA expliqué par le PIB et le taux de chômage vs Vraies valeurs")
lines(MSEAnnTest)

#SMIC & TCHO
SARIMASMICTCHO <- auto.arima(MSEAnnTrain, xreg = cbind(SMICAnnTrain, TCHOAnnTrain))
PredSMICTCHO <- forecast(SARIMASMICTCHO, xreg = cbind(SMICAnnTest[1:28], TCHOAnnTest[1:28]))
plot(PredSMICTCHO$mean, col="red", ylim=c(5474429602, 6499886903),
     main = "SARIMA expliqué par le SMIC et le taux de chômage vs Vraies valeurs")
lines(MSEAnnTest)

plot(MSEAnnTest, main="Comparaison des modèles avec 2 variables", ylim=c(4273400654, 7054008367))
lines(PredAgedPIB$mean, col="red")
lines(PredAgedSMIC$mean, col="blue")
lines(PredAgedTCHO$mean, col="green")
lines(PredPIBSMIC$mean, col="gold")
lines(PredPIBTCHO$mean, col="purple")
lines(PredSMICTCHO$mean, col="brown")
legend('topleft', 
       legend = c('Série MSE', 'Aged & PIB', 'Aged & SMIC', 'Aged & Taux chômage',
                  'PIB & SMIC', 'PIB & Taux chômage', 'SMIC & Taux chômage'),
       col=c('black', 'red', 'blue', 'green', 'gold', 'purple', 'brown'), lty=1, cex=0.8)

## 3 VARIABLES

#Aged & PIB & SMIC
SARIMAAgedPIBSMIC <- auto.arima(MSEAnnTrain, xreg = cbind(AGEDAnnTrain, PIBAnnTrain, SMICAnnTrain))
PredAgedPIBSMIC <- forecast(SARIMAAgedPIBSMIC, xreg = cbind(AGEDAnnTest[1:28], PIBAnnTest[1:28], SMICAnnTest[1:28]))
plot(PredAgedPIBSMIC$mean, col="red", ylim=c(5474429602, 6851478773),
     main = "Lissage exponentiel expliqué par 'Aged', le PIB et le SMIC vs Vraies valeurs")
lines(MSEAnnTest)

#Aged & PIB & TCHO
SARIMAAgedPIBTCHO <- auto.arima(MSEAnnTrain, xreg = cbind(AGEDAnnTrain, PIBAnnTrain, TCHOAnnTrain))
PredAgedPIBTCHO <- forecast(SARIMAAgedPIBTCHO, xreg = cbind(AGEDAnnTest[1:28], PIBAnnTest[1:28], TCHOAnnTest[1:28]))
plot(PredAgedPIBTCHO$mean, col="red", ylim=c(5108085624, 5849552197),
     main = "Lissage exponentiel expliqué par 'Aged', le PIB et le taux de chômage vs Vraies valeurs")
lines(MSEAnnTest)

#Aged & SMIC & TCHO
SARIMAAgedSMICTCHO <- auto.arima(MSEAnnTrain, xreg = cbind(AGEDAnnTrain, SMICAnnTrain, TCHOAnnTrain))
PredAgedSMICTCHO <- forecast(SARIMAAgedSMICTCHO, xreg = cbind(AGEDAnnTest[1:28], SMICAnnTest[1:28], TCHOAnnTest[1:28]))
plot(PredAgedSMICTCHO$mean, col="red", ylim=c(5474429602, 6796461684),
     main = "Lissage exponentiel expliqué par 'Aged', le SMIC et le taux de chômage vs Vraies valeurs")
lines(MSEAnnTest)

#PIB & SMIC & TCHO
SARIMAPIBSMICTCHO <- auto.arima(MSEAnnTrain, xreg = cbind(PIBAnnTrain, SMICAnnTrain, TCHOAnnTrain))
PredPIBSMICTCHO <- forecast(SARIMAPIBSMICTCHO, xreg = cbind(PIBAnnTest[1:28], SMICAnnTest[1:28], TCHOAnnTest[1:28]))
plot(PredPIBSMICTCHO$mean, col="red", ylim=c(5474429602, 6748974133),
     main = "Lissage exponentiel expliqué par le PIB, le SMIC et le taux de chômage vs Vraies valeurs")
lines(MSEAnnTest)

plot(MSEAnnTest, main="Comparaison des modèles avec 3 variables", ylim=c(5108085624, 6851478773))
lines(PredAgedPIBSMIC$mean, col="blue")
lines(PredAgedPIBTCHO$mean, col="green")
lines(PredAgedSMICTCHO$mean, col="red")
lines(PredPIBSMICTCHO$mean, col="gold")
legend('topleft', legend = c('Série MSE', 'Aged & PIB & SMIC', 'Aged & PIB & Taux chômage',
                             'Aged & SMIC & Taux chômage', 'PIB & SMIC & Taux chômage'),
       col=c('black', 'blue', 'green', 'red', 'gold'), lty=1, cex=0.8)

## 4 VARIABLES

SARIMACOMPLET <- auto.arima(MSEAnnTrain, xreg = cbind(PIBAnnTrain, SMICAnnTrain, TCHOAnnTrain, AGEDAnnTrain))
PredCOMPLET <- forecast(SARIMACOMPLET, xreg = cbind(PIBAnnTest[1:2], SMICAnnTest[1:2], TCHOAnnTest[1:2], AGEDAnn[1:2]))
plot(PredCOMPLET$mean, col="red", ylim=c(5474429602, 6245405741),
     main = "Lissage exponentiel expliqué par le PIB, le SMIC et le taux de chômage vs Vraies valeurs")
lines(MSEAnnTest)

plot(MSEAnnTest, main="Comparaison du modèle complet", ylim=c(5474429602, 6245405741))
lines(SARIMACOMPLET$mean, col="blue")
legend('topleft', legend = c('Série MSE', 'Modèle complet'),
       col=c('black', 'blue'), lty=1, cex=0.8)

#Comparaison des 4 modèles

plot(MSEAnnTest, main="Comparaison des 4 modèles", ylim=c(5108085624, 7158992946))
lines(ARIMAMSEAnnTest$mean, col='brown')
lines(PredPIB$mean, col="blue")
lines(PredSMICTCHO$mean, col="green")
lines(PredAgedPIBTCHO$mean, col="red")
lines(PredCOMPLET$mean, col="gold")
legend('topleft', legend = c('Série MSE', 'Pas de variable explicative', 'PIB', 'SMIC + Taux chômage',
                             'Aged + PIB + Taux chômage', 'Complet'),
       col=c('black', 'brown', 'blue', 'green', 'red', 'gold'), lty=1, cex=0.8)
