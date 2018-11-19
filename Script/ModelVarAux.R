#Ce script correspond à la modélisation des variables MSE en fonction des autres séries temporelles


library(smooth)
###MSE annuelle
##Lissage Exponentiel

## 1 VARIABLE

#Aged
LEAged <- es(MSEAnn, model = "CCC", xreg = AGEDAnn[1:28], h = 5, holdout = T)
plot(LEAged$forecast, col="red", ylim=c(5474429602, 6818282513),
     main = "Lissage exponentiel expliqué par 'Aged' vs Vraies valeurs")
lines(MSEAnnTest)

#PIB
LEPIB <- es(MSEAnn, model = "CCC", xreg = PIBAnn[1:28], h = 5, holdout = T)
plot(LEPIB$forecast, col="red", ylim=c(5474429602, 6213315012),
     main = "Lissage exponentiel expliqué par le PIB vs Vraies valeurs")
lines(MSEAnnTest)

#SMIC
LESMIC <- es(MSEAnn, model = "CCC", xreg = SMICAnn[1:28], h = 5, holdout = T)
plot(LESMIC$forecast, col="red", ylim=c(5474429602, 6471879218),
     main = "Lissage exponentiel expliqué par le SMIC vs Vraies valeurs")
lines(MSEAnnTest)

#TCHO
LETCHO <- es(MSEAnn, model = "CCC", xreg = TCHOAnn[1:28], h = 5, holdout = T)
plot(LETCHO$forecast, col="red", ylim=c(5474429602, 6228255409),
     main = "Lissage exponentiel expliqué par le taux de chômage 
     vs Vraies valeurs")
lines(MSEAnnTest)

plot(MSEAnnTest, main="Comparaison des modèles avec 1 variable", ylim=c(5474429602, 6818282513))
lines(LEAged$forecast, col="blue")
lines(LEPIB$forecast, col="green")
lines(LESMIC$forecast, col="red")
lines(LETCHO$forecast, col="gold")
legend('topleft', legend = c('Série MSE', 'Aged', 'PIB', 'SMIC', 'Taux chômage'),
       col=c('black', 'blue', 'green', 'red', 'gold'), lty=1, cex=0.8)

## 2 VARIABLES

#Aged & PIB
LEAgedPIB <- es(MSEAnn, model = "CCC", xreg = cbind(AGEDAnn[1:28], PIBAnn[1:28]), h = 5, holdout = T)
plot(LEAgedPIB$forecast, col="red", ylim=c(5474429602, 5963269156),
     main = "Lissage exponentiel expliqué par 'Aged' et le PIB vs Vraies valeurs")
lines(MSEAnnTest)

#Aged & SMIC
LEAgedSMIC <- es(MSEAnn, model = "CCC", xreg = cbind(AGEDAnn[1:28], SMICAnn[1:28]), h = 5, holdout = T)
plot(LEAgedSMIC$forecast, col="red", ylim=c(5474429602, 5970508508),
     main = "Lissage exponentiel expliqué par 'Aged' et le SMIC vs Vraies valeurs")
lines(MSEAnnTest)

#Aged & TCHO
#WARNING INCONGRU
LEAgedTCHO <- es(MSEAnn, model = "CCC", xreg = cbind(AGEDAnn[1:28], TCHOAnn[1:28]), h = 5, holdout = T)
plot(LEAgedTCHO$forecast, col="red", ylim=c(5474429602, 5970508508),
     main = "Lissage exponentiel expliqué par 'Aged' et le taux de chômage vs Vraies valeurs")
lines(MSEAnnTest)

#PIB & SMIC
LEPIBSMIC <- es(MSEAnn, model = "CCC", xreg = cbind(PIBAnn[1:28], SMICAnn[1:28]), h = 5, holdout = T)
plot(LEPIBSMIC$forecast, col="red", ylim=c(5474429602, 6561846579),
     main = "Lissage exponentiel expliqué par le PIB et le SMIC vs Vraies valeurs")
lines(MSEAnnTest)

#PIB & TCHO
LEPIBTCHO <- es(MSEAnn, model = "CCC", xreg = cbind(PIBAnn[1:28], TCHOAnn[1:28]), h = 5, holdout = T)
plot(LEPIBTCHO$forecast, col="red", ylim=c(5474429602, 6235116423),
     main = "Lissage exponentiel expliqué par le PIB et le taux de chômage vs Vraies valeurs")
lines(MSEAnnTest)

#SMIC & TCHO
LESMICTCHO <- es(MSEAnn, model = "CCC", xreg = cbind(SMICAnn[1:28], TCHOAnn[1:28]), h = 5, holdout = T)
plot(LESMICTCHO$forecast, col="red", ylim=c(5474429602, 6499886903),
     main = "Lissage exponentiel expliqué par le SMIC et le taux de chômage vs Vraies valeurs")
lines(MSEAnnTest)

plot(MSEAnnTest, main="Comparaison des modèles avec 2 variables", ylim=c(5474429602, 6818282513))
lines(LEAgedPIB$forecast, col="red")
lines(LEAgedSMIC$forecast, col="blue")
lines(LEAgedTCHO$forecast, col="green")
lines(LEPIBSMIC$forecast, col="gold")
lines(LEPIBTCHO$forecast, col="purple")
lines(LESMICTCHO$forecast, col="brown")
legend('topleft', 
       legend = c('Série MSE', 'Aged & PIB', 'Aged & SMIC', 'Aged & Taux chômage',
                  'PIB & SMIC', 'PIB & Taux chômage', 'SMIC & Taux chômage'),
       col=c('black', 'red', 'blue', 'green', 'gold', 'purple', 'brown'), lty=1, cex=0.8)

## 3 VARIABLES

#Aged & PIB & SMIC
LEAgedPIBSMIC <- es(MSEAnn, model = "CCC", xreg = cbind(AGEDAnn[1:28], PIBAnn[1:28], SMICAnn[1:28]), h = 5, holdout = T)
plot(LEAgedPIBSMIC$forecast, col="red", ylim=c(5474429602, 6045581604),
     main = "Lissage exponentiel expliqué par 'Aged', le PIB et le SMIC vs Vraies valeurs")
lines(MSEAnnTest)

#Aged & PIB & TCHO
LEAgedPIBTCHO <- es(MSEAnn, model = "CCC", xreg = cbind(AGEDAnn[1:28], PIBAnn[1:28], TCHOAnn[1:28]), h = 5, holdout = T)
plot(LEAgedPIBTCHO$forecast, col="red", ylim=c(5474429602, 5978025204),
     main = "Lissage exponentiel expliqué par 'Aged', le PIB et le taux de chômage vs Vraies valeurs")
lines(MSEAnnTest)

#Aged & SMIC & TCHO
LEAgedSMICTCHO <- es(MSEAnn, model = "CCC", xreg = cbind(AGEDAnn[1:28], SMICAnn[1:28], TCHOAnn[1:28]), h = 5, holdout = T)
plot(LEAgedSMICTCHO$forecast, col="red", ylim=c(5474429602, 5999275427),
     main = "Lissage exponentiel expliqué par 'Aged', le SMIC et le taux de chômage vs Vraies valeurs")
lines(MSEAnnTest)

#PIB & SMIC & TCHO
LEPIBSMICTCHO <- es(MSEAnn, model = "CCC", xreg = cbind(PIBAnn[1:28], SMICAnn[1:28], TCHOAnn[1:28]), h = 5, holdout = T)
plot(LEPIBSMICTCHO$forecast, col="red", ylim=c(5474429602, 6543671066),
     main = "Lissage exponentiel expliqué par le PIB, le SMIC et le taux de chômage vs Vraies valeurs")
lines(MSEAnnTest)

plot(MSEAnnTest, main="Comparaison des modèles avec 3 variables", ylim=c(5474429602, 6818282513))
lines(LEAgedPIBSMIC$forecast, col="blue")
lines(LEAgedPIBTCHO$forecast, col="green")
lines(LEAgedSMICTCHO$forecast, col="red")
lines(LEPIBSMICTCHO$forecast, col="gold")
legend('topleft', legend = c('Série MSE', 'Aged & PIB & SMIC', 'Aged & PIB & Taux chômage',
                             'Aged & SMIC & Taux chômage', 'PIB & SMIC & Taux chômage'),
       col=c('black', 'blue', 'green', 'red', 'gold'), lty=1, cex=0.8)

## 4 VARIABLES

LECOMPLET <- es(MSEAnn, model = "CCC", xreg = cbind(PIBAnn[1:28], SMICAnn[1:28], TCHOAnn[1:28], AGEDAnn[1:28]), h = 5, holdout = T)
plot(LECOMPLET$forecast, col="red", ylim=c(5474429602, 6245405741),
     main = "Lissage exponentiel expliqué par le PIB, le SMIC et le taux de chômage vs Vraies valeurs")
lines(MSEAnnTest)

plot(MSEAnnTest, main="Comparaison du modèle complet", ylim=c(5474429602, 6245405741))
lines(LECOMPLET$forecast, col="blue")
legend('topleft', legend = c('Série MSE', 'Modèle complet'),
       col=c('black', 'blue'), lty=1, cex=0.8)

#Comparaison des 4 modèles

plot(MSEAnnTest, main="Comparaison des 4 modèles", ylim=c(5474429602, 6818282513))
lines(MSEAnnPred$mean, col='brown')
lines(LEPIB$forecast, col="blue")
lines(LEAgedTCHO$forecast, col="green")
lines(LEAgedPIBTCHO$forecast, col="red")
lines(LECOMPLET$forecast, col="gold")
legend('topleft', legend = c('Série MSE', 'Pas de variable explicative', 'PIB', 'Aged + Taux chômage',
                             'Aged + PIB + Taux chômage', 'Complet'),
       col=c('black', 'brown', 'blue', 'green', 'red', 'gold'), lty=1, cex=0.8)

##SARIMA

# 1 VARIABLE

#Aged
SARIMAAged <- auto.arima(MSEAnnTrain, stationary = F, xreg = AGEDAnnTrain)
PredAged <- forecast(SARIMAAged, xreg = AGEDAnnTest[1:5])
plot(PredAged$mean, col="red", ylim=c(5474429602, 7037960127),
     main = "SARIMA expliqué par 'Aged' vs Vraies valeurs")
lines(MSEAnnTest)

#PIB
SARIMAPIB <- auto.arima(MSEAnnTrain, stationary = F, xreg = PIBAnnTrain)
PredPIB <- forecast(SARIMAPIB, xreg = PIBAnnTest[1:5])
plot(PredPIB$mean, col="red", ylim=c(5474429602, 6712539781),
     main = "SARIMA expliqué par le PIB vs Vraies valeurs")
lines(MSEAnnTest)

#SMIC
SARIMASMIC <- auto.arima(MSEAnnTrain, stationary = F, xreg = SMICAnnTrain)
PredSMIC <- forecast(SARIMASMIC, xreg = SMICAnnTest[1:5])
plot(PredSMIC$mean, col="red", ylim=c(5474429602, 6928283434),
     main = "SARIMA expliqué par le SMIC vs Vraies valeurs")
lines(MSEAnnTest)

#TCHO
SARIMATCHO <- auto.arima(MSEAnnTrain, stationary = F, xreg = TCHOAnnTrain)
PredTCHO <- forecast(SARIMATCHO, xreg = TCHOAnnTest[1:5])
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
PredAgedPIB <- forecast(SARIMAAgedPIB, xreg = cbind(AGEDAnnTest[1:5], PIBAnnTest[1:5]))
plot(PredAgedPIB$mean, col="red", ylim=c(4273400654, 5963269156),
     main = "SARIMA expliqué par 'Aged' et le PIB vs Vraies valeurs")
lines(MSEAnnTest)

#Aged & SMIC
SARIMAAgedSMIC <- auto.arima(MSEAnnTrain, xreg = cbind(AGEDAnnTrain, SMICAnnTrain))
PredAgedSMIC <- forecast(SARIMAAgedSMIC, xreg = cbind(AGEDAnnTest[1:5], SMICAnnTest[1:5]))
plot(PredAgedSMIC$mean, col="red", ylim=c(5474429602, 6889864072),
     main = "SARIMA expliqué par 'Aged' et le SMIC vs Vraies valeurs")
lines(MSEAnnTest)

#Aged & TCHO
SARIMAAgedTCHO <- auto.arima(MSEAnnTrain, xreg = cbind(AGEDAnnTrain, TCHOAnnTrain))
PredAgedTCHO <- forecast(SARIMAAgedTCHO, xreg = cbind(AGEDAnnTest[1:5], TCHOAnnTest[1:5]))
plot(PredAgedTCHO$mean, col="red", ylim=c(5474429602, 7054008367),
     main = "SARIMA expliqué par 'Aged' et le taux de chômage vs Vraies valeurs")
lines(MSEAnnTest)

#PIB & SMIC
SARIMAPIBSMIC <- auto.arima(MSEAnnTrain, xreg = cbind(PIBAnnTrain, SMICAnnTrain))
PredPIBSMIC <- forecast(SARIMAPIBSMIC, xreg = cbind(PIBAnnTest[1:5], SMICAnnTest[1:5]))
plot(PredPIBSMIC$mean, col="red", ylim=c(5474429602, 6916290440),
     main = "SARIMA expliqué par le PIB et le SMIC vs Vraies valeurs")
lines(MSEAnnTest)

#PIB & TCHO
SARIMAPIBTCHO <- auto.arima(MSEAnnTrain, xreg = cbind(PIBAnnTrain, TCHOAnnTrain))
PredPIBTCHO <- forecast(SARIMAPIBTCHO, xreg = cbind(PIBAnnTest[1:5], TCHOAnnTest[1:5]))
plot(PredPIBTCHO$mean, col="red", ylim=c(4568330820, 5773709052),
     main = "SARIMA expliqué par le PIB et le taux de chômage vs Vraies valeurs")
lines(MSEAnnTest)

#SMIC & TCHO
SARIMASMICTCHO <- auto.arima(MSEAnnTrain, xreg = cbind(SMICAnnTrain, TCHOAnnTrain))
PredSMICTCHO <- forecast(SARIMASMICTCHO, xreg = cbind(SMICAnnTest[1:5], TCHOAnnTest[1:5]))
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
PredAgedPIBSMIC <- forecast(SARIMAAgedPIBSMIC, xreg = cbind(AGEDAnnTest[1:5], PIBAnnTest[1:5], SMICAnnTest[1:5]))
plot(PredAgedPIBSMIC$mean, col="red", ylim=c(5474429602, 6851478773),
     main = "Lissage exponentiel expliqué par 'Aged', le PIB et le SMIC vs Vraies valeurs")
lines(MSEAnnTest)

#Aged & PIB & TCHO
SARIMAAgedPIBTCHO <- auto.arima(MSEAnnTrain, xreg = cbind(AGEDAnnTrain, PIBAnnTrain, TCHOAnnTrain))
PredAgedPIBTCHO <- forecast(SARIMAAgedPIBTCHO, xreg = cbind(AGEDAnnTest[1:5], PIBAnnTest[1:5], TCHOAnnTest[1:5]))
plot(PredAgedPIBTCHO$mean, col="red", ylim=c(5108085624, 5849552197),
     main = "Lissage exponentiel expliqué par 'Aged', le PIB et le taux de chômage vs Vraies valeurs")
lines(MSEAnnTest)

#Aged & SMIC & TCHO
SARIMAAgedSMICTCHO <- auto.arima(MSEAnnTrain, xreg = cbind(AGEDAnnTrain, SMICAnnTrain, TCHOAnnTrain))
PredAgedSMICTCHO <- forecast(SARIMAAgedSMICTCHO, xreg = cbind(AGEDAnnTest[1:5], SMICAnnTest[1:5], TCHOAnnTest[1:5]))
plot(PredAgedSMICTCHO$mean, col="red", ylim=c(5474429602, 6796461684),
     main = "Lissage exponentiel expliqué par 'Aged', le SMIC et le taux de chômage vs Vraies valeurs")
lines(MSEAnnTest)

#PIB & SMIC & TCHO
SARIMAPIBSMICTCHO <- auto.arima(MSEAnnTrain, xreg = cbind(PIBAnnTrain, SMICAnnTrain, TCHOAnnTrain))
PredPIBSMICTCHO <- forecast(SARIMAPIBSMICTCHO, xreg = cbind(PIBAnnTest[1:5], SMICAnnTest[1:5], TCHOAnnTest[1:5]))
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
PredCOMPLET <- forecast(SARIMACOMPLET, xreg = cbind(PIBAnnTest[1:5], SMICAnnTest[1:5], TCHOAnnTest[1:5], AGEDAnn[1:5]))
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
