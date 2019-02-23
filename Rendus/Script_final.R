require(tseries)
require(forecast)
require(corrplot)
require(fUnitRoots)
require(vars)


########################### Importation des données  #############################################
#trim <- read.csv("~/PFE_Time_Series/Data/Data_Trim.csv", sep=";", dec=",")
trim <- read.csv("~/Cygwin/app/home/Jules/PFE_Time_Series/Data/Data_Trim.csv", sep=";", dec=",")
##################################################################################################

########################### Analyse descriptive des séries #######################################

#MSE
MSE <- ts(trim$MSE, start = 1990, end = c(2017, 2), frequency=4)
plot(MSE, main="Evolution trimestrielle de la masse salariale", xaxt="n", cex.main=0.9)
axis(side=1, at=seq(1990,2015,5), labels=c("1990Q1", "1995Q1", "2000Q1", "2005Q1", "2010Q1", "2015Q1"))
par(mfrow=c(1,2), cex.main=0.8)
acf(MSE, main="Auto-corrélation de la masse salariale trimestrielle", lag.max=20)
pacf(MSE, main="Autocorrélation partielle de la masse salariale trimestrielle", lag.max=20)
kpss.test(MSE)
adf.test(MSE)

#PIB
PIB <- ts(trim$PIB, start = 1990, end = c(2017, 1), frequency=4)
plot(PIB, main="Evolution trimestrielle du PIB", xaxt="n", cex.main=0.9)
axis(side=1, at=seq(1990,2015,5), labels=c("1990Q1", "1995Q1", "2000Q1", "2005Q1", "2010Q1", "2015Q1"))
par(mfrow=c(1,2), cex.main=0.8)
acf(PIB, main="Auto-corrélation du PIB trimestriel", lag.max=40)
pacf(PIB, main="Autocorrélation partielle du PIB trimestriel", lag.max=40)
par(mfrow=c(1,1))
kpss.test(PIB)
adf.test(PIB)

#SMIC
SMIC <- ts(trim$SMIC, start = c(1990,1), end = c(2017, 4), frequency = 4)
plot(SMIC, main="Evolution trimestrielle du SMIC", xaxt="n", cex.main=0.9)
axis(side=1, at=seq(1990,2015,5), labels=c("1990Q1", "1995Q1", "2000Q1", "2005Q1", "2010Q1", "2015Q1"))
par(mfrow=c(1,2), cex.main=0.8)
acf(SMIC, main="Auto-corrélation du SMIC trimestriel", lag.max=20)
pacf(SMIC, main="Autocorrélation partielle du SMIC trimestriel", lag.max=20)
par(mfrow=c(1,1))
kpss.test(SMIC)
adf.test(SMIC)

#TCHOF
TCHOF <- ts(trim$TCHOF, start = c(1990,1), end = c(2017, 4), frequency = 4)
plot(TCHOF, main="Evolution trimestrielle du taux de chômage des femmes", xaxt="n", cex.main=0.9)
axis(side=1, at=seq(1990,2015,5), labels=c("1990Q1", "1995Q1", "2000Q1", "2005Q1", "2010Q1", "2015Q1"))
par(mfrow=c(1,2), cex.main=0.8)
acf(TCHOF, main="Auto-corrélation du taux de chômage des femmes trimestriel", lag.max=20)
pacf(TCHOF, main="Autocorrélation partielle du taux de chômage des femmes trimestriel", lag.max=20)
par(mfrow=c(1,1))
kpss.test(TCHOF)
adf.test(TCHOF)

#Corrélations

#Matrice des corrélations
corrplot(cor(trim[1:109,-1]), method = "number", type="lower",
         p.mat=cor.mtest(trim[1:109,-1], 0.95)[[1]], insig="pch",
         col=colorRampPalette(c("blue", "light blue", "red"))(50))
#P-values associées aux coefficients
corr <- cor.mtest(trim[1:109,-1], 0.95)[[1]]
rownames(corr) <- c("MSE","PIB","SMIC","TCHOF")
colnames(corr) <- c("MSE","PIB","SMIC","TCHOF")
corr

#Découpage des séries en échantillons d'apprentissage et de test
MSETrain <- window(MSE, start=1990, end=c(2015,4))
MSETest <- window(MSE, start=2016, end=c(2017,2))
PIBTrain <- window(PIB, start=1990, end=c(2015,4))
PIBTest <- window(PIB, start=2016, end=c(2017,1))
SMICTrain <- window(SMIC, start=1990, end=c(2015,4))
SMICTest <- window(SMIC, start=2016, end=c(2017,2))
TCHOFTrain <- window(TCHOF, start=1990, end=c(2015,4))
TCHOFTest <- window(TCHOF, start=2016, end=c(2017,2))

##################################################################################################

############################## Modélisation individuelle #########################################

##Lissage exponentiel

#MSE
LEMSE<-ets(MSETrain, "ZAM")
print(LEMSE)
PredLEMSE <- forecast(LEMSE, h = 6)
plot(MSETest, main="Comparaison entre la prédiction du lissage exponentiel et les valeurs réelles pour la masse salariale trimestrielle")
lines(PredLEMSE$mean, col="red")
EQM(MSETest, PredLEMSE$mean)

#PIB
LEPIB<-ets(PIBTrain, "ZAN")
print(LEPIB)
PredLEPIB <- forecast(LEPIB, h = 5)
plot(PIBTest, ylim=c(min(PIBTest,PredLEPIB$mean),max(PIBTest,PredLEPIB$mean)), main= "Comparaison entre la prédiction du lissage exponentiel et les valeurs réelles pour le PIB trimestriel", cex.main=0.8)
lines(PredLEPIB$mean, col="red")
EQM(PIBTest, PredLEPIB$mean)

#SMIC
LESMIC<-ets(SMICTrain, "ZAA")
print(LESMIC)
PredLESMIC <- forecast(LESMIC, h = 6)
plot(SMICTest, ylim=c(min(SMICTest,PredLESMIC$mean),max(SMICTest,PredLESMIC$mean)), main="Comparaison entre la prédiction du lissage exponentiel et les valeurs réelles pour le SMIC trimestriel", cex.main=0.8)
lines(PredLESMIC$mean, col="red")
EQM(SMICTest, PredLESMIC$mean)

#Taux de chômage des femmes
LETCHOF<-ets(TCHOFTrain, "ZNN")
print(LETCHOF)
PredLETCHOF <- forecast(LETCHOF, h = 6)
plot(TCHOFTest, ylim=c(min(TCHOFTest,PredLETCHOF$mean),max(TCHOFTest,PredLETCHOF$mean)), main="Comparaison entre la prédiction du lissage exponentiel et les valeurs réelles pour le taux de chômage trimestriel", cex.main=0.8)
lines(PredLETCHOF$mean, col="red")
EQM(TCHOFTest, PredLETCHOF$mean)

#Modèles ARMA

#MSE
ARIMAMSE<-auto.arima(MSETrain, ic="aicc")
print(ARIMAMSE)
PredARIMAMSE<- forecast(ARIMAMSE, h=6)
plot(MSETest, main="Comparaison entre le modèle SARIMA et les données de validation pour la masse salariale trimestrielle")
lines(PredARIMAMSE$mean, col="red")
EQM(MSETest, PredARIMAMSE$mean)

#PIB
ARIMAPIB<-auto.arima(PIBTrain, ic="aicc", seasonal=F)
print(ARIMAPIB)
PredARIMAPIB<- forecast(ARIMAPIB, h=5)
plot(PIBTest, ylim=c(min(PIBTest,PredARIMAPIB$mean),max(PIBTest,PredARIMAPIB$mean)), 
     main="Comparaison entre le modèle SARIMA et les données de
     validation pour le PIB trimestriel", cex.main=0.8)
lines(PredARIMAPIB$mean, col="red")
EQM(PIBTest, PredARIMAPIB$mean)

#SMIC
ARIMASMIC<-auto.arima(SMICTrain, ic="aicc")
print(ARIMASMIC)
PredARIMASMIC<- forecast(ARIMASMIC, h=6)
plot(SMICTest, ylim=c(min(SMICTest,PredARIMASMIC$mean),max(SMICTest,PredARIMASMIC$mean)), 
     main="Comparaison entre le modèle SARIMA et les données de
    validation pour le SMIC trimestriel", cex.main=0.8)
lines(PredARIMASMIC$mean, col="red")
EQM(SMICTest, PredARIMASMIC$mean)

#TCHOF
ARIMATCHOF<-auto.arima(TCHOFTrain, ic="aicc", seasonal=F)
print(ARIMATCHOF)
PredARIMATCHOF<- forecast(ARIMATCHOF, h=6)
plot(TCHOFTest, main="Comparaison entre le modèle SARIMA et les données de
    validation pour le taux de chômage des femmes trimestriel", cex.main=0.8)
lines(PredARIMATCHOF$mean, col="red")
EQM(TCHOFTest, PredARIMATCHOF$mean)

#Comparaison des résultats
resultats<-matrix(nrow=4, ncol=2, dimnames = list(c("MSE", "PIB", "SMIC", "TCHOF"), 
                                                  c("lissage", "ARMA")))
resultats[1,1] = EQM(MSETest, PredLEMSE$mean)
resultats[2,1] = EQM(PIBTest, PredLEPIB$mean)
resultats[3,1] = EQM(SMICTest, PredLESMIC$mean)
resultats[4,1] = EQM(TCHOFTest, PredLETCHOF$mean)
resultats[1,2] = EQM(MSETest, PredARIMAMSE$mean)
resultats[2,2] = EQM(PIBTest, PredARIMAPIB$mean)
resultats[3,2] = EQM(SMICTest, PredARIMASMIC$mean)
resultats[4,2] = EQM(TCHOFTest, PredARIMATCHOF$mean)
resultats

#Prédiction de la valeur du PIB pour 2017Q2
PredARIMAPIB<- forecast(ARIMAPIB, h=6)
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

################################ Stationnarisation des séries ####################################

#MSE
par(cex.main=0.8)
plot(decompose(MSETrain, "multiplicative"))
MSESta <- na.omit(decompose(MSETrain, "multiplicative")$random)
MSETrend<-window(decompose(MSETrain, "multiplicative")$trend)
MSESeasonal<-window(decompose(MSETrain, "multiplicative")$seasonal)
plot(MSESta, main="Masse salariale trimestrielle stationnarisée", xaxt="n")
axis(side=1, at=seq(1990,2015,5), labels=c("1990Q1", "1995Q1", "2000Q1", "2005Q1", "2010Q1", "2015Q1"))
par(mfrow=c(1,2), cex.main=0.8)
acf(MSESta, main="ACF de la masse salariale stationnarisée")
pacf(MSESta, main="PACF de la masse salariale stationnarisée")
par(mfrow=c(1,1))
kpss.test(MSESta)
adf.test(MSESta)
MSETrendTest <- window(forecast(na.omit(MSETrend), h=8)$mean, start=2016)
MSESeasonalTest<-ts(c(MSESeasonal[1:4],MSESeasonal[1:2]), start=2016,frequency=4)

#PIB
PIBSta <- na.omit(decompose(PIBTrain, "multiplicative")$random)
plot(PIBSta, main="PIB trimestriel stationnarisé", xaxt="n")
axis(side=1, at=seq(1990,2015,5), labels=c("1990Q1", "1995Q1", "2000Q1", "2005Q1", "2010Q1", "2015Q1"))
par(mfrow=c(1,2))
acf(PIBSta, main="Auto-Corrélation du PIB trimestriel stationnarisé")
pacf(PIBSta, main="Auto-Corrélation partielle du PIB trimestriel stationnarisé")
par(mfrow=c(1,1))
kpss.test(PIBSta)
adf.test(PIBSta)

#SMIC
SMICSta <- na.omit(decompose(SMICTrain)$random)
plot(SMICSta, main="SMIC trimestriel stationnarisé", xaxt="n")
axis(side=1, at=seq(1990,2015,5), labels=c("1990Q1", "1995Q1", "2000Q1", "2005Q1", "2010Q1", "2015Q1"))
par(mfrow=c(1,2))
acf(SMICSta, main="Auto-Corrélation du SMIC trimestriellstationnarisé")
pacf(SMICSta, main="Auto-Corrélation partielle du SMIC trimestriel stationnarisé")
par(mfrow=c(1,1))
kpss.test(SMICSta)
adf.test(SMICSta)

#TCHOF
TCHOFSta <- na.omit(decompose(TCHOFTrain)$random)
plot(TCHOFSta, main="Taux de chômage trimestriel des femmes stationnarisé", xaxt="n")
axis(side=1, at=seq(1990,2015,5), labels=c("1990Q1", "1995Q1", "2000Q1", "2005Q1", "2010Q1", "2015Q1"))
par(mfrow=c(1,2))
acf(TCHOFSta, main="Auto-Corrélation du Taux de chômage des femmes trimestrielle stationnarisé")
pacf(TCHOFSta, main="Auto-Corrélation partielle du Taux de chômage des femmes trimestriel stationnarisé")
par(mfrow=c(1,1))
kpss.test(TCHOFSta)
adf.test(TCHOFSta)

##################################################################################################

#################################### Modèles VAR avec vars #######################################
detach("package:MTS", unload=TRUE)

#PIB
VARselect(cbind(MSESta, PIBSta), lag.max=10)

#Ordre 4
modele<-VAR(cbind(MSESta, PIBSta), p=4, type="const")
stabilityvars(modele)
archTest(modele)
normality.test(modele)$jb.mul$JB
calculC0(modele)
Portmanteau(modele)

#Ordre3
modele2<-VAR(cbind(MSESta, PIBSta), p=3, type="const")
stabilityvars(modele2)
archTest(modele2)
normality.test(modele2)$jb.mul$JB
calculC0(modele2)
Portmanteau(modele2)

#Ordre 2
modele3<-VAR(cbind(MSESta, PIBSta), p=2, type="const")
stabilityvars(modele3)
archTest(modele3)
normality.test(modele3)$jb.mul$JB
calculC0(modele3)
Portmanteau(modele3)

PredPIB<-forecast(modele, h=8)$forecast$MSESta$mean
EQM(MSETest, window(PredPIB*MSETrendTest*MSESeasonalTest, start=2016, end=c(2017,2)))
PredPIB2<-forecast(modele2, h=8)$forecast$MSESta$mean
EQM(MSETest, window(PredPIB2*MSETrendTest*MSESeasonalTest, start=2016, end=c(2017,2)))
PredPIB3<-forecast(modele3, h=8)$forecast$MSESta$mean
EQM(MSETest, window(PredPIB3*MSETrendTest*MSESeasonalTest, start=2016, end=c(2017,2)))


plot(MSETest, xlim=c(2016,2017.25), main="Différences entre les véritables
     valeurs et les prédictions du modèle pour la masse salariale", xaxt="n")
axis(side=1, at=seq(2016,2017.25,0.25), labels=c("2016Q1", "2016Q2", "2016Q3", "2016Q4", "2017Q1", "2017Q2"))
lines(window(PredPIB, start=2016, end=c(2017,2))*MSETrendTest*MSESeasonalTest, col = "red")

#SMIC
VARselect(cbind(MSESta, SMICSta), lag.max=10)

#Ordre 3
modele<-VAR(cbind(MSESta, SMICSta), p=3, type="const")
stabilityvars(modele)
archTest(modele)
normality.test(modele)$jb.mul$JB
calculC0(modele)
Portmanteau(modele)

PredSMIC<-forecast(modele, h=8)$forecast$MSESta$mean
EQM(MSETest, window(PredCOMPLET*MSETrendTest*MSESeasonalTest, start=2016, end=c(2017,2)))


plot(MSETest, xlim=c(2016,2017.25), main="Différences entre les véritables
     valeurs et les prédictions du modèle pour la masse salariale", xaxt="n")
axis(side=1, at=seq(2016,2017.25,0.25), labels=c("2016Q1", "2016Q2", "2016Q3", "2016Q4", "2017Q1", "2017Q2"))
lines(window(PredSMIC, start=2016, end=c(2017,2))*MSETrendTest*MSESeasonalTest, col = "red")

#TCHOF
VARselect(cbind(MSESta, TCHOFSta), lag.max=10)

#Ordre 3
modele<-VAR(cbind(MSESta, TCHOFSta), p=3, type="const")
stabilityvars(modele)
archTest(modele)
normality.test(modele)$jb.mul$JB
calculC0(modele)
Portmanteau(modele)

#Ordre 2
modele2<-VAR(cbind(MSESta, PIBSta), p=2, type="const")
stabilityvars(modele2)
archTest(modele2)
normality.test(modele2)$jb.mul$JB
calculC0(modele2)
Portmanteau(modele2)

PredTCHOF<-forecast(modele, h=8)$forecast$MSESta$mean
EQM(MSETest, window(PredTCHOF*MSETrendTest*MSESeasonalTest, start=2016, end=c(2017,2)))
PredTCHOF2<-forecast(modele2, h=8)$forecast$MSESta$mean
EQM(MSETest, window(PredTCHOF2*MSETrendTest*MSESeasonalTest, start=2016, end=c(2017,2)))


plot(MSETest, xlim=c(2016,2017.25), main="Différences entre les véritables
     valeurs et les prédictions du modèle pour la masse salariale", xaxt="n")
axis(side=1, at=seq(2016,2017.25,0.25), labels=c("2016Q1", "2016Q2", "2016Q3", "2016Q4", "2017Q1", "2017Q2"))
lines(window(PredTCHOF, start=2016, end=c(2017,2))*MSETrendTest*MSESeasonalTest, col = "red")

#PIB & SMIC
VARselect(cbind(MSESta, PIBSta, SMICSta), lag.max=10)

#Ordre 3
modele<-VAR(cbind(MSESta, PIBSta, SMICSta), p=3, type="const")
stabilityvars(modele)
archTest(modele)
normality.test(modele)$jb.mul$JB
calculC0(modele)
Portmanteau(modele)

PredPIBSMIC<-forecast(modele, h=8)$forecast$MSESta$mean
EQM(MSETest, window(PredPIBSMIC*MSETrendTest*MSESeasonalTest, start=2016, end=c(2017,2)))


plot(MSETest, xlim=c(2016,2017.25), main="Différences entre les véritables
     valeurs et les prédictions du modèle pour la masse salariale", xaxt="n")
axis(side=1, at=seq(2016,2017.25,0.25), labels=c("2016Q1", "2016Q2", "2016Q3", "2016Q4", "2017Q1", "2017Q2"))
lines(window(PredPIBSMIC, start=2016, end=c(2017,2))*MSETrendTest*MSESeasonalTest, col = "red")

#PIB & TCHOF
VARselect(cbind(MSESta, PIBSta, TCHOFSta), lag.max=10)

#Ordre 4
modele<-VAR(cbind(MSESta, PIBSta, TCHOFSta), p=4, type="const")
stabilityvars(modele)
archTest(modele)
normality.test(modele)$jb.mul$JB
calculC0(modele)
Portmanteau(modele)

#Ordre3
modele2<-VAR(cbind(MSESta, PIBSta, TCHOFSta), p=3, type="const")
stabilityvars(modele2)
archTest(modele2)
normality.test(modele2)$jb.mul$JB
calculC0(modele2)
Portmanteau(modele2)

PredPIBTCHOF<-forecast(modele, h=8)$forecast$MSESta$mean
EQM(MSETest, window(PredPIBTCHOF*MSETrendTest*MSESeasonalTest, start=2016, end=c(2017,2)))
PredPIBTCHOF2<-forecast(modele2, h=8)$forecast$MSESta$mean
EQM(MSETest, window(PredPIBTCHOF2*MSETrendTest*MSESeasonalTest, start=2016, end=c(2017,2)))


plot(MSETest, xlim=c(2016,2017.25), main="Différences entre les véritables
     valeurs et les prédictions du modèle pour la masse salariale", xaxt="n")
axis(side=1, at=seq(2016,2017.25,0.25), labels=c("2016Q1", "2016Q2", "2016Q3", "2016Q4", "2017Q1", "2017Q2"))
lines(window(PredPIBTCHOF, start=2016, end=c(2017,2))*MSETrendTest*MSESeasonalTest, col = "red")

#SMIC & TCHOF
VARselect(cbind(MSESta, SMICSta, TCHOFSta), lag.max=10)

#Ordre 4
modele<-VAR(cbind(MSESta, SMICSta, TCHOFSta), p=4, type="const")
stabilityvars(modele)
archTest(modele)
normality.test(modele)$jb.mul$JB
calculC0(modele)
Portmanteau(modele)

#Ordre3
modele2<-VAR(cbind(MSESta, SMICSta, TCHOFSta), p=3, type="const")
stabilityvars(modele2)
archTest(modele2)
normality.test(modele2)$jb.mul$JB
calculC0(modele2)
Portmanteau(modele2)

PredSMICTCHOF<-forecast(modele, h=8)$forecast$MSESta$mean
EQM(MSETest, window(PredSMICTCHOF*MSETrendTest*MSESeasonalTest, start=2016, end=c(2017,2)))
PredSMICTCHOF2<-forecast(modele2, h=8)$forecast$MSESta$mean
EQM(MSETest, window(PredSMICTCHOF2*MSETrendTest*MSESeasonalTest, start=2016, end=c(2017,2)))


plot(MSETest, xlim=c(2016,2017.25), main="Différences entre les véritables
     valeurs et les prédictions du modèle pour la masse salariale", xaxt="n")
axis(side=1, at=seq(2016,2017.25,0.25), labels=c("2016Q1", "2016Q2", "2016Q3", "2016Q4", "2017Q1", "2017Q2"))
lines(window(PredSMICTCHOF, start=2016, end=c(2017,2))*MSETrendTest*MSESeasonalTest, col = "red")

#Modèle complet
VARselect(cbind(MSESta, PIBSta, SMICSta, TCHOFSta), lag.max=10)

#Ordre 4
modele<-VAR(cbind(MSESta, PIBSta, SMICSta, TCHOFSta), p=4, type="const")
stabilityvars(modele)
archTest(modele)
normality.test(modele)$jb.mul$JB
calculC0(modele)
Portmanteau(modele)

#Ordre3
modele2<-VAR(cbind(MSESta, PIBSta, SMICSta, TCHOFSta), p=3, type="const")
stabilityvars(modele2)
archTest(modele2)
normality.test(modele2)$jb.mul$JB
calculC0(modele2)
Portmanteau(modele2)

#Ordre 2
modele3<-VAR(cbind(MSESta, PIBSta, SMICSta, TCHOFSta), p=2, type="const")
stabilityvars(modele3)
archTest(modele3)
normality.test(modele3)$jb.mul$JB
calculC0(modele3)
Portmanteau(modele3)

PredCOMPLET<-forecast(modele, h=8)$forecast$MSESta$mean
EQM(MSETest, window(PredCOMPLET*MSETrendTest*MSESeasonalTest, start=2016, end=c(2017,2)))
PredCOMPLET2<-forecast(modele2, h=8)$forecast$MSESta$mean
EQM(MSETest, window(PredCOMPLET2*MSETrendTest*MSESeasonalTest, start=2016, end=c(2017,2)))
PredCOMPLET3<-forecast(modele3, h=8)$forecast$MSESta$mean
EQM(MSETest, window(PredCOMPLET3*MSETrendTest*MSESeasonalTest, start=2016, end=c(2017,2)))


plot(MSETest, xlim=c(2016,2017.25), main="Différences entre les véritables
     valeurs et les prédictions du modèle pour la masse salariale", xaxt="n")
axis(side=1, at=seq(2016,2017.25,0.25), labels=c("2016Q1", "2016Q2", "2016Q3", "2016Q4", "2017Q1", "2017Q2"))
lines(window(PredCOMPLET, start=2016, end=c(2017,2))*MSETrendTest*MSESeasonalTest, col = "red")

#Comparaison des résultats
resultats<-matrix(nrow=7, ncol=1, dimnames = list(c("PIB", "SMIC", "TCHOF", "PIB & SMIC", "PIB & TCHOF", "SMIC & TCHOF", "PIB, SMIC & TCHOF"), 
                                                  c("EQM")))
resultats[1,1] = EQM(MSETest, window(PredPIB*MSETrendTest*MSESeasonalTest, start=2016, end=c(2017,2)))
resultats[2,1] = EQM(MSETest, window(PredSMIC*MSETrendTest*MSESeasonalTest, start=2016, end=c(2017,2)))
resultats[3,1] = EQM(MSETest, window(PredTCHOF*MSETrendTest*MSESeasonalTest, start=2016, end=c(2017,2)))
resultats[4,1] = EQM(MSETest, window(PredPIBSMIC*MSETrendTest*MSESeasonalTest, start=2016, end=c(2017,2)))
resultats[5,1] = EQM(MSETest, window(PredPIBTCHOF*MSETrendTest*MSESeasonalTest, start=2016, end=c(2017,2)))
resultats[6,1] = EQM(MSETest, window(PredSMICTCHOF*MSETrendTest*MSESeasonalTest, start=2016, end=c(2017,2)))
resultats[7,1] = EQM(MSETest, window(PredCOMPLET*MSETrendTest*MSESeasonalTest, start=2016, end=c(2017,2)))
resultats

##################################################################################################

#################################### Modèles VAR avec MTS #######################################

require(MTS)

#PIB
VARorder(cbind(MSESta, PIBSta), maxp = 10)

#Ordre 4
modele<-VAR(cbind(MSESta, PIBSta), p=4)
stabilityMTS(modele)
crossCorr(modele)
mq(modele$residuals, lag=10)

#Ordre 3
modele2<-VAR(cbind(MSESta, PIBSta), p=3)
stabilityMTS(modele2)
crossCorr(modele2)
mq(modele2$residuals, lag=10)

PredPIB<-window(ts(VARpred(modele, 8)$pred[,1], start=c(2015,3), frequency=4), start=2016)
EQM(MSETest, window(PredPIB*MSETrendTest*MSESeasonalTest, start=2016, end=c(2017,2)))
PredPIB2<-window(ts(VARpred(modele2, 8)$pred[,1], start=c(2015,3), frequency=4), start=2016)
EQM(MSETest, window(PredPIB2*MSETrendTest*MSESeasonalTest, start=2016, end=c(2017,2)))


plot(MSETest, xlim=c(2016,2017.25), main="Différences entre les véritables
     valeurs et les prédictions du modèle pour la masse salariale", xaxt="n")
axis(side=1, at=seq(2016,2017.25,0.25), labels=c("2016Q1", "2016Q2", "2016Q3", "2016Q4", "2017Q1", "2017Q2"))
lines(window(PredPIB, start=2016, end=c(2017,2))*MSETrendTest*MSESeasonalTest, col = "red")

#SMIC
VARorder(cbind(MSESta, SMICSta), maxp = 10)

#Ordre 3
modele<-VAR(cbind(MSESta, SMICSta), p=3)
stabilityMTS(modele)
crossCorr(modele)
mq(modele$residuals, lag=10)

PredSMIC<-window(ts(VARpred(modele, 8)$pred[,1], start=c(2015,3), frequency=4), start=2016)
EQM(MSETest, window(PredSMIC*MSETrendTest*MSESeasonalTest, start=2016, end=c(2017,2)))

plot(MSETest, xlim=c(2016,2017.25), main="Différences entre les véritables
     valeurs et les prédictions du modèle pour la masse salariale", xaxt="n")
axis(side=1, at=seq(2016,2017.25,0.25), labels=c("2016Q1", "2016Q2", "2016Q3", "2016Q4", "2017Q1", "2017Q2"))
lines(window(PredPIB, start=2016, end=c(2017,2))*MSETrendTest*MSESeasonalTest, col = "red")

#TCHOF
VARorder(cbind(MSESta, TCHOFSta), maxp = 10)

#Ordre 6
modele<-VAR(cbind(MSESta, TCHOFSta), p=6)
stabilityMTS(modele)
crossCorr(modele)
mq(modele$residuals, lag=10)

#Ordre 3
modele2<-VAR(cbind(MSESta, TCHOFSta), p=3)
stabilityMTS(modele2)
crossCorr(modele2)
mq(modele2$residuals, lag=10)

#Ordre 2
modele3<-VAR(cbind(MSESta, TCHOFSta), p=2)
stabilityMTS(modele3)
crossCorr(modele3)
mq(modele2$residuals, lag=10)

PredTCHOF<-window(ts(VARpred(modele, 8)$pred[,1], start=c(2015,3), frequency=4), start=2016)
EQM(MSETest, window(PredTCHOF*MSETrendTest*MSESeasonalTest, start=2016, end=c(2017,2)))
PredTCHOF2<-window(ts(VARpred(modele2, 8)$pred[,1], start=c(2015,3), frequency=4), start=2016)
EQM(MSETest, window(PredTCHOF2*MSETrendTest*MSESeasonalTest, start=2016, end=c(2017,2)))
PredTCHOF3<-window(ts(VARpred(modele3, 8)$pred[,1], start=c(2015,3), frequency=4), start=2016)
EQM(MSETest, window(PredTCHOF3*MSETrendTest*MSESeasonalTest, start=2016, end=c(2017,2)))

plot(MSETest, xlim=c(2016,2017.25), main="Différences entre les véritables
     valeurs et les prédictions du modèle pour la masse salariale", xaxt="n")
axis(side=1, at=seq(2016,2017.25,0.25), labels=c("2016Q1", "2016Q2", "2016Q3", "2016Q4", "2017Q1", "2017Q2"))
lines(window(PredTCHOF, start=2016, end=c(2017,2))*MSETrendTest*MSESeasonalTest, col = "red")

#PIB & SMIC
VARorder(cbind(MSESta, PIBSta, SMICSta), maxp = 10)

#Ordre 3
modele<-VAR(cbind(MSESta, PIBSta, SMICSta), p=3)
stabilityMTS(modele)
crossCorr(modele)
mq(modele$residuals, lag=10)

PredPIBSMIC<-window(ts(VARpred(modele, 8)$pred[,1], start=c(2015,3), frequency=4), start=2016)
EQM(MSETest, window(PredPIBSMIC*MSETrendTest*MSESeasonalTest, start=2016, end=c(2017,2)))


plot(MSETest, xlim=c(2016,2017.25), main="Différences entre les véritables
     valeurs et les prédictions du modèle pour la masse salariale", xaxt="n")
axis(side=1, at=seq(2016,2017.25,0.25), labels=c("2016Q1", "2016Q2", "2016Q3", "2016Q4", "2017Q1", "2017Q2"))
lines(window(PredPIBSMIC, start=2016, end=c(2017,2))*MSETrendTest*MSESeasonalTest, col = "red")

#PIB & TCHOF
VARorder(cbind(MSESta, PIBSta, TCHOFSta), maxp = 10)

#Ordre 4
modele<-VAR(cbind(MSESta, PIBSta, TCHOFSta), p=4)
stabilityMTS(modele)
crossCorr(modele)
mq(modele$residuals, lag=10)

#Ordre 3
modele2<-VAR(cbind(MSESta, PIBSta, TCHOFSta), p=3)
stabilityMTS(modele2)
crossCorr(modele2)
mq(modele2$residuals, lag=10)

#Ordre 3
modele3<-VAR(cbind(MSESta, PIBSta, TCHOFSta), p=2)
stabilityMTS(modele3)
crossCorr(modele3)
mq(modele3$residuals, lag=10)

PredPIBTCHOF<-window(ts(VARpred(modele, 8)$pred[,1], start=c(2015,3), frequency=4), start=2016)
EQM(MSETest, window(PredPIBTCHOF*MSETrendTest*MSESeasonalTest, start=2016, end=c(2017,2)))
PredPIBTCHOF2<-window(ts(VARpred(modele2, 8)$pred[,1], start=c(2015,3), frequency=4), start=2016)
EQM(MSETest, window(PredPIBTCHOF2*MSETrendTest*MSESeasonalTest, start=2016, end=c(2017,2)))
PredPIBTCHOF3<-window(ts(VARpred(modele3, 8)$pred[,1], start=c(2015,3), frequency=4), start=2016)
EQM(MSETest, window(PredPIBTCHOF3*MSETrendTest*MSESeasonalTest, start=2016, end=c(2017,2)))


plot(MSETest, xlim=c(2016,2017.25), main="Différences entre les véritables
     valeurs et les prédictions du modèle pour la masse salariale", xaxt="n")
axis(side=1, at=seq(2016,2017.25,0.25), labels=c("2016Q1", "2016Q2", "2016Q3", "2016Q4", "2017Q1", "2017Q2"))
lines(window(PredPIB, start=2016, end=c(2017,2))*MSETrendTest*MSESeasonalTest, col = "red")

#SMIC & TCHOF
VARorder(cbind(MSESta, SMICSta, TCHOFSta), maxp = 10)

#Ordre 4
modele<-VAR(cbind(MSESta, SMICSta, TCHOFSta), p=4)
stabilityMTS(modele)
crossCorr(modele)
mq(modele$residuals, lag=10)

#Ordre 3
modele2<-VAR(cbind(MSESta, SMICSta, TCHOFSta), p=3)
stabilityMTS(modele2)
crossCorr(modele2)
mq(modele2$residuals, lag=10)

PredSMICTCHOF<-window(ts(VARpred(modele, 8)$pred[,1], start=c(2015,3), frequency=4), start=2016)
EQM(MSETest, window(PredSMICTCHOF*MSETrendTest*MSESeasonalTest, start=2016, end=c(2017,2)))
PredSMICTCHOF2<-window(ts(VARpred(modele2, 8)$pred[,1], start=c(2015,3), frequency=4), start=2016)
EQM(MSETest, window(PredSMICTCHOF2*MSETrendTest*MSESeasonalTest, start=2016, end=c(2017,2)))


plot(MSETest, xlim=c(2016,2017.25), main="Différences entre les véritables
     valeurs et les prédictions du modèle pour la masse salariale", xaxt="n")
axis(side=1, at=seq(2016,2017.25,0.25), labels=c("2016Q1", "2016Q2", "2016Q3", "2016Q4", "2017Q1", "2017Q2"))
lines(window(PredSMICTCHOF, start=2016, end=c(2017,2))*MSETrendTest*MSESeasonalTest, col = "red")

#PIB, SMIC & TCHOF
VARorder(cbind(MSESta, PIBSta, SMICSta, TCHOFSta), maxp = 10)

#Ordre 4
modele<-VAR(cbind(MSESta, PIBSta, SMICSta, TCHOFSta), p=4)
stabilityMTS(modele)
crossCorr(modele)
mq(modele$residuals, lag=10)

#Ordre 3
modele2<-VAR(cbind(MSESta, PIBSta, SMICSta, TCHOFSta), p=3)
stabilityMTS(modele2)
crossCorr(modele2)
mq(modele2$residuals, lag=10)

#Ordre 2
modele3<-VAR(cbind(MSESta, PIBSta, SMICSta, TCHOFSta), p=2)
stabilityMTS(modele3)
crossCorr(modele3)
mq(modele3$residuals, lag=10)

PredCOMPLET<-window(ts(VARpred(modele, 8)$pred[,1], start=c(2015,3), frequency=4), start=2016)
EQM(MSETest, window(PredCOMPLET*MSETrendTest*MSESeasonalTest, start=2016, end=c(2017,2)))
PredCOMPLET2<-window(ts(VARpred(modele2, 8)$pred[,1], start=c(2015,3), frequency=4), start=2016)
EQM(MSETest, window(PredCOMPLET2*MSETrendTest*MSESeasonalTest, start=2016, end=c(2017,2)))
PredCOMPLET3<-window(ts(VARpred(modele3, 8)$pred[,1], start=c(2015,3), frequency=4), start=2016)
EQM(MSETest, window(PredCOMPLET3*MSETrendTest*MSESeasonalTest, start=2016, end=c(2017,2)))


plot(MSETest, xlim=c(2016,2017.25), main="Différences entre les véritables
     valeurs et les prédictions du modèle pour la masse salariale", xaxt="n")
axis(side=1, at=seq(2016,2017.25,0.25), labels=c("2016Q1", "2016Q2", "2016Q3", "2016Q4", "2017Q1", "2017Q2"))
lines(window(PredCOMPLET, start=2016, end=c(2017,2))*MSETrendTest*MSESeasonalTest, col = "red")

#Comparaison des résultats
resultats<-matrix(nrow=7, ncol=1, dimnames = list(c("PIB", "SMIC", "TCHOF", "PIB & SMIC", "PIB & TCHOF", "SMIC & TCHOF", "PIB, SMIC & TCHOF"), c("EQM")))
resultats[1,1] = EQM(MSETest, window(PredPIB*MSETrendTest*MSESeasonalTest, start=2016, end=c(2017,2)))
resultats[2,1] = EQM(MSETest, window(PredSMIC*MSETrendTest*MSESeasonalTest, start=2016, end=c(2017,2)))
resultats[3,1] = EQM(MSETest, window(PredTCHOF*MSETrendTest*MSESeasonalTest, start=2016, end=c(2017,2)))
resultats[4,1] = EQM(MSETest, window(PredPIBSMIC*MSETrendTest*MSESeasonalTest, start=2016, end=c(2017,2)))
resultats[5,1] = EQM(MSETest, window(PredPIBTCHOF*MSETrendTest*MSESeasonalTest, start=2016, end=c(2017,2)))
resultats[6,1] = EQM(MSETest, window(PredSMICTCHOF*MSETrendTest*MSESeasonalTest, start=2016, end=c(2017,2)))
resultats[7,1] = EQM(MSETest, window(PredCOMPLET*MSETrendTest*MSESeasonalTest, start=2016, end=c(2017,2)))
resultats

##################################################################################################

################################### Modèles VARMA avec MTS ######################################

Eccm(cbind(MSESta, PIBSta, SMICSta, TCHOFSta), maxq=5)
modele<-refVARMA(VARMA(cbind(MSESta, PIBSta, SMICSta, TCHOFSta), p=3, q=1))
