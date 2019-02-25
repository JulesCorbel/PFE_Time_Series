require(tseries)
require(forecast)
require(corrplot)
require(vars)


########################### Importation des données  #############################################
trim <- read.csv("~/PFE_Time_Series/Data/Data_Trim.csv", sep=";", dec=",")
#trim <- read.csv("~/Cygwin/app/home/Jules/PFE_Time_Series/Data/Data_Trim.csv", sep=";", dec=",")
##################################################################################################

########################### Analyse descriptive des séries #######################################

#MSE
#Transformation de la masse salariale en série temporelle
MSE <- ts(trim$MSE, start = 1990, end = c(2017, 2), frequency=4)
#Affichage de la série temporelle
plot(MSE, main="Evolution trimestrielle de la masse salariale", xaxt="n", cex.main=0.9)
#Modification de l'axe x pour afficher le nom des trimestres
axis(side=1, at=seq(1990,2015,5), labels=c("1990Q1", "1995Q1", "2000Q1", "2005Q1", "2010Q1", "2015Q1"))
#Division de l'espace d'affichage des graphiques en deux
par(mfrow=c(1,2), cex.main=0.8)
#Calcul de l'ACF et des PACF de la masse salariale
acf(MSE, main="Auto-corrélation de la masse salariale trimestrielle", lag.max=20)
pacf(MSE, main="Autocorrélation partielle de la masse salariale trimestrielle", lag.max=20)
#Réalisation de tests de stationnarité et de racines unitaires
kpss.test(MSE)
adf.test(MSE)

#PIB
#Transformation du PIB en série temporelle
PIB <- ts(trim$PIB, start = 1990, end = c(2017, 1), frequency=4)
#Affichage du PIB
plot(PIB, main="Evolution trimestrielle du PIB", xaxt="n", cex.main=0.9)
#Modification de l'axe x
axis(side=1, at=seq(1990,2015,5), labels=c("1990Q1", "1995Q1", "2000Q1", "2005Q1", "2010Q1", "2015Q1"))
#Division de l'espace d'affichage des graphiques en deux
par(mfrow=c(1,2), cex.main=0.8)
#Calcul de l'ACF et la PACF du PIB
acf(PIB, main="Auto-corrélation du PIB trimestriel", lag.max=40)
pacf(PIB, main="Autocorrélation partielle du PIB trimestriel", lag.max=40)
par(mfrow=c(1,1))
#Réalisation des tests de stationnarité et de racines unitaires
kpss.test(PIB)
adf.test(PIB)

#SMIC
#Transformation du SMIC en série temporelle
SMIC <- ts(trim$SMIC, start = c(1990,1), end = c(2017, 4), frequency = 4)
#Affichage du SMIC
plot(SMIC, main="Evolution trimestrielle du SMIC", xaxt="n", cex.main=0.9)
#Modification de l'axe x
axis(side=1, at=seq(1990,2015,5), labels=c("1990Q1", "1995Q1", "2000Q1", "2005Q1", "2010Q1", "2015Q1"))
#Division de l'espace d'affichage des graphiques en deux
par(mfrow=c(1,2), cex.main=0.8)
#Calcul de l'ACF et la PACF du SMIC
acf(SMIC, main="Auto-corrélation du SMIC trimestriel", lag.max=20)
pacf(SMIC, main="Autocorrélation partielle du SMIC trimestriel", lag.max=20)
par(mfrow=c(1,1))
#Réalisation des tests de stationnarité et de racines unitaires
kpss.test(SMIC)
adf.test(SMIC)

#TCHOF
#Création du taux de chômage en série temporelle
TCHOF <- ts(trim$TCHOF, start = c(1990,1), end = c(2017, 4), frequency = 4)
#Affichage du taux de chômage
plot(TCHOF, main="Evolution trimestrielle du taux de chômage des femmes", xaxt="n", cex.main=0.9)
#Modification de l'axe x
axis(side=1, at=seq(1990,2015,5), labels=c("1990Q1", "1995Q1", "2000Q1", "2005Q1", "2010Q1", "2015Q1"))
#Division de l'espace d'affichage des graphiques en deux
par(mfrow=c(1,2), cex.main=0.8)
#Calcul de l'ACF et de la PACF du taux de chômage
acf(TCHOF, main="Auto-corrélation du taux de chômage des femmes trimestriel", lag.max=20)
pacf(TCHOF, main="Autocorrélation partielle du taux de chômage des femmes trimestriel", lag.max=20)
par(mfrow=c(1,1))
#Réalisation des tests de stationnarité et de racines unitaires
kpss.test(TCHOF)
adf.test(TCHOF)

#Corrélations

#Matrice des corrélations
corrplot(cor(trim[1:109,-1]), method = "number", type="lower",
         p.mat=cor.mtest(trim[1:109,-1], 0.95)[[1]], insig="pch",
         col=colorRampPalette(c("blue", "light blue", "red"))(50))
#Calcul des p-values associées aux coefficients
corr <- cor.mtest(trim[1:109,-1], 0.95)[[1]]
#Ajout des libellés à la matrice des p-values
rownames(corr) <- c("MSE","PIB","SMIC","TCHOF")
colnames(corr) <- c("MSE","PIB","SMIC","TCHOF")
#Affichage des p-values
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
#Création du lissage exponentiel de la masse salariale
LEMSE<-ets(MSETrain, "ZAM")
#Affichage des différents coefficients du lissage
print(LEMSE)
#Prédiction des 6 prochaines valeurs
PredLEMSE <- forecast(LEMSE, h = 6)
#Affichage de l'échantillon de test et des prédictions afin de les comparer
plot(MSETest, main="Comparaison entre la prédiction du lissage exponentiel et les valeurs réelles pour la masse salariale trimestrielle")
lines(PredLEMSE$mean, col="red")
#Calcul de l'EQM entre les prédictions et les valeurs de l'échantillon de test
EQM(MSETest, PredLEMSE$mean)

#PIB
#Création du lissage exponentiel du PIB
LEPIB<-ets(PIBTrain, "ZAN")
#Affichage des différents coefficients du lissage
print(LEPIB)
#Prédiction des 5 prochaines valeurs
PredLEPIB <- forecast(LEPIB, h = 5)
#Affichage de l'échantillon de test et des prédictions afin de les comparer
plot(PIBTest, ylim=c(min(PIBTest,PredLEPIB$mean),max(PIBTest,PredLEPIB$mean)), main= "Comparaison entre la prédiction du lissage exponentiel et les valeurs réelles pour le PIB trimestriel", cex.main=0.8)
lines(PredLEPIB$mean, col="red")
#Calcul de l'EQM entre les prédictions et les valeurs de l'échantillon de test
EQM(PIBTest, PredLEPIB$mean)

#SMIC
#Création du lissage exponentiel du SMIC
LESMIC<-ets(SMICTrain, "ZAA")
#Affichage des différents coefficients du lissage
print(LESMIC)
#Prédiction des 6 prochaines valeurs
PredLESMIC <- forecast(LESMIC, h = 6)
#Affichage de l'échantillon de test et des prédictions afin de les comparer
plot(SMICTest, ylim=c(min(SMICTest,PredLESMIC$mean),max(SMICTest,PredLESMIC$mean)), main="Comparaison entre la prédiction du lissage exponentiel et les valeurs réelles pour le SMIC trimestriel", cex.main=0.8)
lines(PredLESMIC$mean, col="red")
#Calcul de l'EQM entre les prédictions et les valeurs de l'échantillon de test
EQM(SMICTest, PredLESMIC$mean)

#Taux de chômage des femmes
#Création du lissage exponentiel du taux de chômage
LETCHOF<-ets(TCHOFTrain, "ZNN")
#Affichage des différents coefficients
print(LETCHOF)
#Prédiction des 6 prochaines valeurs
PredLETCHOF <- forecast(LETCHOF, h = 6)
#Affichage de l'échantillon de test et des prédictions afin de les comparer
plot(TCHOFTest, ylim=c(min(TCHOFTest,PredLETCHOF$mean),max(TCHOFTest,PredLETCHOF$mean)), main="Comparaison entre la prédiction du lissage exponentiel et les valeurs réelles pour le taux de chômage trimestriel", cex.main=0.8)
lines(PredLETCHOF$mean, col="red")
#Calcul de l'EQM entre les prédictions et les valeurs de l'échantillon de test
EQM(TCHOFTest, PredLETCHOF$mean)

#Modèles ARMA

#MSE
#Création du modèle SARIMA de la masse salariale
ARIMAMSE<-auto.arima(MSETrain, ic="aicc")
#Affichage des ordres et des coefficients du modèle
print(ARIMAMSE)
#Prédiction des 6 prochaines valeurs
PredARIMAMSE<- forecast(ARIMAMSE, h=6)
#Affichage de l'échantillon de test et des prédictions afin de les comparer
plot(MSETest, main="Comparaison entre le modèle SARIMA et les données de validation pour la masse salariale trimestrielle")
lines(PredARIMAMSE$mean, col="red")
#Calcul de l'EQM entre les prédictions et les valeurs de l'échantillon de test
EQM(MSETest, PredARIMAMSE$mean)

#PIB
#Création du modèle ARIMA du PIB
ARIMAPIB<-auto.arima(PIBTrain, ic="aicc", seasonal=F)
#Affichage des ordres et des coefficients du modèle
print(ARIMAPIB)
#Prédiction des 5 prochaines valeurs
PredARIMAPIB<- forecast(ARIMAPIB, h=5)
#Affichage de l'échantillon de test et des prédictions afin de les comparer
plot(PIBTest, ylim=c(min(PIBTest,PredARIMAPIB$mean),max(PIBTest,PredARIMAPIB$mean)), 
     main="Comparaison entre le modèle SARIMA et les données de
     validation pour le PIB trimestriel", cex.main=0.8)
lines(PredARIMAPIB$mean, col="red")
#Calcul de l'EQM entre les prédictions et les valeurs de l'échantillon de test
EQM(PIBTest, PredARIMAPIB$mean)

#SMIC
#Création du modèle SARIMA du SMIC
ARIMASMIC<-auto.arima(SMICTrain, ic="aicc")
#Affichage des ordres et des coefficients du modèle
print(ARIMASMIC)
#Prédiction des 6 prochaines valeurs
PredARIMASMIC<- forecast(ARIMASMIC, h=6)
#Affichage de l'échantillon de test et des prédictions afin de les comparer
plot(SMICTest, ylim=c(min(SMICTest,PredARIMASMIC$mean),max(SMICTest,PredARIMASMIC$mean)), 
     main="Comparaison entre le modèle SARIMA et les données de
    validation pour le SMIC trimestriel", cex.main=0.8)
lines(PredARIMASMIC$mean, col="red")
#Calcul de l'EQM entre les prédictions et les valeurs de l'échantillon de test
EQM(SMICTest, PredARIMASMIC$mean)

#TCHOF
#Création du modèle ARIMA du taux de chômage
ARIMATCHOF<-auto.arima(TCHOFTrain, ic="aicc", seasonal=F)
#Affichage des ordres et des coefficients du modèle
print(ARIMATCHOF)
#Prédiction des 6 prochaines valeurs
PredARIMATCHOF<- forecast(ARIMATCHOF, h=6)
#Affichage de l'échantillon de test et des prédictions afin de les comparer
plot(TCHOFTest, main="Comparaison entre le modèle SARIMA et les données de
    validation pour le taux de chômage des femmes trimestriel", cex.main=0.8)
lines(PredARIMATCHOF$mean, col="red")
#Calcul de l'EQM entre les prédictions et les valeurs de l'échantillon de test
EQM(TCHOFTest, PredARIMATCHOF$mean)

#Comparaison des résultats
#Création d'une matrice stockant dans une colonne les EQM des prédiction des lissages et dans l'autre les
#prédictions des SARIMA
resultats<-matrix(nrow=4, ncol=2, dimnames = list(c("MSE", "PIB", "SMIC", "TCHOF"), 
                                                  c("lissage", "ARMA")))
#Remplissage de la matrice
resultats[1,1] = EQM(MSETest, PredLEMSE$mean)
resultats[2,1] = EQM(PIBTest, PredLEPIB$mean)
resultats[3,1] = EQM(SMICTest, PredLESMIC$mean)
resultats[4,1] = EQM(TCHOFTest, PredLETCHOF$mean)
resultats[1,2] = EQM(MSETest, PredARIMAMSE$mean)
resultats[2,2] = EQM(PIBTest, PredARIMAPIB$mean)
resultats[3,2] = EQM(SMICTest, PredARIMASMIC$mean)
resultats[4,2] = EQM(TCHOFTest, PredARIMATCHOF$mean)
#Affichage de la matrice
resultats

#Prédiction de la valeur du PIB pour 2017Q2
#Prédiction de la valeur manquante du PIB
PredARIMAPIB<- forecast(ARIMAPIB, h=6)
#Stockage de la valeur en question
new.value <- PredARIMAPIB$mean[6]
#Création du nouveau jeu de test du PIB avec la valeur estimée
PIBTest<-ts(c(PredARIMAPIB$mean, new.value), start = 2016, end = c(2017, 2), frequency=4)

##################################################################################################

###################### Modélisation ARMA avec variables exogènes de la MSE #######################

#PIB
#Création d'un SARIMA du la masse salariale prenant en compte le PIB
SARIMAPIB <- auto.arima(MSETrain, xreg = cbind(PIBTrain))
SARIMAPIB
#Prédiction des prochaines valeurs
PredPIB <- forecast(SARIMAPIB, xreg = cbind(PIBTest))
#Affichage de l'échantillon de test et des prédictions afin de les comparer
plot(PredPIB$mean, col="red",
     ylim=c(min(MSETest,PredPIB$mean), max(MSETest,PredPIB$mean)),
     main = "Masse salariale expliquée par le PIB vs Vraies valeurs")
lines(MSETest)
#Calcul de l'EQM entre les prédictions et les valeurs de l'échantillon de test
EQM(PredPIB$mean, MSETest)

#SMIC
#Création d'un SARIMA du la masse salariale prenant en compte le SMIC
SARIMASMIC <- auto.arima(MSETrain, xreg = cbind(SMICTrain))
SARIMASMIC
#Prédiction des prochaines valeurs
PredSMIC <- forecast(SARIMASMIC, xreg = cbind(SMICTest))
#Affichage de l'échantillon de test et des prédictions afin de les comparer
plot(PredSMIC$mean, col="red",
     ylim=c(min(MSETest,PredSMIC$mean), max(MSETest,PredSMIC$mean)),
     main = "Masse salariale expliqué par le SMIC vs Vraies valeurs")
lines(MSETest)
#Calcul de l'EQM entre les prédictions et les valeurs de l'échantillon de test
EQM(PredSMIC$mean, MSETest)

#TCHOF
#Création d'un SARIMA du la masse salariale prenant en compte le taux de chômage
SARIMATCHOF <- auto.arima(MSETrain, xreg = cbind(TCHOFTrain))
SARIMATCHOF
#Prédiction des prochaines valeurs
PredTCHOF <- forecast(SARIMATCHOF, xreg = cbind(TCHOFTest))
#Affichage de l'échantillon de test et des prédictions afin de les comparer
plot(PredTCHOF$mean, col="red",
     ylim=c(min(MSETest,PredTCHOF$mean), max(MSETest,PredTCHOF$mean)),
     main = "Masse salariale expliquée par le taux de chômage vs Vraies valeurs")
lines(MSETest)
#Calcul de l'EQM entre les prédictions et les valeurs de l'échantillon de test
EQM(PredTCHOF$mean, MSETest)

#PIB & SMIC
#Création d'un SARIMA du la masse salariale prenant en compte le PIB et le SMIC
SARIMAPIBSMIC <- auto.arima(MSETrain, xreg = cbind(PIBTrain, SMICTrain))
SARIMAPIBSMIC
#Prédiction des prochaines valeurs
PredPIBSMIC <- forecast(SARIMAPIBSMIC, xreg = cbind(PIBTest, SMICTest))
#Affichage de l'échantillon de test et des prédictions afin de les comparer
plot(PredPIBSMIC$mean, col="red",
     ylim=c(min(MSETest,PredPIBSMIC$mean), max(MSETest,PredPIBSMIC$mean)),
     main = "Masse salariale expliquée par le PIB et le SMIC vs Vraies valeurs")
lines(MSETest)
#Calcul de l'EQM entre les prédictions et les valeurs de l'échantillon de test
EQM(PredPIBSMIC$mean, MSETest)

#PIB & TCHOF
#Création d'un SARIMA du la masse salariale prenant en compte le PIB et le taux de chômage
SARIMAPIBTCHOF <- auto.arima(MSETrain, xreg = cbind(PIBTrain, TCHOFTrain))
SARIMAPIBTCHOF
#Prédiction des prochaines valeurs
PredPIBTCHOF <- forecast(SARIMAPIBTCHOF, xreg = cbind(PIBTest, TCHOFTest))
#Affichage de l'échantillon de test et des prédictions afin de les comparer
plot(PredPIBTCHOF$mean, col="red",
     ylim=c(min(MSETest,PredPIBTCHOF$mean), max(MSETest,PredPIBTCHOF$mean)),
     main = "Masse salariale expliquée par le PIB et le taux de chômage vs Vraies valeurs")
lines(MSETest)
#Calcul de l'EQM entre les prédictions et les valeurs de l'échantillon de test
EQM(PredPIBTCHOF$mean, MSETest)

#SMIC & TCHOF
#Création d'un SARIMA du la masse salariale prenant en compte le SMIC et le taux de chômage
SARIMASMICTCHOF <- auto.arima(MSETrain, xreg = cbind(SMICTrain, TCHOFTrain))
SARIMASMICTCHOF
#Prédiction des prochaines valeurs
PredSMICTCHOF <- forecast(SARIMASMICTCHOF, xreg = cbind(SMICTest, TCHOFTest))
#Affichage de l'échantillon de test et des prédictions afin de les comparer
plot(PredSMICTCHOF$mean, col="red",
     ylim=c(min(MSETest,PredSMICTCHOF$mean), max(MSETest,PredSMICTCHOF$mean)),
     main = "Masse salariale expliquée par le SMIC et le taux de chômage vs Vraies valeurs")
lines(MSETest)
#Calcul de l'EQM entre les prédictions et les valeurs de l'échantillon de test
EQM(PredSMICTCHOF$mean, MSETest)

#PIB, SMIC & TCHOF
#Création d'un SARIMA du la masse salariale prenant en compte toutes les variables exogènes
SARIMACOMPLET <- auto.arima(MSETrain, xreg = cbind(PIBTrain, SMICTrain, TCHOFTrain))
SARIMACOMPLET
#Prédiction des prochaines valeurs
PredCOMPLET <- forecast(SARIMACOMPLET, xreg = cbind(PIBTest, SMICTest, TCHOFTest))
#Affichage de l'échantillon de test et des prédictions afin de les comparer
plot(PredCOMPLET$mean, col="red",
     ylim=c(min(MSETest,PredCOMPLET$mean), max(MSETest,PredCOMPLET$mean)),
     main = "Masse salariale expliquée par le PIB, le SMIC et le taux de chômage vs Vraies valeurs")
lines(MSETest)
#Calcul de l'EQM entre les prédictions et les valeurs de l'échantillon de test
EQM(PredCOMPLET$mean, MSETest)

#Comparaison des résultats
#Création d'un tableau stockant les EQM des différents modèles
resultats<-matrix(nrow=7, ncol=1, dimnames = list(c("PIB", "SMIC", "TCHOF", "PIB & SMIC", "PIB & SMIC", "PIB & TCHOF", "PIB, SMIC & TCHOF"), 
                                                  c("EQM")))
#Remplissage du tableau
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
#Affichage de la décomposition de la masse salariale
plot(decompose(MSETrain, "multiplicative"))
#Stockage des différentes composantes de la masse salariale
MSESta <- na.omit(decompose(MSETrain, "multiplicative")$random)
MSETrend<-window(decompose(MSETrain, "multiplicative")$trend)
MSESeasonal<-window(decompose(MSETrain, "multiplicative")$seasonal)
#Affichage des résidus de la masse salariale
plot(MSESta, main="Masse salariale trimestrielle stationnarisée", xaxt="n")
#Modification de l'axe x
axis(side=1, at=seq(1990,2015,5), labels=c("1990Q1", "1995Q1", "2000Q1", "2005Q1", "2010Q1", "2015Q1"))
#Division de l'espace d'affichage des graphiques en deux
par(mfrow=c(1,2), cex.main=0.8)
#Calcul de l'ACF et la PACF de la masse salariale stationnarisée
acf(MSESta, main="ACF de la masse salariale stationnarisée")
pacf(MSESta, main="PACF de la masse salariale stationnarisée")
par(mfrow=c(1,1))
#Réalisation des tests de stationnarité et de racines unitaires
kpss.test(MSESta)
adf.test(MSESta)
#Création de vecteurs contenant la tendance et la saisonnalité des futures valeurs
MSETrendTest <- window(forecast(na.omit(MSETrend), h=8)$mean, start=2016)
MSESeasonalTest<-ts(c(MSESeasonal[1:4],MSESeasonal[1:2]), start=2016,frequency=4)

#PIB
#Stockage de la composante résiduelle du PIB
PIBSta <- na.omit(decompose(PIBTrain, "multiplicative")$random)
#Affichage des résidus du PIB
plot(PIBSta, main="PIB trimestriel stationnarisé", xaxt="n")
axis(side=1, at=seq(1990,2015,5), labels=c("1990Q1", "1995Q1", "2000Q1", "2005Q1", "2010Q1", "2015Q1"))
#Division de l'espace d'affichage des graphiques en deux
par(mfrow=c(1,2))
#Calcul de l'ACF et la PACF du PIB stationnarisée
acf(PIBSta, main="Auto-Corrélation du PIB trimestriel stationnarisé")
pacf(PIBSta, main="Auto-Corrélation partielle du PIB trimestriel stationnarisé")
par(mfrow=c(1,1))
#Réalisation des tests de stationnarité et de racines unitaires
kpss.test(PIBSta)
adf.test(PIBSta)

#SMIC
#Stockage de la composante résiduelle du SMIC
SMICSta <- na.omit(decompose(SMICTrain)$random)
#Affichage des résidus du SMIC
plot(SMICSta, main="SMIC trimestriel stationnarisé", xaxt="n")
axis(side=1, at=seq(1990,2015,5), labels=c("1990Q1", "1995Q1", "2000Q1", "2005Q1", "2010Q1", "2015Q1"))
#Division de l'espace d'affichage des graphiques en deux
par(mfrow=c(1,2))
#Calcul de l'ACF et la PACF du SMIC stationnarisée
acf(SMICSta, main="Auto-Corrélation du SMIC trimestriellstationnarisé")
pacf(SMICSta, main="Auto-Corrélation partielle du SMIC trimestriel stationnarisé")
par(mfrow=c(1,1))
#Réalisation des tests de stationnarité et de racines unitaires
kpss.test(SMICSta)
adf.test(SMICSta)

#TCHOF
#Stockage de la composante résiduelle du taux de chômage
TCHOFSta <- na.omit(decompose(TCHOFTrain)$random)
#Affichage des résidus du taux de chômage
plot(TCHOFSta, main="Taux de chômage trimestriel des femmes stationnarisé", xaxt="n")
axis(side=1, at=seq(1990,2015,5), labels=c("1990Q1", "1995Q1", "2000Q1", "2005Q1", "2010Q1", "2015Q1"))
#Division de l'espace d'affichage des graphiques en deux
par(mfrow=c(1,2))
#Calcul de l'ACF et la PACF du taux de chômage stationnarisée
acf(TCHOFSta, main="Auto-Corrélation du Taux de chômage des femmes trimestrielle stationnarisé")
pacf(TCHOFSta, main="Auto-Corrélation partielle du Taux de chômage des femmes trimestriel stationnarisé")
par(mfrow=c(1,1))
#Réalisation des tests de stationnarité et de racines unitaires
kpss.test(TCHOFSta)
adf.test(TCHOFSta)

##################################################################################################

#################################### Modèles VAR avec vars #######################################
detach("package:MTS", unload=TRUE)

#PIB
#Selection de l'ordre du modèle VAR en fonction de différents critères
VARselect(cbind(MSESta, PIBSta), lag.max=10)

#Ordre 4
#Réalisation d'un modèle d'ordre 4 avec la masse salariale et le PIB
modele<-VAR(cbind(MSESta, PIBSta), p=4, type="const")
#vérification de la stabilité du modèle
stabilityvars(modele)
#Test d'homoscédasticité
archTest(modele)
#Test de normalité
normality.test(modele)$jb.mul$JB
#Vérification de l'inversibilité de la matrice
calculC0(modele)
#Test d'auto-corrélation et de corrélation croisée
Portmanteau(modele)

#Ordre3
#Réalisation d'un modèle d'ordre 3 avec la masse salariale et le PIB
modele2<-VAR(cbind(MSESta, PIBSta), p=3, type="const")
#vérification de la stabilité du modèle
stabilityvars(modele2)
#Test d'homoscédasticité
archTest(modele2)
#Test de normalité
normality.test(modele2)$jb.mul$JB
#Vérification de l'inversibilité de la matrice
calculC0(modele2)
#Test d'auto-corrélation et de corrélation croisée
Portmanteau(modele2)

#Ordre 2
#Réalisation d'un modèle d'ordre 2 avec la masse salariale et le PIB
modele3<-VAR(cbind(MSESta, PIBSta), p=2, type="const")
#vérification de la stabilité du modèle
stabilityvars(modele3)
#Test d'homoscédasticité
archTest(modele3)
#Test de normalité
normality.test(modele3)$jb.mul$JB
#Vérification de l'inversibilité de la matrice
calculC0(modele3)
#Test d'auto-corrélation et de corrélation croisée
Portmanteau(modele3)

#Prédictions avec le modèle d'ordre 4 et calcul de l'EQM
PredPIB<-forecast(modele, h=8)$forecast$MSESta$mean
EQM(MSETest, window(PredPIB*MSETrendTest*MSESeasonalTest, start=2016, end=c(2017,2)))
#Prédictions avec le modèle d'ordre 3 et calcul de l'EQM
PredPIB2<-forecast(modele2, h=8)$forecast$MSESta$mean
EQM(MSETest, window(PredPIB2*MSETrendTest*MSESeasonalTest, start=2016, end=c(2017,2)))
#Prédictions avec le modèle d'ordre 2 et calcul de l'EQM
PredPIB3<-forecast(modele3, h=8)$forecast$MSESta$mean
EQM(MSETest, window(PredPIB3*MSETrendTest*MSESeasonalTest, start=2016, end=c(2017,2)))

#Affichage des valeurs de l'échantillon de test et comparaison avec les prédictions
plot(MSETest, xlim=c(2016,2017.25), main="Différences entre les véritables
     valeurs et les prédictions du modèle pour la masse salariale", xaxt="n")
axis(side=1, at=seq(2016,2017.25,0.25), labels=c("2016Q1", "2016Q2", "2016Q3", "2016Q4", "2017Q1", "2017Q2"))
lines(window(PredPIB, start=2016, end=c(2017,2))*MSETrendTest*MSESeasonalTest, col = "red")

#SMIC
#Selection de l'ordre du modèle VAR en fonction de différents critères
VARselect(cbind(MSESta, SMICSta), lag.max=10)

#Ordre 3
#Réalisation d'un modèle d'ordre 3 avec la masse salariale et le SMIC
modele<-VAR(cbind(MSESta, SMICSta), p=3, type="const")
#Vérification de la stabilité du modèle
stabilityvars(modele)
#Test d'homoscédasticité
archTest(modele)
#Test de normalité
normality.test(modele)$jb.mul$JB
#Vérification de l'inversibilité de la matrice
calculC0(modele)
#Test d'auto-corrélation et de corrélation croisée
Portmanteau(modele)

#Prédictions
PredSMIC<-forecast(modele, h=8)$forecast$MSESta$mean
EQM(MSETest, window(PredCOMPLET*MSETrendTest*MSESeasonalTest, start=2016, end=c(2017,2)))

#Affichage des valeurs de l'échantillon de test et comparaison avec les prédictions
plot(MSETest, xlim=c(2016,2017.25), main="Différences entre les véritables
     valeurs et les prédictions du modèle pour la masse salariale", xaxt="n")
axis(side=1, at=seq(2016,2017.25,0.25), labels=c("2016Q1", "2016Q2", "2016Q3", "2016Q4", "2017Q1", "2017Q2"))
lines(window(PredSMIC, start=2016, end=c(2017,2))*MSETrendTest*MSESeasonalTest, col = "red")

#TCHOF
#Selection de l'ordre du modèle VAR en fonction de différents critères
VARselect(cbind(MSESta, TCHOFSta), lag.max=10)

#Ordre 3
#Réalisation d'un modèle d'ordre 3 avec la masse salariale et le taux de chômage
modele<-VAR(cbind(MSESta, TCHOFSta), p=3, type="const")
#Vérification de la stabilité du modèle
stabilityvars(modele)
#Test d'homoscédasticité
archTest(modele)
#Test de normalité
normality.test(modele)$jb.mul$JB
#Vérification de l'inversibilité de la matrice
calculC0(modele)
#Test d'auto-corrélation et de corrélation croisée
Portmanteau(modele)

#Ordre 2
#Réalisation d'un modèle d'ordre 2 avec la masse salariale et le taux de chômage
modele2<-VAR(cbind(MSESta, PIBSta), p=2, type="const")
#Vérification de la stabilité du modèle
stabilityvars(modele2)
#Test d'homoscédasticité
archTest(modele2)
#Test de normalité
normality.test(modele2)$jb.mul$JB
#Vérification de l'inversibilité de la matrice
calculC0(modele2)
#Test d'auto-corrélation et de corrélation croisée
Portmanteau(modele2)

#Prédictions avec le modèle d'ordre 3 et calcul de l'EQM
PredTCHOF<-forecast(modele, h=8)$forecast$MSESta$mean
EQM(MSETest, window(PredTCHOF*MSETrendTest*MSESeasonalTest, start=2016, end=c(2017,2)))
#Prédictions avec le modèle d'ordre 2 et calcul de l'EQM
PredTCHOF2<-forecast(modele2, h=8)$forecast$MSESta$mean
EQM(MSETest, window(PredTCHOF2*MSETrendTest*MSESeasonalTest, start=2016, end=c(2017,2)))

#Affichage des valeurs de l'échantillon de test et comparaison avec les prédictions
plot(MSETest, xlim=c(2016,2017.25), main="Différences entre les véritables
     valeurs et les prédictions du modèle pour la masse salariale", xaxt="n")
axis(side=1, at=seq(2016,2017.25,0.25), labels=c("2016Q1", "2016Q2", "2016Q3", "2016Q4", "2017Q1", "2017Q2"))
lines(window(PredTCHOF, start=2016, end=c(2017,2))*MSETrendTest*MSESeasonalTest, col = "red")

#PIB & SMIC
#Selection de l'ordre du modèle VAR en fonction de différents critères
VARselect(cbind(MSESta, PIBSta, SMICSta), lag.max=10)

#Ordre 3
#Réalisation d'un modèle d'ordre 3 avec la masse salariale, le PIB et le SMIC
modele<-VAR(cbind(MSESta, PIBSta, SMICSta), p=3, type="const")
#Vérification de la stabilité du modèle
stabilityvars(modele)
#Test d'homoscédasticité
archTest(modele)
#Test de normalité
normality.test(modele)$jb.mul$JB
#Vérification de l'inversibilité de la matrice
calculC0(modele)
#Test d'auto-corrélation et de corrélation croisée
Portmanteau(modele)

#Prédictions
PredPIBSMIC<-forecast(modele, h=8)$forecast$MSESta$mean
EQM(MSETest, window(PredPIBSMIC*MSETrendTest*MSESeasonalTest, start=2016, end=c(2017,2)))

#Affichage des valeurs de l'échantillon de test et comparaison avec les prédictions
plot(MSETest, xlim=c(2016,2017.25), main="Différences entre les véritables
     valeurs et les prédictions du modèle pour la masse salariale", xaxt="n")
axis(side=1, at=seq(2016,2017.25,0.25), labels=c("2016Q1", "2016Q2", "2016Q3", "2016Q4", "2017Q1", "2017Q2"))
lines(window(PredPIBSMIC, start=2016, end=c(2017,2))*MSETrendTest*MSESeasonalTest, col = "red")

#PIB & TCHOF
#Selection de l'ordre du modèle VAR en fonction de différents critères
VARselect(cbind(MSESta, PIBSta, TCHOFSta), lag.max=10)

#Ordre 4
#Réalisation d'un modèle d'ordre 4 avec la masse salariale et le taux de chômage et le PIB
modele<-VAR(cbind(MSESta, PIBSta, TCHOFSta), p=4, type="const")
#Vérification de la stabilité du modèle
stabilityvars(modele)
#Test d'homoscédasticité
archTest(modele)
#Test de normalité
normality.test(modele)$jb.mul$JB
#Vérification de l'inversibilité de la matrice
calculC0(modele)
#Test d'auto-corrélation et de corrélation croisée
Portmanteau(modele)

#Ordre3
#Réalisation d'un modèle d'ordre 3 avec la masse salariale et le taux de chômage et le PIB
modele2<-VAR(cbind(MSESta, PIBSta, TCHOFSta), p=3, type="const")
#Vérification de la stabilité du modèle
stabilityvars(modele2)
#Test d'homoscédasticité
archTest(modele2)
#Test de normalité
normality.test(modele2)$jb.mul$JB
#Vérification de l'inversibilité de la matrice
calculC0(modele2)
#Test d'auto-corrélation et de corrélation croisée
Portmanteau(modele2)

#Prédictions
PredPIBTCHOF<-forecast(modele, h=8)$forecast$MSESta$mean
EQM(MSETest, window(PredPIBTCHOF*MSETrendTest*MSESeasonalTest, start=2016, end=c(2017,2)))
PredPIBTCHOF2<-forecast(modele2, h=8)$forecast$MSESta$mean
EQM(MSETest, window(PredPIBTCHOF2*MSETrendTest*MSESeasonalTest, start=2016, end=c(2017,2)))

#Affichage des valeurs de l'échantillon de test et comparaison avec les prédictions
plot(MSETest, xlim=c(2016,2017.25), main="Différences entre les véritables
     valeurs et les prédictions du modèle pour la masse salariale", xaxt="n")
axis(side=1, at=seq(2016,2017.25,0.25), labels=c("2016Q1", "2016Q2", "2016Q3", "2016Q4", "2017Q1", "2017Q2"))
lines(window(PredPIBTCHOF, start=2016, end=c(2017,2))*MSETrendTest*MSESeasonalTest, col = "red")

#SMIC & TCHOF
#Selection de l'ordre du modèle VAR en fonction de différents critères
VARselect(cbind(MSESta, SMICSta, TCHOFSta), lag.max=10)

#Ordre 4
#Réalisation d'un modèle d'ordre 4 avec la masse salariale et le taux de chômage et le SMIC
modele<-VAR(cbind(MSESta, SMICSta, TCHOFSta), p=4, type="const")
#Vérification de la stabilité du modèle
stabilityvars(modele)
#Test d'homoscédasticité
archTest(modele)
#Test de normalité
normality.test(modele)$jb.mul$JB
#Vérification de l'inversibilité de la matrice
calculC0(modele)
#Test d'auto-corrélation et de corrélation croisée
Portmanteau(modele)

#Ordre3
#Réalisation d'un modèle d'ordre 3 avec la masse salariale et le taux de chômage et le SMIC
modele2<-VAR(cbind(MSESta, SMICSta, TCHOFSta), p=3, type="const")
#Vérification de la stabilité du modèle
stabilityvars(modele2)
#Test d'homoscédasticité
archTest(modele2)
#Test de normalité
normality.test(modele2)$jb.mul$JB
#Vérification de l'inversibilité de la matrice
calculC0(modele2)
#Test d'auto-corrélation et de corrélation croisée
Portmanteau(modele2)

#Prédictions
PredSMICTCHOF<-forecast(modele, h=8)$forecast$MSESta$mean
EQM(MSETest, window(PredSMICTCHOF*MSETrendTest*MSESeasonalTest, start=2016, end=c(2017,2)))
PredSMICTCHOF2<-forecast(modele2, h=8)$forecast$MSESta$mean
EQM(MSETest, window(PredSMICTCHOF2*MSETrendTest*MSESeasonalTest, start=2016, end=c(2017,2)))

#Affichage des valeurs de l'échantillon de test et comparaison avec les prédictions
plot(MSETest, xlim=c(2016,2017.25), main="Différences entre les véritables
     valeurs et les prédictions du modèle pour la masse salariale", xaxt="n")
axis(side=1, at=seq(2016,2017.25,0.25), labels=c("2016Q1", "2016Q2", "2016Q3", "2016Q4", "2017Q1", "2017Q2"))
lines(window(PredSMICTCHOF, start=2016, end=c(2017,2))*MSETrendTest*MSESeasonalTest, col = "red")

#Modèle complet
#Selection de l'ordre du modèle VAR en fonction de différents critères
VARselect(cbind(MSESta, PIBSta, SMICSta, TCHOFSta), lag.max=10)

#Ordre 4
#Réalisation d'un modèle d'ordre 4 avec toutes les variables
modele<-VAR(cbind(MSESta, PIBSta, SMICSta, TCHOFSta), p=4, type="const")
#Vérification de la stabilité du modèle
stabilityvars(modele)
#Test d'homoscédasticité
archTest(modele)
#Test de normalité
normality.test(modele)$jb.mul$JB
#Vérification de l'inversibilité de la matrice
calculC0(modele)
#Test d'auto-corrélation et de corrélation croisée
Portmanteau(modele)

#Ordre3
#Réalisation d'un modèle d'ordre 3 avec toutes les variables
modele2<-VAR(cbind(MSESta, PIBSta, SMICSta, TCHOFSta), p=3, type="const")
#Vérification de la stabilité du modèle
stabilityvars(modele2)
#Test d'homoscédasticité
archTest(modele2)
#Test de normalité
normality.test(modele2)$jb.mul$JB
#Vérification de l'inversibilité de la matrice
calculC0(modele2)
#Test d'auto-corrélation et de corrélation croisée
Portmanteau(modele2)

#Ordre 2
#Réalisation d'un modèle d'ordre 2 avec toutes les variables
modele3<-VAR(cbind(MSESta, PIBSta, SMICSta, TCHOFSta), p=2, type="const")
#Vérification de la stabilité du modèle
stabilityvars(modele3)
#Test d'homoscédasticité
archTest(modele3)
#Test de normalité
normality.test(modele3)$jb.mul$JB
#Vérification de l'inversibilité de la matrice
calculC0(modele3)
#Test d'auto-corrélation et de corrélation croisée
Portmanteau(modele3)

#Prédictions
PredCOMPLET<-forecast(modele, h=8)$forecast$MSESta$mean
EQM(MSETest, window(PredCOMPLET*MSETrendTest*MSESeasonalTest, start=2016, end=c(2017,2)))
PredCOMPLET2<-forecast(modele2, h=8)$forecast$MSESta$mean
EQM(MSETest, window(PredCOMPLET2*MSETrendTest*MSESeasonalTest, start=2016, end=c(2017,2)))
PredCOMPLET3<-forecast(modele3, h=8)$forecast$MSESta$mean
EQM(MSETest, window(PredCOMPLET3*MSETrendTest*MSESeasonalTest, start=2016, end=c(2017,2)))

#Affichage des valeurs de l'échantillon de test et comparaison avec les prédictions
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
#Selection de l'ordre du modèle VAR en fonction de différents critères
VARorder(cbind(MSESta, PIBSta), maxp = 10)

#Ordre 4
#Réalisation d'un modèle d'ordre 4 avec la masse salariale et le PIB
modele<-VAR(cbind(MSESta, PIBSta), p=4)
#Vérification de la stabilité du modèle
stabilityMTS(modele)
#Test d'auto-corrélation et de corrélation croisée
crossCorr(modele)
#test du Portmanteau
mq(modele$residuals, lag=10)

#Ordre 3
#Réalisation d'un modèle d'ordre 3 avec la masse salariale et le PIB
modele2<-VAR(cbind(MSESta, PIBSta), p=3)
#Vérification de la stabilité du modèle
stabilityMTS(modele2)
#Test d'auto-corrélation et de corrélation croisée
crossCorr(modele2)
#test du Portmanteau
mq(modele2$residuals, lag=10)

#Prédictions
PredPIB<-window(ts(VARpred(modele, 8)$pred[,1], start=c(2015,3), frequency=4), start=2016)
EQM(MSETest, window(PredPIB*MSETrendTest*MSESeasonalTest, start=2016, end=c(2017,2)))
PredPIB2<-window(ts(VARpred(modele2, 8)$pred[,1], start=c(2015,3), frequency=4), start=2016)
EQM(MSETest, window(PredPIB2*MSETrendTest*MSESeasonalTest, start=2016, end=c(2017,2)))

#Affichage des valeurs de l'échantillon de test et comparaison avec les prédictions
plot(MSETest, xlim=c(2016,2017.25), main="Différences entre les véritables
     valeurs et les prédictions du modèle pour la masse salariale", xaxt="n")
axis(side=1, at=seq(2016,2017.25,0.25), labels=c("2016Q1", "2016Q2", "2016Q3", "2016Q4", "2017Q1", "2017Q2"))
lines(window(PredPIB, start=2016, end=c(2017,2))*MSETrendTest*MSESeasonalTest, col = "red")

#SMIC
#Selection de l'ordre du modèle VAR en fonction de différents critères
VARorder(cbind(MSESta, SMICSta), maxp = 10)

#Ordre 3
#Réalisation d'un modèle d'ordre 3 avec la masse salariale et le SMIC
modele<-VAR(cbind(MSESta, SMICSta), p=3)
#Vérification de la stabilité du modèle
stabilityMTS(modele)
#Test d'auto-corrélation et de corrélation croisée
crossCorr(modele)
#test du Portmanteau
mq(modele$residuals, lag=10)

#Prédictions
PredSMIC<-window(ts(VARpred(modele, 8)$pred[,1], start=c(2015,3), frequency=4), start=2016)
EQM(MSETest, window(PredSMIC*MSETrendTest*MSESeasonalTest, start=2016, end=c(2017,2)))

#Affichage des valeurs de l'échantillon de test et comparaison avec les prédictions
plot(MSETest, xlim=c(2016,2017.25), main="Différences entre les véritables
     valeurs et les prédictions du modèle pour la masse salariale", xaxt="n")
axis(side=1, at=seq(2016,2017.25,0.25), labels=c("2016Q1", "2016Q2", "2016Q3", "2016Q4", "2017Q1", "2017Q2"))
lines(window(PredPIB, start=2016, end=c(2017,2))*MSETrendTest*MSESeasonalTest, col = "red")

#TCHOF
#Selection de l'ordre du modèle VAR en fonction de différents critères
VARorder(cbind(MSESta, TCHOFSta), maxp = 10)

#Ordre 6
#Réalisation d'un modèle d'ordre 6 avec la masse salariale et le taux de chômage
modele<-VAR(cbind(MSESta, TCHOFSta), p=6)
#Vérification de la stabilité du modèle
stabilityMTS(modele)
#Test d'auto-corrélation et de corrélation croisée
crossCorr(modele)
#test du Portmanteau
mq(modele$residuals, lag=10)

#Ordre 3
#Réalisation d'un modèle d'ordre 3 avec la masse salariale et le taux de chômage
modele2<-VAR(cbind(MSESta, TCHOFSta), p=3)
#Vérification de la stabilité du modèle
stabilityMTS(modele2)
#Test d'auto-corrélation et de corrélation croisée
crossCorr(modele2)
#test du Portmanteau
mq(modele2$residuals, lag=10)

#Ordre 2
#Réalisation d'un modèle d'ordre 2 avec la masse salariale et le taux de chômage
modele3<-VAR(cbind(MSESta, TCHOFSta), p=2)
#Vérification de la stabilité du modèle
stabilityMTS(modele3)
#Test d'auto-corrélation et de corrélation croisée
crossCorr(modele3)
#test du Portmanteau
mq(modele2$residuals, lag=10)

#Prédictions
PredTCHOF<-window(ts(VARpred(modele, 8)$pred[,1], start=c(2015,3), frequency=4), start=2016)
EQM(MSETest, window(PredTCHOF*MSETrendTest*MSESeasonalTest, start=2016, end=c(2017,2)))
PredTCHOF2<-window(ts(VARpred(modele2, 8)$pred[,1], start=c(2015,3), frequency=4), start=2016)
EQM(MSETest, window(PredTCHOF2*MSETrendTest*MSESeasonalTest, start=2016, end=c(2017,2)))
PredTCHOF3<-window(ts(VARpred(modele3, 8)$pred[,1], start=c(2015,3), frequency=4), start=2016)
EQM(MSETest, window(PredTCHOF3*MSETrendTest*MSESeasonalTest, start=2016, end=c(2017,2)))

#Affichage des valeurs de l'échantillon de test et comparaison avec les prédictions
plot(MSETest, xlim=c(2016,2017.25), main="Différences entre les véritables
     valeurs et les prédictions du modèle pour la masse salariale", xaxt="n")
axis(side=1, at=seq(2016,2017.25,0.25), labels=c("2016Q1", "2016Q2", "2016Q3", "2016Q4", "2017Q1", "2017Q2"))
lines(window(PredTCHOF, start=2016, end=c(2017,2))*MSETrendTest*MSESeasonalTest, col = "red")

#PIB & SMIC
#Selection de l'ordre du modèle VAR en fonction de différents critères
VARorder(cbind(MSESta, PIBSta, SMICSta), maxp = 10)

#Ordre 3
#Réalisation d'un modèle d'ordre 3 avec la masse salariale, le PIB et le SMIC
modele<-VAR(cbind(MSESta, PIBSta, SMICSta), p=3)
#Vérification de la stabilité du modèle
stabilityMTS(modele)
#Test d'auto-corrélation et de corrélation croisée
crossCorr(modele)
#test du Portmanteau
mq(modele$residuals, lag=10)

#Prédictions
PredPIBSMIC<-window(ts(VARpred(modele, 8)$pred[,1], start=c(2015,3), frequency=4), start=2016)
EQM(MSETest, window(PredPIBSMIC*MSETrendTest*MSESeasonalTest, start=2016, end=c(2017,2)))

#Affichage des valeurs de l'échantillon de test et comparaison avec les prédictions
plot(MSETest, xlim=c(2016,2017.25), main="Différences entre les véritables
     valeurs et les prédictions du modèle pour la masse salariale", xaxt="n")
axis(side=1, at=seq(2016,2017.25,0.25), labels=c("2016Q1", "2016Q2", "2016Q3", "2016Q4", "2017Q1", "2017Q2"))
lines(window(PredPIBSMIC, start=2016, end=c(2017,2))*MSETrendTest*MSESeasonalTest, col = "red")

#PIB & TCHOF
#Selection de l'ordre du modèle VAR en fonction de différents critères
VARorder(cbind(MSESta, PIBSta, TCHOFSta), maxp = 10)

#Ordre 4
#Réalisation d'un modèle d'ordre 4 avec la masse salariale, le PIB et le taux de chômage
modele<-VAR(cbind(MSESta, PIBSta, TCHOFSta), p=4)
#Vérification de la stabilité du modèle
stabilityMTS(modele)
#Test d'auto-corrélation et de corrélation croisée
crossCorr(modele)
#test du Portmanteau
mq(modele$residuals, lag=10)

#Ordre 3
#Réalisation d'un modèle d'ordre 3 avec la masse salariale, le PIB et le taux de chômage
modele2<-VAR(cbind(MSESta, PIBSta, TCHOFSta), p=3)
#Vérification de la stabilité du modèle
stabilityMTS(modele2)
#Test d'auto-corrélation et de corrélation croisée
crossCorr(modele2)
#test du Portmanteau
mq(modele2$residuals, lag=10)

#Ordre 2
#Réalisation d'un modèle d'ordre 2 avec la masse salariale, le PIB et le taux de chômage
modele3<-VAR(cbind(MSESta, PIBSta, TCHOFSta), p=2)
#Vérification de la stabilité du modèle
stabilityMTS(modele3)
#Test d'auto-corrélation et de corrélation croisée
crossCorr(modele3)
#test du Portmanteau
mq(modele3$residuals, lag=10)

#Prédictions
PredPIBTCHOF<-window(ts(VARpred(modele, 8)$pred[,1], start=c(2015,3), frequency=4), start=2016)
EQM(MSETest, window(PredPIBTCHOF*MSETrendTest*MSESeasonalTest, start=2016, end=c(2017,2)))
PredPIBTCHOF2<-window(ts(VARpred(modele2, 8)$pred[,1], start=c(2015,3), frequency=4), start=2016)
EQM(MSETest, window(PredPIBTCHOF2*MSETrendTest*MSESeasonalTest, start=2016, end=c(2017,2)))
PredPIBTCHOF3<-window(ts(VARpred(modele3, 8)$pred[,1], start=c(2015,3), frequency=4), start=2016)
EQM(MSETest, window(PredPIBTCHOF3*MSETrendTest*MSESeasonalTest, start=2016, end=c(2017,2)))

#Affichage des valeurs de l'échantillon de test et comparaison avec les prédictions
plot(MSETest, xlim=c(2016,2017.25), main="Différences entre les véritables
     valeurs et les prédictions du modèle pour la masse salariale", xaxt="n")
axis(side=1, at=seq(2016,2017.25,0.25), labels=c("2016Q1", "2016Q2", "2016Q3", "2016Q4", "2017Q1", "2017Q2"))
lines(window(PredPIB, start=2016, end=c(2017,2))*MSETrendTest*MSESeasonalTest, col = "red")

#SMIC & TCHOF
#Selection de l'ordre du modèle VAR en fonction de différents critères
VARorder(cbind(MSESta, SMICSta, TCHOFSta), maxp = 10)

#Ordre 4
#Réalisation d'un modèle d'ordre 4 avec la masse salariale, le SMIC et le taux de chômage
modele<-VAR(cbind(MSESta, SMICSta, TCHOFSta), p=4)
#Vérification de la stabilité du modèle
stabilityMTS(modele)
#Test d'auto-corrélation et de corrélation croisée
crossCorr(modele)
#test du Portmanteau
mq(modele$residuals, lag=10)

#Ordre 3
#Réalisation d'un modèle d'ordre 3 avec la masse salariale, le SMIC et le taux de chômage
modele2<-VAR(cbind(MSESta, SMICSta, TCHOFSta), p=3)
#Vérification de la stabilité du modèle
stabilityMTS(modele2)
#Test d'auto-corrélation et de corrélation croisée
crossCorr(modele2)
#test du Portmanteau
mq(modele2$residuals, lag=10)

#Prédictions
PredSMICTCHOF<-window(ts(VARpred(modele, 8)$pred[,1], start=c(2015,3), frequency=4), start=2016)
EQM(MSETest, window(PredSMICTCHOF*MSETrendTest*MSESeasonalTest, start=2016, end=c(2017,2)))
PredSMICTCHOF2<-window(ts(VARpred(modele2, 8)$pred[,1], start=c(2015,3), frequency=4), start=2016)
EQM(MSETest, window(PredSMICTCHOF2*MSETrendTest*MSESeasonalTest, start=2016, end=c(2017,2)))

#Affichage des valeurs de l'échantillon de test et comparaison avec les prédictions
plot(MSETest, xlim=c(2016,2017.25), main="Différences entre les véritables
     valeurs et les prédictions du modèle pour la masse salariale", xaxt="n")
axis(side=1, at=seq(2016,2017.25,0.25), labels=c("2016Q1", "2016Q2", "2016Q3", "2016Q4", "2017Q1", "2017Q2"))
lines(window(PredSMICTCHOF, start=2016, end=c(2017,2))*MSETrendTest*MSESeasonalTest, col = "red")

#PIB, SMIC & TCHOF
#Selection de l'ordre du modèle VAR en fonction de différents critères
VARorder(cbind(MSESta, PIBSta, SMICSta, TCHOFSta), maxp = 10)

#Ordre 4
#Réalisation d'un modèle d'ordre 4 avec toutes les variables
modele<-VAR(cbind(MSESta, PIBSta, SMICSta, TCHOFSta), p=4)
#Vérification de la stabilité du modèle
stabilityMTS(modele)
#Test d'auto-corrélation et de corrélation croisée
crossCorr(modele)
#test du Portmanteau
mq(modele$residuals, lag=10)

#Ordre 3
#Réalisation d'un modèle d'ordre 3 avec toutes les variables
modele2<-VAR(cbind(MSESta, PIBSta, SMICSta, TCHOFSta), p=3)
#Vérification de la stabilité du modèle
stabilityMTS(modele2)
#Test d'auto-corrélation et de corrélation croisée
crossCorr(modele2)
#test du Portmanteau
mq(modele2$residuals, lag=10)

#Ordre 2
#Réalisation d'un modèle d'ordre 2 avec toutes les variables
modele3<-VAR(cbind(MSESta, PIBSta, SMICSta, TCHOFSta), p=2)
#Vérification de la stabilité du modèle
stabilityMTS(modele3)
#Test d'auto-corrélation et de corrélation croisée
crossCorr(modele3)
#test du Portmanteau
mq(modele3$residuals, lag=10)

#Prédictions
PredCOMPLET<-window(ts(VARpred(modele, 8)$pred[,1], start=c(2015,3), frequency=4), start=2016)
EQM(MSETest, window(PredCOMPLET*MSETrendTest*MSESeasonalTest, start=2016, end=c(2017,2)))
PredCOMPLET2<-window(ts(VARpred(modele2, 8)$pred[,1], start=c(2015,3), frequency=4), start=2016)
EQM(MSETest, window(PredCOMPLET2*MSETrendTest*MSESeasonalTest, start=2016, end=c(2017,2)))
PredCOMPLET3<-window(ts(VARpred(modele3, 8)$pred[,1], start=c(2015,3), frequency=4), start=2016)
EQM(MSETest, window(PredCOMPLET3*MSETrendTest*MSESeasonalTest, start=2016, end=c(2017,2)))

#Affichage des valeurs de l'échantillon de test et comparaison avec les prédictions
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

#Estimation de l'ordre du modèle
Eccm(cbind(MSESta, PIBSta, SMICSta, TCHOFSta), maxq=5)
#Réalisation d'un modèle d'ordre p=3 et q=1 avec toutes les variables
modele<-VARMA(cbind(MSESta, PIBSta, SMICSta, TCHOFSta), p=3, q=1)
#Elimination des paramètres non significatifs du modèle
modele<-refVARMA(modele)

##################################################################################################