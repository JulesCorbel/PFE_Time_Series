###################################
#							                    #
# STAGE SERIES TEMPORELLES - GIS4 #
#								 				          #
###################################
#Dossier de travail
setwd("C:/Users/Devignes/Desktop/Semestre 8/Stage")
#Netoyage de l'environnement
rm(list=ls())
#Chargement des librairies
library(forecast)
library(TSA)
library(arfima)
library(smooth)
library(Hmisc)
library(astsa)
library(aod)
#Chargement des donnees
data = read.csv2("Data_trimestres.csv", sep = ";", dec = ",")
#Debut des donnees
head(data)
#Fin des donnees
tail(data, 8)
#Transformation des donnees en serie temporelle
data_ts = ts(data$MSE,start=c(1990,1), end=c(2017,2), frequency=4)
data_ts
#Representation graphique de la serie
plot.ts(data_ts,xlab="Année",ylab="Masse salariale de l'entreprise",
        main = "Série temporelle trimestrielle de la masse salariale")
#Les ecarts augmentent au fur et à mesure : multiplicatif

#Test pour savoir si additif ou multiplicatif
data_matrix = c()
#Transformation de la serie en data frame
for (i in 1990:2017){
  annee = window(data_ts, start=c(i,1), end=c(i,4), extend=TRUE)
  print(annee)
  data_matrix = rbind(data_matrix, annee)
}
#Moyenne par trimestre
colMeans(data_matrix, na.rm = TRUE)
#Moyenne par annee
m = rowMeans(data_matrix, na.rm = TRUE)
#Ecart type par annee
s = apply(data_matrix, 1, sd, na.rm = TRUE)
#Modele de regression pour determiner additif ou multiplicatif
modele = lm(m~s)
summary(modele) #P-valeur < 0.05, donc multiplicatif

#Echantillons d'apprentissage et de test
data_appr = window(data_ts, start = c(1990,1), end = c(2008,4))
data_test = window(data_ts, start = c(2009,1), end = c(2017,2))

##################################
#      LISSAGE EXPONENTIEL       #
##################################
#####Methode Holt-Winters saisonnier multiplicatif
modeleHWM = HoltWinters(data_appr, seasonal="mult")
modeleHWM
#Representation graphique
plot(modeleHWM, main ="Holt-Winters multiplicatif (HWM)", xlab = "Année",
     ylab ="Masse salariale de l'entreprise")
legend("topleft", legend=c("Série", "HWM"),
       col=c("black", "red"), lty=1, cex=0.8)
#Calcul de la somme des ecarts au carre moyen
modeleHWM_pred = forecast(modeleHWM, h = length(data_test))
SE = sum((data_test-modeleHWM_pred$mean)**2)
SCE = SE / length(data_test)
SCE
#Representation de la prevision
plot(data_test, main ="Holt-Winters multiplicatif (HWM)", xlab = "Année",
     ylab ="Masse salariale de l'entreprise",
     ylim = c(min(data_test, modeleHWM_pred$mean), max(data_test, modeleHWM_pred$mean)))
legend("bottomright", legend=c("Série", "Prévision"),
       col=c("black", "red"), lty=1, cex=0.8)
lines(modeleHWM_pred$mean, col="red")
mtext(paste0("SCE = ", formatC(SCE, format = "e")), side = 4)
#Criteres de precision de la prevision
accuracy(modeleHWM_pred, data_test)

#Prevision avec hw() du package forecast
modeleHWM_pred2 = hw(data_appr, h=length(data_test), seasonal = "multiplicative")
SE = sum((data_test-modeleHWM_pred2$mean)**2)
SCE = SE / length(data_test)
SCE
#Representation de la prevision
plot(data_test, main ="Holt-Winters multiplicatif avec hw() (HWM)", xlab = "Année",
     ylab ="Masse salariale de l'entreprise",
     ylim = c(min(data_test, modeleHWM_pred2$mean), max(data_test, modeleHWM_pred2$mean)))
legend("bottomright", legend=c("Série", "Prévision"),
       col=c("black", "red"), lty=1, cex=0.8)
lines(modeleHWM_pred2$mean, col="red")
mtext(paste0("SCE = ", formatC(SCE, format = "e")), side = 4)
#Criteres de precision de la prevision
accuracy(modeleHWM_pred2, data_test)

#Modification des tailles d'échantillon d'apprentissage
data_appr = window(data_ts, start = c(1990,1), end = c(2015,4))
data_test = window(data_ts, start = c(2016,1), end = c(2017,2))

#####Methode Holt-Winters saisonnier multiplicatif
modeleHWM = HoltWinters(data_appr, seasonal="mult")
modeleHWM
#Representation graphique
plot(modeleHWM, main ="Holt-Winters multiplicatif (HWM)", xlab = "Année",
     ylab ="Masse salariale de l'entreprise")
legend("topleft", legend=c("Série", "HWM"),
       col=c("black", "red"), lty=1, cex=0.8)
#Calcul de la somme des ecarts au carre moyen
modeleHWM_pred = forecast(modeleHWM, h = length(data_test))
SE = sum((data_test-modeleHWM_pred$mean)**2)
SCE = SE / length(data_test)
SCE
#Representation de la prevision
plot(data_test, main ="Holt-Winters multiplicatif (HWM)", xlab = "Année",
     ylab ="Masse salariale de l'entreprise",
     ylim = c(min(data_test, modeleHWM_pred$mean), max(data_test, modeleHWM_pred$mean)))
legend("bottomright", legend=c("Série", "Prévision"),
       col=c("black", "red"), lty=1, cex=0.8)
lines(modeleHWM_pred$mean, col="red")
mtext(paste0("SCE = ", formatC(SCE, format = "e")), side = 4)
#Criteres de precision de la prevision
accuracy(modeleHWM_pred, data_test)

#Prevision avec hw() du package forecast
modeleHWM_pred2 = hw(data_appr, h=length(data_test), seasonal = "multiplicative")
#Calcul de la somme des ecarts au carre moyen
SE = sum((data_test-modeleHWM_pred2$mean)**2)
SCE = SE / length(data_test)
SCE
#Representation de la prevision
plot(data_test, main ="Holt-Winters multiplicatif avec hw() (HWM)", xlab = "Année",
     ylab ="Masse salariale de l'entreprise",
     ylim = c(min(data_test, modeleHWM_pred2$mean), max(data_test, modeleHWM_pred2$mean)))
legend("bottomright", legend=c("Série", "Prévision"),
       col=c("black", "red"), lty=1, cex=0.8)
lines(modeleHWM_pred2$mean, col="red")
mtext(paste0("SCE = ", formatC(SCE, format = "e")), side = 4)
#Criteres de precision de la prevision
accuracy(modeleHWM_pred2, data_test)

############################ SMIC ###############################
#Transformation des donnees en serie temporelle
data_ts = ts(data$SMIC,start=c(1990,1), end=c(2017,4), frequency=4)
data_ts
#Representation graphique de la serie
plot.ts(data_ts,xlab="Année",ylab="SMIC horaire",
        main = "Série temporelle trimestrielle du SMIC horaire")
#Il ne semblent pas y avoir de saisonnalité

#Echantillons d'apprentissage et de test
data_appr = window(data_ts, start = c(1990,1), end = c(2015,4))
data_test = window(data_ts, start = c(2016,1), end = c(2017,4))

#####Lissage exponentiel double
#Recherche du alpha optimal
#Initialisation
a = 0
modele = HoltWinters(data_appr, alpha = 1 - a^2, beta = (1-a)/(1+a), gamma=FALSE)
SSEb = modele$SSE
#Boucle de 0.01 à 0.99
for(i in seq(0.01,0.99,0.01)){
  modele = HoltWinters(data_appr, alpha = 1 - i^2, beta = (1-i)/(1+i), gamma=FALSE)
  if(modele$SSE < SSEb){
    SSEb = modele$SSE
    a = i
  }
}
#Realisation du modele et de la prevision
modeleD = HoltWinters(data_appr, alpha = 1 - a^2, beta = (1-a)/(1+a), gamma=FALSE)
modeleD
#Representation graphique
plot(modeleD, main ="Lissage exponentiel double (LED)", xlab = "Année",
     ylab ="SMIC horaire")
legend("topleft", legend=c("Série", "LED"),
       col=c("black", "red"), lty=1, cex=0.8)
#Calcul de la somme des ecarts au carre moyen
modeleD_pred = forecast(modeleD, h = length(data_test))
SE = sum((data_test-modeleD_pred$mean)**2)
SCE = SE / length(data_test)
SCE
#Representation de la prevision
plot(data_test, main ="Lissage exponentiel double (LED)", xlab = "Année",
     ylab ="SMIC horaire",
     ylim = c(min(data_test, modeleD_pred$mean), max(data_test, modeleD_pred$mean)))
legend("topleft", legend=c("Série", "Prévision"),
       col=c("black", "red"), lty=1, cex=0.8)
lines(modeleD_pred$mean, col="red")
mtext(paste0("SCE = ", round(SCE,5)), side = 4)

#####Lissage de Holt-Winters non saisonnier
modeleHWNS = HoltWinters(data_appr, gamma=FALSE)
modeleHWNS
#Representation graphique
plot(modeleHWNS, main ="Holt-Winters non saisonnier (HWNS)", xlab = "Année",
     ylab ="SMIC horaire")
legend("topleft", legend=c("Série", "HWNS"),
       col=c("black", "red"), lty=1, cex=0.8)
#Calcul de la somme des ecarts au carre moyen
modeleHWNS_pred = forecast(modeleHWNS, h = length(data_test))
SE = sum((data_test-modeleHWNS_pred$mean)**2)
SCE = SE / length(data_test)
SCE
#Representation de la prevision
plot(data_test, main ="Holt-Winters non saisonnier (HWNS)", xlab = "Année",
     ylab ="SMIC horaire",
     ylim = c(min(data_test, modeleHWNS_pred$mean), max(data_test, modeleHWNS_pred$mean)))
legend("topleft", legend=c("Série", "Prévision"),
       col=c("black", "red"), lty=1, cex=0.8)
lines(modeleHWNS_pred$mean, col="red")
mtext(paste0("SCE = ", round(SCE,5)), side = 4)

#Test si serie est stationnaire
acf(data_appr, lag = 40, main = "SMIC horaire")
Box.test(data_appr, type="Ljung-Box", lag = 20)

#Methode des differences
# On enlève la tendance
data_diff = diff(data_appr, lag = 1, differences = 1)
plot(data_diff, main = "Série corrigée de la tendance")
#Test bruit blanc
acf(data_diff, lag = 40, main = "Méthode des différences")
pacf(data_diff, lag = 40, main = "Méthode des différences")
Box.test(data_diff, type="Ljung-Box", lag = 20)

modeleARIMA = auto.arima(data_appr, seasonal = F)
modeleARIMA
#Representation graphique
plot(data_appr, main ="ARIMA", xlab = "Année",
     ylab ="SMIC horaire")
lines(modeleARIMA$fitted, col = "red")
legend("topleft", legend=c("Série", "ARIMA"),
       col=c("black", "red"), lty=1, cex=0.8)
#Calcul de la somme des ecarts au carre moyen
modeleARIMA_pred = forecast(modeleARIMA, h=length(data_test))
SE = sum((data_test-modeleARIMA_pred$mean)**2)
SCE = SE / length(data_test)
SCE
#Representation de la prevision
plot(data_test, main ="ARIMA", xlab = "Année",
     ylab ="SMIC horaire",
     ylim = c(min(data_test, modeleARIMA_pred$mean), max(data_test, modeleARIMA_pred$mean)))
legend("topleft", legend=c("Série", "Prévision"),
       col=c("black", "red"), lty=1, cex=0.8)
lines(modeleARIMA_pred$mean, col="red")
mtext(paste0("SCE = ", round(SCE,2)), side = 4)

############################ PIB ###############################
#Transformation des donnees en serie temporelle
data_ts = ts(data$PIB,start=c(1990,1), end=c(2017,4), frequency=4)
data_ts
#Representation graphique de la serie
plot.ts(data_ts,xlab="Année",ylab="PIB en volume",
        main = "Série temporelle trimestrielle du PIB en volume")
#Il ne semblent pas y avoir de saisonnalité

#Echantillons d'apprentissage et de test
data_appr = window(data_ts, start = c(1990,1), end = c(2015,4))
data_test = window(data_ts, start = c(2016,1), end = c(2017,4))

#####Lissage exponentiel double
#Recherche du alpha optimal
#Initialisation
a = 0
modele = HoltWinters(data_appr, alpha = 1 - a^2, beta = (1-a)/(1+a), gamma=FALSE)
SSEb = modele$SSE
#Boucle de 0.01 à 0.99
for(i in seq(0.01,0.99,0.01)){
  modele = HoltWinters(data_appr, alpha = 1 - i^2, beta = (1-i)/(1+i), gamma=FALSE)
  if(modele$SSE < SSEb){
    SSEb = modele$SSE
    a = i
  }
}
#Realisation du modele et de la prevision
modeleD = HoltWinters(data_appr, alpha = 1 - a^2, beta = (1-a)/(1+a), gamma=FALSE)
modeleD
#Representation graphique
plot(modeleD, main ="Lissage exponentiel double (LED)", xlab = "Année",
     ylab ="PIB en volume")
legend("topleft", legend=c("Série", "LED"),
       col=c("black", "red"), lty=1, cex=0.8)
#Calcul de la somme des ecarts au carre moyen
modeleD_pred = forecast(modeleD, h = length(data_test))
SE = sum((data_test-modeleD_pred$mean)**2)
SCE = SE / length(data_test)
SCE
#Representation de la prevision
plot(data_test, main ="Lissage exponentiel double (LED)", xlab = "Année",
     ylab ="PIB en volume",
     ylim = c(min(data_test, modeleD_pred$mean), max(data_test, modeleD_pred$mean)))
legend("topleft", legend=c("Série", "Prévision"),
       col=c("black", "red"), lty=1, cex=0.8)
lines(modeleD_pred$mean, col="red")
mtext(paste0("SCE = ", round(SCE,2)), side = 4)

#####Lissage de Holt-Winters non saisonnier
modeleHWNS = HoltWinters(data_appr, gamma=FALSE)
modeleHWNS
#Representation graphique
plot(modeleHWNS, main ="Holt-Winters non saisonnier (HWNS)", xlab = "Année",
     ylab ="PIB en volume")
legend("topleft", legend=c("Série", "HWNS"),
       col=c("black", "red"), lty=1, cex=0.8)
#Calcul de la somme des ecarts au carre moyen
modeleHWNS_pred = forecast(modeleHWNS, h = length(data_test))
SE = sum((data_test-modeleHWNS_pred$mean)**2)
SCE = SE / length(data_test)
SCE
#Representation de la prevision
plot(data_test, main ="Holt-Winters non saisonnier (HWNS)", xlab = "Année",
     ylab ="PIB en volume",
     ylim = c(min(data_test, modeleHWNS_pred$mean), max(data_test, modeleHWNS_pred$mean)))
legend("topleft", legend=c("Série", "Prévision"),
       col=c("black", "red"), lty=1, cex=0.8)
lines(modeleHWNS_pred$mean, col="red")
mtext(paste0("SCE = ", round(SCE,2)), side = 4)

#Test si serie est stationnaire
acf(data_appr, lag = 40, main = "PIB en volume")
Box.test(data_appr, type="Ljung-Box", lag = 20)

#Methode des differences
# On enlève la tendance
data_diff = diff(data_appr, lag = 1, differences = 1)
plot(data_diff, main = "Série corrigée de la tendance")
#Test bruit blanc
acf(data_diff, lag = 40, main = "Méthode des différences")
pacf(data_diff, lag = 40, main = "Méthode des différences")
Box.test(data_diff, type="Ljung-Box", lag = 20)

modeleARIMA = auto.arima(data_appr, seasonal = F)
modeleARIMA
#Representation graphique
plot(data_appr, main ="ARIMA", xlab = "Année",
     ylab ="PIB en volume")
lines(modeleARIMA$fitted, col = "red")
legend("topleft", legend=c("Série", "ARIMA"),
       col=c("black", "red"), lty=1, cex=0.8)
#Calcul de la somme des ecarts au carre moyen
modeleARIMA_pred = forecast(modeleARIMA, h=length(data_test))
SE = sum((data_test-modeleARIMA_pred$mean)**2)
SCE = SE / length(data_test)
SCE
#Representation de la prevision
plot(data_test, main ="ARIMA", xlab = "Année",
     ylab ="PIB en volume",
     ylim = c(min(data_test, modeleARIMA_pred$mean), max(data_test, modeleARIMA_pred$mean)))
legend("topleft", legend=c("Série", "Prévision"),
       col=c("black", "red"), lty=1, cex=0.8)
lines(modeleARIMA_pred$mean, col="red")
mtext(paste0("SCE = ", round(SCE,2)), side = 4)

############################ TCHOF ###############################
#Transformation des donnees en serie temporelle
data_ts = ts(data$TCHOF,start=c(1990,1), end=c(2017,4), frequency=4)
data_ts

#Representation graphique de la serie
plot.ts(data_ts,xlab="Année",ylab="Taux de chômage (en %)",
        main = "Série temporelle trimestrielle du taux de chômage en France")
#Il ne semblent pas y avoir de saisonnalité

#Echantillons d'apprentissage et de test
data_appr = window(data_ts, start = c(1990,1), end = c(2015,4))
data_test = window(data_ts, start = c(2016,1), end = c(2017,4))

#####Lissage exponentiel simple
modeleS = HoltWinters(data_appr, beta=FALSE, gamma=FALSE)
modeleS
#Representation graphique
plot(modeleS, main ="Lissage exponentiel simple (LES)", xlab = "Année",
     ylab ="Taux de chômage")
legend("bottomleft", legend=c("Série", "LES"),
       col=c("black", "red"), lty=1, cex=0.8)
#Calcul de la somme des ecarts au carre moyen
modeleS_pred = forecast(modeleS, h=length(data_test))
#Criteres de precision de la prevision
accuracy(modeleS_pred)
SE = sum((data_test-modeleS_pred$mean)**2)
SCE = SE / length(data_test)
SCE
#Representation de la prevision
plot(data_test, main ="Lissage exponentiel simple (LES)", xlab = "Année",
     ylab ="Taux de chômage")
legend("topleft", legend=c("Série", "Prévision"),
       col=c("black", "red"), lty=1, cex=0.8)
lines(modeleS_pred$mean, col="red")
mtext(paste0("SCE = ", round(SCE,2)), side = 4)

#Test si serie est stationnaire
acf(data_appr, lag = 40, main = "Taux de chômage")
Box.test(data_appr, type="Ljung-Box", lag = 20)

modeleARIMA = auto.arima(data_appr, seasonal = F)
modeleARIMA
#Representation graphique
plot(data_appr, main ="ARIMA", xlab = "Année",
     ylab ="Taux de chômage")
lines(modeleARIMA$fitted, col = "red")
legend("topright", legend=c("Série", "ARIMA"),
       col=c("black", "red"), lty=1, cex=0.8)
#Calcul de la somme des ecarts au carre moyen
modeleARIMA_pred = forecast(modeleARIMA, h=length(data_test))
SE = sum((data_test-modeleARIMA_pred$mean)**2)
SCE = SE / length(data_test)
SCE
#Representation de la prevision
plot(data_test, main ="ARIMA", xlab = "Année",
     ylab ="Taux de chômage",
     ylim = c(min(data_test, modeleARIMA_pred$mean), max(data_test, modeleARIMA_pred$mean)))
legend("topright", legend=c("Série", "Prévision"),
       col=c("black", "red"), lty=1, cex=0.8)
lines(modeleARIMA_pred$mean, col="red")
mtext(paste0("SCE = ", round(SCE,2)), side = 4)

############### TEST PACKAGE SMOOTH #################

#Transformation des donnees en serie temporelle
data_ts = ts(data$MSE,start=c(1990,1), end=c(2017,2), frequency=4)
#Echantillons d'apprentissage et de test
data_appr = window(data_ts, start = c(1990,1), end = c(2015,4))
data_test = window(data_ts, start = c(2016,1), end = c(2017,2))

####### ES #########
test = es(data_appr)
test
#Representation graphique
plot(data_appr, main ="ES", xlab = "Année", ylab ="Masse salariale de l'entreprise")
lines(test$fitted, col = "red")
legend("topleft", legend=c("Série", "ES"),
       col=c("black", "red"), lty=1, cex=0.8)
#Calcul de la somme des ecarts au carre moyen
test_pred = forecast(test, h = length(data_test))
v = as.ts(test_pred$mean)
SE = sum((data_test-v)**2)
SCE = SE / length(data_test)
SCE
#Representation de la prevision
plot(data_test, main ="ES", xlab = "Année",
     ylab ="Masse salariale de l'entreprise",
     ylim = c(min(data_test, v), max(data_test, v)))
legend("bottomright", legend=c("Série", "Prévision"),
       col=c("black", "red"), lty=1, cex=0.8)
lines(v, col="red")
mtext(paste0("SCE = ", formatC(SCE, format = "e")), side = 4)
#Criteres de precision de la prevision
accuracy(test_pred, data_test)

####### ES AVEC REGRESSEURS #########
# Regresseurs
reg_appr = data[c(1:length(data_appr)),c(3:5)]
reg_test = data[-c(1:length(data_appr)),c(3:5)]
#Modele
test = es(data_appr, model = "CCC",xreg=reg_appr)
test

#Representation graphique
plot(data_appr, main ="ES avec variables explicatives", xlab = "Année",
     ylab ="Masse salariale de l'entreprise")
lines(test$fitted, col = "red")
legend("topleft", legend=c("Série", "ES"),
       col=c("black", "red"), lty=1, cex=0.8)
#Calcul de la somme des ecarts au carre moyen
test_pred = forecast(test, xreg = reg_test, h = nrow(reg_test))
v = as.ts(test_pred$mean)
SE = sum((data_test-v)**2)
SCE = SE / length(data_test)
SCE
#Representation de la prevision
plot(data_test, main ="ES avec variables explicatives", xlab = "Année",
     ylab ="Masse salariale de l'entreprise",
     ylim = c(min(data_test, v), max(data_test, v)),
     xlim = c(2016, 2018))
legend("bottomright", legend=c("Série", "Prévision"),
       col=c("black", "red"), lty=1, cex=0.8)
lines(v, col="red")
mtext(paste0("SCE = ", formatC(SCE, format = "e")), side = 4)
#Criteres de precision de la prevision
accuracy(test_pred, data_test)

##################################
#         PROCESSUS ARMA         #
##################################

#Test si serie est stationnaire
acf(data_appr, lag = 60, main = "Masse salariale de l'entreprise")
Box.test(data_appr, type="Ljung-Box", lag = 20)

#Méthode de la décomposition
data_decomp = decompose(data_appr, type = "multiplicative")
#Representation graphique
plot(data_decomp)
#Test bruit blanc
acf(data_decomp$random, lag = 40, main = "Méthode de la décomposition", na.action = na.pass)
pacf(data_decomp$random, lag = 40, main = "Méthode de la décomposition", na.action = na.pass)
Box.test(data_decomp$random, type="Ljung-Box", lag = 20)
#Methode des differences
# On enlève la saisonnalité
data_diff = diff(data_appr, lag = 4, differences = 1)
# On enlève la tendance
data_diff = diff(data_diff, lag = 1, differences = 1)
plot(data_diff, main = "Série corrigée de la tendance et de la saisonnalité")
#Test bruit blanc
acf(data_diff, lag = 40, main = "Méthode des différences")
pacf(data_diff, lag = 40, main = "Méthode des différences")
Box.test(data_diff, type="Ljung-Box", lag = 20)

#####Modele SARIMA
modeleSARIMA = auto.arima(data_appr)
modeleSARIMA
#Representation graphique
plot(data_appr, main ="SARIMA", xlab = "Année",
     ylab ="Masse salariale de l'entreprise")
lines(modeleSARIMA$fitted, col = "red")
legend("topleft", legend=c("Série", "SARIMA"),
       col=c("black", "red"), lty=1, cex=0.8)
#Calcul de la somme des ecarts au carre moyen
modeleSARIMA_pred = forecast(modeleSARIMA, h=length(data_test))
SE = sum((data_test-modeleSARIMA_pred$mean)**2)
SCE = SE / length(data_test)
SCE
#Representation de la prevision
plot(data_test, main ="SARIMA", xlab = "Année",
     ylab ="Masse salariale de l'entreprise",
     ylim = c(min(data_test, modeleSARIMA_pred$mean), max(data_test, modeleSARIMA_pred$mean)))
legend("bottomright", legend=c("Série", "Prévision"),
       col=c("black", "red"), lty=1, cex=0.8)
lines(modeleSARIMA_pred$mean, col="red")
mtext(paste0("SCE = ", formatC(SCE, format = "e")), side = 4)
#Criteres de precision de la prevision
accuracy(modeleSARIMA_pred, data_test)

#####Modele SARIMA avec regresseurs
reg_appr = data[c(1:length(data_appr)),c(5)]
reg_test = data[-c(1:length(data_appr)),c(5)]
modeleSARIMA_exp = auto.arima(data_appr, xreg = reg_appr)
modeleSARIMA_exp
#Representation graphique
plot(data_appr, main ="SARIMA avec le taux de chômage", xlab = "Année",
     ylab ="Masse salariale de l'entreprise")
lines(modeleSARIMA_exp$fitted, col = "red")
legend("topleft", legend=c("Série", "SARIMA"),
       col=c("black", "red"), lty=1, cex=0.8)
#Calcul de la somme des ecarts au carre moyen
modeleSARIMA_exp_pred = forecast(modeleSARIMA_exp, xreg = reg_test)
SE = sum((data_test-modeleSARIMA_exp_pred$mean)**2)
SCE = SE / length(data_test)
SCE
#Representation de la prevision
plot(data_test, main ="Modele SARIMA avec le taux de chômage", xlab = "Année",
     ylab ="Masse salariale de l'entreprise", xlim = c(2016,2018),
     ylim = c(min(data_test, modeleSARIMA_exp_pred$mean), max(data_test, modeleSARIMA_exp_pred$pred)))
legend("bottomright", legend=c("Série", "Prévision"),
       col=c("black", "red"), lty=1, cex=0.8)
lines(modeleSARIMA_exp_pred$mean, col="red")
mtext(paste0("SCE = ", formatC(SCE, format = "e")), side = 4)
#Criteres de precision de la prevision
accuracy(modeleSARIMA_exp_pred, data_test)

#MEILLEUR AVEC VARS EXPLICATIVES
#Analyse des ecarts entre valeurs predites et reelles
diffSARIMA = data_test - modeleSARIMA_exp_pred$mean
diffES = data_test - test_pred$mean
diff = diffES
for(i in 1:length(diffES)){
  if(abs(diffES[i]) < abs(diffSARIMA[i])){
    diff[i] = "ES"
  }
  else {
    diff[i] = "SARIMA"
  }
}
diffSARIMA
diffES
diff
modeleSARIMA_exp_pred$mean[7:8]
test_pred$mean[7:8]
round((modeleSARIMA_exp_pred$mean[7:8] - test_pred$mean[7:8])/
        modeleSARIMA_exp_pred$mean[7:8]*100,2)

######### FONCTION DE TRANSFERT #####
reg_appr = data[c(1:length(data_appr)),c(3:5)]
reg_test = data[-c(1:length(data_appr)),c(3:5)]
modeleARIMAX=arimax(data_appr,order=c(0,1,1),seasonal=list(order=c(0,1,1),period=4),
              xtransf = reg_appr[,2],
              transfer = list(c(2,0)))
modeleARIMAX
#Representation graphique
plot(data_appr, main ="Fonction de transfert avec le PIB", xlab = "Année",
     ylab ="Masse salariale de l'entreprise")
lines(fitted(modeleARIMAX), col = "red")
legend("topleft", legend=c("Série", "Fonction de transfert"),
       col=c("black", "red"), lty=1, cex=0.8)
#Calcul de la somme des ecarts au carre moyen
modeleARIMAX_pred = forecast(fitted(modeleARIMAX), h = nrow(reg_test))
SE = sum((data_test-modeleARIMAX_pred$mean)**2)
SCE = SE / length(data_test)
SCE
#Representation de la prevision
plot(data_test, main ="Fonction de transfert avec le PIB", xlab = "Année",
     ylab ="Masse salariale de l'entreprise", xlim = c(2016,2018),
     ylim = c(min(data_test, modeleARIMAX_pred$mean), max(data_test, modeleARIMAX_pred$pred)))
legend("topright", legend=c("Série", "Prévision"),
       col=c("black", "red"), lty=1, cex=0.8)
lines(modeleARIMAX_pred$mean, col="red")
mtext(paste0("SCE = ", formatC(SCE, format = "e")), side = 4)

######### SIMULATION #########
#Processus SARIMA
reg_appr = data[c(1:length(data_appr)),c(5)]
reg_test = data[-c(1:length(data_appr)),c(5)]
#SARIMA associe au meilleur processus trouve precedemment
SARIMA = Arima(data_appr, order=c(0,0,0),
      seasonal=list(order=c(2,1,0),period=4), xreg = reg_appr, include.constant = T)
#Enregistrement du drift pour le remettre plus tard
drift = as.numeric(SARIMA$coef["drift"])
#On retire le drift pour pouvoir faire la simulation, sinon simulate() marche pas
SARIMA$xreg = matrix(reg_appr, dimnames = list(NULL, "reg_appr"))
#Initialisation des valeurs simulees par le processus
simu = ts(0, start = c(1990, 1), end = c(2015,4), frequency = 4)
#Initialisation des valeurs predites par le processus
simu_prev = ts(0, start = c(2016, 1), end = c(2017,4), frequency = 4)
#Nombre d'iterations
n = 100
for(i in 1:n){
  #Simulation
  sim = simulate(SARIMA, future = F)
  #Ajout du drift
  sim = sim[,1] + drift
  simu = simu + sim
  simu_prev = simu_prev + forecast(sim)$mean
}
#Moyenne des valeurs
simu = simu/n
simu_prev = simu_prev/n
#Representation graphique
plot(data_appr, main ="Simulation du processus SARIMA", xlab = "Année",
     ylab ="Masse salariale de l'entreprise",
     ylim = c(min(data_appr, sim), max(data_appr, sim)))
lines(sim, col = "red")
legend("topleft", legend=c("Série", "Simulation"),
       col=c("black", "red"), lty=1, cex=0.8)
#Representation de la prevision
plot(data_test, main ="Simulation du processus SARIMA", xlab = "Année",
     ylab ="Masse salariale de l'entreprise", xlim = c(2016,2018.5),
     ylim = c(min(data_test, forecast(sim)$mean), max(data_test, forecast(sim)$mean)))
lines(forecast(sim)$mean, col = "red")
legend("topright", legend=c("Série", "Simulation"),
       col=c("black", "red"), lty=1, cex=0.8)

