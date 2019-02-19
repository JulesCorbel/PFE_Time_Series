#Ce script correspond à la modélisation des différentes séries temporelles

#rm(list=ls())
setwd("PFE_Time_Series")
library(tseries)
library(forecast)
library(corrplot)

annuelle <- read.csv("~/Cygwin/app/home/Jules/PFE_Time_Series/Data/Data_Annuel.csv", sep=";", dec=",", nrows=30)
trim <- read.csv("~/Cygwin/app/home/Jules/PFE_Time_Series/Data/Data_Trim.csv", sep=";", dec=",")

cor.mtest <- function(mat, conf.level = 0.95){
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat <- lowCI.mat <- uppCI.mat <- matrix(NA, n, n)
  diag(p.mat) <- 0
  diag(lowCI.mat) <- diag(uppCI.mat) <- 1
  for(i in 1:(n-1)){
    for(j in (i+1):n){
      tmp <- cor.test(mat[,i], mat[,j], conf.level = conf.level)
      p.mat[i,j] <- p.mat[j,i] <- tmp$p.value
      lowCI.mat[i,j] <- lowCI.mat[j,i] <- tmp$conf.int[1]
      uppCI.mat[i,j] <- uppCI.mat[j,i] <- tmp$conf.int[2]
    }
  }
  return(list(p.mat, lowCI.mat, uppCI.mat))
}
res<-cor.mtest(annuelle, 0.95)
res

#Visualisation de la matrice de correlation
corrplot(cor(annuelle[1:28,]), method = "number", type="lower", p.mat=res[[1]], insig="pch", col=colorRampPalette(c("blue", "light blue", "red"))(50))

###Séries temporelles MSE
MSEAnn<-visualisation(annuelle$MSE, 1990, 2017, 1, "de la masse salariale annuelle")
MSETrim<-visualisation(trim$MSE, 1990, c(2017,2), 4, "de la masse salariale trimestrielle")

MSEAnnTrain <- window(MSEAnn, start=1990, end=2015)
MSETrimTrain <- window(MSETrim, start=1990, end=c(2015,4))
MSEAnnTest <- window(MSEAnn, start=2016)
MSETrimTest <- window(MSETrim, start=2016, end=c(2017,2))

##Lissage Exponentiel
MSEAnnPred<-lissage_exponentiel(MSEAnnTrain, 1990, 2015, 1, 2)

plot(MSEAnnTest, type='l', ylim=c(min(MSEAnnTest,MSEAnnPred$mean),max(MSEAnnTest,MSEAnnPred$mean)),
     main="Comparaison entre la prédiction du lissage exponentiel et 
     les valeurs réelles pour la masse salariale annuelle")
lines(MSEAnnPred$mean, col="red")

plot(MSEAnnTrain, type="l", ylim=c(min(MSEAnnTrain,MSEAnnPred$fitted),max(MSEAnnTrain,MSEAnnPred$fitted)),
     main="Comparaison entre les données prédites par le lissage et 
     les valeurs réelles pour la masse salariale annuelle")
lines(MSEAnnPred$fitted, col="red")

MSETrimPred<-lissage_exponentiel(MSETrimTrain, 1990, 2015, 4, 6)

plot(MSETrimTest, type='l', ylim=c(min(MSETrimTest,MSETrimPred$mean),max(MSETrimTest,MSETrimPred$mean)),
     main="Comparaison entre la prédiction du lissage exponentiel et
     les valeurs réelles pour la masse salariale trimestrielle")
lines(MSETrimPred$mean, col='red')

##Modèles SARIMA
#Train

ARIMAMSETrimTrain <- auto.arima(MSETrimTrain, stationary = F)
plot(MSETrimTrain, main="Comparaison entre le modèle SARIMA et les données
    d'apprentissage pour la masse salariale trimestrielle")
lines(ARIMAMSETrimTrain$fitted, col="red")

ARIMAMSEAnnTrain <- auto.arima(MSEAnnTrain, stationary = F)
plot(MSEAnnTrain, main="Comparaison entre le modèle ARIMA et les données
    d'apprentissage pour la masse salariale annuelle", 
    ylim=c(min(MSEAnnTrain,ARIMAMSEAnnTrain$fitted),max(MSEAnnTrain,ARIMAMSEAnnTrain$fitted)))
lines(ARIMAMSEAnnTrain$fitted, col="red")

#Test
ARIMAMSETrimTest <- forecast(ARIMAMSETrimTrain, h=6)
plot(MSETrimTest, main="Comparaison entre le modèle SARIMA et les données de
    validation pour la masse salariale trimestrielle")
lines(ARIMAMSETrimTest$mean, col="red")

ARIMAMSEAnnTest <- forecast(ARIMAMSEAnnTrain, h = 2)
plot(MSEAnnTest, main="Comparaison entre le modèle ARIMA et les données de 
    validation pour la masse salariale annuelle",
    ylim=c(min(MSEAnnTest,ARIMAMSEAnnTest$mean),max(MSEAnnTest,ARIMAMSEAnnTest$mean)))
lines(ARIMAMSEAnnTest$mean, col="red")

##Choix du modèle
best_model(MSEAnnTest, MSEAnnPred$mean, ARIMAMSEAnnTest$mean)
#-> Lissage Exponentiel
best_model(MSETrimTest, MSETrimPred$mean, ARIMAMSETrimTest$mean)
#-> Lissage Exponentiel

###SMIC
SMICAnn<-visualisation(annuelle$SMIC, 1990, 2019, 1, "du SMIC annuel")
SMICTrim<-visualisation(trim$SMIC, 1990, 2017, 4, "du SMIC trimestriel")

#On sépare les séries : modélisations jusque 2015, prévision à partir de 2016

SMICAnnTrain <- window(SMICAnn, start=1990, end=2015)
SMICTrimTrain <- window(SMICTrim, start=1990, end=c(2015,4))
SMICAnnTest <- window(SMICAnn, start=2016)
SMICTrimTest <- window(SMICTrim, start=2016)

##Lissage exponentiel
SMICAnnPred<-lissage_exponentiel(SMICAnnTrain, 1990, 2015, 1, 4)

plot(SMICAnnTest, type='l', 
     ylim=c(min(SMICAnnTest,SMICAnnPred$mean),max(SMICAnnTest,SMICAnnPred$mean)),
     main="Comparaison entre la prédiction du lissage exponentiel et les 
      valeurs réelles pour le SMIC annuel")
lines(SMICAnnPred$mean, col="red")

SMICTrimPred<-lissage_exponentiel(SMICTrimTrain, 1990, 2015, 1, 8)

plot(SMICTrimTest, type='l', 
     ylim=c(min(SMICTrimTest,SMICTrimPred$mean),max(SMICTrimTest,SMICTrimPred$mean)),
     main="Comparaison entre la prédiction du lissage exponentiel et les
      valeurs réelles pour le SMIC trimestriel")
lines(SMICTrimPred$mean, col='red')

##Modèles SARIMA

#Train

ARIMASMICTrimTrain <- auto.arima(SMICTrimTrain, stationary = F)
plot(SMICTrimTrain, main="Comparaison entre le modèle SARIMA et les données 
      d'apprentissage pour le SMIC trimestriel")
lines(ARIMASMICTrimTrain$fitted, col="red")

ARIMASMICAnnTrain <- auto.arima(SMICAnnTrain, stationary = F)
plot(SMICAnnTrain, main="Comparaison entre le modèle ARIMA et les données
      d'apprentissage pour le SMIC annuel")
lines(ARIMASMICAnnTrain$fitted, col="red")

#Test
ARIMASMICTrimTest <- forecast(ARIMASMICTrimTrain, h = 8)
plot(SMICTrimTest, main="Comparaison entre le modèle SARIMA et les données de
      validation pour le SMIC trimestriel", 
     ylim=c(min(SMICTrimTest,ARIMASMICTrimTest$mean),max(SMICTrimTest,ARIMASMICTrimTest$mean)))
lines(ARIMASMICTrimTest$mean, col="red")

ARIMASMICAnnTest <- forecast(ARIMASMICAnnTrain, h = 4)
plot(SMICAnnTest, main="Comparaison entre le modèle ARIMA et les données de
      validation pour le SMIC annuel", 
     ylim=c(min(SMICAnnTest,ARIMASMICAnnTest$mean),max(SMICAnnTest,ARIMASMICAnnTest$mean)))
lines(ARIMASMICAnnTest$mean, col="red")

##Choix du modèle
best_model(SMICAnnTest, SMICAnnPred$mean, ARIMASMICAnnTest$mean)
#-> modèle ARIMA
best_model(SMICTrimTest, SMICTrimPred$mean, ARIMASMICTrimTest$mean)
#-> modèle SARIMA

###PIB
PIBAnn<-visualisation(annuelle$PIB, 1990, 2019, 1, "du PIB annuel")
PIBTrim<-visualisation(trim$PIB, 1990, 2017, 4, "du PIB trimestriel")
PIBTrim<-window(PIBTrim, end=c(2017,1))

#On sépare les séries : modélisations jusque 2015, prévision à partir de 2016
PIBAnnTrain <- window(PIBAnn, start=1990, end=2015)
PIBTrimTrain <- window(PIBTrim, start=1990, end=c(2015,4))
PIBAnnTest <- window(PIBAnn, start=2016)
PIBTrimTest <- window(PIBTrim, start=2016, end=c(2017,1))

##Lissage Exponentiel
PIBAnnPred<-lissage_exponentiel(PIBAnnTrain, 1990, 2015, 1, 4)

plot(PIBAnnTest, type='l',
     ylim=c(min(PIBAnnTest,PIBAnnPred$mean),max(PIBAnnTest,PIBAnnPred$mean)),
     main="Comparaison entre la prédiction du lissage exponentiel et les 
      valeurs réelles pour le PIB annuel")
lines(PIBAnnPred$mean, col="red")

PIBTrimPred<-lissage_exponentiel(PIBTrimTrain, 1990, 2015, 1, 6)

plot(PIBTrimTest, type='l', 
     ylim=c(min(PIBTrimTest,PIBTrimPred$mean),max(PIBTrimTest,PIBTrimPred$mean)),
     main="Comparaison entre la prédiction du lissage exponentiel et les
      valeurs réelles pour le PIB trimestriel")
lines(PIBTrimPred$mean, col='red')

##Modèles SARIMA

#Train
ARIMAPIBTrimTrain <- auto.arima(PIBTrimTrain, stationary = F)
plot(PIBTrimTrain, main="Comparaison entre le modèle SARIMA et les données 
      d'apprentissage pour le PIB trimestriel")
lines(ARIMAPIBTrimTrain$fitted, col="red")

ARIMAPIBAnnTrain <- auto.arima(PIBAnnTrain, stationary = F)
plot(PIBAnnTrain, main="Comparaison entre le modèle ARIMA et les données 
      d'apprentissage pour le PIB annuel")
lines(ARIMAPIBAnnTrain$fitted, col="red")

#Test
ARIMAPIBTrimTest <- forecast(ARIMAPIBTrimTrain, h = 5)
plot(PIBTrimTest, main="Comparaison entre le modèle SARIMA et les données de 
      validation pour le PIB trimestriel",
     ylim=c(min(PIBTrimTest,ARIMAPIBTrimTest$mean),max(PIBTrimTest,ARIMAPIBTrimTest$mean)))
lines(ARIMAPIBTrimTest$mean, col="red")

ARIMAPIBAnnTest <- forecast(ARIMAPIBAnnTrain, h = 4)
plot(PIBAnnTest, main="Comparaison entre le modèle ARIMA et les données de 
      validation pour le PIB trimestriel")
lines(ARIMAPIBAnnTest$mean, col="red")

##Choix du modèle
best_model(PIBAnnTest, PIBAnnPred$mean, ARIMAPIBAnnTest$mean)
#-> Modèle ARIMA
best_model(PIBTrimTest, PIBTrimPred$mean, ARIMAPIBTrimTest$mean)
#-> Modèle SARIMA

###Taux de chômage

TCHOAnn<-visualisation(annuelle$TCHO, 1990, 2019, 1, "du taux de chômage annuel")
TCHOTrim<-visualisation(trim$TCHO, 1990, 2017, 4, "du taux de chômage trimestriel")

TCHOAnnTrain <- window(TCHOAnn, start=1990, end=2015)
TCHOTrimTrain <- window(TCHOTrim, start=1990, end=c(2015,4))
TCHOAnnTest <- window(TCHOAnn, start=2016)
TCHOTrimTest <- window(TCHOTrim, start=2016)

##Lissage Exponentiel

TCHOAnnPred<-lissage_exponentiel(TCHOAnnTrain, 1990, 2016, 1, 4)

plot(TCHOAnnTest, type='l',
     ylim=c(min(TCHOAnnTest,TCHOAnnPred$mean),max(TCHOAnnTest,TCHOAnnPred$mean)),
     main="Comparaison entre la prédiction du lissage exponentiel et les 
      valeurs réelles pour le taux de chômage annuel")
lines(TCHOAnnPred$mean, col="red")

TCHOTrimPred<-lissage_exponentiel(TCHOTrimTrain, 1990, 2016, 1, 8)

plot(TCHOTrimTest, type='l',
     main="Comparaison entre la prédiction du lissage exponentiel et les 
      valeurs réelles pour le taux de chômage trimestriel")
lines(TCHOTrimPred$mean, col='red')
##Modèles SARIMA

#Train
ARIMATCHOTrimTrain <- auto.arima(TCHOTrimTrain, stationary = F)
plot(TCHOTrimTrain, main="Comparaison entre le modèle ARIMA et les données 
      d'apprentissage pour le taux de chômage trimestriel")
lines(ARIMATCHOTrimTrain$fitted, col="red")

ARIMATCHOAnnTrain <- auto.arima(TCHOAnnTrain, stationary = F)
plot(TCHOAnnTrain, main="Comparaison entre le modèle ARMA et les données 
      d'apprentissage pour le taux de chômage annuel")
lines(ARIMATCHOAnnTrain$fitted, col="red")

#Test
ARIMATCHOTrimTest <- forecast(ARIMATCHOTrimTrain, h = 8)
plot(TCHOTrimTest, main="Comparaison entre le modèle ARIMA et les données de 
      validation pour le taux de chômage trimestriel")
lines(ARIMATCHOTrimTest$mean, col="red")

ARIMATCHOAnnTest <- forecast(ARIMATCHOAnnTrain, h = 4)
plot(TCHOAnnTest, main="Comparaison entre le modèle ARMA et les données de 
      validation pour le taux de chômage annuel")
lines(ARIMATCHOAnnTest$mean, col="red")

##Choix du modèle
best_model(TCHOAnnTest, TCHOAnnPred$mean, ARIMATCHOAnnTest$mean)
#-> Lissage Exponentiel
best_model(TCHOTrimTest, TCHOTrimPred$mean, ARIMATCHOTrimTest$mean)
#-> Modèle ARIMA

###Aged

AGEDAnn<-visualisation(annuelle$AGED, 1990, 2019, 1, "aged")

AGEDAnnTrain <- window(AGEDAnn, start=1990, end=2015)
AGEDAnnTest <- window(AGEDAnn, start=2016)


##Lissage Exponentiel
AGEDAnnPred<-lissage_exponentiel(AGEDAnnTrain, 1990, 2015, 1, 4)

plot(AGEDAnnTest, type='l',
     main="Comparaison entre la prédiction du lissage exponentiel et les 
      valeurs réelles pour le AGED annuel", ylim=c(min(AGEDAnnPred$mean), max(AGEDAnnTest)))
lines(AGEDAnnPred$mean, col="red")

##Modèles SARIMA

#Train
ARIMAAGEDAnnTrain <- auto.arima(AGEDAnnTrain, stationary = F)
plot(AGEDAnnTrain, main="Comparaison entre le modèle ARIMA et les données 
      d'apprentissage pour le AGED annuel")
lines(ARIMAAGEDAnnTrain$fitted, col="red")

#Test
ARIMAAGEDAnnTest <- forecast(ARIMAAGEDAnnTrain, h = 4)
plot(AGEDAnnTest, main="Comparaison entre le modèle ARIMA et les données de 
      validation pour le AGED annuel")
lines(ARIMAAGEDAnnTest$mean, col="red")

##Choix du modèle
best_model(AGEDAnnTest, AGEDAnnPred$mean, ARIMAAGEDAnnTest$mean)
#-> Modèle ARIMA

