#Imports
library(MTS)
library(Matrix)
library(fUnitRoots)

annuelle <- read.csv("Data/Data_Annuel.csv", sep=";", dec=",", nrows=30)

#Stationnarisation des variables

#MSE
MSEAnn<-ts(annuelle$MSE, start=1990, end=2017)
adf.test(MSEAnn)
MSEAnnSta <- diff(MSEAnn, differences = 2)
acf(MSEAnnSta)
pacf(MSEAnnSta)
kpss.test(MSEAnnSta)
MSEAnnTrain <- window(MSEAnnSta, start=1992, end=2015)
MSEAnnTest <- window(MSEAnnSta, start=2016)

#PIB
PIBAnn<-ts(annuelle$PIB, start=1990, end=2017)
summary(lm(MSEAnn ~ PIBAnn[1:28]))
adf.test(PIBAnn)
PIBAnnSta <- diff(PIBAnn, differences = 1)
acf(PIBAnnSta)
pacf(PIBAnnSta)
kpss.test(PIBAnnSta)
plot(PIBAnnSta)
PIBAnnTrain <- window(PIBAnnSta, start=1992, end=2015)
PIBAnnTest <- window(PIBAnnSta, start=2016)

#SMIC
SMICAnn<-ts(annuelle$SMIC, start=1990, end=2017)
summary(lm(MSEAnn ~ SMICAnn[1:28]))
adf.test(SMICAnn)
SMICAnnSta <- diff(SMICAnn, differences = 2)
acf(SMICAnnSta)
pacf(SMICAnnSta)
kpss.test(SMICAnnSta)
plot(SMICAnnSta)
SMICAnnTrain <- window(SMICAnnSta, start=1992, end=2015)
SMICAnnTest <- window(SMICAnnSta, start=2016)

#TCHO
TCHOAnn<-ts(annuelle$TCHO, start=1990, end=2017)
summary(lm(MSEAnn ~ TCHOAnn[1:28]))
adf.test(TCHOAnn)
TCHOAnnSta <- diff(TCHOAnn, differences = 1)
acf(TCHOAnnSta)
pacf(TCHOAnnSta)
kpss.test(TCHOAnnSta)
plot(TCHOAnnSta)
TCHOAnnTrain <- window(TCHOAnnSta, start=1992, end=2015)
TCHOAnnTest <- window(TCHOAnnSta, start=2016)

#AGED
AGEDAnn<-ts(annuelle$AGED, start=1990, end=2017)
summary(lm(MSEAnn ~ AGEDAnn[1:28]))
adf.test(AGEDAnn)
AGEDAnnSta <- diff(AGEDAnn, differences = 1)
acf(AGEDAnnSta)
pacf(AGEDAnnSta)
kpss.test(AGEDAnnSta)
plot(AGEDAnnSta)
AGEDAnnTrain <- window(AGEDAnnSta, start=1992, end=2015)
AGEDAnnTest <- window(AGEDAnnSta, start=2016)

app<-cbind(MSEAnnTrain, PIBAnnTrain, SMICAnnTrain, TCHOAnnTrain, AGEDAnnTrain)
cor(app)

VAR(app, p=1)

