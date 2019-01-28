#Ce script correspond à la mise en place de modèles VARMA

library(MTS)
library(MASS)
library(vars)

##Modèles VAR
#1 Variable auxiliaire

#AGED

VAR(cbind(MSEAnnTrain, AGEDAnnTrain), p=5, type="both")
predtemp <- forecast(VAR(cbind(MSEAnnTrain, AGEDAnnTrain), p=5))
plot(MSEAnnTest, ylim=c(5e+09, 6e+09))
lines(predtemp$forecast$MSEAnnTrain$mean, col = "red")
lines(PredAged$mean, col="green")
EQM(MSEAnnTest, predtemp$forecast$MSEAnnTrain$mean)
EQM(MSEAnnTest, PredAged$mean)

plot(AGEDAnnTest, ylim=c(200,300))
lines(predtemp$forecast$AGEDAnnTrain$mean, col = "red")

VAR(cbind(MSETrimTrain, PIBTrimTrain), p=5, season = 4)
predtemp <- forecast(VAR(cbind(MSETrimTrain, PIBTrimTrain), p=5, season = 4))
plot(MSETrimTest, ylim=c(1.3e+09, 1.7e+09))
lines(predtemp$forecast$MSETrimTrain$mean, col = "red")
lines(PredPIB$mean, col="green")
EQM(MSETrimTest, predtemp$forecast$MSETrimTrain$mean)
EQM(MSETrimTest, PredPIB$mean)

plot(AGEDAnnTest, ylim=c(200,300))
lines(predtemp$forecast$AGEDAnnTrain$mean, col = "red")

