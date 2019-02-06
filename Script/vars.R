#Ce script correspond à la mise en place de modèles VARMA

require(MASS)
require(vars)
require(portes)

###Modèles VAR
##Annuel
#1 Variable auxiliaire

#AGED
VARselect(cbind(MSEAnnTrain, AGEDAnnTrain), lag.max = 7)
VAR(cbind(MSEAnnTrain, AGEDAnnTrain), p=7, type="both")
VARAGED <- forecast(VAR(cbind(MSEAnnTrain, AGEDAnnTrain), p=7, type="both"))
plot(MSEAnnTest, ylim=c(5.25e+9, 5.75e+9))
lines(VARAGED$forecast$MSEAnnTrain$mean, col = "red")
lines(PredAged$mean, col="green")
EQM(MSEAnnTest, VARAGED$forecast$MSEAnnTrain$mean)
EQM(MSEAnnTest, PredAged$mean)
plot(AGEDAnnTest, ylim=c(-100,300))
lines(VARAGED$forecast$AGEDAnnTrain$mean, col = "red")

#PIB
VARselect(cbind(MSEAnnTrain, PIBAnnTrain), lag.max = 7)
VAR(cbind(MSEAnnTrain, PIBAnnTrain), p=7, type="both")
VARPIB <- forecast(VAR(cbind(MSEAnnTrain, PIBAnnTrain), p=7, type="both"))
plot(MSEAnnTest, ylim=c(5e+09, 6e+09))
lines(VARPIB$forecast$MSEAnnTrain$mean, col = "red")
lines(PredAged$mean, col="green")
EQM(MSEAnnTest, VARPIB$forecast$MSEAnnTrain$mean)
EQM(MSEAnnTest, PredAged$mean)
plot(PIBAnnTest, ylim=c(2000,2300))
lines(VARPIB$forecast$PIBAnnTrain$mean, col = "red")

#SMIC
VARselect(cbind(MSEAnnTrain, SMICAnnTrain), lag.max = 7)
VAR(cbind(MSEAnnTrain, SMICAnnTrain), p=6, type="both")
VARSMIC <- forecast(VAR(cbind(MSEAnnTrain, SMICAnnTrain), p=6, type="both"))
plot(MSEAnnTest, ylim=c(5e+09, 6e+09))
lines(VARSMIC$forecast$MSEAnnTrain$mean, col = "red")
lines(PredAged$mean, col="green")
EQM(MSEAnnTest, VARSMIC$forecast$MSEAnnTrain$mean)
EQM(MSEAnnTest, PredAged$mean)
plot(SMICAnnTest, ylim=c(9,11))
lines(VARSMIC$forecast$SMICAnnTrain$mean, col = "red")

#TCHO
VARselect(cbind(MSEAnnTrain, TCHOAnnTrain), lag.max = 6)
VAR(cbind(MSEAnnTrain, TCHOAnnTrain), p=6, type="const")
VARTCHO <- forecast(VAR(cbind(MSEAnnTrain, TCHOAnnTrain), p=6, type="const"))
plot(MSEAnnTest, ylim=c(5e+09, 6e+09))
lines(VARTCHO$forecast$MSEAnnTrain$mean, col = "red")
lines(PredAged$mean, col="green")
EQM(MSEAnnTest, VARTCHO$forecast$MSEAnnTrain$mean)
EQM(MSEAnnTest, PredAged$mean)
plot(TCHOAnnTest, ylim=c(8,10))
lines(VARTCHO$forecast$TCHOAnnTrain$mean, col = "red")

plot(MSEAnnTest, main="Comparaison des modèles avec 1 variable", ylim=c(5e+09,6e+09))
lines(VARAGED$forecast$MSEAnnTrain$mean, col="blue")
lines(VARPIB$forecast$MSEAnnTrain$mean, col="green")
lines(VARSMIC$forecast$MSEAnnTrain$mean, col="red")
lines(VARTCHO$forecast$MSEAnnTrain$mean, col="gold")
legend('topleft', legend = c('Série MSE', 'Aged', 'PIB', 'SMIC', 'Taux chômage'),
       col=c('black', 'blue', 'green', 'red', 'gold'), lty=1, cex=0.8)

#2 Variables

#Aged & PIB
VARselect(cbind(MSEAnnTrain, AGEDAnnTrain, PIBAnnTrain), lag.max=5)
VAR(cbind(MSEAnnTrain, AGEDAnnTrain, PIBAnnTrain), p=5, type="both")
VARAGEDPIB <- forecast(VAR(cbind(MSEAnnTrain, AGEDAnnTrain, PIBAnnTrain), p=5, type="both"))
plot(MSEAnnTest, ylim=c(5e+09, 6e+09))
lines(VARAGEDPIB$forecast$MSEAnnTrain$mean, col = "red")
lines(PredAged$mean, col="green")
EQM(MSEAnnTest, VARAGEDPIB$forecast$MSEAnnTrain$mean)
EQM(MSEAnnTest, PredAged$mean)
plot(AGEDAnnTest, ylim=c(-60,300))
lines(VARAGEDPIB$forecast$AGEDAnnTrain$mean, col = "red")
plot(PIBAnnTest, ylim=c(2000,3000))
lines(VARAGEDPIB$forecast$PIBAnnTrain$mean, col = "red")

#Aged & SMIC
VARselect(cbind(MSEAnnTrain, AGEDAnnTrain, SMICAnnTrain), lag.max=5)
VAR(cbind(MSEAnnTrain, AGEDAnnTrain, SMICAnnTrain), p=5, type="both")
VARAGEDSMIC <- forecast(VAR(cbind(MSEAnnTrain, AGEDAnnTrain, SMICAnnTrain), p=4, type="both"))
plot(MSEAnnTest, ylim=c(4.5e+09, 6e+09))
lines(VARAGEDSMIC$forecast$MSEAnnTrain$mean, col = "red")
lines(PredAged$mean, col="green")
EQM(MSEAnnTest, VARAGEDSMIC$forecast$MSEAnnTrain$mean)
EQM(MSEAnnTest, PredAged$mean)
plot(AGEDAnnTest, ylim=c(-60,300))
lines(VARAGEDSMIC$forecast$AGEDAnnTrain$mean, col = "red")
plot(SMICAnnTest, ylim=c(9.5,10.5))
lines(VARAGEDSMIC$forecast$SMICAnnTrain$mean, col = "red")

#Aged & TCHO
VARselect(cbind(MSEAnnTrain, AGEDAnnTrain, TCHOAnnTrain), lag.max=5)
VAR(cbind(MSEAnnTrain, AGEDAnnTrain, TCHOAnnTrain), p=5, type="both")
VARAGEDTCHO <- forecast(VAR(cbind(MSEAnnTrain, AGEDAnnTrain, TCHOAnnTrain), p=5, type="both"))
plot(MSEAnnTest, ylim=c(5e+09, 6e+09))
lines(VARAGEDTCHO$forecast$MSEAnnTrain$mean, col = "red")
lines(PredAged$mean, col="green")
EQM(MSEAnnTest, VARAGEDTCHO$forecast$MSEAnnTrain$mean)
EQM(MSEAnnTest, PredAged$mean)
plot(AGEDAnnTest, ylim=c(-60,300))
lines(VARAGEDTCHO$forecast$AGEDAnnTrain$mean, col = "red")
plot(TCHOAnnTest, ylim=c(5,10))
lines(VARAGEDTCHO$forecast$TCHOAnnTrain$mean, col = "red")

#PIB & SMIC
VARselect(cbind(MSEAnnTrain, PIBAnnTrain, SMICAnnTrain), lag.max=5)
VAR(cbind(MSEAnnTrain, PIBAnnTrain, SMICAnnTrain), p=5, type="both")
VARPIBSMIC <- forecast(VAR(cbind(MSEAnnTrain, PIBAnnTrain, SMICAnnTrain), p=5, type="both"))
plot(MSEAnnTest, ylim=c(5e+09, 6e+09))
lines(VARPIBSMIC$forecast$MSEAnnTrain$mean, col = "red")
lines(PredAged$mean, col="green")
EQM(MSEAnnTest, VARPIBSMIC$forecast$MSEAnnTrain$mean)
EQM(MSEAnnTest, PredAged$mean)
plot(PIBAnnTest, ylim=c(1900,2300))
lines(VARPIBSMIC$forecast$PIBAnnTrain$mean, col = "red")
plot(SMICAnnTest, ylim=c(9.5,10.5))
lines(VARPIBSMIC$forecast$SMICAnnTrain$mean, col = "red")

#PIB & TCHO
VARselect(cbind(MSEAnnTrain, PIBAnnTrain, TCHOAnnTrain), lag.max=5)
VAR(cbind(MSEAnnTrain, PIBAnnTrain, TCHOAnnTrain), p=5, type="both")
VARPIBTCHO <- forecast(VAR(cbind(MSEAnnTrain, PIBAnnTrain, TCHOAnnTrain), p=5, type="both"))
plot(MSEAnnTest, ylim=c(5e+09, 6e+09))
lines(VARPIBTCHO$forecast$MSEAnnTrain$mean, col = "red")
lines(PredAged$mean, col="green")
EQM(MSEAnnTest, VARPIBTCHO$forecast$MSEAnnTrain$mean)
EQM(MSEAnnTest, PredAged$mean)
plot(PIBAnnTest, ylim=c(1900,2300))
lines(VARPIBTCHO$forecast$PIBAnnTrain$mean, col = "red")
plot(TCHOAnnTest, ylim=c(8.5,12))
lines(VARPIBTCHO$forecast$TCHOAnnTrain$mean, col = "red")

#SMIC & TCHO
VARselect(cbind(MSEAnnTrain, SMICAnnTrain, TCHOAnnTrain), lag.max=5)
VAR(cbind(MSEAnnTrain, SMICAnnTrain, TCHOAnnTrain), p=5, type="both")
VARSMICTCHO <- forecast(VAR(cbind(MSEAnnTrain, SMICAnnTrain, TCHOAnnTrain), p=5, type="both"))
plot(MSEAnnTest, ylim=c(5e+09, 6e+09))
lines(VARSMICTCHO$forecast$MSEAnnTrain$mean, col = "red")
lines(PredAged$mean, col="green")
EQM(MSEAnnTest, VARSMICTCHO$forecast$MSEAnnTrain$mean)
EQM(MSEAnnTest, PredAged$mean)
plot(SMICAnnTest, ylim=c(9.5,11))
lines(VARSMICTCHO$forecast$SMICAnnTrain$mean, col = "red")
plot(TCHOAnnTest, ylim=c(8.5,20))
lines(VARSMICTCHO$forecast$TCHOAnnTrain$mean, col = "red")

plot(MSEAnnTest, main="Comparaison des modèles avec 2 variables", ylim=c(4.5e+09, 6e+09))
lines(VARAGEDPIB$forecast$MSEAnnTrain$mean, col="red")
lines(VARAGEDSMIC$forecast$MSEAnnTrain$mean, col="blue")
lines(VARAGEDTCHO$forecast$MSEAnnTrain$mean, col="green")
lines(VARPIBSMIC$forecast$MSEAnnTrain$mean, col="gold")
lines(VARPIBTCHO$forecast$MSEAnnTrain$mean, col="purple")
lines(VARSMICTCHO$forecast$MSEAnnTrain$mean, col="brown")
legend('bottomleft', 
       legend = c('Série MSE', 'Aged & PIB', 'Aged & SMIC', 'Aged & Taux chômage',
                  'PIB & SMIC', 'PIB & Taux chômage', 'SMIC & Taux chômage'),
       col=c('black', 'red', 'blue', 'green', 'gold', 'purple', 'brown'), lty=1, cex=0.8)

#3 Variables
#Aged & PIB & SMIC
VARselect(cbind(MSEAnnTrain, AGEDAnnTrain, PIBAnnTrain, SMICAnnTrain), lag.max=4)
VAR(cbind(MSEAnnTrain, AGEDAnnTrain, PIBAnnTrain, SMICAnnTrain), p=4, type="both")
VARAGEDPIBSMIC <- forecast(VAR(cbind(MSEAnnTrain, AGEDAnnTrain, PIBAnnTrain, SMICAnnTrain), p=4, type="both"))
plot(MSEAnnTest, ylim=c(4e+09, 6e+09))
lines(VARAGEDPIBSMIC$forecast$MSEAnnTrain$mean, col = "red")
lines(PredAged$mean, col="green")
EQM(MSEAnnTest, VARAGEDPIBSMIC$forecast$MSEAnnTrain$mean)
EQM(MSEAnnTest, PredAged$mean)
plot(AGEDAnnTest, ylim=c(0,375))
lines(VARAGEDPIBSMIC$forecast$AGEDAnnTrain$mean, col = "red")
plot(PIBAnnTest, ylim=c(2000,2300))
lines(VARAGEDPIBSMIC$forecast$PIBAnnTrain$mean, col = "red")
plot(SMICAnnTest, ylim=c(9.5,10.5))
lines(VARAGEDPIBSMIC$forecast$SMICAnnTrain$mean, col = "red")

#Aged & PIB & TCHO
VARselect(cbind(MSEAnnTrain, AGEDAnnTrain, PIBAnnTrain, TCHOAnnTrain), lag.max=4)
VAR(cbind(MSEAnnTrain, AGEDAnnTrain, PIBAnnTrain, TCHOAnnTrain), p=4, type="both")
VARAGEDPIBTCHO <- forecast(VAR(cbind(MSEAnnTrain, AGEDAnnTrain, PIBAnnTrain, TCHOAnnTrain), p=4, type="both"))
plot(MSEAnnTest, ylim=c(5e+09, 6e+09))
lines(VARAGEDPIBTCHO$forecast$MSEAnnTrain$mean, col = "red")
lines(PredAged$mean, col="green")
EQM(MSEAnnTest, VARAGEDPIBTCHO$forecast$MSEAnnTrain$mean)
EQM(MSEAnnTest, PredAged$mean)
plot(AGEDAnnTest, ylim=c(20,375))
lines(VARAGEDPIBTCHO$forecast$AGEDAnnTrain$mean, col = "red")
plot(PIBAnnTest, ylim=c(2000,2500))
lines(VARAGEDPIBTCHO$forecast$PIBAnnTrain$mean, col = "red")
plot(TCHOAnnTest, ylim=c(7,11))
lines(VARAGEDPIBTCHO$forecast$TCHOAnnTrain$mean, col = "red")

#Aged & SMIC & TCHO
VARselect(cbind(MSEAnnTrain, AGEDAnnTrain, SMICAnnTrain, TCHOAnnTrain), lag.max=4)
VAR(cbind(MSEAnnTrain, AGEDAnnTrain, SMICAnnTrain, TCHOAnnTrain), p=4, type="both")
VARAGEDSMICTCHO <- forecast(VAR(cbind(MSEAnnTrain, AGEDAnnTrain, SMICAnnTrain, TCHOAnnTrain), p=4, type="both"))
plot(MSEAnnTest, ylim=c(4e+09, 6e+09))
lines(VARAGEDSMICTCHO$forecast$MSEAnnTrain$mean, col = "red")
lines(PredAged$mean, col="green")
EQM(MSEAnnTest, VARAGEDSMICTCHO$forecast$MSEAnnTrain$mean)
EQM(MSEAnnTest, PredAged$mean)
plot(AGEDAnnTest, ylim=c(20,375))
lines(VARAGEDSMICTCHO$forecast$AGEDAnnTrain$mean, col = "red")
plot(SMICAnnTest, ylim=c(9,12.5))
lines(VARAGEDSMICTCHO$forecast$SMICAnnTrain$mean, col = "red")
plot(TCHOAnnTest, ylim=c(8.5,15))
lines(VARAGEDSMICTCHO$forecast$TCHOAnnTrain$mean, col = "red")

#PIB & SMIC & TCHO
VARselect(cbind(MSEAnnTrain, PIBAnnTrain, SMICAnnTrain, TCHOAnnTrain), lag.max=4)
VAR(cbind(MSEAnnTrain, PIBAnnTrain, SMICAnnTrain, TCHOAnnTrain), p=4, type="both")
VARPIBSMICTCHO <- forecast(VAR(cbind(MSEAnnTrain, PIBAnnTrain, SMICAnnTrain, TCHOAnnTrain), p=4, type="both"))
plot(MSEAnnTest, ylim=c(5e+09, 6e+09))
lines(VARPIBSMICTCHO$forecast$MSEAnnTrain$mean, col = "red")
lines(PredAged$mean, col="green")
EQM(MSEAnnTest, VARPIBSMICTCHO$forecast$MSEAnnTrain$mean)
EQM(MSEAnnTest, PredAged$mean)
plot(PIBAnnTest, ylim=c(2000,2500))
lines(VARPIBSMICTCHO$forecast$PIBAnnTrain$mean, col = "red")
plot(SMICAnnTest, ylim=c(9,12.5))
lines(VARPIBSMICTCHO$forecast$SMICAnnTrain$mean, col = "red")
plot(TCHOAnnTest, ylim=c(8.5,11))
lines(VARPIBSMICTCHO$forecast$TCHOAnnTrain$mean, col = "red")


plot(MSEAnnTest, main="Comparaison des modèles avec 3 variables", ylim=c(4e+09, 6e+09))
lines(VARAGEDPIBSMIC$forecast$MSEAnnTrain$mean, col="blue")
lines(VARAGEDPIBTCHO$forecast$MSEAnnTrain$mean, col="green")
lines(VARAGEDSMICTCHO$forecast$MSEAnnTrain$mean, col="red")
lines(VARPIBSMICTCHO$forecast$MSEAnnTrain$mean, col="gold")
legend('bottomleft', legend = c('Série MSE', 'Aged & PIB & SMIC', 'Aged & PIB & Taux chômage',
                           'Aged & SMIC & Taux chômage', 'PIB & SMIC & Taux chômage'),
       col=c('black', 'blue', 'green', 'red', 'gold'), lty=1, cex=0.8)

#4 Variables
VARselect(cbind(MSEAnnTrain, AGEDAnnTrain, PIBAnnTrain, SMICAnnTrain, TCHOAnnTrain), lag.max=3, type="trend")
VAR(cbind(MSEAnnTrain, AGEDAnnTrain, PIBAnnTrain, SMICAnnTrain, TCHOAnnTrain), p=3, type="trend")
VARCOMPLET <- forecast(VAR(cbind(MSEAnnTrain, AGEDAnnTrain, PIBAnnTrain, SMICAnnTrain, TCHOAnnTrain), p=3, type="trend"))
plot(MSEAnnTest, ylim=c(5e+09, 6e+09))
lines(VARCOMPLET$forecast$MSEAnnTrain$mean, col = "red")
lines(PredAged$mean, col="green")
EQM(MSEAnnTest, VARCOMPLET$forecast$MSEAnnTrain$mean)
EQM(MSEAnnTest, PredAged$mean)
plot(PIBAnnTest, ylim=c(2000,2500))
lines(VARCOMPLET$forecast$PIBAnnTrain$mean, col = "red")
plot(SMICAnnTest, ylim=c(9,12.5))
lines(VARCOMPLET$forecast$SMICAnnTrain$mean, col = "red")
plot(TCHOAnnTest, ylim=c(8.5,11))
lines(VARCOMPLET$forecast$TCHOAnnTrain$mean, col = "red")
plot(AGEDAnnTest, ylim=c(199,300))
lines(VARCOMPLET$forecast$AGEDAnnTrain$mean, col = "red")

####### STATIONAIRE
#1 Variable
#AGED
VARselect(cbind(MSEAnnStaTrain, AGEDAnnStaTrain), lag.max=10)
VAR(cbind(MSEAnnStaTrain, AGEDAnnStaTrain[2:25]), p=7, type="const")
VARAGEDSta <- forecast(VAR(cbind(MSEAnnStaTrain, AGEDAnnStaTrain[2:25]), p=7, type="const"))
plot(MSEAnnStaTest, ylim=c(-1.5e+8, 1.5e+8))
lines(VARAGEDSta$forecast$MSEAnnStaTrain$mean, col = "red")
EQM(MSEAnnStaTest, VARAGEDSta$forecast$MSEAnnStaTrain$mean)
EQM(MSEAnnTest, PredAged$mean)
plot(AGEDAnnStaTest, ylim=c(-30,30))
lines(VARAGEDSta$forecast$AGEDAnnStaTrain$mean, col = "red")

#PIB
VARselect(cbind(MSEAnnStaTrain, PIBAnnStaTrain), lag.max=6)
VAR(cbind(MSEAnnStaTrain, PIBAnnStaTrain), p=1, type="const")
VARPIBSta <- forecast(VAR(cbind(MSEAnnStaTrain, PIBAnnStaTrain), p=1, type="const"))
plot(MSEAnnStaTest, ylim=c(-1.5e+8, 1.5e+8))
lines(VARPIBSta$forecast$MSEAnnStaTrain$mean, col = "red")
EQM(MSEAnnStaTest, VARPIBSta$forecast$MSEAnnStaTrain$mean)
EQM(MSEAnnTest, PredAged$mean)
plot(PIBAnnStaTest, ylim=c(0,45))
lines(VARPIBSta$forecast$PIBAnnStaTrain$mean, col = "red")

#SMIC
VARselect(cbind(MSEAnnStaTrain, SMICAnnStaTrain), lag.max=7)
VAR(cbind(MSEAnnStaTrain, SMICAnnStaTrain), p=7, type="const")
VARSMICSta <- forecast(VAR(cbind(MSEAnnStaTrain, SMICAnnStaTrain), p=7, type="const"))
plot(MSEAnnStaTest, ylim=c(-5e+8, 5e+8))
lines(VARSMICSta$forecast$MSEAnnStaTrain$mean, col = "red")
EQM(MSEAnnStaTest, VARSMICSta$forecast$MSEAnnStaTrain$mean)
EQM(MSEAnnTest, PredAged$mean)
plot(SMICAnnStaTest, ylim=c(-0.5,0.5))
lines(VARSMICSta$forecast$SMICAnnStaTrain$mean, col = "red")

#TCHO
VARselect(cbind(MSEAnnStaTrain, TCHOAnnStaTrain), lag.max=6)
VAR(cbind(MSEAnnStaTrain, TCHOAnnStaTrain), p=4, type="const")
VARTCHOSta <- forecast(VAR(cbind(MSEAnnStaTrain, TCHOAnnStaTrain), p=4, type="const"))
plot(MSEAnnStaTest, ylim=c(-1.5e+8, 1.5e+8))
lines(VARTCHOSta$forecast$MSEAnnStaTrain$mean, col = "red")
EQM(MSEAnnStaTest, VARTCHOSta$forecast$MSEAnnStaTrain$mean)
EQM(MSEAnnTest, PredAged$mean)
plot(TCHOAnnStaTest, ylim=c(-1,1))
lines(VARTCHOSta$forecast$TCHOAnnStaTrain$mean, col = "red")


plot(MSEAnnStaTest, main="Comparaison des modèles avec 1 variable", ylim=c(-1.5e+8, 1.5e+8))
lines(VARAGEDSta$forecast$MSEAnnStaTrain$mean, col="blue")
lines(VARPIBSta$forecast$MSEAnnStaTrain$mean, col="green")
lines(VARSMICSta$forecast$MSEAnnStaTrain$mean, col="red")
lines(VARTCHOSta$forecast$MSEAnnStaTrain$mean, col="gold")
legend('bottomleft', legend = c('Série MSE', 'Aged', 'PIB', 'SMIC', 'Taux chômage'),
       col=c('black', 'blue', 'green', 'red', 'gold'), lty=1, cex=0.8)

#2 Variables
#Aged & PIB
VARselect(cbind(MSEAnnStaTrain, AGEDAnnStaTrain, PIBAnnStaTrain), lag.max=4)
VAR(cbind(MSEAnnStaTrain, AGEDAnnStaTrain, PIBAnnStaTrain), p=4, type="const")
VARAGEDPIBSta <- forecast(VAR(cbind(MSEAnnStaTrain, AGEDAnnStaTrain, PIBAnnStaTrain), p=4, type="const"))
plot(MSEAnnStaTest, ylim=c(-1.5e+8, 1.5e+8))
lines(VARAGEDPIBSta$forecast$MSEAnnStaTrain$mean, col = "red")
EQM(MSEAnnStaTest, VARAGEDPIBSta$forecast$MSEAnnStaTrain$mean)
plot(AGEDAnnStaTest, ylim=c(-30,30))
lines(VARAGEDPIBSta$forecast$AGEDAnnStaTrain$mean, col = "red")
plot(PIBAnnStaTest, ylim=c(-10,100))
lines(VARAGEDPIBSta$forecast$PIBAnnStaTrain$mean, col = "red")

#Aged & SMIC
VARselect(cbind(MSEAnnStaTrain, AGEDAnnStaTrain, SMICAnnStaTrain), lag.max=5)
VAR(cbind(MSEAnnStaTrain, AGEDAnnStaTrain, SMICAnnStaTrain), p=5, type="const")
VARAGEDSMICSta <- forecast(VAR(cbind(MSEAnnStaTrain, AGEDAnnStaTrain, SMICAnnStaTrain), p=5, type="const"))
plot(MSEAnnStaTest, ylim=c(-1.5e+8, 1.5e+8))
lines(VARAGEDSMICSta$forecast$MSEAnnStaTrain$mean, col = "red")
EQM(MSEAnnStaTest, VARAGEDSMICSta$forecast$MSEAnnStaTrain$mean)
EQM(MSEAnnTest, PredAged$mean)
plot(AGEDAnnStaTest, ylim=c(-100,100))
lines(VARAGEDSMICSta$forecast$AGEDAnnStaTrain$mean, col = "red")
plot(SMICAnnStaTest, ylim=c(-1,1))
lines(VARAGEDSMICSta$forecast$SMICAnnStaTrain$mean, col = "red")

#Aged & TCHO
VARselect(cbind(MSEAnnStaTrain, AGEDAnnStaTrain, TCHOAnnStaTrain), lag.max=5)
VAR(cbind(MSEAnnStaTrain, AGEDAnnStaTrain, TCHOAnnStaTrain), p=5, type="const")
VARAGEDTCHOSta <- forecast(VAR(cbind(MSEAnnStaTrain, AGEDAnnStaTrain, TCHOAnnStaTrain), p=5, type="const"))
plot(MSEAnnStaTest, ylim=c(-1.5e+8, 1.5e+8))
lines(VARAGEDTCHOSta$forecast$MSEAnnStaTrain$mean, col = "red")
EQM(MSEAnnStaTest, VARAGEDTCHOSta$forecast$MSEAnnStaTrain$mean)
plot(AGEDAnnStaTest, ylim=c(-100,100))
lines(VARAGEDTCHOSta$forecast$AGEDAnnStaTrain$mean, col = "red")
plot(TCHOAnnStaTest, ylim=c(-2,2))
lines(VARAGEDTCHOSta$forecast$TCHOAnnStaTrain$mean, col = "red")

#PIB & SMIC
VARselect(cbind(MSEAnnStaTrain, PIBAnnStaTrain, SMICAnnStaTrain), lag.max=5)
VAR(cbind(MSEAnnStaTrain, PIBAnnStaTrain, SMICAnnStaTrain), p=5, type="const")
VARPIBSMICSta <- forecast(VAR(cbind(MSEAnnStaTrain, PIBAnnStaTrain, SMICAnnStaTrain), p=5, type="const"))
plot(MSEAnnStaTest, ylim=c(-1.5e+8, 1.5e+8))
lines(VARPIBSMICSta$forecast$MSEAnnStaTrain$mean, col = "red")
EQM(MSEAnnStaTest, VARPIBSMICSta$forecast$MSEAnnStaTrain$mean)
plot(PIBAnnStaTest, ylim=c(-100,100))
lines(VARPIBSMICSta$forecast$PIBAnnStaTrain$mean, col = "red")
plot(SMICAnnStaTest, ylim=c(-2,2))
lines(VARPIBSMICSta$forecast$SMICAnnStaTrain$mean, col = "red")

#PIB & TCHO
VARselect(cbind(MSEAnnStaTrain, PIBAnnStaTrain, TCHOAnnStaTrain), lag.max=5)
VAR(cbind(MSEAnnStaTrain, PIBAnnStaTrain, TCHOAnnStaTrain), p=5, type="const")
VARPIBTCHOSta <- forecast(VAR(cbind(MSEAnnStaTrain, PIBAnnStaTrain, TCHOAnnStaTrain), p=5, type="const"))
plot(MSEAnnStaTest, ylim=c(-1.5e+8, 1.5e+8))
lines(VARPIBTCHOSta$forecast$MSEAnnStaTrain$mean, col = "red")
EQM(MSEAnnStaTest, VARPIBTCHOSta$forecast$MSEAnnStaTrain$mean)
plot(PIBAnnStaTest, ylim=c(-100,100))
lines(VARPIBTCHOSta$forecast$PIBAnnStaTrain$mean, col = "red")
plot(TCHOAnnStaTest, ylim=c(-2,2))
lines(VARPIBTCHOSta$forecast$TCHOAnnStaTrain$mean, col = "red")

#SMIC & TCHO
VARselect(cbind(MSEAnnStaTrain, SMICAnnStaTrain, TCHOAnnStaTrain), lag.max=5)
VAR(cbind(MSEAnnStaTrain, SMICAnnStaTrain, TCHOAnnStaTrain), p=5, type="const")
VARSMICTCHOSta <- forecast(VAR(cbind(MSEAnnStaTrain, SMICAnnStaTrain, TCHOAnnStaTrain), p=5, type="const"))
plot(MSEAnnStaTest, ylim=c(-1.5e+8, 1.5e+8))
lines(VARSMICTCHOSta$forecast$MSEAnnStaTrain$mean, col = "red")
EQM(MSEAnnStaTest, VARSMICTCHOSta$forecast$MSEAnnStaTrain$mean)
plot(SMICAnnStaTest, ylim=c(-1,1))
lines(VARSMICTCHOSta$forecast$SMICAnnStaTrain$mean, col = "red")
plot(TCHOAnnStaTest, ylim=c(-2,2))
lines(VARSMICTCHOSta$forecast$TCHOAnnStaTrain$mean, col = "red")

plot(MSEAnnStaTest, main="Comparaison des modèles avec 2 variables", ylim=c(-1.5e+8, 1.5e+8))
lines(VARAGEDPIBSta$forecast$MSEAnnStaTrain$mean, col="red")
lines(VARAGEDSMICSta$forecast$MSEAnnStaTrain$mean, col="blue")
lines(VARAGEDTCHOSta$forecast$MSEAnnStaTrain$mean, col="green")
lines(VARPIBSMICSta$forecast$MSEAnnStaTrain$mean, col="gold")
lines(VARPIBTCHOSta$forecast$MSEAnnStaTrain$mean, col="purple")
lines(VARSMICTCHOSta$forecast$MSEAnnStaTrain$mean, col="brown")
legend('topright', 
       legend = c('Série MSE', 'Aged & PIB', 'Aged & SMIC', 'Aged & Taux chômage',
                  'PIB & SMIC', 'PIB & Taux chômage', 'SMIC & Taux chômage'),
       col=c('black', 'red', 'blue', 'green', 'gold', 'purple', 'brown'), lty=1, cex=0.8)

#3 Variables
#Aged & PIB & SMIC
VARselect(cbind(MSEAnnStaTrain, AGEDAnnStaTrain, PIBAnnStaTrain, SMICAnnStaTrain), lag.max=4)
VAR(cbind(MSEAnnStaTrain, AGEDAnnStaTrain, PIBAnnStaTrain, SMICAnnStaTrain), p=4, type="const")
VARAGEDPIBSMICSta <- forecast(VAR(cbind(MSEAnnStaTrain, AGEDAnnStaTrain, PIBAnnStaTrain, SMICAnnStaTrain), p=4, type="const"))
plot(MSEAnnStaTest, ylim=c(-1.5e+8, 1.5e+8))
lines(VARAGEDPIBSMICSta$forecast$MSEAnnStaTrain$mean, col = "red")
EQM(MSEAnnStaTest, VARAGEDPIBSMICSta$forecast$MSEAnnStaTrain$mean)
plot(AGEDAnnStaTest, ylim=c(-100,100))
lines(VARAGEDPIBSMICSta$forecast$AGEDAnnStaTrain$mean, col = "red")
plot(PIBAnnStaTest, ylim=c(-150,150))
lines(VARAGEDPIBSMICSta$forecast$PIBAnnStaTrain$mean, col = "red")
plot(SMICAnnStaTest, ylim=c(-1,1))
lines(VARAGEDPIBSMICSta$forecast$SMICAnnStaTrain$mean, col = "red")

#Aged & PIB & TCHO
VARselect(cbind(MSEAnnStaTrain, AGEDAnnStaTrain, PIBAnnStaTrain, TCHOAnnStaTrain), lag.max=4)
VAR(cbind(MSEAnnStaTrain, AGEDAnnStaTrain, PIBAnnStaTrain, TCHOAnnStaTrain), p=4, type="const")
VARAGEDPIBTCHOSta <- forecast(VAR(cbind(MSEAnnStaTrain, AGEDAnnStaTrain, PIBAnnStaTrain, TCHOAnnStaTrain), p=4, type="const"))
plot(MSEAnnStaTest, ylim=c(-1.5e+8, 1.5e+8))
lines(VARAGEDPIBTCHOSta$forecast$MSEAnnStaTrain$mean, col = "red")
EQM(MSEAnnStaTest, VARAGEDPIBTCHOSta$forecast$MSEAnnStaTrain$mean)
plot(AGEDAnnStaTest, ylim=c(-100,100))
lines(VARAGEDPIBTCHOSta$forecast$AGEDAnnStaTrain$mean, col = "red")
plot(PIBAnnStaTest, ylim=c(-150,150))
lines(VARAGEDPIBTCHOSta$forecast$PIBAnnStaTrain$mean, col = "red")
plot(TCHOAnnStaTest, ylim=c(-1,1))
lines(VARAGEDPIBTCHOSta$forecast$TCHOAnnStaTrain$mean, col = "red")

#Aged & SMIC & TCHO
VARselect(cbind(MSEAnnStaTrain, AGEDAnnStaTrain, SMICAnnStaTrain, TCHOAnnStaTrain), lag.max=4)
VAR(cbind(MSEAnnStaTrain, AGEDAnnStaTrain, SMICAnnStaTrain, TCHOAnnStaTrain), p=4, type="const")
VARAGEDSMICTCHOSta <- forecast(VAR(cbind(MSEAnnStaTrain, AGEDAnnStaTrain, SMICAnnStaTrain, TCHOAnnStaTrain), p=4, type="const"))
plot(MSEAnnStaTest, ylim=c(-1.5e+8, 1.5e+8))
lines(VARAGEDSMICTCHOSta$forecast$MSEAnnStaTrain$mean, col = "red")
EQM(MSEAnnStaTest, VARAGEDSMICTCHOSta$forecast$MSEAnnStaTrain$mean)
plot(AGEDAnnStaTest, ylim=c(-100,100))
lines(VARAGEDSMICTCHOSta$forecast$AGEDAnnStaTrain$mean, col = "red")
plot(SMICAnnStaTest, ylim=c(-1,1))
lines(VARAGEDSMICTCHOSta$forecast$SMICAnnStaTrain$mean, col = "red")
plot(TCHOAnnStaTest, ylim=c(-10,10))
lines(VARAGEDSMICTCHOSta$forecast$TCHOAnnStaTrain$mean, col = "red")


#PIB & SMIC & TCHO
VARselect(cbind(MSEAnnStaTrain, PIBAnnStaTrain, SMICAnnStaTrain, TCHOAnnStaTrain), lag.max=4)
VAR(cbind(MSEAnnStaTrain, PIBAnnStaTrain, SMICAnnStaTrain, TCHOAnnStaTrain), p=4, type="const")
VARPIBSMICTCHOSta <- forecast(VAR(cbind(MSEAnnStaTrain, PIBAnnStaTrain, SMICAnnStaTrain, TCHOAnnStaTrain), p=4, type="const"))
plot(MSEAnnStaTest, ylim=c(-1.5e+8, 1.5e+8))
lines(VARPIBSMICTCHOSta$forecast$MSEAnnStaTrain$mean, col = "red")
EQM(MSEAnnStaTest, VARPIBSMICTCHOSta$forecast$MSEAnnStaTrain$mean)
plot(PIBAnnStaTest, ylim=c(-100,100))
lines(VARPIBSMICTCHOSta$forecast$PIBAnnStaTrain$mean, col = "red")
plot(SMICAnnStaTest, ylim=c(-0.5,0.5))
lines(VARPIBSMICTCHOSta$forecast$SMICAnnStaTrain$mean, col = "red")
plot(TCHOAnnStaTest, ylim=c(-0.7,0.7))
lines(VARPIBSMICTCHOSta$forecast$TCHOAnnStaTrain$mean, col = "red")

plot(MSEAnnStaTest, main="Comparaison des modèles avec 1 variable", ylim=c(-1.5e+8, 1.5e+8))
lines(VARAGEDPIBSMICSta$forecast$MSEAnnStaTrain$mean, col="blue")
lines(VARAGEDPIBTCHOSta$forecast$MSEAnnStaTrain$mean, col="green")
lines(VARAGEDSMICTCHOSta$forecast$MSEAnnStaTrain$mean, col="red")
lines(VARPIBSMICTCHOSta$forecast$MSEAnnStaTrain$mean, col="gold")
legend('bottomright', legend = c('Série MSE', 'Aged', 'PIB', 'SMIC', 'Taux chômage'),
       col=c('black', 'blue', 'green', 'red', 'gold'), lty=1, cex=0.8)

#4 Variables
VARselect(cbind(MSEAnnStaTrain, AGEDAnnStaTrain, PIBAnnStaTrain, SMICAnnStaTrain, TCHOAnnStaTrain), lag.max=3)
VAR(cbind(MSEAnnStaTrain, AGEDAnnStaTrain, PIBAnnStaTrain, SMICAnnStaTrain, TCHOAnnStaTrain), p=3, type="const")
VARCOMPLETSta <- forecast(VAR(cbind(MSEAnnStaTrain, AGEDAnnStaTrain, PIBAnnStaTrain, SMICAnnStaTrain, TCHOAnnStaTrain), p=3, type="const"))
plot(MSEAnnStaTest, ylim=c(-1.5e+8, 1.5e+8))
lines(VARCOMPLETSta$forecast$MSEAnnStaTrain$mean, col = "red")
EQM(MSEAnnStaTest, VARCOMPLETSta$forecast$MSEAnnStaTrain$mean)
plot(AGEDAnnStaTest, ylim=c(-40,40))
lines(VARCOMPLETSta$forecast$AGEDAnnStaTrain$mean, col = "red")
plot(PIBAnnStaTest, ylim=c(-100,100))
lines(VARCOMPLETSta$forecast$PIBAnnStaTrain$mean, col = "red")
plot(SMICAnnStaTest, ylim=c(-0.5,0.5))
lines(VARCOMPLETSta$forecast$SMICAnnStaTrain$mean, col = "red")
plot(TCHOAnnStaTest, ylim=c(-0.7,0.7))
lines(VARCOMPLETSta$forecast$TCHOAnnStaTrain$mean, col = "red")

##Trimestrielles
#1 Variable
#PIB
VARselect(cbind(MSETrimStaTrain, PIBTrimStaTrain), lag.max=10)
VAR(cbind(MSETrimStaTrain, PIBTrimStaTrain), p=4, type="const")
VARPIBStaTrim <- forecast(VAR(cbind(MSETrimStaTrain, PIBTrimStaTrain), p=4, type="const"))
plot(window(MSETrimTest, end=c(2016,4)))
lines(VARPIBStaTrim$forecast$MSETrimStaTrain$mean*MSETrimSeasonalTest*MSETrimTrendTest, col = "red")
EQM(window(MSETrimTest, end=c(2016,4)), VARPIBStaTrim$forecast$MSETrimStaTrain$mean*MSETrimSeasonalTest*MSETrimTrendTest)
plot(PIBTrimStaTest, ylim=c(0.99,1.01))
lines(VARPIBStaTrim$forecast$PIBTrimStaTrain$mean, col = "red")

#SMIC
VARselect(cbind(MSETrimStaTrain, SMICTrimStaTrain), lag.max=10)
VAR(cbind(MSETrimStaTrain, SMICTrimStaTrain), p=3, type="const")
VARSMICStaTrim <- forecast(VAR(cbind(MSETrimStaTrain, SMICTrimStaTrain), p=3, type="const"))
plot(window(MSETrimTest, end=c(2016,4)))
lines(VARSMICStaTrim$forecast$MSETrimStaTrain$mean*MSETrimSeasonalTest*MSETrimTrendTest, col = "red")
EQM(window(MSETrimTest, end=c(2016,4)), VARSMICStaTrim$forecast$MSETrimStaTrain$mean*MSETrimSeasonalTest*MSETrimTrendTest)
plot(SMICTrimStaTest, ylim=c(-0.5,0.5))
lines(VARSMICStaTrim$forecast$SMICTrimStaTrain$mean, col = "red")

#TCHO
VARselect(cbind(MSETrimStaTrain, TCHOTrimStaTrain), lag.max=10)
VAR(cbind(MSETrimStaTrain, TCHOTrimStaTrain), p=6, type="const")
VARTCHOStaTrim <- forecast(VAR(cbind(MSETrimStaTrain, TCHOTrimStaTrain), p=4, type="const"))
plot(window(MSETrimTest, end=c(2016,4)))
lines(VARTCHOStaTrim$forecast$MSETrimStaTrain$mean*MSETrimSeasonalTest*MSETrimTrendTest, col = "red")
EQM(window(MSETrimTest, end=c(2016,4)), VARTCHOStaTrim$forecast$MSETrimStaTrain$mean*MSETrimSeasonalTest*MSETrimTrendTest)
plot(TCHOTrimStaTest, ylim=c(-1,1))
lines(VARTCHOStaTrim$forecast$TCHOTrimStaTrain$mean, col = "red")

plot(MSETrimStaTest*MSETrimSeasonalTest*MSETrimTrendTest)
lines(VARPIBStaTrim$forecast$MSETrimStaTrain$mean*MSETrimSeasonalTest*MSETrimTrendTest, col="green")
lines(VARSMICStaTrim$forecast$MSETrimStaTrain$mean*MSETrimSeasonalTest*MSETrimTrendTest, col="red")
lines(VARTCHOStaTrim$forecast$MSETrimStaTrain$mean*MSETrimSeasonalTest*MSETrimTrendTest, col="gold")
legend('bottomleft', legend = c('Série MSE', 'PIB', 'SMIC', 'Taux chômage'),
       col=c('black', 'green', 'red', 'gold'), lty=1, cex=0.8)

#2 Variables
#PIB & SMIC
VARselect(cbind(MSETrimStaTrain, PIBTrimStaTrain, SMICTrimStaTrain), lag.max=10)
VAR(cbind(MSETrimStaTrain, PIBTrimStaTrain, SMICTrimStaTrain), p=5, type="const")
VARPIBSMICStaTrim <- forecast(VAR(cbind(MSETrimStaTrain, PIBTrimStaTrain, SMICTrimStaTrain), p=5, type="const"))
plot(window(MSETrimTest, end=c(2016,4)))
lines(VARPIBSMICStaTrim$forecast$MSETrimStaTrain$mean*MSETrimSeasonalTest*MSETrimTrendTest, col = "red")
EQM(window(MSETrimTest, end=c(2016,4)), VARPIBSMICStaTrim$forecast$MSETrimStaTrain$mean*MSETrimSeasonalTest*MSETrimTrendTest)
plot(PIBTrimStaTest, ylim=c(0.99,1.01))
lines(VARPIBSMICStaTrim$forecast$PIBTrimStaTrain$mean, col = "red")
plot(SMICTrimStaTest, ylim=c(-0.5,0.5))
lines(VARPIBSMICStaTrim$forecast$SMICTrimStaTrain$mean, col = "red")

#PIB & TCHO
VARselect(cbind(MSETrimStaTrain, PIBTrimStaTrain, TCHOTrimStaTrain), lag.max=5)
VAR(cbind(MSETrimStaTrain, PIBTrimStaTrain, TCHOTrimStaTrain), p=5, type="const")
VARPIBTCHOStaTrim <- forecast(VAR(cbind(MSETrimStaTrain, PIBTrimStaTrain, TCHOTrimStaTrain), p=5, type="const"))
plot(window(MSETrimTest, end=c(2016,4)))
lines(VARPIBTCHOStaTrim$forecast$MSETrimStaTrain$mean*MSETrimSeasonalTest*MSETrimTrendTest, col = "red")
EQM(window(MSETrimTest, end=c(2016,4)), VARPIBTCHOStaTrim$forecast$MSETrimStaTrain$mean*MSETrimSeasonalTest*MSETrimTrendTest)
plot(PIBTrimStaTest, ylim=c(0.99,1.01))
lines(VARPIBTCHOStaTrim$forecast$PIBTrimStaTrain$mean, col = "red")
plot(TCHOTrimStaTest, ylim=c(-0.5,0.5))
lines(VARPIBTCHOStaTrim$forecast$TCHOTrimStaTrain$mean, col = "red")

#SMIC & TCHO
VARselect(cbind(MSETrimStaTrain, SMICTrimStaTrain, TCHOTrimStaTrain), lag.max=5)
VAR(cbind(MSETrimStaTrain, SMICTrimStaTrain, TCHOTrimStaTrain), p=5, type="const")
VARSMICTCHOStaTrim <- forecast(VAR(cbind(MSETrimStaTrain, SMICTrimStaTrain, TCHOTrimStaTrain), p=5, type="const"))
plot(window(MSETrimTest, end=c(2016,4)))
lines(VARSMICTCHOStaTrim$forecast$MSETrimStaTrain$mean*MSETrimSeasonalTest*MSETrimTrendTest, col = "red")
EQM(window(MSETrimTest, end=c(2016,4)), VARSMICTCHOStaTrim$forecast$MSETrimStaTrain$mean*MSETrimSeasonalTest*MSETrimTrendTest)
plot(SMICTrimStaTest, ylim=c(-0.5,0.5))
lines(VARSMICTCHOStaTrim$forecast$SMICTrimStaTrain$mean, col = "red")
plot(TCHOTrimStaTest, ylim=c(-1,1))
lines(VARSMICTCHOStaTrim$forecast$TCHOTrimStaTrain$mean, col = "red")

plot(MSETrimStaTest*MSETrimSeasonalTest*MSETrimTrendTest)
lines(VARPIBSMICStaTrim$forecast$MSETrimStaTrain$mean*MSETrimSeasonalTest*MSETrimTrendTest, col="green")
lines(VARPIBTCHOStaTrim$forecast$MSETrimStaTrain$mean*MSETrimSeasonalTest*MSETrimTrendTest, col="red")
lines(VARSMICTCHOStaTrim$forecast$MSETrimStaTrain$mean*MSETrimSeasonalTest*MSETrimTrendTest, col="gold")
legend('bottomleft', legend = c('Série MSE', 'PIB', 'SMIC', 'Taux chômage'),
       col=c('black', 'green', 'red', 'gold'), lty=1, cex=0.8)


#3 Variables
#PIB & SMIC & TCHO
VARselect(cbind(MSETrimStaTrain, PIBTrimStaTrain, SMICTrimStaTrain, TCHOTrimStaTrain), lag.max=10)
VAR(cbind(MSETrimStaTrain, PIBTrimStaTrain, SMICTrimStaTrain, TCHOTrimStaTrain), p=4, type="const")
VARPIBSMICTCHOStaTrim <- forecast(VAR(cbind(MSETrimStaTrain, PIBTrimStaTrain, SMICTrimStaTrain, TCHOTrimStaTrain), p=10, type="const"))
plot(window(MSETrimTest, end=c(2016,4)))
lines(VARPIBSMICTCHOStaTrim$forecast$MSETrimStaTrain$mean*MSETrimSeasonalTest*MSETrimTrendTest, col = "red")
EQM(window(MSETrimTest, end=c(2016,4)), VARPIBSMICTCHOStaTrim$forecast$MSETrimStaTrain$mean*MSETrimSeasonalTest*MSETrimTrendTest)
plot(PIBTrimStaTest, ylim=c(0.99,1.01))
lines(VARPIBSMICTCHOStaTrim$forecast$PIBTrimStaTrain$mean, col = "red")
plot(SMICTrimStaTest, ylim=c(-0.25,0.25))
lines(VARPIBSMICTCHOStaTrim$forecast$SMICTrimStaTrain$mean, col = "red")
plot(TCHOTrimStaTest, ylim=c(-0.7,0.7))
lines(VARPIBSMICTCHOStaTrim$forecast$TCHOTrimStaTrain$mean, col = "red")

#Test du Portmanteau
test <- VAR(cbind(MSETrimStaTrain, PIBTrimStaTrain, SMICTrimStaTrain, TCHOTrimStaTrain), p=5, type="const")
LjungBox(residuals(test),(1:10))
