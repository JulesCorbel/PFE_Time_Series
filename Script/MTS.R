#Imports
library(MTS)
library(Matrix)

appAnn<-cbind(MSEAnnStaTrain, PIBAnnStaTrain, SMICAnnStaTrain, TCHOAnnStaTrain, AGEDAnnStaTrain)
testAnn<-cbind(MSEAnnStaTest, PIBAnnStaTest, SMICAnnStaTest, TCHOAnnStaTest, AGEDAnnStaTest)
cor(appAnnSta)
MTSplot(appAnn)

#1 variable
VARorder(appAnn[,c(1:2)], maxp=5)
MTSPIB<-VAR(appAnn[,c(1:2)], p=1, output=F, include.mean = F)
PredMTSPIB<-ts(VARpred(MTSPIB, 2)$pred[,1], start=2016)
plot(MSEAnnStaTest, ylim=c(min(MSEAnnStaTest, PredMTSPIB), max(MSEAnnStaTest, PredMTSPIB)))
lines(PredMTSPIB, col="red")
EQM(MSEAnnStaTest, PredMTSPIB)

VARorder(appAnn[,c(1:3)], maxp=5)
MTSSMIC<-VAR(appAnn[,c(1:3)], p=1, output=F, include.mean = T)
PredMTSSMIC<-ts(VARpred(MTSSMIC, 2)$pred[,1], start=2016)
plot(MSEAnnStaTest, ylim=c(min(MSEAnnStaTest, PredMTSSMIC), max(MSEAnnStaTest, PredMTSSMIC)))
lines(PredMTSSMIC, col="red")
EQM(MSEAnnStaTest, PredMTSSMIC)

VARorder(appAnn[,c(1:4)], maxp=5)
MTSTCHO<-VAR(appAnn[,c(1:4)], p=1, output=F, include.mean = T)
PredMTSTCHO<-ts(VARpred(MTSTCHO, 2)$pred[,1], start=2016)
plot(MSEAnnStaTest, ylim=c(min(MSEAnnStaTest, PredMTSTCHO), max(MSEAnnStaTest, PredMTSTCHO)))
lines(PredMTSTCHO, col="red")
EQM(MSEAnnStaTest, PredMTSTCHO)

VARorder(appAnn[,c(1:5)], maxp=5)
MTSAGED<-VAR(appAnn[,c(1:5)], p=1, output=F, include.mean = T)
PredMTSAGED<-ts(VARpred(MTSAGED, 2)$pred[,1], start=2016)
plot(MSEAnnStaTest, ylim=c(min(MSEAnnStaTest, PredMTSAGED), max(MSEAnnStaTest, PredMTSAGED)))
lines(PredMTSAGED, col="red")
EQM(MSEAnnStaTest, PredMTSAGED)
