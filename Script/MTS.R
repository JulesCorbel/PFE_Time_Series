#Imports
library(MTS)
library(Matrix)

appAnn<-cbind(MSEAnnStaTrain, PIBAnnStaTrain, SMICAnnStaTrain, TCHOAnnStaTrain, AGEDAnnStaTrain)
testAnn<-cbind(MSEAnnStaTest, PIBAnnStaTest, SMICAnnStaTest, TCHOAnnStaTest, AGEDAnnStaTest)
cor(appAnnSta)
MTSplot(appAnn)

#1 variable
VARorder(appAnn[,c(1:2)], maxp=5)
MTSAnnPIB<-VAR(appAnn[,c(1:2)], p=1, output=F, include.mean = F)
PredAnnMTSPIB<-ts(VARpred(MTSAnnPIB, 2)$pred[,1], start=2016)
plot(MSEAnnStaTest, ylim=c(min(MSEAnnStaTest, PredAnnMTSPIB), max(MSEAnnStaTest, PredAnnMTSPIB)))
lines(PredAnnMTSPIB, col="red")
EQM(MSEAnnStaTest, PredAnnMTSPIB)

VARorder(appAnn[,c(1:3)], maxp=5)
MTSAnnSMIC<-VAR(appAnn[,c(1:3)], p=1, output=F, include.mean = T)
PredAnnMTSSMIC<-ts(VARpred(MTSAnnSMIC, 2)$pred[,1], start=2016)
plot(MSEAnnStaTest, ylim=c(min(MSEAnnStaTest, PredAnnMTSSMIC), max(MSEAnnStaTest, PredAnnMTSSMIC)))
lines(PredAnnMTSSMIC, col="red")
EQM(MSEAnnStaTest, PredAnnMTSSMIC)

VARorder(appAnn[,c(1:4)], maxp=5)
MTSAnnTCHO<-VAR(appAnn[,c(1:4)], p=1, output=F, include.mean = T)
PredAnnMTSTCHO<-ts(VARpred(MTSAnnTCHO, 2)$pred[,1], start=2016)
plot(MSEAnnStaTest, ylim=c(min(MSEAnnStaTest, PredAnnMTSTCHO), max(MSEAnnStaTest, PredAnnMTSTCHO)))
lines(PredMTSTCHO, col="red")
EQM(MSEAnnStaTest, PredAnnMTSTCHO)

VARorder(appAnn[,c(1:5)], maxp=5)
MTSAnnAGED<-VAR(appAnn[,c(1:5)], p=1, output=F, include.mean = T)
PredAnnMTSAGED<-ts(VARpred(MTSAnnAGED, 2)$pred[,1], start=2016)
plot(MSEAnnStaTest, ylim=c(min(MSEAnnStaTest, PredAnnMTSAGED), max(MSEAnnStaTest, PredAnnMTSAGED)))
lines(PredMTSAGED, col="red")
EQM(MSEAnnStaTest, PredAnnMTSAGED)



#Trimestrielle

  #1 variable
    appTrim<-cbind(MSETrimStaTrain, PIBTrimStaTrain, SMICTrimStaTrain, TCHOTrimStaTrain)
    testTrim<-cbind(MSETrimStaTest, PIBTrimStaTest, SMICTrimStaTest, TCHOTrimStaTest)
    cor(appTrim)
    MTSplot(appTrim)
    
    VARorder(appTrim[,c(1:2)], maxp=10)
    MTSTrimPIB<-VAR(appTrim[,c(1:2)], p=4, output=F)
    PredTrimMTSPIB<-ts(VARpred(MTSTrimPIB, 4)$pred[,1], start=2016, frequency=4)
    plot(MSETrimStaTest, ylim=c(min(MSETrimStaTest, PredTrimMTSPIB), max(MSETrimStaTest, PredTrimMTSPIB)))
    lines(PredTrimMTSPIB, col="red")
    EQM(MSETrimStaTest, PredTrimMTSPIB)
    
    VARorder(appTrim[,c(1,3)], maxp=10)
    MTSTrimSMIC<-VAR(appTrim[,c(1,3)], p=3, output=F)
    PredTrimMTSSMIC<-ts(VARpred(MTSTrimSMIC, 4)$pred[,1], start=2016, frequency=4)
    plot(MSETrimStaTest, ylim=c(min(MSETrimStaTest, PredTrimMTSSMIC), max(MSETrimStaTest, PredTrimMTSSMIC)))
    lines(PredTrimMTSSMIC, col="red")
    EQM(MSETrimStaTest, PredTrimMTSSMIC)
    
    VARorder(appTrim[,c(1,4)], maxp=20)
    MTSTrimTCHO<-VAR(appTrim[,c(1,4)], p=3, output=F)
    PredTrimMTSTCHO<-ts(VARpred(MTSTrimTCHO, 4)$pred[,1], start=2016, frequency=4)
    plot(MSETrimStaTest, ylim=c(min(MSETrimStaTest, PredTrimMTSTCHO), max(MSETrimStaTest, PredTrimMTSTCHO)))
    lines(PredTrimMTSTCHO, col="red")
    EQM(MSETrimStaTest, PredTrimMTSTCHO)
    
    plot(MSETrimStaTest, ylim=c(min(MSETrimStaTest), max(MSETrimStaTest)))
    lines(PredTrimMTSPIB, col="red")
    lines(PredTrimMTSSMIC, col="blue")
    lines(PredTrimMTSTCHO, col="green")
    legend('bottomleft', 
           legend = c('Série MSE', 'PIB', 'SMIC', 'Taux chômage'),
           col=c('black', 'red', 'blue', 'green'), lty=1, cex=0.8)
  
  #2 variables
    VARorder(appTrim[,c(1:3)], maxp=10)
    MTSTrimPIBSMIC<-VAR(appTrim[,c(1:3)], p=4, output=F)
    PredTrimMTSPIBSMIC<-ts(VARpred(MTSTrimPIBSMIC, 4)$pred[,1], start=2016, frequency=4)
    plot(MSETrimStaTest, ylim=c(min(MSETrimStaTest, PredTrimMTSPIBSMIC), max(MSETrimStaTest, PredTrimMTSPIBSMIC)))
    lines(PredTrimMTSPIBSMIC, col="red")
    EQM(MSETrimStaTest, PredTrimMTSPIBSMIC)
    
    VARorder(appTrim[,c(1,2,4)], maxp=10)
    MTSTrimPIBTCHO<-VAR(appTrim[,c(1,2,4)], p=3, output=F)
    PredTrimMTSPIBTCHO<-ts(VARpred(MTSTrimPIBTCHO, 4)$pred[,1], start=2016, frequency=4)
    plot(MSETrimStaTest, ylim=c(min(MSETrimStaTest, PredTrimMTSPIBTCHO), max(MSETrimStaTest, PredTrimMTSPIBTCHO)))
    lines(PredTrimMTSPIBTCHO, col="red")
    EQM(MSETrimStaTest, PredTrimMTSPIBTCHO)
    
    VARorder(appTrim[,c(1,3,4)], maxp=20)
    MTSTrimSMICTCHO<-VAR(appTrim[,c(1,3,4)], p=19, output=F)
    PredTrimMTSSMICTCHO<-ts(VARpred(MTSTrimSMICTCHO, 4)$pred[,1], start=2016, frequency=4)
    plot(MSETrimStaTest, ylim=c(min(MSETrimStaTest, PredTrimMTSSMICTCHO), max(MSETrimStaTest, PredTrimMTSSMICTCHO)))
    lines(PredTrimMTSSMICTCHO, col="red")
    EQM(MSETrimStaTest, PredTrimMTSSMICTCHO)

    plot(MSETrimStaTest, ylim=c(min(PredTrimMTSSMICTCHO), max(PredTrimMTSSMICTCHO)))
    lines(PredTrimMTSPIBSMIC, col="red")
    lines(PredTrimMTSPIBTCHO, col="blue")
    lines(PredTrimMTSSMICTCHO, col="green")
    legend('bottomleft', 
           legend = c('Série MSE', 'PIB & SMIC', 'PIB & Taux chômage', 'SMIC & Taux chômage'),
           col=c('black', 'red', 'blue', 'green'), lty=1, cex=0.8)
    
    VARorder(appTrim[,c(1:4)], maxp=20)
    MTSTrimCOMPLET<-VAR(appTrim[,c(1:4)], p=3, output=F)
    PredTrimMTSCOMPLET<-ts(VARpred(MTSTrimCOMPLET, 4)$pred[,1], start=2016, frequency=4)
    EQM(MSETrimStaTest, PredTrimMTSCOMPLET)
    
    plot(MSETrimStaTest, ylim=c(min(MSETrimStaTest, PredTrimMTSPIB, PredTrimMTSPIBSMIC, PredTrimMTSCOMPLET), max(MSETrimStaTest, PredTrimMTSPIB, PredTrimMTSPIBSMIC, PredTrimMTSCOMPLET)))
    lines(PredTrimMTSPIB, col="red")
    lines(PredTrimMTSPIBSMIC, col="blue")
    lines(PredTrimMTSCOMPLET, col="green")
    legend('bottomleft', 
           legend = c('Série MSE', 'SMIC', 'SMIC & Taux chômage', 'COMPLET'),
           col=c('black', 'red', 'blue', 'green'), lty=1, cex=0.8)
    
    
    