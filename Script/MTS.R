#Imports
library(MTS)
library(Matrix)

# appAnn<-cbind(MSEAnnStaTrain, PIBAnnStaTrain, SMICAnnStaTrain, TCHOAnnStaTrain, AGEDAnnStaTrain)
# testAnn<-cbind(MSEAnnStaTest, PIBAnnStaTest, SMICAnnStaTest, TCHOAnnStaTest, AGEDAnnStaTest)
# cor(appAnn)
# MTSplot(appAnn)
# 
# x<-t(appAnn[,-c(3:4)]) %*% appAnn[,-c(3:4)]
# rankMatrix(x)

appTrim<-cbind(MSETrimSta, PIBTrimSta, SMICTrimSta, TCHOFTrimSta)
cor(appTrim)
MTSplot(appTrim)

# #Annuelle
#   #1 variable
#   VARorder(appAnn[,-c(3:4)], maxp=5)
#   MTSAnnPIB<-VAR(appAnn[,-c(3:4)], p=1, output=F)
#   PredAnnMTSPIB<-ts(VARpred(MTSAnnPIB, 2)$pred[,1], start=2016)
#   plot(MSEAnnStaTest, xlim=c(2015,2017), ylim=c(-1.5e+08, max(PredAnnMTSPIB)), col="blue", pch=20, cex=2)
#   points(PredAnnMTSPIB, col="red")
#   EQM(MSEAnnStaTest, PredAnnMTSPIB)
#   
#   VARorder(appAnn[,c(1:3)], maxp=5)
#   MTSAnnSMIC<-VAR(appAnn[,c(1:3)], p=1, output=F, include.mean = T)
#   PredAnnMTSSMIC<-ts(VARpred(MTSAnnSMIC, 2)$pred[,1], start=2016)
#   plot(MSEAnnStaTest, ylim=c(min(MSEAnnStaTest, PredAnnMTSSMIC), max(MSEAnnStaTest, PredAnnMTSSMIC)))
#   lines(PredAnnMTSSMIC, col="red")
#   EQM(MSEAnnStaTest, PredAnnMTSSMIC)
#   
#   VARorder(appAnn[,c(1:4)], maxp=5)
#   MTSAnnTCHO<-VAR(appAnn[,c(1:4)], p=1, output=F, include.mean = T)
#   PredAnnMTSTCHO<-ts(VARpred(MTSAnnTCHO, 2)$pred[,1], start=2016)
#   plot(MSEAnnStaTest, ylim=c(min(MSEAnnStaTest, PredAnnMTSTCHO), max(MSEAnnStaTest, PredAnnMTSTCHO)))
#   lines(PredMTSTCHO, col="red")
#   EQM(MSEAnnStaTest, PredAnnMTSTCHO)
#   
#   VARorder(appAnn[,c(1:5)], maxp=5)
#   MTSAnnAGED<-VAR(appAnn[,c(1:5)], p=1, output=F, include.mean = T)
#   PredAnnMTSAGED<-ts(VARpred(MTSAnnAGED, 2)$pred[,1], start=2016)
#   plot(MSEAnnStaTest, ylim=c(min(MSEAnnStaTest, PredAnnMTSAGED), max(MSEAnnStaTest, PredAnnMTSAGED)))
#   lines(PredMTSAGED, col="red")
#   EQM(MSEAnnStaTest, PredAnnMTSAGED)

#Trimestrielle

  #1 variable
    VARorder(appTrim[,c(1:2)], maxp=10)
    MTSTrimPIB<-VAR(appTrim[,c(1:2)], p=3, output=F)
    MTSTrimPIB2<-VAR(appTrim[,c(1:2)], p=4, output=F)
    
    stabilityMTS(MTSTrimPIB)
    stabilityMTS(MTSTrimPIB2)
    
    PredTrimMTSPIB<-ts(VARpred(MTSTrimPIB, 6)$pred[,1], start=2016, frequency=4)
    PredTrimMTSPIB2<-ts(VARpred(MTSTrimPIB2, 6)$pred[,1], start=2016, frequency=4)
    
    plot(window(MSETrimTest, end=c(2016,4)))
    lines(PredTrimMTSPIB*MSESeasonalTest*MSETrendTest, col="red")
    EQM(window(MSETrimTest, end=c(2016,4)), PredTrimMTSPIB*MSETrimSeasonalTest*MSETrimTrendTest)
    
    VARorder(appTrim[,c(1,3)], maxp=10)
    MTSTrimSMIC<-VAR(appTrim[,c(1,3)], p=3, output=F)
    PredTrimMTSSMIC<-ts(VARpred(MTSTrimSMIC, 4)$pred[,1], start=2016, frequency=4)
    plot(window(MSETrimTest, end=c(2016,4)),
         ylim=c(min(MSETrimTest, PredTrimMTSSMIC*MSETrimSeasonalTest*MSETrimTrendTest),
                max(MSETrimTest, PredTrimMTSSMIC*MSETrimSeasonalTest*MSETrimTrendTest)))
    lines(PredTrimMTSSMIC*MSETrimSeasonalTest*MSETrimTrendTest, col="red")
    EQM(window(MSETrimTest, end=c(2016,4)), PredTrimMTSSMIC*MSETrimSeasonalTest*MSETrimTrendTest)
    
    VARorder(appTrim[,c(1,4)], maxp=20)
    MTSTrimTCHO<-VAR(appTrim[,c(1,4)], p=3, output=F)
    PredTrimMTSTCHO<-ts(VARpred(MTSTrimTCHO, 4)$pred[,1], start=2016, frequency=4)
    plot(window(MSETrimTest, end=c(2016,4)),
         ylim=c(min(MSETrimTest, PredTrimMTSTCHO*MSETrimSeasonalTest*MSETrimTrendTest),
                max(MSETrimTest, PredTrimMTSTCHO*MSETrimSeasonalTest*MSETrimTrendTest)))
    lines(PredTrimMTSTCHO*MSETrimSeasonalTest*MSETrimTrendTest, col="red")
    EQM(window(MSETrimTest, end=c(2016,4)), PredTrimMTSTCHO*MSETrimSeasonalTest*MSETrimTrendTest)
    
    plot(window(MSETrimTest, end=c(2016,4)))
    lines(PredTrimMTSPIB*MSETrimSeasonalTest*MSETrimTrendTest, col="red")
    lines(PredTrimMTSSMIC*MSETrimSeasonalTest*MSETrimTrendTest, col="blue")
    lines(PredTrimMTSTCHO*MSETrimSeasonalTest*MSETrimTrendTest, col="green")
    legend('bottomleft', 
           legend = c('Série MSE', 'PIB', 'SMIC', 'Taux chômage'),
           col=c('black', 'red', 'blue', 'green'), lty=1, cex=0.8)
  
  #2 variables
    VARorder(appTrim[,c(1:3)], maxp=10)
    MTSTrimPIBSMIC<-VAR(appTrim[,c(1:3)], p=4, output=F)
    PredTrimMTSPIBSMIC<-ts(VARpred(MTSTrimPIBSMIC, 4)$pred[,1], start=2016, frequency=4)
    plot(window(MSETrimTest, end=c(2016,4)),
         ylim=c(min(MSETrimTest, PredTrimMTSPIBSMIC*MSETrimSeasonalTest*MSETrimTrendTest),
                max(MSETrimTest, PredTrimMTSPIBSMIC*MSETrimSeasonalTest*MSETrimTrendTest)))
    lines(PredTrimMTSPIBSMIC*MSETrimSeasonalTest*MSETrimTrendTest, col="red")
    EQM(window(MSETrimTest, end=c(2016,4)), PredTrimMTSPIBSMIC*MSETrimSeasonalTest*MSETrimTrendTest)
    
    VARorder(appTrim[,c(1,2,4)], maxp=10)
    MTSTrimPIBTCHO<-VAR(appTrim[,c(1,2,4)], p=3, output=F)
    PredTrimMTSPIBTCHO<-ts(VARpred(MTSTrimPIBTCHO, 4)$pred[,1], start=2016, frequency=4)
    plot(window(MSETrimTest, end=c(2016,4)),
         ylim=c(min(MSETrimTest, PredTrimMTSPIBTCHO*MSETrimSeasonalTest*MSETrimTrendTest),
                max(MSETrimTest, PredTrimMTSPIBTCHO*MSETrimSeasonalTest*MSETrimTrendTest)))
    lines(PredTrimMTSPIBTCHO*MSETrimSeasonalTest*MSETrimTrendTest, col="red")
    EQM(window(MSETrimTest, end=c(2016,4)), PredTrimMTSPIBTCHO*MSETrimSeasonalTest*MSETrimTrendTest)
    
    VARorder(appTrim[,c(1,3,4)], maxp=10)
    MTSTrimSMICTCHO<-VAR(appTrim[,c(1,3,4)], p=3, output=F)
    PredTrimMTSSMICTCHO<-ts(VARpred(MTSTrimSMICTCHO, 4)$pred[,1], start=2016, frequency=4)
    plot(window(MSETrimTest, end=c(2016,4)),
         ylim=c(min(MSETrimTest, PredTrimMTSSMICTCHO*MSETrimSeasonalTest*MSETrimTrendTest),
                max(MSETrimTest, PredTrimMTSSMICTCHO*MSETrimSeasonalTest*MSETrimTrendTest)))
    lines(PredTrimMTSSMICTCHO*MSETrimSeasonalTest*MSETrimTrendTest, col="red")
    EQM(window(MSETrimTest, end=c(2016,4)), PredTrimMTSSMICTCHO*MSETrimSeasonalTest*MSETrimTrendTest)

    plot(window(MSETrimTest, end=c(2016,4)))
    lines(PredTrimMTSPIBSMIC*MSETrimSeasonalTest*MSETrimTrendTest, col="red")
    lines(PredTrimMTSPIBTCHO*MSETrimSeasonalTest*MSETrimTrendTest, col="blue")
    lines(PredTrimMTSSMICTCHO*MSETrimSeasonalTest*MSETrimTrendTest, col="green")
    legend('bottomleft', 
           legend = c('Série MSE', 'PIB & SMIC', 'PIB & Taux chômage', 'SMIC & Taux chômage'),
           col=c('black', 'red', 'blue', 'green'), lty=1, cex=0.8)
    
    #Modèle complet
    
    VARorder(appTrim[,c(1:4)], maxp=10)
    MTSTrimCOMPLET<-VAR(appTrim[,c(1:4)], p=4)
    PredTrimMTSCOMPLET<-ts(VARpred(MTSTrimCOMPLET, 4)$pred[,1], start=2016, frequency=4)
    EQM(window(MSETrimTest, end=c(2016,4)), PredTrimMTSCOMPLET+MSETrimFitted)
    
    plot(window(MSETrimTest, end=c(2016,4)))
    lines(PredTrimMTSPIB*MSETrimSeasonalTest*MSETrimTrendTest, col="red")
    lines(PredTrimMTSPIBSMIC*MSETrimSeasonalTest*MSETrimTrendTest, col="blue")
    lines(PredTrimMTSCOMPLET*MSETrimSeasonalTest*MSETrimTrendTest, col="green")
    legend('bottomleft', 
           legend = c('Série MSE', 'SMIC', 'SMIC & Taux chômage', 'COMPLET'),
           col=c('black', 'red', 'blue', 'green'), lty=1, cex=0.8)
    
    
    