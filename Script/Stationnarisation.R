library(fUnitRoots)

#Stationnarisation des variables

  # #MSE
  # adf.test(MSEAnn)
  # MSEAnnSta <- MSEAnn - window(ma(MSEAnn, order=3), start=1991, end=2016)
  # acf(MSEAnnSta)
  # pacf(MSEAnnSta)
  # kpss.test(MSEAnnSta)
  # plot(MSEAnnSta)
  # MSEAnnStaTrain <- window(MSEAnnSta, end=2015)
  # MSEAnnStaTest <- window(MSEAnnSta, start=2016)
  # 
  # adf.test(MSETrim)
  # MSETrimSta <- na.omit(decompose(MSETrim, "multiplicative")$random)
  # MSETrimTrendTest<-window(decompose(MSETrim, "multiplicative")$trend, start=2016, end=c(2016,4))
  # MSETrimSeasonalTest<-window(decompose(MSETrim, "multiplicative")$seasonal, start=2016, end=c(2016,4))
  # acf(MSETrimSta)
  # pacf(MSETrimSta)
  # kpss.test(MSETrimSta)
  # plot(MSETrimSta)
  # MSETrimStaTrain <- window(MSETrimSta, end=c(2015,4))
  # MSETrimStaTest <- window(MSETrimSta, start=2016)
  # 
  # #PIB
  # summary(lm(MSEAnn ~ PIBAnn[1:28]))
  # adf.test(PIBAnn)
  # PIBAnnSta <- PIBAnn - window(ma(PIBAnn, order=3), start=1991, end=2016)
  # acf(PIBAnnSta)
  # pacf(PIBAnnSta)
  # kpss.test(PIBAnnSta)
  # plot(PIBAnnSta)
  # PIBAnnStaTrain <- window(PIBAnnSta, end=2015)
  # PIBAnnStaTest <- window(PIBAnnSta, start=2016)
  # 
  # summary(lm(MSETrim[1:109] ~ PIBTrim))
  # adf.test(PIBTrim)
  # PIBTrimSta <- na.omit(decompose(PIBTrim, "multiplicative")$random)
  # acf(PIBTrimSta)
  # pacf(PIBTrimSta)
  # kpss.test(PIBTrimSta)
  # adfTest(PIBTrimSta, type='c')
  # plot(PIBTrimSta)
  # PIBTrimStaTrain <- window(PIBTrimSta, end=c(2015,4))
  # PIBTrimStaTest <- window(PIBTrimSta, start=2016)
  # 
  # #SMIC
  # summary(lm(MSEAnn ~ SMICAnn[1:28]))
  # adf.test(SMICAnn)
  # SMICAnnSta <- SMICAnn - window(ma(SMICAnn, order=3), start=1991, end=2016)
  # acf(SMICAnnSta)
  # pacf(SMICAnnSta)
  # kpss.test(SMICAnnSta)
  # plot(SMICAnnSta)
  # SMICAnnStaTrain <- window(SMICAnnSta, end=2015)
  # SMICAnnStaTest <- window(SMICAnnSta, start=2016)
  # 
  # summary(lm(MSETrim ~ SMICTrim[1:110]))
  # adf.test(SMICTrim[1:110])
  # SMICTrimSta <- na.omit(decompose(SMICTrim)$random)
  # acf(SMICTrimSta)
  # pacf(SMICTrimSta)
  # kpss.test(SMICTrimSta)
  # plot(SMICTrimSta)
  # SMICTrimStaTrain <- window(SMICTrimSta, end=c(2015,4))
  # SMICTrimStaTest <- window(SMICTrimSta, start=2016)
  # 
  # #TCHO
  # summary(lm(MSEAnn ~ TCHOAnn[1:28]))
  # adf.test(TCHOAnn)
  # TCHOAnnSta <- TCHOAnn - window(ma(TCHOAnn, order=3), start=1991, end=2016)
  # acf(TCHOAnnSta)
  # pacf(TCHOAnnSta)
  # kpss.test(TCHOAnnSta)
  # plot(TCHOAnnSta)
  # TCHOAnnStaTrain <- window(TCHOAnnSta, end=2015)
  # TCHOAnnStaTest <- window(TCHOAnnSta, start=2016)
  # 
  # summary(lm(MSETrim ~ TCHOTrim[1:110]))
  # adf.test(TCHOTrim[1:110])
  # TCHOTrimSta <- na.omit(decompose(TCHOTrim)$random)
  # acf(TCHOTrimSta)
  # pacf(TCHOTrimSta)
  # kpss.test(TCHOTrimSta)
  # plot(TCHOTrimSta)
  # TCHOTrimStaTrain <- window(TCHOTrimSta, end=c(2015,4))
  # TCHOTrimStaTest <- window(TCHOTrimSta, start=2016)
  # 
  # #AGED
  # summary(lm(MSEAnn ~ AGEDAnn[1:28]))
  # adf.test(AGEDAnn)
  # AGEDAnnSta <- AGEDAnn - window(ma(AGEDAnn, order=3), start=1991, end=2016)
  # acf(AGEDAnnSta)
  # pacf(AGEDAnnSta)
  # kpss.test(AGEDAnnSta)
  # plot(AGEDAnnSta)
  # AGEDAnnStaTrain <- window(AGEDAnnSta, end=2015)
  # AGEDAnnStaTest <- window(AGEDAnnSta, start=2016)
  
  
  adf.test(MSETrim)
  MSETrimSta<-residuals(ets(MSETrimTrain, "ZAM"))
  acf(MSETrimSta)
  pacf(MSETrimSta)
  kpss.test(MSETrimSta)
  adf.test(MSETrimSta)
  plot(MSETrimSta)
  MSETrimFitted<-forecast(ets(MSETrimTrain, "ZAM"), h=6)$mean
  
  adf.test(PIBTrimTrain)
  PIBTrimSta<-auto.arima(PIBTrimTrain)$residuals
  acf(PIBTrimSta)
  pacf(PIBTrimSta)
  kpss.test(PIBTrimSta)
  adf.test(PIBTrimSta)
  plot(PIBTrimSta)
  PIBTrimFitted<-forecast(auto.arima(PIBTrimTrain), h=6)$mean
  
  adf.test(SMICTrimTrain)
  SMICTrimSta<-residuals(ets(SMICTrimTrain, "ZAA"))
  acf(SMICTrimSta)
  pacf(SMICTrimSta)
  kpss.test(SMICTrimSta)
  adf.test(SMICTrimSta)
  plot(SMICTrimSta)
  SMICTrimFitted<-forecast(ets(SMICTrimTrain, "ZAA"), h=6)$mean
  
  adf.test(TCHOFTrimTrain)
  TCHOFTrimSta<-residuals(ets(TCHOFTrimTrain, "ZNN"))
  acf(TCHOFTrimSta)
  pacf(TCHOFTrimSta)
  kpss.test(TCHOFTrimSta)
  adf.test(TCHOFTrimSta)
  plot(TCHOFTrimSta)
  TCHOFTrimFitted<-forecast(ets(TCHOFTrimTrain, "ZNN"), h=6)$mean
  