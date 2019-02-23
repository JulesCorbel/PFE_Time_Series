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
  adf.test(MSETrimTrain)
  MSETrimSta <- na.omit(decompose(MSETrimTrain, "multiplicative")$random)
  MSETrimTrend<-window(decompose(MSETrimTrain, "multiplicative")$trend)
  MSETrimSeasonal<-window(decompose(MSETrimTrain, "multiplicative")$seasonal)
  
  MSETrimSta2 <- window(na.omit(decompose(MSETrim, "multiplicative")$random), end=c(2015,4))
  MSETrendTest2<-window(na.omit(decompose(MSETrim, "multiplicative")$trend), start=2016, end=c(2016,4))
  MSESeasonalTest2<-window(na.omit(decompose(MSETrim, "multiplicative")$seasonal), start=2016)
  
  acf(MSETrimSta)
  pacf(MSETrimSta)
  kpss.test(MSETrimSta)
  plot(MSETrimSta)
  MSETrendTest <- forecast(na.omit(MSETrimTrend), h=14)
  MSESeasonalTest<-ts(c(MSETrimSeasonal[103:104], rep(MSETrimSeasonal[1:4],3)), start=c(2015,3),frequency=4)

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
  summary(lm(MSETrim[1:109] ~ PIBTrim))
  adf.test(PIBTrimTrain)
  PIBTrimSta <- na.omit(decompose(PIBTrimTrain, "multiplicative")$random)
  PIBTrimSta2 <- window(na.omit(decompose(PIBTrim, "multiplicative")$random), end=c(2015,4))
  acf(PIBTrimSta)
  pacf(PIBTrimSta)
  kpss.test(PIBTrimSta)
  adf.test(PIBTrimSta)
  plot(PIBTrimSta)
  

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
  summary(lm(MSETrim ~ SMICTrim))
  adf.test(SMICTrimTrain)
  SMICTrimSta <- na.omit(decompose(SMICTrimTrain)$random)
  SMICTrimSta2 <- window(na.omit(decompose(SMICTrim)$random), end=c(2015,4))
  acf(SMICTrimSta)
  pacf(SMICTrimSta)
  kpss.test(SMICTrimSta)
  plot(SMICTrimSta)

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

  summary(lm(MSETrim ~ TCHOFTrim[1:110]))
  adf.test(TCHOFTrimTrain)
  TCHOFTrimSta <- na.omit(decompose(TCHOFTrimTrain)$random)
  TCHOFTrimSta2 <- window(na.omit(decompose(TCHOFTrim)$random), end=c(2015,4))
  acf(TCHOFTrimSta)
  pacf(TCHOFTrimSta)
  kpss.test(TCHOFTrimSta)
  plot(TCHOFTrimSta)

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
  