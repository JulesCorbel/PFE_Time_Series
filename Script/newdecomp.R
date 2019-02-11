t <- auto.arima(MSETrain, stationary = F)
t$residuals
diff(MSETrain, differences=4)
par(mfrow=c(2,1))
plot(MSETrain)
lines(t$fitted, col="red")
plot(t$residuals)
abline(h=0, col="red")
par(mfrow=c(1,1))

tPIB <- auto.arima(PIBTrain, stationary = F)
tPIB$residuals
diff(PIBTrain, differences=4)
par(mfrow=c(2,1))
plot(PIBTrain)
lines(tPIB$fitted, col="red")
plot(tPIB$residuals)
abline(h=0, col="red")
par(mfrow=c(1,1))

PredSAR <- forecast(t, h=5)
t3 <- VAR(cbind(t$residuals, tPIB$residuals), p=8)
Predt3 <- forecast(t3, h=5)
 
plot(PredSAR$mean + Predt3$forecast$t.residuals$mean)
lines(MSETest, col="green")

MSE[109]
MSE[108]+t$residuals[109]
length(t$residuals)

plot(cumsum(t$residuals) + MSE[1], type="l")
plot(MSE, col="red")

MSETrain <- window(MSE, end=c(2015,4))
MSETest <- window(MSE, start=2016)
auto.arima(MSE, stationary = F)
plot(MSEAnnTrain, main="Comparaison entre le modèle ARIMA et les données
    d'apprentissage pour la masse salariale annuelle", 
     ylim=c(min(MSEAnnTrain,ARIMAMSEAnnTrain$fitted),max(MSEAnnTrain,ARIMAMSEAnnTrain$fitted)))
lines(ARIMAMSEAnnTrain$fitted, col="red")
plot(decompose(MSETrain, "multiplicative"))
MSESta <- na.omit(decompose(MSETrain, "multiplicative")$random)
#MSETrendTest<-window(decompose(MSETrain, "multiplicative")$trend)
#MSESeasonalTest<-window(decompose(MSETrain, "multiplicative")$seasonal)
plot(MSESta, main="Masse salariale trimestrielle stationnarisée", xaxt="n")
axis(side=1, at=seq(1990,2015,5), labels=c("1990Q1", "1995Q1", "2000Q1", "2005Q1", "2010Q1", "2015Q1"))
par(mfrow=c(1,2))
acf(MSESta, main="Auto-Corrélation de la Masse
      salariale trimestrielle stationnarisée")
pacf(MSESta, main="Auto-Corrélation partielle de la Masse
       salariale trimestrielle stationnarisée")
par(mfrow=c(1,1))
kpss.test(MSESta)
adf.test(MSESta)