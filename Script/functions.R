#Ce script contient toutes les fonctions créées et utilisées au sein du projet
rm(list=ls())

visualisation <- function(variable, start, end, freq, titre) {

  serie_temporelle <- ts(variable, start = start, end = c(end, freq), frequency = freq)
  plot(serie_temporelle, main=paste("Evolution", titre, sep=" "))
  acf(serie_temporelle, main=paste("Auto-corrélation", titre, sep=" "), na.action=na.pass)
  pacf(serie_temporelle, main=paste("Autocorrélation partielle", titre, sep=" "), na.action=na.pass)
  
  return(serie_temporelle)
}

lissage_exponentiel<-function(serie_train, start, end_train, freq, nb_predicted_values){
  lissage<-ets(serie_train, "ZZZ")
  print(lissage)
  predictions <- forecast(lissage, h = nb_predicted_values)
  return(predictions)
}

best_model<-function(serie_test, LEpredictions, SARIMApredictions){
  EQM_LE<-EQM(serie_test, LEpredictions)
  EQM_SARIMA<-EQM(serie_test, SARIMApredictions)
  return(c(EQM_LE, EQM_SARIMA, ifelse(EQM_LE < EQM_SARIMA, "lissage exponentiel", "modèle SARIMA")))
}

EQM<-function(serie, prediction){
  return(sum((serie - prediction)^2)/length(serie))
}

stabilityMTS<-function(modele){
  p<-modele$order
  A <- matrix(0,nrow=p*4, ncol=p*4)
  deb<-nrow(A)-3
  for(i in seq(1,(nrow(A)-7),4)){
    A[i:(i+3),(i+4):(i+7)] = diag(4)
  }
  j = ncol(A)
  for(i in seq(1,deb,4)){
    A[deb:nrow(A),(j-3):j] = t(modele$coef[(i+1):(i+4),])
    j = j-4
  }
  vp<-eigen(A)$values
  print(Mod(vp))
  plot(seq(1,nrow(A)), Mod(vp), xlab="",
       ylab="Modules des valeurs propres", ylim=c(0,1.5))
  abline(h=1, col="red")
}
