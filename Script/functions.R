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
