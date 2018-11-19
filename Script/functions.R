#Ce script contient toutes les fonctions créées et utilisées au sein du projet

visualisation <- function(variable, start, end, freq, titre) {

  serie_temporelle <- ts(variable, start = start, end = c(end, freq), frequency = freq)
  plot(serie_temporelle, main=paste("Evolution", titre, sep=" "))
  acf(serie_temporelle, main=paste("Auto-corrélation", titre, sep=" "), na.action=na.pass)
  pacf(serie_temporelle, main=paste("Autocorrélation partielle", titre, sep=" "), na.action=na.pass)
  
  return(serie_temporelle)
}


#On fait l'hypothèse que la série possède toujours une tendance
lissage_exponentiel<-function(serie_train, start, end_train, freq, nb_predicted_values){
  if(!tendance_exists(serie_train)){
    lissage<- HoltWinters(serie_train, alpha=NULL,beta=F, gamma=F)
  }
  else if(freq > 1){
    lissage<- HoltWinters(serie_train, alpha=NULL,beta=NULL, gamma=NULL, seasonal=calcul_saisonnalite(serie_train, start, end_train, freq))
  }
  else{
    lissage<- HoltWinters(serie_train, alpha=NULL,beta=NULL, gamma=F)
  }
  
  predictions <- forecast(lissage, h = nb_predicted_values)
  return(predictions)
}

tendance_exists<-function(serie){
  regression<-lm(seq(1, length(serie))~serie)
  pvalue<-summary(regression)$coefficients[2,4]
  if(pvalue < 0.05)
    return (TRUE)
  else
    return (FALSE)
}

calcul_saisonnalite<-function(serie, start, end, freq){
  serie_matrix<-matrix(data=serie, nrow=end-start+1, byrow=T)
  mean<-apply(serie_matrix, 1, mean, na.rm=T)
  sd<-apply(serie_matrix, 1, sd, na.rm=T)
  regression<-lm(sd~mean)
  pvalue<-summary(regression)$coefficients[2,4]
  if(pvalue > 0.05)
    return ("add")
  else
    return ("mul")
}

best_model<-function(serie_test, LEpredictions, SARIMApredictions){
  EQM_LE<-EQM(serie_test, LEpredictions)
  EQM_SARIMA<-EQM(serie_test, SARIMApredictions)
  return(c(EQM_LE, EQM_SARIMA, ifelse(EQM_LE < EQM_SARIMA, "lissage exponentiel", "modèle SARIMA")))
}

EQM<-function(serie, prediction){
  return(mean((serie - prediction)^2)/length(serie))
}
