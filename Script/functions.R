visualisation <- function(variable, start, end, freq, titre) {

  serie_temporelle <- ts(variable, start = start, end = c(end, freq), frequency = freq)
  plot(serie_temporelle, main=paste("Evolution de", titre, sep=" "))
  acf(serie_temporelle, main=paste("Auto-corrélation de", titre, sep=" "), na.action=na.pass)
  pacf(serie_temporelle, main=paste("Autocorrélation partielle de", titre, sep=" "), na.action=na.pass)
  
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
  regression<-lm(seq(1, length(AGEDAnn))~AGEDAnn)
  pvalue<-summary(regression)$coefficients[2,4]
  if(pvalue < 0.05)
    return (FALSE)
  else
    return (TRUE)
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

modele_SARIMA<-function(serie_train, serie_test, titre, nb_values_predicted){
  
  #Train
  sarima <- auto.arima(serie_train, max.p = 10, max.q=10)
  plot(serie_train, main=paste("Comparaison entre le modèle et les données d'apprentissage
     pour", titre, sep=" "))
  lines(sarima$fitted, col="red")
  
  #Test
  predictions <- forecast(sarima, h = nb_values_predicted)
  plot(serie_test, main=paste("Comparaison entre le modèle et les données de validation
     pour", titre, sep=" "))
  lines(predictions$mean, col="red")
  
  return(predictions)
}


