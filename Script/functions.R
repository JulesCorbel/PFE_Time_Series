#Ce script contient toutes les fonctions créées et utilisées au sein du projet
rm(list=ls())

#Erreur quadratique moyenne
EQM<-function(serie, prediction){
  return(sum((serie - prediction)^2)/length(serie))
}

#Construction de la matrice A et calcul des valeurs propres pour un modèle du package vars
stabilityvars<-function(modele){
  k<-modele$K
  p<-modele$p
  A <- matrix(0,nrow=p*k, ncol=p*k)
  deb<-nrow(A)-k+1
  for(i in seq(1,(nrow(A)-2*k+1),k)){
    A[(i+k):(i+k*2-1),i:(i+k-1)] = diag(k)
  }
  for(i in seq(1,deb,k)){
    A[1:k,i:(i+k-1)] = getCoefMatrix(modele, i)
  }
  vp<-eigen(A)$values
  print(Mod(vp))
  plot(seq(1,nrow(A)), Mod(vp), xlab="",
       ylab="Modules des valeurs propres", ylim=c(0,1.5))
  abline(h=1, col="red")
}

getCoefMatrix<-function(modele, p){
  k<-modele$K
  result<-matrix(nrow=k, ncol=k)
  for(i in 1:length(modele$varresult)){
    result[i,] = modele$varresult[[i]]$coefficients[1:(1+k-1)]
  }
  return(result)
}

#Construction de la matrice A et calcul des valeurs propres pour un modèle du package MTS
stabilityMTS<-function(modele){
  k<-ncol(modele$data)
  p<-modele$order
  A <- matrix(0,nrow=p*k, ncol=p*k)
  deb<-nrow(A)-k+1
  for(i in seq(1,(nrow(A)-2*k+1),k)){
    A[i:(i+k-1),(i+k):(i+k*2-1)] = diag(k)
  }
  j = ncol(A)
  for(i in seq(1,deb,k)){
    A[deb:nrow(A),(j-k+1):j] = t(modele$coef[(i+1):(i+k),])
    j = j-k
  }
  vp<-eigen(A)$values
  print(Mod(vp))
  plot(seq(1,nrow(A)), Mod(vp), xlab="",
       ylab="Modules des valeurs propres", ylim=c(0,1.5))
  abline(h=1, col="red")
}

#Calcul des p-values d'une matrice de corrélation
cor.mtest <- function(mat, conf.level = 0.95){
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat <- lowCI.mat <- uppCI.mat <- matrix(NA, n, n)
  diag(p.mat) <- 0
  diag(lowCI.mat) <- diag(uppCI.mat) <- 1
  for(i in 1:(n-1)){
    for(j in (i+1):n){
      tmp <- cor.test(mat[,i], mat[,j], conf.level = conf.level)
      p.mat[i,j] <- p.mat[j,i] <- tmp$p.value
      lowCI.mat[i,j] <- lowCI.mat[j,i] <- tmp$conf.int[1]
      uppCI.mat[i,j] <- uppCI.mat[j,i] <- tmp$conf.int[2]
    }
  }
  return(list(p.mat, lowCI.mat, uppCI.mat))
}

#Visualisation des p-values d'un test de ARCH pour un lag de 1 à 18
archTest<-function(modele){
  a1 <- c()
  for(i in 1:18){
    a1[i] <- arch.test(modele, lags.multi = i)$arch.mul$p.value
  }
  plot(a1, type="l", main="Evolution de la p-value en fonction du lag", xlab="lag", ylab="p-value")
  abline(h=0.05, col="red")
}

#Construction de la matrice C0 et calcul de son déterminant
calculC0<-function(modele){
  C0 <- matrix(nrow = modele$K, ncol=modele$K, 0)
  for(i in 1:nrow(residuals(modele))){
    C0 <- C0 + residuals(modele)[i,]%*%t(residuals(modele)[i,])
  }
  C0 <- (1/nrow(residuals(modele))) * C0
  C0
  d <- det(C0)
  names(d) <- "Déterminant de la matrice"
  d
}

#Visualisation des p-values d'un test de Portmanteau pour un lag de 1 à 50
Portmanteau<-function(modele){
  a1 <- c()
  for(i in 1:3){
    a1[i] <- NA
  } 
  for(i in 4:50){
    a1[i] <- serial.test(modele, lags.pt=i, type="PT.asymptotic")$serial$p.value
  }
  plot(a1, type="l", main="Evolution de la p-value en fonction du lag", xlab="lag", ylab="p-value")
  abline(h=0.05, col="red")
}

#Visualisation de la significativité globale des corrélation croisées pour le package MTS
crossCorr<-function(modele){
  crossCorr<-ccm(modele2$residuals, lag=10, output=F)
  plot(crossCorr$pvalue, xlab = "lag", ylab = "p-value", ylim = c(0,1), main="Significance plot of CCM")
  abline(h = 0)
  crit = 2/sqrt(length(modele$residuals))
  abline(h = crit, lty = 2, col="red")
}
