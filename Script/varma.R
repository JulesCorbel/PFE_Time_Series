#Imports
library(MTS)
library(Matrix)

appTrim<-cbind(MSETrimStaTrain, PIBTrimStaTrain, SMICTrimStaTrain, TCHOTrimStaTrain)
testTrim<-cbind(MSETrimStaTest, PIBTrimStaTest, SMICTrimStaTest, TCHOTrimStaTest)
cor(appTrim)
MTSplot(appTrim)

#Trimestrielle
  #1 variable
  Eccm(appTrim[,c(1:2)], maxp=5, maxq=5)
# VAR(3,0) => déjà fait
  
  Eccm(appTrim[,c(1,3)], maxp=5, maxq=5)
# VAR(4,0) => déjà fait
  
  Eccm(appTrim[,c(1,4)], maxp=5, maxq=5)
# VAR(4,0) => déjà fait
  
  #2 variables
  Eccm(appTrim[,c(1:3)], maxp=5, maxq=5)
# VAR(3,0) => déjà fait

  Eccm(appTrim[,c(1,2,4)], maxp=5, maxq=5)
# VAR(3,0) => déjà fait

  Eccm(appTrim[,c(1,3,4)], maxp=5, maxq=5)
# VAR(4,0) => déjà fait
  
  #Modèle complet
  Eccm(appTrim[,c(1:4)], maxp=5, maxq=5)
# VAR(3,0) => déjà fait