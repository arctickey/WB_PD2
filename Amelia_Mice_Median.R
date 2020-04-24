options(stringsAsFactors = FALSE)
options(warn=-1)
library(dplyr)


prepareAmelia <- function(data,noms=NULL,ords=NULL,idvars=NULL){
  #Amelia srednio sobie radzi ze zmiennymi kategorycznymi w ktorych jest duzo unikalnych wartosci.
  #Tak dokladniej to wcale sobie nie radzi wiec trzeba tego unikac
  
  #noms <-  zmienne kategoryczne
  #ords <- zmienne liczbowe (na intach) wśród ktorych wiemy ze istnieje jakis porządek
  #idvars <-  zmienna celu w ktorej nie bedzie robiona imputacja
  
  #Funkcja wypluwa 5 datasetow ktore pozniej trzeba jakos ze soba polaczyc, najprosciej chyba uzyc tego Zelig
  library(Amelia)
  try(
  imputed<- amelia(data,m=1,noms = noms,ords=ords,idvars=idvars)
  )
  return(imputed)
}


prepareMice <- function(data){
  #W sumie nic skomlikowanego tutaj nie ma
  library(mice)
  imp <- mice(data,m=1,maxit=1,meth='sample',seed=500)
  tempData <- complete(imp)
  return(tempData)
}

Mode <- function (x, na.rm) {
  #Funckja pomocnicza licząca modę dla każdej kolumny
  xtab <- table(x)
  xmode <- names(which(xtab == max(xtab)))
  if (length(xmode) > 1) xmode <- ">1 mode"
  return(xmode)
}

prepareMedian <- function(data){
  #uzupełnianie medianą i modą w zależności od typu kolumny
  for (var in 1:ncol(data)) {
    if (class(data[,var])=="numeric") {
      data[is.na(data[,var]),var] <- median(data[,var], na.rm = TRUE)
    } else if (class(data[,var]) %in% c("character", "factor")) {
      data[is.na(data[,var]),var] <- Mode(data[,var], na.rm = TRUE)
    }
  }
  return(data)
}

