VIM_knn <- function (data){
  
  
  
  
  #Imputacja danych dla zbioru treningowego i testoweg 
  data <- kNN(data)
  
  return(data)
}


VIM_irmi <-  function (data){
  
  
  
 
  dop <- length(colnames(data)) 
  #Imputacja danych dla zbioru treningowego i testoweg 
  data<- irmi(data)[1:dop]
  
  return(data)
}

