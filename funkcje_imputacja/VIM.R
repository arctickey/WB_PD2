VIM_knn <- function (data){
  
  
  
  
  #Imputacja danych dla zbioru treningowego i testoweg 
  data <- VIM::kNN(data)
  
  return(data)
}


VIM_irmi <-  function (data){
  
  
  
 try(
  dop <- length(colnames(data)) ,
  #Imputacja danych dla zbioru treningowego i testoweg 
  data<- VIM::irmi(data)[1:dop]
 )
  
  return(data)
}

