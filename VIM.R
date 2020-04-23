VIM_knn <- function (data,path_to_jason){
  
  library(rjson)
  
  c <- fromJSON(file = path_to_jason)
  
  
  w <- 1:length(colnames(data))
  w <- w[ifelse(colnames(data)==c[5],TRUE,FALSE)]
  
  w <- 1:length(colnames(data))
  w <- w[ifelse(colnames(data)==c[5],TRUE,FALSE)]
  #Imputacja danych dla zbioru treningowego i testoweg 
  data[,-w] <- kNN(data[,-w])
  
  return(data)
}


VIM_irmi <-  function (data,path_to_jason){
  
  library(rjson)
  
  c <- fromJSON(file = path_to_jason)
  
  
  w <- 1:length(colnames(data))
  w <- w[ifelse(colnames(data)==c[5],TRUE,FALSE)]
  
  w <- 1:length(colnames(data))
  w <- w[ifelse(colnames(data)==c[5],TRUE,FALSE)]
  dop <- length(colnames(data)) -1 
  #Imputacja danych dla zbioru treningowego i testoweg 
  data[,-w] <- irmi(data[,-w])[1:dop]
  
  return(data)
}

