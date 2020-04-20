library(OpenML)
library(mlr3)
library(missForest)
library(mlr)



misinf_forest_aut <- function (datapath_to_jason){
  ### data - surowe dane zawierające kolumne celu 
  ### scieżka do pliku jason opisującego dane 
  ### model do trenowania string z nazwą modelu
  ### train_set index
  ### test_set index 
  ### ZWRACA PRAWDOPODOBIEństwo klas
  library(rjson)
  
  c <- fromJSON(file = path_to_jason)
  
  
  w <- 1:length(colnames(data))
  w <- w[ifelse(colnames(data)==c[5],TRUE,FALSE)]
  #Imputacja danych dla zbioru treningowego i testoweg 
  
  

  # tworzenie taska 
 # task_mfml = TaskClassif$new(id = "mf_task", backend = data, target = toString(c[5]))
  
#  learner = mlr_learners$get(model)
  
 # learner$train(task_mfml, row_ids = train_set)
  
#  learner$predict_type = "prob"
  
 # prediction = learner$predict(task_credit, row_ids = test_set)
  data[,-w] <-  missForest(data[,-w])
  return(data)
}








  
  