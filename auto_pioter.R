#Biblioteki
library(mlr3)
library(tidyverse)
library(R.utils) 
#setwd('./WB_PD2')
#Wymaga podmiany reszta powinna działać
path_to_datasets <- "/home/piotr/Programowanie/WB/fork_grupy/2020L-WarsztatyBadawcze-Imputacja/datasets"
#path_to_datasets <- "/home/arctickey/2020L-WarsztatyBadawcze-Imputacja/datasets"

scores = tibble(Dataset = numeric(),Method = character(),Score = numeric())
folder <- list.dirs(path_to_datasets)
folder <- folder[-1]
script_paths <- paste(folder, '/', 'code.R', sep='')

#Przeniosłem bo jak widzę nie czyścisz env po każdym zbiorze to może być raz wczytane
source("./Imputation.R")
loading()

pass <- c(1, 6, 7, 8, 10, 11)

for(i in script_paths[-pass]){
  source(i, chdir = TRUE)
  
  #Nie działa na zbiorach  29 - błędy w kodowaniu,
  # nie działa tez na 40536 i 41278 -  za duze
  
  #Poprawki na zbiorach
  if(openml_id == 6332){
    dataset <- subset(dataset, select = -timestamp)
  }
  
  imput_result <- all_imputation(dataset, target_column)
  
  measure = msr("classif.acc")
  k = length(imput_result[[1]])
  j = 1
  
  try(
  while (j<=k){
    task= TaskClassif$new(id =imput_result[[1]][[j]] , backend =imput_result[[2]][[j]], target = target_column)
    learner = lrn("classif.rpart",predict_type='prob')
    learner$train(task,row_ids = imput_result[[3]])
    prediction <- learner$predict(task,row_ids=imput_result[[4]])
    acc <- prediction$score(measure)
    print(imput_result[[1]][[j]])
    scores <- rbind(scores,c(openml_id,imput_result[[1]][[j]],acc))
    j= j+1
  }
  )
}

colnames(scores) <- c('Dataset','Method','Score')
write.csv(scores, './wyniki_pioter.csv')
