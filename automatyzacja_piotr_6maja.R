#Biblioteki
library(mlr3)
library(tidyverse)
library(R.utils) 
#setwd('./WB_PD2')

path_to_datasets <- "/home/piotr/Programowanie/WB/fork_grupy/2020L-WarsztatyBadawcze-Imputacja/datasets"
#path_to_datasets <- "/home/arctickey/2020L-WarsztatyBadawcze-Imputacja/datasets"

scores = tibble(Dataset = numeric(),Method = character(),Acc = numeric(),Auc=numeric(),
                recall = numeric(),precision=numeric())

folder <- list.dirs(path_to_datasets)
folder <- folder[-1]
script_paths <- paste(folder, '/', 'code.R', sep='')
out <- vector()

#Przeniosłem bo jak widzę nie czyścisz env po każdym zbiorze to może być raz wczytane
source("./Imputation.R")
loading()

for(i in script_paths){
  
  source(i, chdir = TRUE)
  
  #####################Propozycja: wyrzucanie kolumn z bardzo dużą liczbą kategorii#################
  #inne ew poprawki do zbiorow
  if(openml_id == 40536){
    dataset <- subset(dataset, select = -field)
  }
  
  if(openml_id == 41278){
    #kolumny nie obrobione, ktoś mogl tu kepszy FE zrobić
    dataset <- subset(dataset, select = -c(speaks, sign, religion, location, ethnicity))
    dataset <- droplevels(dataset)
  }
  
  if(openml_id == 6332){
    dataset <- subset(dataset, select = -c(timestamp, job_number, customer))
  }
  ###################################################################################################
  
  
  imput_result <- all_imputation(dataset, target_column)
  measure = msr("classif.acc")
  auc = msr('classif.auc')
  recall = msr('classif.recall')
  precision = msr('classif.precision')
  k = length(imput_result[[1]])
  j = 1
  
  
  while (j<=k){
    
    #nwm jak to szło ostatnio w koncu, ale ten try wylapuje fail na df z konkretnej imputacji
    #ostatni kod ktory mialem wywalal caly while wiec nastepne df-y z innych imputacji juz nie szly w model
    tryCatch( expr = {
      
    task= TaskClassif$new(id =imput_result[[1]][[j]] , backend =imput_result[[2]][[j]], target = target_column)
    learner = lrn("classif.rpart",predict_type='prob')
    learner$train(task,row_ids = imput_result[[3]])
    prediction <- learner$predict(task,row_ids=imput_result[[4]])
    acc <- prediction$score(measure)
    au <- prediction$score(auc)
    rec <- prediction$score(recall)
    precis <- prediction$score(precision)
    scores <- rbind(scores,c(openml_id,imput_result[[1]][[j]],acc,au,rec,precis,imput_result[[5]][[j]]))
    
    }, 
    error = function(err) {
      message(paste("Fail in model", imput_result[[1]][[j]]))
    }, warning = function(w) {
      message(paste("Fail in model", imput_result[[1]][[j]]))
    }, finally = {
      j = j+1})
    
    }
}

colnames(scores) <- c('Dataset','Method','Acc','Auc','Recall','Precision','Time')
scores
#write.csv(scores, './wyniki_test.csv')

