#setwd('./WB_PD2')

#Biblioteki
library(mlr3)
library(tidyverse)
library(R.utils) 
library(MLmetrics)

#SCIEZKI DO DATASETOW
path_to_datasets <- "/home/piotr/Programowanie/WB/fork_grupy/2020L-WarsztatyBadawcze-Imputacja/datasets"
#path_to_datasets <- "/home/arctickey/2020L-WarsztatyBadawcze-Imputacja/datasets"

#SKRYPTY
source("./Imputation.R")
loading()
source("./algorytmy.R")
loading_algoritms()
source("./categoricals.R")


#Tabela na wyniki
scores = tibble(Dataset = numeric(),Method = character(),Acc = numeric(),Auc=numeric(),
                recall = numeric(),precision=numeric())


#AUTOMATYZACJA
folder <- list.dirs(path_to_datasets)
folder <- folder[-1]
script_paths <- paste(folder, '/', 'code.R', sep='')
out <- vector()


for(i in script_paths){
  
  #Załadowanie zbioru ze skryptu
  source(i, chdir=T)
  
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
  
  
  #IMPUTACJA
  imput_result <- all_imputation(dataset, target_column)
  
  
  #MODELE
  k = length(imput_result[[1]])
  for (j in c(1:k)){
    
    #XD ==> poniższy try, który ostatnio na taki zmieniłem i fajnie działał, dzisiaj wyłapuje warningi czy coś takiego,
    #których w rzeczywistości nie ma, a może są, ale nie mogą tej pętli wywalać bo dostałem wyniki
    #tryCatch( expr = {
      
      #ZMIENNE PO IMPUTACJI
      type_imputation <- imput_result[[1]][[j]]
      df <- imput_result[[2]][[j]]
      train_id <- imput_result[[3]]
      test_id <- imput_result[[4]]
      times <- imput_result[[5]][[j]]
      
      #Omijanie nieudanych imputacji
      if(sum(is.na(df))>0 || is.null(df)){
        print("next")
        next
      }
      
      #ENCODING
      encoded <- categorical(df, train_set = train_id, test_set = test_id, target = target_column)
      df_encoded <- encoded[[1]]
      train_id <- encoded[[2]]
      test_id <- encoded[[3]]
      
      #MODELE
      model_results <- learining(target = target_column, data_encoding = df_encoded, data_no_encoding = df,
                                 train_index = train_id, test_index = test_id)
      
      #SCORES
      for(p in 1:length(model_results)){
        models <- c("xgb", "log_reg", "svm", "rf")
        f1 <- F1_Score(y_true = df[test_id, target_column], y_pred = model_results[[p]])
        acc <- Accuracy(y_true = df[test_id, target_column], y_pred = model_results[[p]])
        scores <- rbind(scores, c(openml_id, type_imputation, models[p], f1, acc, times))
      } 
      
    
    # }, 
    # error = function(err) {
    #   message(paste("Fail in encoding-model", imput_result[[1]][[j]]))
    # }, warning = function(w) {
    #   message(paste("Fail in encoding-model", imput_result[[1]][[j]]))
    # }, finally = {
    #   j = j+1})
    
    }
}

colnames(scores) <- c('Dataset','Method','Model', 'F1', 'acc', 'Imp_time')
scores
#write.csv(scores, './wyniki_csv/wyniki.csv')

