#setwd('./WB_PD2')

#Biblioteki
library(mlr3)
library(tidyverse)
library(R.utils) 
library(MLmetrics)
library(OpenML)
library(mlr)

#SCIEZKI DO DATASETOW FORK GRUPY !!!
path_to_fork_datasets <- "/home/piotr/Programowanie/WB/fork_grupy/2020L-WarsztatyBadawcze-Imputacja/datasets"   #<<<<<<<=======
#path_to_datasets <- "/home/arctickey/2020L-WarsztatyBadawcze-Imputacja/datasets"

folder <- list.dirs(path_to_fork_datasets)[-1]
script_paths <- paste(folder, '/', 'code.R', sep='')

#SKRYPTY
source("./algorytmy.R")
loading_algoritms()
source("./categoricals.R")


#Tabela na wyniki
scores = tibble(Dataset = numeric(),Method = character(),Model = character(),F1 = numeric(),acc=numeric(),Imp_time = numeric())
colnames(scores) <- c('Dataset','Method','Model', 'F1', 'acc')


#AUTOMATYZACJA
path <- "/home/piotr/Programowanie/WB/WB_PD2/imputed_data/"   #<<<<<=========
folders <- c("median/", "softimpute/", "missmda/", "missForest/", "mice/")
#folders <- c("median/")
csv_df <- paste(path, folders, sep="")


for(i in script_paths[1:length(script_paths)]){
  
  #Załadowanie zbioru ze skryptu
  source(i, chdir=T)
  
  if(openml_id==41278){
    next
  }
  
  for(j in 1:length(csv_df)){
    
    csv_train <- paste(csv_df[j], openml_id, "_train.csv", sep = "")
    csv_test <- paste(csv_df[j], openml_id, "_test.csv", sep = "")
    
    type_imputation <- folders[j]
    type_imputation <- sub(".$", "", type_imputation)
    
    try({#poczatek try
    
    if(file.exists(csv_train) && file.exists(csv_test)){
      
      df_train <- read.csv(file = csv_train)[-1]
      df_test <- read.csv(file = csv_test)[-1]
      
      
      df <- rbind(df_train, df_test)
      df[sapply(df, is.character)] <- lapply(df[sapply(df, is.character)], as.factor)
      train_id <- 1:nrow(df_train)
      test_id <- (nrow(df_train)+1):nrow(df)
      
      #ENCODING
      encoded <- categorical(df, train_set = train_id, test_set = test_id, target = target_column)
      df_encoded <- encoded[[1]]
      train_id <- encoded[[2]]
      test_id <- encoded[[3]]
      
      model_results <- learining(target = target_column, data_encoding = df_encoded, data_no_encoding = df, 
                                 train_index = train_id, test_index = test_id)
      
      #SCORES
      for(p in 1:length(model_results)){
          models <- c("xgb", "log_reg", "svm", "rf")
          f1 <- F1_Score(y_true = df[test_id, target_column], y_pred = model_results[[p]])
          acc <- Accuracy(y_true = df[test_id, target_column], y_pred = model_results[[p]])
          score <- t(c(openml_id, type_imputation, models[p], f1, acc))
          scores <- rbind(scores, score)
          write.table(score, file = "./wyniki_csv/wyniki.csv", sep = ",", append = TRUE, quote = FALSE,
                      col.names = FALSE, row.names = FALSE)
      }
    }
    })#koniec try
  }
}
  
