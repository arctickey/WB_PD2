#Wymaga podmiany reszta powinna działać
path_to_datasets <- "/home/piotr/Programowanie/WB/fork_grupy/2020L-WarsztatyBadawcze-Imputacja/datasets"

folder <- list.dirs(path_to_datasets)
folder <- folder[-1]
script_paths <- paste(folder, '/', 'code.R', sep='')

for(i in script_paths){
  source(i, chdir = TRUE)
  source("Imputation.R")
  loading()
  
  #NIE IDZIE missmda na id: 40536, 41278 - imo za duże + zm kategoryczne
  
  #Poprawki na zbiorach
  if(openml_id == 6332){
    dataset <- subset(dataset, select = -timestamp)
  }
  
  imput_result <- all_imputation(dataset, target_column)
  
  #Cleaning environment - wszystko musi się policzyć wyżej
  rm(list = ls())
  gc()
}


