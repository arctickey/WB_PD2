#Wymaga podmiany reszta powinna działać
path_to_datasets <- "/home/piotr/Programowanie/WB/fork_grupy/2020L-WarsztatyBadawcze-Imputacja/datasets"

folder <- list.dirs(path_to_datasets)
folder <- folder[-1]
script_paths <- paste(folder, '/', 'code.R', sep='')

for(i in script_paths){
  source(i, chdir = TRUE)
  
  
  #Test
  print(target_column)
  print(nrow(dataset))
  
  
  #Cleaning environment - wszystko musi się policzyć wyżej
  rm(list = ls())
  gc()
}


