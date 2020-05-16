#SCIEZKI DO DATASETOW
path_to_datasets <- "/home/piotr/Programowanie/WB/fork_grupy/2020L-WarsztatyBadawcze-Imputacja/datasets"

#AUTOMATYZACJA
folder <- list.dirs(path_to_datasets)
folder <- folder[-1]
script_paths <- paste(folder, '/', 'code.R', sep='')


for(i in script_paths){
  
  source(i, chdir=T)
  
  data <- dataset
  target <- target_column
  
  train_set = sample(nrow(data), 0.8 * nrow(data))
  test_set = setdiff(seq_len(nrow(data)), train_set)
  
  write.csv(train_set, file = paste("./indeksy/train", openml_id, ".csv", sep = ""))
  write.csv(test_set, file = paste("./indeksy/test", openml_id, ".csv", sep = ""))
  
}