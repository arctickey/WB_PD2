#Załadowanie funkcji pojedynczej
setwd('/home/arctickey/WB_PD2')
source("./funkcje_imputacja/Amelia_Mice_Median.R")

#SCIEZKI DO DATASETOW
#path_to_datasets <- "/home/piotr/Programowanie/WB/fork_grupy/2020L-WarsztatyBadawcze-Imputacja/datasets"
path_to_datasets <- "/home/arctickey/2020L-WarsztatyBadawcze-Imputacja/datasets"


#AUTOMATYZACJA
folder <- list.dirs(path_to_datasets)
folder <- folder[-1]
script_paths <- paste(folder, '/', 'code.R', sep='')

czasy <- c()

for(i in script_paths){
  
  source(i, chdir=T)
  print(openml_id)
  
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
  
  data <- dataset
  target <- target_column
  
  target_col <- data[,target]
  data <- data[,ifelse(colnames(data)==target,FALSE,TRUE)]
  
  train_set = sample(nrow(data), 0.8 * nrow(data))
  test_set = setdiff(seq_len(nrow(data)), train_set)
  
  #IMPUTACJA
  time_mice <- Sys.time()
  data_mice <- data
  data_mice[train_set,] <- prepareMice(data_mice[train_set,])
  data_mice[test_set,] <- prepareMice(data_mice[test_set,])
  time_mice <- Sys.time() - time_mice
  print('mice successful')
  data_mice[,target] <- target_col
  
  czasy <- rbind(czasy, c(openml_id, time_mice))
  
  write.csv(data_mice[train_set, ], file = paste("./imputed_data/mice/", openml_id, "_train.csv", sep=""))
  write.csv(data_mice[test_set, ], file = paste("./imputed_data/mice/", openml_id, "_test.csv", sep=""))
  
  
}

write.csv(czasy, file = paste("./imputed_data/czasy/mice.csv"))


