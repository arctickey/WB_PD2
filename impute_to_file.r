#Załadowanie funkcji pojedynczej
source("./funkcje_imputacja/Amelia_Mice_Median.R")

#SCIEZKI DO DATASETOW
path_to_datasets <- "/home/piotr/Programowanie/WB/fork_grupy/2020L-WarsztatyBadawcze-Imputacja/datasets"
#path_to_datasets <- "/home/arctickey/2020L-WarsztatyBadawcze-Imputacja/datasets"


#AUTOMATYZACJA
folder <- list.dirs(path_to_datasets)
folder <- folder[-1]
script_paths <- paste(folder, '/', 'code.R', sep='')

czasy <- c()

for(i in script_paths){
  
  source(i, chdir=T)
  
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
  
  train_set = read.csv(file = paste("./indeksy/train", openml_id, ".csv", sep = ""))$x
  test_set = read.csv(file = paste("./indeksy/test", openml_id, ".csv", sep = ""))$x
  
  #IMPUTACJA
  time_median <- Sys.time()
  data_median <- data
  data_median[train_set,] <- prepareMedian(data_median[train_set,])
  data_median[test_set,] <- prepareMedian(data_median[test_set,])
  time_median <- Sys.time() - time_median
  print('modeMedian successful')
  data_median[,target] <- target_col

  czasy <- rbind(czasy, c(openml_id, time_median))

  if(sum(is.na(data_median))>0 || is.null(data_median)){
    next
  }
  
  write.csv(data_median[train_set, ], file = paste("./imputed_data/median/", openml_id, "_train.csv", sep=""))
  write.csv(data_median[test_set, ], file = paste("./imputed_data/median/", openml_id, "_test.csv", sep=""))

}

write.csv(czasy, file = paste("./imputed_data/czasy/median.csv"))
  