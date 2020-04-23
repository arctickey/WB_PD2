#Funkcja do imputacji pakietem missMDA
library(missMDA)

impute_missMDA <- function(df){
  #Co może psuć:
  #- kolumna z samymi NA
  #- kolumna z jedną wartością
  #- estimncp_FAMD() może nie być zbieżności jeśli ncp.max za duże,
  #  jak testowałem to były problemy już dla ncp.max=3,
  #  do tego zajmuje dużo czasu
  #- zmienne kategoryczne mogą dostać przedrostków z nazwą kolumny (dla factorów)
  #- lub po prostu nie działa (algorytm nie zbiega albo liczy zdecydowanie zbyt długo)
  
  num_of_components = tryCatch({
    estim_ncpFAMD(df, ncp.max = 3, method.cv = "Kfold", nbsim = 5)$ncp
  }, warning = function(c){
    message("WARN: estim_ncp (missMDA)")
    return(0)
  }, error = function(c){
    message("ERROR: estim_ncp (missMDA)")
    return(0)
  })
  
  
  result_df = tryCatch({
    df <- imputeFAMD(df, ncp = num_of_components)$completeObs
    message('missMDA successful')
    return(df)
  }, warning = function(c){
    message("WARN: impute FAIL (missMDA)")
    return(NA)
  }, error = function(c){
    message("ERROR: impute FAIL (missMDA)")
    return(NA)
  })
  
  return(result_df)
}

# #Test
# library(OpenML)
# source("/home/piotr/Programowanie/WB/fork_grupy/2020L-WarsztatyBadawcze-Imputacja/datasets/openml_dataset_40536/code.R", chdir=TRUE)
# imputed_df <- impute_missMDA(dataset)
