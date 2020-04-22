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
  
  num_of_components <- estim_ncpFAMD(df, ncp.max = 3, method.cv = "Kfold", nbsim = 10)
  imputed_df <- imputeFAMD(df, ncp = num_of_components$ncp)
  result_df <- imputed_df$completeObs
  
  return(result_df)
}

# #Test
# library(OpenML)
# task <- getOMLDataSet(data.id = 29)
# df <- task$data
# 
# imputed_df <- impute_missMDA(df)