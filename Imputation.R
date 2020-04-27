
loading <- function(){
  # Funkcja wczytuje niezbędne funkcije z plików 
source('./Amelia_Mice_Median.R')
source('./missForest.R')
source('./missmda.R')
source('./VIM.R')
source('./softimpute.R')
source('./try.R')
}
loading()

all_imputation <- function(data,target){
  # Dane podajemy bez zmiennej celu 
  # target w sumie średnoi potrzebny ale i tak będzie 
  
  
  # Dla pojedynczej ramki zwraca 2 listy i 2 wektory
  # 1 lista Informacja jaki encoding jest używany 
  # 2 lista Zbiór lub obiekt który zwraca imputacja( np. Amelia  zwraca w tym wypadku osobno obiekt dla testowego i treningowego zbioru ) 
  # 3 Indeksy zbioru treningowego 
  # 4 Indeksy zbioru testoweg
  # oczywiście ponieważ R nie lubi krotek to wszystko jeszcze wpycham w liste 
  
  
  train_set = sample(nrow(data), 0.8 * nrow(data))
  test_set = setdiff(seq_len(nrow(data)), train_set)
  
 
 
  ## soft wszystkie kolumny gdzie nie można uzyć pakietu chcę wypełniać modą 
  # zamknąłem w time_it dla bezpieczeństwa
  time_softImpute <- Sys.time()
  data_softImpute <- data
  im_train <- impute_softimpute(data_softImpute[train_set,])
  im_test <- impute_softimpute(data_softImpute[test_set,])
  r <- rbind(im_train, im_test)
  ord <- as.numeric(row.names(r))
  data_softImpute <- r[order(ord), ]
  time_softImpute <- Sys.time() - time_softImpute
  print('softImpute successful')
  

  # missForest z tego co patrzyłem ogarniał wszyskie rodzaje zmiennych ale długo działa 
  time_forest <- Sys.time()
  data_missForest <- data
  try(
  data_missForest[train_set,] <- misinf_forest_aut(data_missForest[train_set,]),
  data_missForest[test_set,] <- misinf_forest_aut(data_missForest[test_set,]))
  time_forest <- Sys.time() - time_forest
  
  
  
  # Uzupełnanie medianą/modą 
  time_median <- Sys.time()
  data_median <- data
  data_median[train_set,] <- prepareMedian(data_median[train_set,])
  data_median[test_set,] <- prepareMedian(data_median[test_set,])
  time_median <- Sys.time() - time_median
  print('modeMedian successful')
  
  #Mice
  time_mice <- Sys.time()
  data_mice <- data
  data_mice[train_set,] <- prepareMice(data_mice[train_set,])
  data_mice[test_set,] <- prepareMice(data_mice[test_set,])
  time_mice <- Sys.time() - time_mice
  print('Mice successful')
   
  time_irmi <- Sys.time()
  data_irmi <- data
  data_irmi[train_set,] <- VIM_irmi(data_irmi[train_set,])
  data_irmi[test_set,] <- VIM_irmi(data_irmi[test_set,])
  time_irmi <- Sys.time() - time_irmi
  print('VIM_irmi successful')


  
  #missMDA 
  time_mda <- Sys.time()
  data_missMDA <- data
  data_missMDA[train_set,] <- impute_missMDA(data_missMDA[train_set,])
  data_missMDA[test_set,] <- impute_missMDA(data_missMDA[test_set,])
  time_mda <- Sys.time() - time_mda

   

  type_of_imputation <- list('median','irmi','mice','forest','MDA','softImpute')
  datasets <- list(data_median,data_irmi,data_mice,data_missForest,data_missMDA,data_softImpute)
  times <- list(time_median,time_irmi,time_mice,time_forest,time_mda,time_softImpute)


  
  return(list(type_of_imputation,datasets,train_set,test_set,times))

  
}


