
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
  
  
  #Printy do śledzenia postępu
  
 
  ## soft wszystkie kolumny gdzie nie można uzyć pakietu chcę wypełniać modą 
  # zamknąłem w time_it dla bezpieczeństwa
  #data_softImpute <- data
  #time_it({
  #im_train <- impute_softimpute(data_softImpute[train_set,])
  #im_test <- impute_softimpute(data_softImpute[test_set,])
  #r <- rbind(im_train, im_test)
  #ord <- as.numeric(row.names(r))
  #data_softImpute <- r[order(ord), ]},
  #time_limit = 300
  #)
  #print('softImpute successful')

  # missForest z tego co patrzyłem ogarniał wszyskie rodzaje zmiennych ale długo działa 
  # data_missForest <- data
  # data_missForest[train_set,] <- misinf_forest_aut(data_missForest[train_set,])
  # data_missForest[test_set,] <- misinf_forest_aut(data_missForest[test_set,])
  # print('missForest successful')
  # 
  
  # Uzupełnanie medianą/modą 
  data_median <- data
  data_median[train_set,] <- prepareMedian(data_median[train_set,])
  data_median[test_set,] <- prepareMedian(data_median[test_set,])
  print('modeMedian successful')
  
  #Mice
  data_mice <- data
  data_mice[train_set,] <- prepareMice(data_mice[train_set,])
  data_mice[test_set,] <- prepareMice(data_mice[test_set,])
  print('Mice successful')
  # 
  # VIM_irmi też raczej ogrnia wszystko 
  #a <- TRUE
  #time_it({
  #data_irmi <- data
  #data_irmi[train_set,] <- VIM_irmi(data_irmi[train_set,])
  #data_irmi[test_set,] <- VIM_irmi(data_irmi[test_set,])
  #print('VIM_irmi successful')
  #a <- FALSE
  #},600)
  #if (a){data_irmi <- NULL}
  
  #VIM_knn to może być problematyczne dla sytuacij gdy nie ma prawie wcale danych numerycznych ale zobaczymy 
  #data_knn <- data
  #data_knn[train_set,] <- VIM_knn(data_knn[train_set,])
  #data_knn[test_set,] <- VIM_knn(data_knn[test_set,])
  #print('VIMM_knn successful')
  
  # missMDA 
  #data_missMDA <- data
  #data_missMDA[train_set,] <- impute_missMDA(data_missMDA[train_set,])
  #data_missMDA[test_set,] <- impute_missMDA(data_missMDA[test_set,])
  #print('missmda success')
   
  
  #type_of_imputation <- list('missMDA','softImpute','MissForest','Amelia','Mediana/Moda','Mice','VIM_irmi','VIM_knn')
  #datasets <- list(data_missMDA,data_softImpute,data_missForest,c(data_Amelia_train,data_Amelia_test),data_median,data_mice,data_irmi,data_knn)
  

  
  type_of_imputation <- list('Mediana/Moda','mice')
  datasets <- list(data_median,data_mice)
  
  
  
  return(list(type_of_imputation,datasets,train_set,test_set))

  
}




#ifelse(class(dataset_raw) %in% c("character", "factor"),TRUE,FALSE)


