
loading <- function(){
  # Funkcja wczytuje niezbędne funkcije z plików 
source('./Amelia_Mice_Median.R')
source('./missForest.R')
source('./missmda.R')
source('./VIM.R')
source('./softimpute.R')
}


all_imputation <- function(data,target){
  # Dane podajemy bez zmiennej celu 
  # target w sumie średnoi potrzebny ale i tak będzie 
  
  
  # Dla pojedynczej ramki zwraca 2 listy i 2 wektory
  # 1 lista Informacja jaki encoding jest używany 
  # 2 lista Zbiór lub obiekt który zwraca imputacja( np. Amelia  zwraca w tym wypadku osobno obiekt dla testowego i treningowego zbioru ) 
  # 3 Indeksy zbioru treningowego 
  # 4 Indeksy zbioru testoweg
  # oczywiście ponieważ R nie lubi krotek to wszystko jeszcze wpycham w wektor 
  
  
  
  train_set = sample(nrow(data), 0.8 * nrow(data))
  test_set = setdiff(seq_len(nrow(data)), train_set)
  
  # Factory w OpenML kategoryczne i miejmy nadzieję ,że gdzie indziej też 
  factors <- sapply(data, is.factor)
  # Żyje nadzieją ,że jak coś nie jest factorem(kategorycznym) lub zmienną celu (usunięta wcześcniej) to jest zmienną numeryczną 
  numeric <- !factors
  
  #Printy do śledzenia postępu
  
 
  
  ## soft wszystkie kolumny gdzie nie można uzyć pakietu chcę wypełniać modą 
  data_softImpute <- data
  data_softImpute[train_set,] <- impute_softimpute(data_softImpute[train_set,])
  data_softImpute[test_set,] <- impute_softimpute(data_softImpute[test_set,])  
  print('softImpute successful')
  
  ## missForest z tego co patrzyłem ogarniał wszyskie rodzaje zmiennych ale długo działa 
  data_missForest <- data
  data_missForest[train_set,] <- misinf_forest_aut(data_missForest[train_set,])
  data_missForest[test_set,] <- misinf_forest_aut(data_missForest[test_set,])
  print('missForest successful')
  # Amelia problem ze zwracanym obiektem nie wiem za bardzo jak to podzielić i skleić w jedno 
  data_Amelia <- data
  data_Amelia_train <- prepareAmelia(data_Amelia[train_set,],noms = factors)
  data_Amelia_test <-  prepareAmelia(data_Amelia[test_set,],noms = factors)
  print('Aemlia successful')
  
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
  
  # VIM_irmi też raczej ogrnia wszystko 
  data_irmi <- data
  data_irmi[train_set,] <- VIM_irmi(data_irmi[train_set,])
  data_irmi[test_set,] <- VIM_irmi(data_irmi[test_set,])
  print('VIM_irmi successful')
  
  # VIM_knn to może być problematyczne dla sytuacij gdy nie ma prawie wcale danych numerycznych ale zobaczymy 
  data_knn <- data
  data_knn[train_set,] <- VIM_knn(data_knn[train_set,])
  data_knn[test_set,] <- VIM_knn(data_knn[test_set,])
  print('VIMM_knn successful')
  
  # missMDA 
  
  data_missMDA <- data
  data_missMDA[train_set,] <- impute_missMDA(data_missMDA[train_set,])
  data_missMDA[test_set,] <- impute_missMDA(data_missMDA[test_set,])
  
  print('missMDA successful')
  
  
  type_of_imputation <- list('missMDA','softImpute','MissForest','Amelia','Mediana/Moda','Mice','VIM_irmi','VIM_knn')
  datasets <- list(data_missMDA,data_softImpute,data_missForest,c(data_Amelia_train,data_Amelia_test),data_median,data_mice,data_irmi,data_knn)
  
  return(c(type_of_imputation,datasets,train_set,test_set))

  
}



#w<- all_imputation(dataset_raw[,-56],target_column)

#ifelse(class(dataset_raw) %in% c("character", "factor"),TRUE,FALSE)
