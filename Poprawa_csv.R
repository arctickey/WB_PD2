
library(R.utils)
library(prob)
dane_o_imputacij <- data
ids <- dane_o_imputacij$dataset
x <- c(1,2,3)

for (i in ids) {
  
  indeksy <- ifelse(dane_o_imputacij$dataset==i,TRUE,FALSE)
  dane_id <-  dane_o_imputacij[indeksy,]
  expectet <- c('median','softimpute','missmda')
  founded <- dane_id$method
  insert_<- setdiff(expectet,founded)
  
 
  for (j in insert_){
  dane_o_imputacij <- rbind(dane_o_imputacij,c(1,i,j,NA,NA,NA))
  }
  dane_wynik <- dane_o_imputacij
}
