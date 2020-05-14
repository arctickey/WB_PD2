
categorical <- function(data,train_set,test_set,target){
  
library(tidyverse)
library(h2o)

h2o.init()
#Import the titanic dataset
rows <- nrow(data)
cols <- ncol(data)
data[,target]<- as.factor(data[,target])
categorical_cols <- colnames(data[,colnames(data)[grepl('factor|logical|character',sapply(data,class))]])
categorical_cols <- categorical_cols[categorical_cols !=target_column]
if(length(categorical_cols)==0){
  return(list(data,train_set,test_set))
}
# Split the dataset into train and test
seed=1234
# Set target encoding parameters
blended_avg = TRUE
inflection_point = 3
smoothing = 10
noise = 0.15

X_train <- as.h2o(data[train_set,])


X_test <- as.h2o(data[test_set,])
# Train a TE model
target_encoder <- h2o.targetencoder(training_frame = X_train, x = categorical_cols,y =target,blending=blended_avg, k=inflection_point, f=smoothing, noise=noise)
# New target encoded train and test sets
transformed_train <- h2o.transform(target_encoder, X_train, noise=noise)
transformed_test <- h2o.transform(target_encoder,X_test,noise=0.0)
X_train <- as_tibble(transformed_train)
X_test <- as_tibble(transformed_test)

X_train<- X_train %>% select(-one_of(categorical_cols)) 
X_test<- X_test %>% select(-one_of(categorical_cols))
data <- rbind(X_train,X_test)
train_set <- 1:nrow(X_train)
test_set <- nrow(X_train)+1:nrow(X_test)
return(list(data,train_set,test_set))
}
