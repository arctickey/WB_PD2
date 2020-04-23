#Softimpute: działa tylko dla numerycznych, dlatego:
#- softimpute dla zmiennych numerycznych
#- zmienne kategoryczne: uzupełnienie modą lub pozostawienie braku jako nowa kategoria
# Jak zostaje nam 1 lub 0 kolumn numerycznych, softimpute nie działa i zwracam NA

library(softImpute)

impute_softimpute <- function(df, mode_vars = FALSE, leave_missing = NULL){
  #mode_vars{colnames-string}: variables where impute mode, default: all factors
  #leave_missing{colnames-string}: variables where NA becomes new category (convert to factor)
  
  df <- dataset
  
  factors <- sapply(df, is.factor)
  
  if(any(factors)){
    
    #softimpute for numerical
    #If only one numerical variable left it's not working => no sense
    df_numerical <- as.matrix(df[, !factors])
    if(length(df_numerical[1]) <= 1){
      #Returning NA when no numerical variables to inpute
      return(NA)
    }else{
      fits <- softImpute(df_numerical)
      ready_numerical <- softImpute::complete(df_numerical, fits)
    }
    
    
    df_factors <- df[, factors]
    
    #Mode function
    Mode <- function (x, na.rm=TRUE) {
      xtab <- table(x)
      xmode <- names(which(xtab == max(xtab)))
      if (length(xmode) > 1)
        xmode <- xmode[1]
      return(xmode)
    }
    
    if(mode_vars != FALSE){
      #If user specifies variables for mode imputation
      mode_vars_col <- mode_vars[!mode_vars %in% leave_missing]
    }else{
      #Default behaviour: mode for all factors despite leave_missing
      mode_vars_col <- colnames(df)[factors]
      mode_vars_col <- mode_vars_col[!mode_vars_col %in% leave_missing]
    }
    
    #for categorical where mode
    for (var in mode_vars_col){
      df_factors[is.na(df_factors[, var]), var] <- Mode(df_factors[, var])
    }
    
    #for categorical where impute category => NA as factor
    for (var in leave_missing){
      df_factors[, var] <-  addNA(df_factors[, var])
    }
    
    ready_df <- cbind(df_factors, as.data.frame(ready_numerical))
    return(ready_df)
    
  }else{
    
    #softimpute for all when all vars numerical
    df_matrix <- as.matrix(df)
    fits <- softImpute(df_matrix)
    ready_numerical <- softImpute::complete(df_matrix, fits)
    return(as.data.frame(ready_numerical))
    
  }
}


#Test1
# source("/home/piotr/Programowanie/WB/fork_grupy/2020L-WarsztatyBadawcze-Imputacja/datasets/openml_dataset_23381/code.R", chdir = TRUE)
# source("Amelia_Mice_Median.R")
# imputed_df <- impute_softimpute(dataset)
# 
# #Test2
# task <- getOMLDataSet(data.id = 29)
# df <- task$data
# #Another categoricals vars (except A4) will keep missing, A6 will have NA as factor
# imputed_df <- impute_softimpute(df, mode_vars = "A4", leave_missing = "A6")

