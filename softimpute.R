#Softimpute: działa tylko dla numerycznych, dlatego:
#- softimpute dla zmiennych numerycznych
#- zmienne kategoryczne: uzupełnienie modą lub pozostawienie braku jako nowa kategoria
#Rozwiązuje to problem jeśli zmienne kategoryczne mamy jako factory,
#jak użyjemy jakiegoś kodowania, np. do 0-1 to taka zmienna będzie już numeryczna
#i softimpute wstawi dla braków wartości z przedziału [0,1], więc na razie założyłem
#że ewentualna inżynieria zmiennych będzie po imputacji
library(softImpute)

impute_softimpute <- function(df, mode_vars = FALSE, leave_missing = NULL){
  #mode_vars{colnames-string}: variables where impute mode, default: all factors
  #leave_missing{colnames-string}: variables where NA becomes new category (convert to factor)
  
  factors <- sapply(df, is.factor)
  
  if(any(factors)){
    
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
    
    #softimpute for numerical
    df_numerical <- as.matrix(df[, !factors])
    fits <- softImpute(df_numerical)
    ready_numerical <- softImpute::complete(df_numerical, fits)
    
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


# #Test1
# library(OpenML)
# task <- getOMLDataSet(data.id = 38)
# df <- task$data
# df <- subset(df, select = -c(TBG, TBG_measured))
# #All categorical vars imputed with mode
# imputed_df <- impute_softimpute(df)
# 
# #Test2
# task <- getOMLDataSet(data.id = 29)
# df <- task$data
# #Another categoricals vars (except A4) will keep missing, A6 will have NA as factor
# imputed_df <- impute_softimpute(df, mode_vars = "A4", leave_missing = "A6")

