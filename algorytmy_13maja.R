source('Imputation.R')

loading_algoritms <- function() {
  library(mlr3)
  library(mlr3learners)
  library(party)
  library(mlr3pipelines)
  library(mlr3tuning)
}

#Ładujemy
loading_algoritms()

#XGB classif.xgboost
#Random Forest classif.ranger
#Logistic regression classif.glmnet
#SVM classif.svm

# Funkcja do cross validacij
cv_tuning <- function(task, classificator, param_grid) {
  ctrl = makeTuneControlGrid()
  rdesc = makeResampleDesc("CV", iters = 5L)
  res = tuneParams(
    classificator,
    task = task,
    resampling = rdesc,
    par.set = param_grid,
    control = ctrl
  )
  return(res)
  
}



learining <- function(target, data_encoding, data_no_encoding, train_index, test_index, encoding_where_unnessesery = TRUE) {
    
    # chyba nie ma co tumaczyć
    # dane mają zawierać zmienną celu i być nie kodowane
    # podajemy ramke po kodowaniu i przed
    #  DO POPRAWIENIA NA PIPLINE JAK KTOŚ OGARNIE JAK TO DO KURWY ZROBIĆ
    # Zwraca liste z predykcjami odpowiedni xgb,randomForest , regresja ,SVM
    
    
    # Tworzenie w chuj tasków bo nie widzę prostrzego sposobu ponieważ R ssie
    
    train_task_encoded = makeClassifTask(id = "train_task_encoded",
                                         data = data_encoding[train_index, ],
                                         target = target)
    train_task_no_encoded = makeClassifTask(id = "train_task_no_encoded",
                                            data = data_no_encoding[train_index, ],
                                            target = target)
    
    task_encoded = makeClassifTask(id = "task_encoded",
                                   data = data_encoding,
                                   target = target)
    task_no_encoded = makeClassifTask(id = "task_no_encoded",
                                      data = data_no_encoding,
                                      target = target)
    
    # Specjalnie dla xgb bo on pierdoli zasady
    
    test_task_encoded = makeClassifTask(id = "test_task_encoded",
                                        data = data_encoding[test_index, ],
                                        target = target)
    test_task_no_encoded = makeClassifTask(id = "test_task_no_encoded",
                                           data = data_no_encoding[test_index, ],
                                           target = target)
    # XGB
    
    xgb_learner <- makeLearner(
      "classif.xgboost",
      predict.type = "response",
      par.vals = list(objective = "binary:logistic")
    )
    # parametry dla xgb
    discrete_ps = makeParamSet(
      makeDiscreteParam("eta", values = c(0.2, 0.4)),
      makeDiscreteParam("gamma", values = c(0.1, 0.2, 0.3)),
      makeDiscreteParam('max_depth', values = c(5, 6, 7)),
      makeDiscreteParam('subsample', values = c(0.2, 0.4, 0.6))
      )
    
    if (encoding_where_unnessesery) {
      res_xgb <- cv_tuning(train_task_encoded, xgb_learner,  discrete_ps)
    } else {
      res_xgb <- cv_tuning(train_task_no_encoded, xgb_learner, discrete_ps)
    }
    
    xgb_learner <- makeLearner(
      "classif.xgboost",
      predict.type = "response",
      par.vals = list(
        objective = "binary:logistic",
        eta = res_xgb$x$eta,
        gamma = res_xgb$x$gamma,
        max_depth = res_xgb$x$max_depth,
        subsample = res_xgb$x$subsample
      )
    )
    
    # training
    if (encoding_where_unnessesery) {
      xgb_model <- train(xgb_learner, train_task_encoded)
    } else {
      xgb_model <- train(xgb_learner, train_task_no_encoded)
    }
    # Resoult
    if (encoding_where_unnessesery) {
      xgb_result <- predict(xgb_model, test_task_encoded)$data[, 3]
    } else {
      xgb_result <- predict(xgb_model, test_task_no_encoded)$data[, 3]
    }
    
    # RADNOM FOREST
    # discrete_ps = makeParamSet(
    #   makeDiscreteParam("num.trees", values = c(100, 200, 300,400,500,600)),
    #   makeDiscreteParam('min.node.size',values = c(1,2,3,4,5)),
    #
    #   #makeDiscreteParam('max.depth',values = c(1))
    #   )
    #
    # if (encoding_where_unnessesery){
    #   res_rf <- cv_tuning(train_task_encoded,'classif.ranger',  discrete_ps )} else { res_rf<- cv_tuning(train_task_no_encoded,'classif.ranger',discrete_ps)}
    #
    # lerner_randomForest = mlr_learners$get('classif.ranger')
    # lerner_randomForest$param_set$values =mlr3misc::insert_named(
    #   learner$param_set$values,
    #   list(num.trees= res_rf$x$num.trees, min.node.size = res_rf$x$min.node.size,
    #        #max.depth=res_rf$x$max.depth,alpha=res_rf$x$alpha
    #        )
    # )
    # if (encoding_where_unnessesery){
    #   lerner_randomForest$train(task_encoded,row_ids = train_index)}  else { lerner_randomForest$train(task_no_encoded,row_ids = train_index)}
    #
    # if (encoding_where_unnessesery){
    #   rf_results <- lerner_randomForest$predict(task_encoded,row_ids = test_index)} else { rf_results <- lerner_randomForest$predict(task_no_encoded,row_ids = test_index)}
    
    
    # Logistic Regression
    logistic.learner <- makeLearner("classif.glmnet", predict.type = "response")
    
    discrete_ps = makeParamSet(makeDiscreteParam("alpha", values = c(0, 0.2)),
                               makeDiscreteParam('nlambda', values = c(50, 100)))
    
    res_lr <- cv_tuning(train_task_encoded, logistic.learner, discrete_ps)
    
    logistic.learner <- makeLearner(
        "classif.glmnet",
        predict.type = "response",
        par.vals = list(alpha = res_lr$x$alpha, nlambda = res_lr$x$nlambda)
      )
    
    lg_model <- train(logistic.learner, train_task_encoded)
    lg_result <- predict(lg_model, test_task_encoded)$data[, 3]
    
    
    # SVM
    
    # discrete_ps = makeParamSet(
    #   makeDiscreteParam('gamma',values = c(0.001,0.01,1)),
    #   makeDiscreteParam('kernel',values = c('sigmoid')),
    #   makeDiscreteParam('cost',values = c(0.5,1))
    #   )
    #
    # res_SVM <- cv_tuning(train_task_encoded,'classif.svm',discrete_ps)
    #
    # lerner_SVM = mlr_learners$get('classif.svm')
    # lerner_SVM$param_set$values = mlr3misc::insert_named(
    #   lerner_SVM$param_set$values,
    #   list(gamma= res_SVM$x$gamma, kernel = res_SVM$x$kernel,
    #        cost=res_SVM$x$cost
    #        )
    # )
    #
    # lerner_SVM$train(task_encoded,row_ids = train_index_index)
    # SVM_result <- lerner_SVM$predict(task_encoded,row_ids = test_index)
    
    
    return(list(xgb_result, lg_result))
  }

#TEST
task <- OpenML::getOMLDataSet(data.id = 31)
df <- task$data
df_2 <- Filter(is.numeric, df)
df_2$class <- df$class

data <- TaskClassif$new(id = "test",
                  backend = df_2,
                  target = "class")
tr <- sample(data$nrow, 0.8 * data$nrow)
tst <- setdiff(seq_len(data$nrow), tr)

r_test <- learining('class', df_2, df_2, train_index = tr, test_index = tst)
