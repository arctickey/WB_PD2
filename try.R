time_it <- function(impute_exp, time_limit){
  #Czas w sekundach
  tryCatch(
    expr = {
      res <- R.utils::withTimeout(impute_exp,
                         timeout = time_limit)
    },
    TimeoutException = function(ex){
      print("Timeout-Skip.")}
  )
}

#Tak działa jak Janek używał miejmy nadzieję dla wszystkich
# time_it({
#   flag <- T
#   x <- impute_softimpute(dataset)
#   flag <- F},
#   time_limit = 0.001
# )
# if(flag){x <- NULL}