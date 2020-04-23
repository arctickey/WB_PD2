library(R.utils)

time_it <- function(impute_exp, time_limit){
  #Czas w sekundach
  tryCatch(
    expr = {
      res <- withTimeout({impute_exp},
                         timeout = time_limit)
    },
    TimeoutException = function(ex){
      print("Timeout. Skip.\n")
      return(TRUE)
      
    }
  )
}

dziala <- function(){
  Sys.sleep(3)
  return(runif(1))
}

x <- time_it(dziala(), 2)
y <- time_it(dziala(), 5)

#nie działa
z <- time_it(impute_softimpute(dataset), 1)
