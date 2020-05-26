tryFun <- function(fun){
  if (is.character(fun)) {
    fun <- parse(text = fun)
  }

  index <- 1
  while (index <= 3) {
    result <- try(eval(fun), silent = T)
    if (inherits(result, "try-error")) {
      index <- index + 1
      Sys.sleep(10)
    } else {
      return(result)
      index = 4
    }
  }
}
