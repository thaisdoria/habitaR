tryPrefHab <- function(spp, key){
  index <- 1
  while(index <= 3){
    result <- try(rl_habitats(spp, key = key), silent = T)
    if (inherits(result, "try-error")) {
      index <- index + 1
      Sys.sleep(10)
    } else {
      return(result)
      index = 4
    }
  }
}
