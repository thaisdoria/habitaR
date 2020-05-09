resolucao <- function(x, y){
  factor <- trunc(xres(y) / xres(x))
  if(factor > 1){
    x <- aggregate(x, fact = factor, fun = sum)
    x <- x / (factor^2)
  }
  x <- resample(x, y, method = 'ngb')
}
