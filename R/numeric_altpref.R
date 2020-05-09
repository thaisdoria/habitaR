numeric.altpref <- function(x){
  if(is.factor(x[, 2])){
    x[, 2] <- as.numeric(levels(x[, 2]))[x[, 2]]
  }
  if(is.factor(x[, 3])){
    x[, 3] <- as.numeric(levels(x[, 3]))[x[, 3]]
  }
  if(is.character(x[, 2])){
    x[, 2] <- as.numeric(x[, 2])
  }
  if(is.character(x[, 3])){
    x[, 3] <- as.numeric(x[, 3])
  }
  return(x)
}
