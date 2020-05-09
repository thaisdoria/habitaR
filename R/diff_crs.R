diff.crs <- function(x){
  if(length(x) == 1){
    return(FALSE)
  }
  if(length(x) == 2){
    return(!compareCRS(x[[1]], x[[2]]))
  }
  if(length(x) == 3){
    return(any(!compareCRS(x[[1]], x[[2]]) | !compareCRS(x[[1]], x[[3]])))
  }
}
