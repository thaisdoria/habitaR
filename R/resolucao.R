resolucao <- function(x, y, type = 0, lbl = 'blank'){
  #RasterStack or #RasterBrick
  if(class(x) %in% c('RasterBrick', 'RasterStack')){
    new.x <- list()
    for(i in 1:nlayers(x)){
      new.x[[i]] <- x[[i]]
      if(lbl %in% 'adq' | type == 1){
        new.x[[i]] <- resample(new.x[[i]], y)
      }
      if(lbl %in% c('lc', 'alt'| type == 2)){
        factor <- trunc(xres(y) / xres(new.x))
        if(factor > 1){
          new.x[[i]] <- aggregate(new.x[[i]], fact = factor, fun = sum)
          new.x[[i]] <- new.x[[i]] / (factor^2)
        }
        new.x[[i]] <- resample(new.x[[i]], y, method = 'ngb')
      }
    }
    new.x <- stack(new.x)
    return(new.x)
  }
  #RasterLayer
  if(lbl %in% 'climSuit'| type == 1){
    x <- resample(x, y)
  }
  if(lbl %in% c('lc', 'alt')| type == 2){
    factor <- trunc(xres(y) / xres(x))
    if(factor > 1){
      x <- aggregate(x, fact = factor, fun = sum)
      x <- x / (factor^2)
    }
    x <- resample(x, y, method = 'ngb')
  }
  return(x)
}
