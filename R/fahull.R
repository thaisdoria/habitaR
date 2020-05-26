#' Internal function to automatize the bulding of ahull to multiple-species
#'
#' @param occ.ahul A list with SpatialPoints of species with, at least, 3 occurrences records not duplicated.
#' @param spp.names A vector of species names in the same order as the occ.ahul object.
#' @noRd

f.ahull <-function(occ.ahul, fraction, partCount, buff, alphaIncrement){
  ah <- getDynamicAlphaHull(occ.ahul@coords, fraction = fraction,
                            partCount = partCount, buff = buff,
                            initialAlpha = 0.0, clipToCoast='no',
                            alphaIncrement = alphaIncrement,
                            verbose = TRUE)
  ahul<-ah[[1]]
  alpha <- data.frame(matrix(unlist(ah[[2]])), stringsAsFactors=FALSE)
  return(resul)
  }





