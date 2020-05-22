#' Internal function to automatize the bulding of ahull to multiple-species
#'
#' @param occ.ahul A list with SpatialPoints of species with, at least, 3 occurrences records not duplicated.
#' @param spp.names A vector of species names in the same order as the occ.ahul object.
#' @noRd

f.ahull <-function(occ.ahul, spp.names){
  ah <- getDynamicAlphaHull(occ.ahul@coords, fraction=fraction, partCount=partCount,
                            initialAlpha = 0.0, alphaIncrement=alphaIncrement,
                            clipToCoast='no', verbose=T)
  ah1<-al[[1]]
  ahul<-shapefile(ah1)
  ah2 <- data.frame(matrix(unlist(ah[[2]])), stringsAsFactors=FALSE)
  df[,2] <- ah2
  return(ah1)
}




