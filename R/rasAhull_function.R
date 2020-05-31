### rasAhull: rasterize the aHull polygons
#'
#' Rasterize the aHull distribution maps (or other objects from a 'SpatialPolygons'
#' or 'SpatialDataFrame' classes) based on a 'raster' object.
#'
#' @usage rasAhull (aHull, ras, mask = FALSE)
#' @param aHull Alpha hull polygons or other types of distribution data
#' representing the species extent of occurrence (EOO). It might be a 'aHull' object
#' (see \code{\link[habitaR]{aHull}} to obtain such class of object) or a list of
#' 'SpatialPolygons' or 'SpatialPolygonsDataFrame' from a multiple-species.
#' @param ras A raster object (file in .asc or .tif format) to be used as baseline
#' to rasterize the provided polygons.
#'
#' @return \code{rasAhull} returns a list of features from 'raster' class.
#'
#' @examples
#'
#' # Example for signature 'aHull'
#'
#' ras_ahull <- rasAhul(aHull, ras)
#'
#' @encoding UTF-8
#' @author Thaís Dória & Daniel Gonçalves-Souza
#' @export rasAhull
#' @import raster

rasAhull<-function(aHull, ras){

  # Checklist and warning messages
  if (missing(aHull))
    stop('aHull is missing')
  if (missing(ras))
    stop('ras is missing')

  ahull.r <- mapply(rasterize, aHull, MoreArgs = list(ras, background = 0, mask=FALSE))
  for (i in 1:length(ahull.r)){
  ahull.r[[i]][ahull.r[[i]] > 1] <- 1
  }
  return(ahull.r)
}
