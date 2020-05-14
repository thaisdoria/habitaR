#' checkOcc function
#'
#' From a list of 'SpatialPoints' for multiple species, gives the species and its 
#' occurrences that are inside of a polygon (ESRI shapefile format) representing 
#' a region of interest (e.g. study area, continent, country or other regions with 
#' specific boundaries). 
#' 
#' @usage checkOcc (poly, sp.occ)
#' @param poly A polygon (ESRI shapefile format) of the specific area to be checked in 
#' 'SpatialPolygonsDataFrame' class.
#' @param sp.occ A list of 'SpatialPoints' for multiple species. \code{\link[habitaR]
#' {readOcc}} can help obtain this data.
#' @import sp
#' @import rlist
#' @return \code{checkOcc} returns a list of species occurring inside of the provided 
#' polygon with their respective number of ocurrences falling in this polygon. 
#' total of occurrences inside of the polygon.
#' @encoding UTF-8
#' @author Thaís Dória & Daniel Gonçalves-Souza
#' @export checkOcc

checkOcc<-function(poly, sp.occ){
  # Identifying spatial limits of the extent of interest (research area)
  lin <- as(poly, "SpatialLinesDataFrame")  
  pts <- as.data.frame(as(lin, "SpatialPointsDataFrame"))
  pol.x<-pts$coords.x1
  pol.y<-pts$coords.x2
  # Function to check if spatial points are inside of specific polygon
f.cf.occ<-function(y){
  cf.occ=point.in.polygon(y@coords[,1], y@coords[,2], pol.x, pol.y,
                                    mode.checked=FALSE)
}
cf.occ<-lapply(sp.occ, f.cf.occ) # object with occurrences checked (0 represents absence inside of biome and 1 represents presence inside of biome)
cf.occsum<- lapply(cf.occ, sum) # object with the sum of the checked records (0 represents complete absence of species inside of biome) 
cf.occsum[cf.occsum==0] <- NA
cf.occclean<-list.clean(cf.occsum, fun = is.na, recursive = TRUE)
return(cf.occclean)
}
