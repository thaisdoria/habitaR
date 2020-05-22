#' Internal function to convert the occurrences records in dataframe to 'SpatialPoints'
#' and clean the data by removal of duplicates records based on a specified distance to
#' consider two coordinates as coincident.
#'
#' @param sd A list of dataframes with occurrences records in coordinates degrees
#' (longitude and latitude).
#' @noRd

f.clean1 <- function(occ, distOcc = NULL){
  long<-as.numeric(as.character(occ$long))
  lat<-as.numeric(as.character(occ$lat))
  c<-cbind(as.numeric(as.character(long)), as.numeric(as.character(lat)))
  c<-data.frame(c)
  options(digits=4)
  if (is.character(crs)){
    sp<-SpatialPoints(c, proj4string = CRS(crs))
  }
  if (class(crs) == "CRS"){
    sp<-SpatialPoints(c, proj4string = crs)
  }
  if (!is.null(distOcc)){
    sp<-remove.duplicates(sp, zerodist(sp, zero=as.numeric(distOcc)))
  }
  if (is.null(distOcc)){
    sp<-remove.duplicates(sp)
  }
}

