#'  readOcc function
#'
#' Read and clean the occurrences records files (.csv format) of multiples species
#' from a specified folder and provide a list of 'SpatialPoints' of species through
#' the removal of duplicates coordinates.
#'
#' @usage readOcc (occ, crs, dist)
#' @param occ Path for a folder with the species occurrence records files
#' (.csv format). The files names should correspond to the species names
#' and must have be 3 columns identified as "species" (species names or other
#' identification of taxa), "long" (longitude), "lat" (latitude).
#' @param crs The Coordinate Reference System (CRS) specifing the projection and
#' datum of dataset. Could be a CRS object or a character string.
#' @param dist A value corresponding to the minimum distance assigned to consider
#' two coordinates as not duplicate. Values up to this distance will be consider
#' as duplicates and removed. Units of this value must be the same as those of
#' the coordinates for projected data or in km if coordinates are defined to be
#' longitude/latitude. Optional. Default is zero (i.e. only exactly coincindent
#' coordinates will be removed). For more details, see \code{\link[sp:remove.duplicates]
#' {remove.duplicates}}.
#' @import sp
#' @return \code{readOcc} returns a 'sp.occ' object corresponding to a list of species
#' with features from a class of 'SpatialPoints'.
#' @encoding UTF-8
#' @author Thaís Dória & Daniel Gonçalves-Souza
#' @export readOcc

readOcc <- function(occ, crs, dist = NULL){

    if(substr(occ, nchar(occ), nchar(occ)) == '/'){
    path <- substr(occ, 1, nchar(occ) - 1)
  }

  files.sp <- list.files(occ, pattern = ".csv$")
  sd <- do.call("list", lapply (files.sp, read.csv, header = TRUE))
  names(sd) <- gsub(".csv", " ", files.sp)
  f.clean1 <- function(sd){
  long=as.numeric(as.character(sd$long))
  lat=as.numeric(as.character(sd$lat))
  c=cbind(as.numeric(as.character(long)), as.numeric(as.character(lat)))
  c=data.frame(c)
  options(digits=4)
  if (is.character(crs)){
  sp=SpatialPoints(c, proj4string = CRS(crs))
  }
  if (class(crs) == "CRS"){
  sp=SpatialPoints(c, proj4string = crs)
  }
  sp2=remove.duplicates(sp, zerodist(sp, zero=as.numeric(dist)))
  }
  sp.pointsclean <- lapply(sd, f.clean1)
  class(sp.pointsclean) <- "sp.occ"
  return(sp.pointsclean)
}

