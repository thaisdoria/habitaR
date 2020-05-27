#' readOcc: read species occurrences
#'
#' Read and clean the occurrences records files (.csv format) of multiples species
#' from a specified folder and provide a list of 'SpatialPoints' of species through
#' the removal of duplicates coordinates.
#'
#' @usage readOcc (occ, crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0",
#' distOcc = 0.25)
#' @param occ Path for a folder with the species occurrences records files
#' (.csv format). Coordinates should be in decimal degrees. The files names should
#' correspond to the species names and must have be 3 columns identified as
#' "species" (species names or other identification of taxa), "long" (longitude),
#' "lat" (latitude). NOTE: Longitude must be at a column before the latitude column.
#' @param crs The Coordinate Reference System (CRS) specifing the projection and
#' datum of dataset. Could be a CRS object or a character string.
#' @param distOcc A value corresponding to the minimum distance assigned to consider
#' two coordinates as not duplicate. Values up to this distance will be consider
#' as duplicates and removed. Units of this value must be in km. Optional.
#' Default is zero (i.e. only exactly coincindent coordinates will be removed).
#' For more details, see \code{\link[sp:remove.duplicates]{remove.duplicates}}.
#' @return \code{readOcc} returns a 'sp.occ' object corresponding to a list of species
#' with features from a class of 'SpatialPoints'.
#' @encoding UTF-8
#' @author Thaís Dória & Daniel Gonçalves-Souza
#' @export readOcc
#' @import sp

readOcc1 <- function(occ, crs, distOcc = NULL, occSum=FALSE){

    if(substr(occ, nchar(occ), nchar(occ)) == '/'){
    path <- substr(occ, 1, nchar(occ) - 1)
  }

  files.sp <- list.files(occ, pattern = ".csv$")
  occ <- do.call("list", lapply (files.sp, read.csv, header = TRUE, sep=";"))
  names(occ) <- gsub(".csv", " ", files.sp)
  sp.pointsclean <- lapply(occ, f.clean1)
  for (i in 1:length(sp.pointsclean)){
  colnames(sp.pointsclean[[i]]@coords) <- c("long", "lat")
  }
  class(sp.pointsclean) <- "spOcc"

  if(occSum == FALSE){
  return(sp.pointsclean)
  }
  if(occSum == TRUE){
  df <- data.frame (matrix(ncol = 2, nrow = length(sp.pointsclean)))
  names(df) <- c('Species', 'OccSum')
  df[,1] <- names(sp.pointsclean)
  for (i in 1:length(sp.pointsclean)){
  df[i,2] <- length(sp.pointsclean[[i]]@coords[,1])}
  sp.pointsclean.l<-list(sp.pointsclean, df)
  return(sp.pointsclean.l)
  }
}

