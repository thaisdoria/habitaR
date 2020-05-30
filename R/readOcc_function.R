#' readOcc: read species occurrences
#'
#' Read and clean the occurrences records files (.csv format) of multiples species
#' from a specified folder and provide a list of 'SpatialPoints' of species through
#' the removal of duplicates coordinates.
#'
#' @usage readOcc (occ, crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0",
#' distOcc = NULL, occSum = TRUE)
#'
#' @param occ Path for a folder with the species occurrences records files
#' (.csv format). Coordinates should be in decimal degrees. The names of .csv files
#' should correspond to the species names and must have 3 columns identified as
#' "species" (species names or other identification of taxa), "long" (longitude),
#' "lat" (latitude). NOTE: Longitude must be in a column before the latitude column.
#' @param crs The Coordinate Reference System (CRS) specifing the projection and
#' datum of dataset. Could be a CRS object or a character string.
#' @param distOcc A value corresponding to the minimum distance assigned to consider
#' two coordinates as not duplicate. Values up to this distance will be consider
#' as duplicates and removed. Units of this value must be in km. Optional.
#' Default is zero (i.e. only exactly coincident coordinates will be removed).
#' For more details, see \code{\link[sp:remove.duplicates]{remove.duplicates}}.
#' @param occSum (logical) Whether the output should include also a data.frame
#' with the number of occurrences records after the removal of duplicate coordinates.
#' Default is \code{TRUE}.
#'
#' @seealso \code{\link[sp:remove.duplicates]{remove.duplicates}}
#'
#' @return By default, \code{readOcc} returns a list with two elements. The first
#' element is a 'spOcc' object corresponding to a species list with features of
#' 'SpatialPoints' class. The second element is a data.frame with the number of
#' occurrences records after the cleaning step to remove duplicate coordinates.
#' If occSum is \code{FALSE}, \code{readOcc} returns only the 'spOcc' object.
#' @encoding UTF-8
#' @author Thaís Dória & Daniel Gonçalves-Souza
#' @export readOcc
#' @import sp

readOcc <- function(occ, crs, distOcc = NULL, occSum = TRUE){

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

  if(occSum == TRUE){
  df <- data.frame (matrix(ncol = 2, nrow = length(sp.pointsclean)))
  names(df) <- c('Species', 'Cleaned Occurrences')
  df[,1] <- names(sp.pointsclean)
  for (i in 1:length(sp.pointsclean)){
  df[i,2] <- length(sp.pointsclean[[i]]@coords[,1])}
  sp.pointsclean.l<-list(sp.pointsclean, df)
  return(sp.pointsclean.l)
  }

  if(occSum == FALSE){
    return(sp.pointsclean)
  }
}

