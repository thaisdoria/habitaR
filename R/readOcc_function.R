#' readOcc: read species occurrences
#'
#' Read and clean the occurrences records files (.csv format) of multiples species
#' from a specified folder and provide a list of 'SpatialPoints' of species through
#' the removal of duplicates coordinates.
#'
#' @usage readOcc (occ = occ, crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0",
#' distOcc = NULL, occSum = FALSE)
#'
#' @param occ Path for a folder with the species occurrences records files
#' (.csv format). Coordinates should be in decimal degrees. The names of .csv files
#' should correspond to the species names and must have 3 columns identified as
#' "species" (species names or other identification of taxa), "long" (longitude),
#' "lat" (latitude). NOTE: Longitude must be in a column before the latitude column.
#' @param crs The Coordinate Reference System (CRS) specifing the projection and
#' datum of dataset. Could be a CRS object or a character string.
#' @param distOcc A value corresponding to the minimum distance to consider
#' two coordinates as not duplicate. Values up to this distance will be consider
#' as duplicates and removed. Units of this value must be in km. Optional.
#' Default is zero (i.e. only exactly coincident coordinates will be removed).
#' For more details, see \code{\link[sp:remove.duplicates]{remove.duplicates}}.
#' @param occSum (logical) Whether the output should include also a data.frame
#' with the number of occurrences records after the removal of duplicate coordinates.
#' Default is \code{FALSE}.
#'
#' @seealso \code{\link[sp:remove.duplicates]{remove.duplicates}}
#'
#' @return By default, \code{readOcc} returns a list with two elements:
#' \itemize{
#'   \item A 'spOcc' object corresponding to a species list with features of
#' 'SpatialPoints' class.
#'   \item A data.frame with the number of occurrences records after the cleaning
#' step to remove duplicate coordinates.
#' }
#' If occSum is \code{FALSE}, \code{readOcc} returns only the 'spOcc' object.
#' @encoding UTF-8
#' @author Thaís Dória & Daniel Gonçalves-Souza
#' @export readOcc
#' @import sp

readOcc <- function(occ = NULL, crs = NULL, distOcc = NULL, occSum = FALSE) {

  # Checklist and warning messages
  if (missing(occ))
    stop("occ is missing")
  if (class(occ)!= "character")
    stop("occ must be a path for folder with the species occurrences records files")
  if (is.null(crs))
    stop("a crs must be informed")
  if (class(crs)=='numeric')
    stop("crs as 'numeric' is not valid for slot ‘proj4string’ in an object of class “Spatial”. See usage.")
  if (is.null(distOcc))
    warning("distOcc is not provided, so default (zero) is used")
  if (!is.null(distOcc) & class(distOcc) != "numeric")
    stop("distOcc must be provided as 'numeric' or NULL")


  # 1. Input data
  # Occurrences as .csv files
  # Path for a folder
  if(substr(occ, nchar(occ), nchar(occ)) == '/'){
    occ <- substr(occ, 1, nchar(occ) - 1)
  }

  files.sp <- list.files(occ, pattern = ".csv$", full.names = T)
  names <- list.files(occ, pattern = ".csv$", full.names = F)
  occ <- do.call("list", lapply (files.sp, read.csv, header = TRUE, sep=";"))
  names(occ) <- gsub("*\\.csv", '', names)

  # 2. To get coords from data.frame
  l<-list()
  for (i in 1:length(occ)){
    long<-as.numeric(as.character(occ[[i]]$long))
    lat<-as.numeric(as.character(occ[[i]]$lat))
    l[[i]]<-as.matrix(cbind(long, lat))
  }
  names(l) <- names(occ) # list of coords

  # 3. To convert occurrences into 'SpatialPoints'
  # CRS is required
  sp.clean<-list()
  for (i in 1:length(l)){
    if (is.character(crs)){
      spcc<-SpatialPoints(l[[i]], proj4string = CRS(crs))
    }
    if (class(crs) == "CRS"){
      spcc<-SpatialPoints(l[[i]], proj4string = crs)
    }
    sp.clean[[i]]<-spcc
  }
  names(sp.clean) <- names(l)

  # Removing duplicates (distOcc)
  sp.clean2<-list()
  for (i in 1:length(sp.clean)){
    if (!is.null(distOcc)){
      sp<-remove.duplicates(sp.clean[[i]], zerodist(sp.clean[[i]], zero=as.numeric(distOcc)))
    }
    if (is.null(distOcc)){
      sp<-remove.duplicates(sp.clean[[i]])
    }
    sp.clean2[[i]]<-sp
  }
  names(sp.clean2) <- names(sp.clean)

  # 4. To convert object in a "spOcc" class
  for (i in 1:length(sp.clean2)){
    colnames(sp.clean2[[i]]@coords) <- c("long", "lat")
  }
  class(sp.clean2) <- "spOcc" # a 'spOcc' object


  # 5. Output
  # Without return a data.frame with total of spatial points after removing duplicates
  if(occSum == FALSE){
    return(sp.clean2)
  }
  # Returning a data.frame with total of spatial points after removing duplicates
  if(occSum == TRUE){
    # Data frame of results
    df <- data.frame (matrix(ncol = 2, nrow = length(sp.clean2)))
    names(df) <- c('Species', 'Cleaned Occurrences')
    df[,1] <- names(sp.clean2)
    for (i in 1:length(sp.clean2)){
      df[i,2] <- length(sp.clean2[[i]]@coords[,1])}
    sp.clean.l<-list(sp.clean2, df)
    return(sp.clean.l)}
}


