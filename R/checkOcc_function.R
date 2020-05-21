#' checkOcc: check if occurrences fall inside of an specific are
#'
#' From a list of 'SpatialPoints' for multiple species, gives the species and its
#' occurrences that are inside of a polygon (ESRI shapefile format) representing
#' a region of interest (e.g. study area, continent, country or other regions with
#' specific boundaries).
#'
#' @usage checkOcc (occ, dist = 0.25, poly)
#' @param occ Occurrences records of the species (coordinates in decimal degrees).
#' It might be a 'sp.occ' object corresponding to a list of 'SpatialPoints' for
#' multiple species (see \code{\link[aoh]{readOcc}} to obtain such class of object),
#' a 'list' of 'data.frames' with the occurrences from multiple species, or a path
#' for a folder with the species occurrences files (.csv format). Each file should
#' corresponding to only one species and named with the corresponding species names.
#' This file must have be 3 columns identified as "species" (species names or other
#' identification of taxa), "long" (longitude), "lat" (latitude).
#' @param poly A polygon (ESRI shapefile as 'SpatialPolygonsDataFrame' class) of
#' the specific area to be checked.
#' #' @param dist A value corresponding to the minimum distance assigned to consider
#' two coordinates as not duplicate. Values up to this distance will be consider
#' as duplicates and removed. Units of this value must be in km. Default is zero
#' (i.e. only exactly coincindent coordinates will be removed). For more details,
#' see \code{\link[sp:remove.duplicates]{remove.duplicates}}. Optional and only
#' used if 'occ' is a path for .csv files or a list of' data.frame.
#' If 'occ' correspond to 'SpatialPoints', 'dist' should be ignored.
#' @import sp
#' @import rlist
#' @return \code{checkOcc} returns the species occurring inside of the polygon
#' provided with the respective total of ocurrences falling in this polygon.
#' @encoding UTF-8
#' @author Thaís Dória & Daniel Gonçalves-Souza
#' @export checkOcc

checkOcc<-function(occ, poly, dist = NULL){

  # Warning messages
  if (missing(occ))
    stop("occ is missing")
  if (missing(poly))
    stop("poly is missing")

   # Possibilities of input data
  # 1. Occurrences as .csv files or list of data.frame
  {
    # Path for a folder
    if (is.character(occ)){
      if(substr(occ, nchar(occ), nchar(occ)) == '/'){
        occ <- substr(occ, 1, nchar(occ) - 1)
      }
      files.sp <- list.files(occ, pattern = ".csv$")
      sd <- do.call("list", lapply (files.sp, read.csv, header = TRUE, sep=";"))
      names(sd) <- gsub(".csv", " ", files.sp)
    }

    # List with occurrences data from a multiple species in a 'data.frame' class
    if (class(occ) == "list" & class(occ[[1]]) =="data.frame"){
      sd <- occ
    }

    # To convert occurrences in data.frame into 'SpatialPoints'
    # Warning message
    if (is.null(dist))
      warning("dist is missing, so zero (default) is used")
    occ <- lapply(sd, f.clean1)
    class(occ) <- "sp.occ" # a 'sp.occ' object
  }

  # 2. Ocurrences as 'SpatialPoints' or 'sp.occ' (obtained with readOcc function)
  {
    # List with occurrences data from a multiple species in a 'data.frame' class
    if (class(occ) == "list" & class(occ[[1]]) =="SpatialPoints"){
      class(occ) <- "sp.occ"
    }

    # List with occurrences data from a multiple species in a 'SpatialPoints' class
    if (class(occ) == "sp.occ"){
      # Identifying spatial limits of the extent of interest (research area)
      lin <- as(poly, "SpatialLinesDataFrame")
      pts <- as.data.frame(as(lin, "SpatialPointsDataFrame"))
      pol.x<-pts$coords.x1
      pol.y<-pts$coords.x2
      cf.occ<-lapply(occ, f.cf.occ) # object with occurrences checked (0 represents absence inside of biome and 1 represents presence inside of biome)
      cf.occsum<- lapply(cf.occ, sum) # object with the sum of the checked records (0 represents complete absence of species inside of biome)
      cf.occsum[cf.occsum==0] <- NA
      cf.occclean<-list.clean(cf.occsum, fun = is.na, recursive = TRUE)
      return(cf.occclean)
    }
  }
}
