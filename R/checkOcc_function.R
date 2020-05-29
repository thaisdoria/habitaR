#' checkOcc: check if occurrences fall inside of an specific are
#'
#' From a list of 'SpatialPoints' for multiple species, gives the species and its
#' occurrences that are inside of a polygon (ESRI shapefile format) representing
#' a region of interest (e.g. study area, continent, country or other regions with
#' specific boundaries).
#'
#' @usage checkOcc (occ, distOcc = 0.25, poly)
#' @param occ Occurrences records of the species (coordinates in decimal degrees).
#' It might be a 'spOcc' object corresponding to a list of 'SpatialPoints' for
#' multiple species (see \code{\link[aoh]{readOcc}} to obtain such class of object),
#' a 'list' of 'data.frames' with the occurrences from multiple species, or a path
#' for a folder with the species occurrences files (.csv format). Each file should
#' corresponding to only one species and named with the corresponding species names.
#' This file must have be 3 columns identified as "species" (species names or other
#' identification of taxa), "long" (longitude), "lat" (latitude). NOTE: Longitude
#' must be at a column before the latitude column.
#' @param poly A polygon (ESRI shapefile as 'SpatialPolygonsDataFrame' class) of
#' the specific area to be checked.
#' #' @param distOcc A value corresponding to the minimum distance assigned to consider
#' two coordinates as not duplicate. Values up to this distance will be consider
#' as duplicates and removed. Units of this value must be in km. Default is zero
#' (i.e. only exactly coincindent coordinates will be removed). For more details,
#' see \code{\link[sp:remove.duplicates]{remove.duplicates}}. Optional and only
#' used if 'occ' is a path for .csv files or a list of 'data.frame'. If 'occ'
#' correspond to 'SpatialPoints', 'distOcc' should be ignored.
#' @param SpOcc (logical) Whether the output should also include a list with
#' features from 'SpatialPoints' class corresponding only to the occurrences of
#' species filtered based on the poly area. Default if \code{FALSE}.
#' @return \code{checkOcc} returns the species occurring inside of the polygon
#' provided with the respective total of ocurrences falling in this polygon.
#' If SpOcc is \code{TRUE}, \code{checkdOcc} also returns the 'SpatialPoints'
#' of species filtered based on the poly area.
#' @encoding UTF-8
#' @author Thaís Dória & Daniel Gonçalves-Souza
#' @export checkOcc
#' @import sp
#' @import rlist

checkOcc<-function(occ, poly, distOcc = NULL, SpOcc=FALSE){

  # Checklist and warning messages
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
      occ <- do.call("list", lapply (files.sp, read.csv, header = TRUE, sep=";"))
      names(occ) <- gsub(".csv", " ", files.sp)
    }

    # List with occurrences data from a multiple species in a 'data.frame' class
    if (class(occ) == "list" & class(occ[[1]]) =="data.frame"){
      occ <- occ
    }

    # To convert occurrences in data.frame into 'SpatialPoints'
    # Warning message
    if (is.null(distOcc))
      warning("distOcc is not provided, so default (zero) is used")
    occ <- lapply(occ, f.clean1)
    for (i in 1:length(occ)){
      colnames(occ[[i]]@coords) <- c("long", "lat")
    }
    class(occ) <- "spOcc" # a 'spOcc' object
  }

  # 2. Ocurrences as 'SpatialPoints' or 'spOcc' (obtained with readOcc function)
  {
    # List with occurrences data from a multiple species in a 'data.frame' class
    if (class(occ) == "list" & class(occ[[1]]) =="SpatialPoints"){
      for (i in 1:length(occ)){
        colnames(occ[[i]]@coords) <- c("long", "lat")
      }
      class(occ) <- "spOcc"
    }

    # List with occurrences data from a multiple species in a 'SpatialPoints' class
    if (class(occ) == "spOcc"){
      # Identifying spatial limits of the extent of interest (research area)
      lin <- as(poly, "SpatialLinesDataFrame")
      pts <- as.data.frame(as(lin, "SpatialPointsDataFrame"))
      pol.x<-pts$coords.x1
      pol.y<-pts$coords.x2
      cf.occ<-lapply(occ, f.cf.occ) # object with occurrences checked (0 represents absence inside of biome and 1 represents presence inside of biome)
      cf.occsum<- lapply(cf.occ, sum) # object with the sum of the checked records (0 represents complete absence of species inside of biome)
      cf.occsum[cf.occsum==0] <- NA
      cf.occcheck<-list.clean(cf.occsum, fun = is.na, recursive = TRUE)

      if(SpOcc == FALSE){
        return(cf.occcheck)
      }

      if(SpOcc == TRUE){
        sp.occcheck<-occ[match(names(occ), names(cf.occcheck))]
        sp.occcheck<-list.clean(sp.occcheck, fun = is.null, recursive = TRUE)
        cf.occcheck.l<-list(CheckedOcc = cf.occcheck , CheckedSpatialPoints = sp.occcheck)
        return(cf.occcheck.l)
      }
    }
  }
}
