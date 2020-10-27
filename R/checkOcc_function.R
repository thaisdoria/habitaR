#' checkOcc: check if occurrences fall inside of an specific are
#'
#' From 'SpatialPoints' of multiple species, gives the species and its
#' occurrences that are inside of a polygon (ESRI shapefile format) representing
#' a region of interest (e.g. study area, continent, country or other regions with
#' specific boundaries).
#'
#' @usage checkOcc (occ, crs, poly, distOcc = NULL, SpOcc = FALSE)
#' @param occ Occurrences records of the species (coordinates in decimal degrees).
#' It might be a path for a folder with the species occurrences files (.csv format),
#' a 'list' of 'data.frames' with the occurrences from multiple species (see data
#' examples), or a 'spOcc' object corresponding to a list of 'SpatialPoints' from
#' multiple species (see \code{\link[habitaR]{readOcc}} to obtain such object).
#' NOTE: If path is provided, each .csv file should correspond to only one species
#' and the file must be named with the corresponding species names. The files must
#' have 3 columns identified as "species" (species names or other identification of taxa),
#' "long" (longitude), "lat" (latitude). Longitude must be in a column before the
#' latitude column.
#' @param crs The Coordinate Reference System (CRS) specifing the projection and
#' datum of dataset. Could be a CRS object or a character string.
#' @param poly A polygon (ESRI shapefile from a 'SpatialPolygonsDataFrame' class)
#' of the specific area to be checked.
#' @param distOcc A value corresponding to the minimum distance assigned to
#' consider two coordinates as not duplicate. Values up to this distance will
#' correspond to duplicates and removed. Units of this value must be in km. Default
#' is zero (i.e. only exactly coincindent coordinates will be removed).
#' For more details,see \code{\link[sp:remove.duplicates]{remove.duplicates}}.
#' Optional and only used if 'occ' is a path for .csv files or a list of 'data.frame'.
#' If 'occ' correspond to 'SpatialPoints', this argument ('distOcc') should be ignored
#' (i.e. NULL).
#' @param SpOcc (logical) Whether the output should also return the 'SpatialPoints'
#' of species filtered based on the poly boundaries. Default is \code{FALSE}.
#'
#' @details  The function filter the initial dataset of species (\emph{occ})
#' based on a given region (\emph{poly}). NOTE: if \emph{SpOcc} is \code{TRUE}, the
#' 'SpatialPoints' returned encompass all set of occurrences and not a subset
#' that match with the specified region (\emph{poly}). Therefore, the 'SpatialPoints'
#' are \emph{NOT} cleaned to remove the records falling outside the polygon checked and to
#' restrict the 'SpatialPoints' only to occurrences recorded inside of polygon,
#' but just filtered to reduce the species from initial dataset based in the area
#' of \emph{poly}.
#'
#' @return \code{checkOcc} returns a data.frame of species and the respective
#' occurrences matching with the provided polygon. If SpOcc is \code{TRUE},
#' \code{checkdOcc} returns a list with two elements. The first element is the
#' data.frame with the species and occurrences filtered by the polygon. The second
#' is the 'SpatialPoints' of each species that match with polygon, but without clean
#' the records to remove those falling outside the polygon checked. See details.
#'
#' @examples
#'
#' # Example for signature 'list' of 'data.frames'
#'
#' check <- checkOcc(occ_plants, poly, distOcc = NULL, SpOcc = TRUE)
#'
#' @encoding UTF-8
#' @author Thaís Dória & Daniel Gonçalves-Souza
#' @export checkOcc
#' @import sp
#' @import rlist

checkOcc<-function(occ = NULL, crs = NULL, distOcc = NULL, poly = NULL, SpOcc = FALSE){

  # Checklist and warning messages
 { if (missing(occ))
    stop("occ is missing")
   if (is.null(poly))
    stop("a poly object must be informed. See usage and examples to check other arguments.")

   # When occ is NOT a 'spOcc' object or a list of 'SpatialPoints'
   if(class(occ) != "spOcc" |
       class(occ) == "list" & class(occ[[1]]) != "SpatialPoints"){
     if (is.null(crs))
         stop("a crs must be informed")
     if (class(crs)=="numeric")
         stop("crs as 'numeric' is not valid for slot ‘proj4string’ in an object of class “Spatial”. See usage.")
     if (!is.null(distOcc) & class(distOcc) != "numeric")
         stop("distOcc must be provided as 'numeric' or NULL")
     if (is.null(distOcc))
         warning("distOcc is not provided, so default (zero) is used")
   }
   # When occ is a 'spOcc' object or a list of 'SpatialPoints'
   if(class(occ) == "spOcc" |
      class(occ) == "list" & class(occ[[1]]) == "SpatialPoints"){
   if (!is.null(crs) & class(crs) != "numeric")
     warning("'occ' already correspond to 'SpatialPoints', so 'crs' is not used")
   if (!is.null(distOcc) & class(distOcc) == "numeric")
     warning("'occ' already correspond to 'SpatialPoints', so 'distOcc' is not used")
   }
 }


  # Possibilities of input data
  # 1. Occurrences as .csv files or list of data.frame
  {
    # Path for a folder
    if (class(occ) == "character"){
    if(substr(occ, nchar(occ), nchar(occ)) == '/'){
      occ <- substr(occ, 1, nchar(occ) - 1)
    }

    files.sp <- list.files(occ, pattern = ".csv$", full.names = T)
    names <- list.files(occ, pattern = ".csv$", full.names = F)
    occ <- do.call("list", lapply (files.sp, read.csv, header = TRUE, sep=";"))
    names(occ) <- gsub("*\\.csv", '', names)
    }

    # List with occurrences data from a multiple species in a 'data.frame' class
    if (class(occ) == "list" & class(occ[[1]]) =="data.frame"){
      occ <- occ
    }


  # To get coords from data.frame
  l<-list()
  for (i in 1:length(occ)){
    long<-as.numeric(as.character(occ[[i]]$long))
    lat<-as.numeric(as.character(occ[[i]]$lat))
    l[[i]]<-as.matrix(cbind(long, lat))
  }
  names(l) <- names(occ) # list of coords

  # To convert occurrences into 'SpatialPoints'
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

  # To convert object in a "spOcc" class
  for (i in 1:length(sp.clean2)){
    colnames(sp.clean2[[i]]@coords) <- c("long", "lat")
  }
  class(sp.clean2) <- "spOcc" # a 'spOcc' object
  occ<-sp.clean2
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
      cf.occ<-lapply(occ, f.cf.occ) # object with occurrences checked (0 represents absence inside of poly and 1 represents presence inside of poly)
      cf.occsum<- lapply(cf.occ, sum) # object with the sum of the checked records (0 represents complete absence of species inside of poly)
      cf.occsum[cf.occsum==0] <- NA
      cf.occcheck<-list.clean(cf.occsum, fun = is.na, recursive = TRUE)
      class(cf.occcheck) <- "spOcc"

      # Output
      # Data frame of results
      df.res <- data.frame (matrix(ncol = 2, nrow = length(cf.occcheck)))
      names(df.res) <- c('Species', 'Checked Occurrences')
      df.res[, 1] <- names(cf.occcheck)
      for (i in 1:length(cf.occcheck)){
      df.res[i,2] <- cf.occcheck[[i]]
      }

      if(SpOcc == FALSE){
        return(df.res)
      }

      if(SpOcc == TRUE){
        sp.occcheck<-occ[match(names(cf.occcheck), names(occ))]
        sp.occcheck<-list.clean(sp.occcheck, fun = is.null, recursive = TRUE)
        cf.occcheck.l<-list(CheckedOcc = df.res , CheckedSpatialPoints = sp.occcheck)
        return(cf.occcheck.l)
      }
    }
  }
}
