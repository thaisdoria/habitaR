#' aHull function
#'
#' Provide the alpha hull polygon to represent the extent of occurrence (EOO) of
#' species from the occurrence records of multiple species. It is build based on
#' the original function 'getDynamicAlphaHull' from 'rangeBuilder' package.
#'
#' @usage aHull (occ, crs, dist)
#' @param occ Occurrences records of the species. It might be a 'sp.occ'
#' object corresponding to a list of 'SpatialPoints' for multiple species
#' (see \code{\link[aoh]{readOcc}} to obtain such class of object), a 'list' of
#' 'data.frames' with the occurrences from multiple species, or a path for a folder
#' with the species occurrences files (.csv format). Each file, corresponding to
#' only one species and named with the corresponding species names, must have be 3 columns
#' identified as "species" (species names or other identification of taxa), "long"
#' (longitude), "lat" (latitude).
#' @param crs The Coordinate Reference System (CRS) specifing the projection and
#' datum of dataset. Could be a CRS object or a character string.
#' @param dist A value corresponding to the minimum distance assigned to consider
#' two coordinates as not duplicate. Values up to this distance will be consider
#' as duplicates and removed. Default is zero (i.e. only exactly coincindent
#' coordinates will be removed). Units of this value must be the same as those of
#' the coordinates for projected data or in km if coordinates are defined to be
#' longitude/latitude. For more details, see \code{\link[sp:remove.duplicates]
#' {remove.duplicates}}.
#' @param poly Optional. A polygon (ESRI shapefile in 'SpatialPolygonsDataFrame'
#' class) of the specific area to be checked.

aHull <- function(occ, crs, dist = NULL, poly = NULL, fraction, partCount,
                  initialAlpha, alphaIncrement, clipToCoast, verbose = TRUE){

  # Warning messages
  if (missing(occ))
    stop("occ is missing")
  if (missing(crs))
    stop("crs is missing")

  # Reading .csv files with geo-referenced records of multiple species to generate a 'sp.occ' object
  if (is.character(occ)){
      if(substr(occ, nchar(occ), nchar(occ)) == '/'){
      occ <- substr(occ, 1, nchar(occ) - 1)
    }
      files.sp <- list.files(occ, pattern = ".csv$")
      sd <- do.call("list", lapply (files.sp, read.csv, header = TRUE))
      names(sd) <- gsub(".csv", " ", files.sp)
    }

  # Reading a list object with occurrences data from a multiple species in a 'data.frame' class
  if (class(occ) == "list" & class(occ[[1]]) =="data.frame"){
    sd <- occ
  }
  # To convert occurrences in data.frame into 'SpatialPoints'

  # Warning messages
  if (missing(dist))
    warning("dist is missing, so zero (default) is used")

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
    occ <- lapply(sd, f.clean1) # a 'sp.occ' object

  # Checking and filtering the species occurring in a specified area (if 'poly is provided)
  if (class(occ) == "sp.occ" & !is.null(poly) | class(occ) == "list" &
      class(occ[[1]]) =="data.frame" & !is.null(poly)){
  spcheck<-checkOcc(poly, occ)
  occ<-occ[match(names(occ), names(spcheck))]
  occ<-list.clean(occ, fun = is.null, recursive = TRUE)
  }

 # To identify and subset the species with less than 3 records
    f.clean2 <- function (y){
    options(digits=4)
    sp.clean <- nrow(coordinates(y@coords)) < 3
    }
    occ.ahul <- list.clean(occ, fun = f.clean2, recursive = TRUE) # List with SpatialPoints of species with, at least, 3 occurrences records not duplicated (4913 spp)
    spp.names <- names(occ.ahul)

  # Function to automatize the bulding of ahull to multiple-species
    f.ahull <-function(occ.ahul, spp.names){
    al <- getDynamicAlphaHull(occ.ahul@coords, fraction=1, partCount=2, initialAlpha = 0.0, alphaIncrement=0.01, clipToCoast='no', verbose=T)
    al1 <- data.frame(matrix(unlist(al[[2]])),stringsAsFactors=FALSE)
    write.table(al1, file=paste(spp.names,"alpha.csv", sep=";"))
    al2=al[[1]]
    shapefile(al2, filename = paste(spp.names,"AHULL", sep=""), overwrite=TRUE)
  return(al1)
    }

    # Applying the function
    sp.ahull <- mapply(f.ahull, occ.ahul, spp.names)
}

