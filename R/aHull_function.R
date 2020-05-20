#' aHull: Generate alpha hull polygons for a multiple-species
#'
#' From the raw occurrences records of multiple species, provides the alpha hull
#' polygon to represent the extent of occurrence (EOO) of each species which have
#' a minimun of 3 not duplicate records. It is build based on the original function
#' \code{\link[rangeBuilder:getDynamicAlphaHull]{getDynamicAlphaHull}}
#' (rangeBuilder package), that determines the α parameter by the spatial
#' distribution of the coordinates.
#'
#' @usage aHull (occ, crs, dist = 0.25, poly = NULL, fraction = 1.0, partCount = 2,
#' buff = 1000, alphaIncrement = 0.01, cropToPoly = FALSE)
#'
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
#' @param poly Optional. A polygon (ESRI shapefile as 'SpatialPolygonsDataFrame'
#' class) of the specific area to be checked for occurrences of species and to
#' restrict alpha hull development only to the species occurring inside of the polygon.
#' See details.
#' @param fraction The minimum fraction of occurrences that must be included in polygon.
#' @param partCount The maximum number of disjunct polygons that are allowed.
#' @param buff A buffer distance in meters to increase boundaries of alpha hull.
#' @param alphaIncrement The amount to increase alpha with each iteration.
#' @param cropToPoly (logical) Whether the output should also include the alpha
#' hull cropped by the provided poly. Only used if poly provided.
#' @import rangeBuilder
#' @return \code{aHull} returns the alpha hull polygon (ESRI shapefile as
#' 'SpatialPolygonsDataFrame') representing the extent of occurrence (EOO) of
#' species and the respective alpha value assigned to build the provided polygon.
#' If a \emph{poly} is provided and \emph{cropToPoly} is \code{TRUE}, \code{aHull}
#' also returns the cropped alpha hull.
#' @details The function generate an alpha hull polygon from the occurrences
#' records of multiple species by sequentially increasing α parameter (starting from 0
#' in steps of defined \emph{alphaIncrease} until find the smallest value that return the
#' \emph{partCount} polygon
#' encompassing the fraction of occurrences provided.
#' If \emph{poly} is provided, the function filter the original species dataset by
#' keeping only those species occurring inside of the polygon. In this case, the
#' alpha hull construction is restrict to those filtered species, but the extend of
#' these polygons can extrapolate the area of polygon if are occurrences beyond the boundaries of 'poly' objetc. If the
#' spatial is desired, assign TRUE to 'clipToPoly'.
#' generated
#' , even if there are records
#' besides this area
#' @seealso Dynamic alpha hulls from increasing alpha values are created with
#' \code{\link[rangeBuilder:getDynamicAlphaHull]{getDynamicAlphaHull}}
#' @encoding UTF-8
#' @author Thaís Dória & Daniel Gonçalves-Souza
#' @export aHull

aHull <- function(occ, crs, dist = NULL, poly = NULL, fraction, partCount, buff,
                  alphaIncrement =  NULL, cropToPoly = FALSE){

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
  if (is.null(dist))
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
  spcheck<-checkOcc(occ, poly)
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
    al <- getDynamicAlphaHull(occ.ahul@coords, fraction=fraction, partCount=partCound,
                              initialAlpha = 0.0, alphaIncrement=alphaIncrement,
                              clipToCoast='no', verbose=T)
    al1 <- data.frame(matrix(unlist(al[[2]])),stringsAsFactors=FALSE)
    write.table(al1, file=paste(spp.names,"alpha.csv", sep=";"))
    al2=al[[1]]
    ahul<-shapefile(al2, filename = paste(spp.names,"AHULL", sep=""), overwrite=TRUE)

    if (cropToPoly == TRUE){
      croped<-crop(ahul, poly)
      return(croped)
    }
  return(al1)
    }

    # Applying the function
    sp.ahull <- mapply(f.ahull, occ.ahul, spp.names)
}


