#' aHull: generate alpha hull polygons for multiple-species
#'
#' From the occurrences records of multiple species, provides the alpha hull
#' polygon representing the extent of occurrence (EOO) of each species which have
#' a minimun of 3 not duplicate records. It is build based on the original function
#' \code{\link[rangeBuilder]{getDynamicAlphaHull}} (rangeBuilder package), that
#' determines the α parameter by the spatial distribution of the coordinates.
#'
#' @usage aHull (occ, crs, alphaIncrement = 0.01, fraction = 1.0, partCount = 2,
#' buff = 1000, distOcc = 0.25, poly = NULL, cropToPoly = FALSE)
#'
#' @param occ Occurrences records of the species. It might be a 'spOcc'
#' object corresponding to a list of 'SpatialPoints' for multiple species
#' (see \code{\link[aoh]{readOcc}} to obtain such class of object), a 'list' of
#' 'data.frames' with the occurrences from multiple species, or a path for a folder
#' with the species occurrences files (.csv format). Each file, corresponding to
#' only one species and named with the corresponding species names, must have be 3 columns
#' identified as "species" (species names or other identification of taxa), "long"
#' (longitude), "lat" (latitude). NOTE: Longitude must be at a column before the
#' latitude column.
#' @param crs The Coordinate Reference System (CRS) specifing the projection and
#' datum of dataset. Could be a CRS object or a character string.
#' @param distOcc A value corresponding to the minimum distance assigned to consider
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
#' species and the respective alpha value assigned to build the alpha hull.
#' If a \emph{poly} is provided and \emph{cropToPoly} is \code{TRUE}, \code{aHull}
#' also returns the cropped alpha hull.
#' @details The function generate an alpha hull polygon from the occurrences
#' records of multiple species by sequentially increasing α parameter (starting from 0
#' in steps of defined \emph{alphaIncrease} until find the smallest value that return the
#' \emph{partCount} polygon encompassing the \emph{fraction} of occurrences provided.
#' If \emph{poly} is provided, the function filter the original species dataset by
#' keeping only those species occurring inside of the specified polygon. In this case, the
#' alpha hull construction is restrict to the filtered species, but the extension of
#' these polygons can extrapolate the area of polygon if there are occurrences beyond
#' the boundaries of 'poly' object. If the user want also have the alpha hull cropped
#' by the area of \emph{poly}, assign \code{TRUE} to 'cropToPoly'.
#' @seealso Dynamic alpha hulls from increasing alpha values are created with
#' \code{\link[rangeBuilder:getDynamicAlphaHull]{getDynamicAlphaHull}}
#' @return \code{aHull} returns a 'aHull' object
#'
#' If the conditions cannot be satisfied, then a minimum convex hull is returned
#' .
#' It gives the number of records matching with the EOO ("MATCH.EOO) and with the
#' AOH ("MATCH.AOH) and the values of prevalence of points ("PP", Rondinini et al.
#' 2011) through the proportion of occurrences spatially congruent with suitable
#' cells, and of model prevalence ("MP", Rondinini et al. 2011), which represents
#' EOOs proportion assigned as suitable). AOH predict species occurrences
#' correctly when all available records match with its suitable cells (PP = 1).
#' The evaluation of AOHs performance is based on difference between PP and MP.
#' If PP > MP, AOH performs better than EOO in predict presences (Ficetola et al.
#' 2015).
#'
#' If plot is \code{TRUE}, the function return also a plot with a graphical representation
#' of validation results (see Rondinini et al. 2011). If plot \code{FALSE}, the function
#' return a data.frame
#'
#' @details The function gives the summary of validation steps followed to evaluate
#' the quality of models as described and performed by Rondidini et al. (2011) and
#' Ficetola et al. (2015).
#'
#' @examples
#'
#' ### Fictitious plants data
#'
#' # Example for signature 'DataFrame' (occ).
#'
#' ahull_plants<-aHull(eoo = eoo_birdShp, aoh = aoh_birdRas, plot = TRUE, progress = TRUE)
#'
#' @references
#' 1. Capinha Brooks, T. M, Fonseca, S.L. Pimm, Akçakaya, H.R., Buchanan, G.M., …,
#' Rondinini C. (2019). Measuring Terrestrial Area of Habitat (AOH) and Its
#' Utility for the IUCN Red List. Trends in Ecology &amp; Evolution, 34(11),
#' 977–986.
#'
#' 2. CRIADOR DO RANGE BUILDER.
#'
#' 3. Meyer et al. (2018)
#'
#' @encoding UTF-8
#' @author Thaís Dória & Daniel Gonçalves-Souza
#' @export aHull

aHull <- function(occ, crs, fraction = NULL, partCount = NULL, alphaIncrement = NULL,
                   buff = NULL, distOcc = NULL, poly = NULL, cropToPoly = FALSE,
                   occSum= FALSE){

  # Checking list and warning messages
  {
   if (missing(occ))
    stop("occ is missing")
  if (missing(crs))
    stop("crs is missing")
  }

  # Input data as 'data.frame' will be converted into a 'spOcc' object
  {
  # If ocurrences records are in .csv files of multiple species:
  # To read the files and create a list of data.frames
  if (is.character(occ)){
      if(substr(occ, nchar(occ), nchar(occ)) == '/'){
      occ <- substr(occ, 1, nchar(occ) - 1)
    }
      files.sp <- list.files(occ, pattern = ".csv$")
      occ <- do.call("list", lapply (files.sp, read.csv, header = TRUE))
      names(occ) <- gsub(".csv", " ", files.sp)
    }

    # If ocurrences records of multiple species are already in a list of 'data.frames
  if (class(occ) == "list" & class(occ[[1]]) =="data.frame"){
        occ <- occ
     # Converting data.frame occurrences into 'SpatialPoints'
     # Warning messages
     {
  if (is.null(distOcc))
    warning("distOcc is missing, so default (zero) is used")
  }
     occ <- lapply(occ, f.clean1) # a 'spOcc' object
       for (i in 1:length(occ)){
       colnames(occ[[i]]@coords) <- c("long", "lat")
       }
     class(occ) <- "spOcc"
  }
}

  # Checking and filtering the species occurrences based on a specified area (if 'poly is provided)
  if (class(occ) == "spOcc" & !is.null(poly) | class(occ) == "list" &
      class(occ[[1]]) =="SpatialPoints" & !is.null(poly)){
  spcheck<-checkOcc(occ, poly)
  occ<-occ[match(names(occ), names(spcheck))]
  occ<-list.clean(occ, fun = is.null, recursive = TRUE)
  }


    # GENERATING THE ALPHA HULL
    # Removing from dataset those species with less than 3 records
    occ.ahul <- list.clean(occ, fun = f.clean2, recursive = TRUE) # List with SpatialPoints of species with, at least, 3 occurrences records not duplicated
    spp.names <- names(occ.ahul)

    # Data frame of results
    df <- data.frame (matrix(ncol = 2, nrow = length(occ.ahul)))
    names(df) <- c('Species', 'Alpha')
    df[, 1] <- spp.names

    # Building the alpha hull for each species
    sp.ahull <- mapply(f.ahull, occ.ahul, fraction, partCount, buff, alphaIncrement)
    ahulls <- sp.ahull[1,] # a 'SpatialPolygonsDataFrame' object
    alphas <- matrix(unlist(sp.ahull[2,]))
    alphas <-gsub("alpha", "", alphas[,1])
    df[,2]<- alphas

    # Results
    if(occSum == FALSE){
    ahull.result <- list(AlphaValues = df, AhullShps = ahulls)
    class(ahull.result) <- "aHull"
    return(ahull.result)
    }

    if(occSum == TRUE){
      df2 <- data.frame (matrix(ncol = 1, nrow = length(occ.ahul)))
      names(df2) <- 'Occurrences'
      for (i in 1:length(occ.ahul)){
        df2[i,1] <- length(occ.ahul[[i]]@coords[,1])}
      df<-cbind(df, df2)
      ahull.result <- list(AlphaValues = df, AhullShps = ahulls)
      class(ahull.result) <- "aHull"
      return(ahull.result)
          }
  }








