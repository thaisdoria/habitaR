#' aHull: generate alpha hull polygons for multiple-species
#'
#' From a set of occurrences records, provides the alpha hull representing the
#' extent of occurrence (EOO) of multiple species which have a minimun of 3 not
#' duplicate records.
#' It is build based on the original function \code{\link[rangeBuilder]{getDynamicAlphaHull}}
#' from the \pkg{rangeBuilder} package, that determines the α parameter by the spatial distribution of the
#' coordinates.
#'
#' @usage aHull (occ, crs, fraction = NULL, partCount = NULL, alphaIncrement = NULL,
#' buff = 0.0, distOcc = NULL, poly = NULL, cropToPoly = FALSE, occSum = TRUE)
#'
#' @param occ Occurrences records of the species (coordinates in decimal degrees).
#' It might be a path for a folder with the species occurrences files (.csv format),
#' a 'list' of 'data.frames' with the occurrences from multiple species (see data
#' examples), or a 'spOcc' object corresponding to a list of 'SpatialPoints' from
#' multiple species (see \code{\link[aoh]{readOcc}} to obtain such object).
#' NOTE: If path is provided, each .csv file should correspond to only one species
#' and the file must be named with the corresponding species names. The files must
#' have 3 columns identified as "species" (species names or other identification of taxa),
#' "long" (longitude), "lat" (latitude). Longitude must be in a column before the
#' latitude column.
#' @param crs The Coordinate Reference System (CRS) specifing the projection and
#' datum of dataset. Could be a CRS object or a character string.
#' @param fraction The minimum fraction of occurrences that must be included in polygon.
#' @param partCount The maximum number of disjunct polygons that are allowed.
#' @param alphaIncrement The amount to increase alpha with each iteration.
#' @param buff A buffer distance in meters to increase boundaries of alpha hull.
#' Default is zero (0).
#' @param distOcc A value corresponding to the minimum distance assigned to
#' consider two coordinates as not duplicate. Values up to this distance will
#' correspond to duplicates and removed. Units of this value must be in km.
#' Default is zero (i.e. only exactly coincindent coordinates will be removed).
#' Optional and only used if 'occ' is a path for .csv files or a list of data.frames.
#' If 'occ' correspond to 'SpatialPoints', this argument should be ignored
#' (i.e., distOcc = NULL). For more details, see \code{\link[sp:remove.duplicates]{remove.duplicates}}
#' in the \pkg{sp} package.
#' @param poly Optional. A polygon (ESRI shapefile in a 'SpatialPolygonsDataFrame'
#' class) of a given area to be checked for occurrences of species and to restrict
#' the building of alpha hull only to the species occurring inside of the provided
#' polygon. See details.
#' @param cropToPoly (logical) Whether the output should also include the alpha
#' hull cropped by the provided poly. Only used if 'poly' is provided.
#' @param occSum (logical) Whether the output should include also a data.frame
#' with the number of occurrences records after the removal of duplicate coordinates.
#' Default is \code{TRUE}.
#'#'
#' @return \code{aHull} returns a list with two elements. The first is a data.frame
#' of species and the respective alpha values and occurrences used to construct
#' the alpha hull. The second element is a 'aHull' object corresponding to a list
#' of alpha hulls of each species ('SpatialPolygons class). If the conditions
#' assigned by the user to build the alpha hulls cannot be satisfied, is returned
#' the minimum convex (MCH). If a \emph{poly} is provided and \emph{cropToPoly}
#' is \code{TRUE}, \code{aHull} also returns the cropped alpha hulls as a third
#' element from the resulting list.
#'
#'
#' @details Based on a set of occurrences records of multiple-species, the
#' function generate alpha hull polygons ('SpatialPolygons' class) by sequentially
#' increasing the value of α parameter (starting from 0 in steps of defined
#' \emph{alphaIncrement}) until find the smallest value that met the \emph{partCount} condition and
#' encompass the \emph{fraction} of occurrences assigned by the user.
#' If \emph{poly} is provided, the function filter the original species dataset
#' by keeping only those species occurring inside of the specified region.
#' In this case, the construction of alpha hulls will be restricted only to the
#' filtered species, but the extension of these resulting maps can extrapolate
#' the area of 'poly' if the occurrences extend far away beyond the boundaries
#' of 'poly' object. NOTE: if the user want have the alpha hull cropped by the
#' area of \emph{poly}, 'cropToPoly' should be assign as \code{TRUE}, but even in
#' this case, the alpha hull is firstly constructed based on all dataset of
#' occurrences and then cropped by some desired region in a second step. Thus,
#' cropped alpha hulls correspond to the original alpha hull clipped/trinned by
#' some region and not drawn based in a restrict set of occurrences recorded inside
#' of some specific region.
#'
#' @seealso The dynamic alpha hulls drawn from the sequentially increase of alpha
#' values are created with \code{\link[rangeBuilder:getDynamicAlphaHull]{getDynamicAlphaHull}}
#' from \pkg{rangeBuilder} package.
#'
#' @examples
#'
#' ### Fictitious plants data
#'
#' # Example for signature 'DataFrame' (occ).
#'
#' ahull_plants<-aHull(occ = occ_plants, crs=, plot = TRUE, progress = TRUE)
#'
#' @references
#' 1. Capinha Brooks, T. M, Fonseca, S.L. Pimm, Akçakaya, H.R., Buchanan, G.M., …,
#' Rondinini C. (2019). Measuring Terrestrial Area of Habitat (AOH) and Its
#' Utility for the IUCN Red List. Trends in Ecology &amp; Evolution, 34(11),
#' 977–986.
#'
#' 2. CRIADOR DO RANGE BUILDER.
#'
#' 3. Meyer et al. (2018).
#'
#' @encoding UTF-8
#' @author Thaís Dória & Daniel Gonçalves-Souza
#' @export aHull
#' @import rangeBuilder
#' @import sp

aHull <- function(occ, crs, fraction = NULL, partCount = NULL, alphaIncrement = NULL,
                   buff = 0, distOcc = NULL, poly = NULL, cropToPoly = FALSE,
                   occSum= FALSE){

  # Checking list and warning messages
  {
   if (missing(occ))
    stop("occ is missing")
  if (missing(crs))
    stop("crs is missing")
  }

  # Converting input data into a 'spOcc' object
  if (class(occ) != "spOcc"){
  # Input data as 'data.frame' will be converted into a 'spOcc' object
       # 1. ocurrences records as .csv files of multiple species:
       # To read the files and create a list of data.frames
       if (is.character(occ)){
          if(substr(occ, nchar(occ), nchar(occ)) == '/'){
        occ <- substr(occ, 1, nchar(occ) - 1)
        }
      files.sp <- list.files(occ, pattern = ".csv$")
      occ <- do.call("list", lapply (files.sp, read.csv, sep=";", header = TRUE))
      names(occ) <- gsub(".csv", " ", files.sp)
    }

       # 2. Ocurrences records of multiple species as a list of 'data.frames
       if (class(occ) == "list" & class(occ[[1]]) =="data.frame"){
        # Converting data.frame occurrences into 'SpatialPoints'
        # Warning messages
     {
  if (is.null(distOcc))
    warning("distOcc is not provided, so default (zero) is used")
  }
     occ <- lapply(occ, f.clean1) # a 'spOcc' object
       for (i in 1:length(occ)){
       colnames(occ[[i]]@coords) <- c("long", "lat")
       }
     class(occ) <- "spOcc"
  }

 # Input data as 'SpatialPoints' will be converted into a 'spOcc' object
    if(class(occ) == "list" & class(occ[[1]]) =="SpatialPoints"){
      class(occ) <- "spOcc"
    }

}

  # Checking and filtering the species occurrences based on a specified area (if 'poly' is provided)
  if (class(occ) == "spOcc" & !is.null(poly)){
  spcheck<-checkOcc(occ, poly, SpOcc=TRUE)
  occ<-spcheck[[2]]
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
    sp.ahull <- mapply(f.ahull, occ.ahul, MoreArgs = list(fraction, partCount, buff, alphaIncrement))
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








