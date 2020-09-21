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
#' buff = 0.0, distOcc = NULL, poly = NULL, cropToPoly = FALSE, rasOut = FALSE,
#' ras = NULL)
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
#' datum of dataset. Could be a CRS object or a character string. Only used
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
#' polygon. NOTE: this argument does not reduce the set of occurrences based on
#' the poly, but only reduce the species dataset based on its presence inside of poly.
#' See details.
#' @param cropToPoly (logical) Whether the output should also include the alpha
#' hull cropped by the provided poly. Only used if 'poly' is provided. Default is
#' \code{FALSE}.
#' @param rasOut (logical) Whether the output should be also include a raster objects.
#' Default is \code{FALSE}.
#' @param ras A raster object (file in .asc or .tif format) to be used as baseline
#' to rasterize the ahull polygons. Only used if rasOut' is \code{TRUE}.
#'
#' @return By default, \code{aHull} returns a list with two elements:
#'  \itemize{
#'   \item A data.frame of species and the respective alpha values and number of occurrences
#'   used to construct the alpha hull (i.e. after the removal of duplicate coordinates).
#'    \item A 'aHull' object corresponding to a list of species-specific
#' alpha hulls ('SpatialPolygons class). If the conditions assigned by the user to
#' build the alpha hulls cannot be satisfied, is returned the minimum convex hull (MCH)
#' and the alpha value is identified as 'MCH'.
#' }
#' NOTE: if a \emph{poly} is provided and \emph{cropToPoly} is \code{TRUE}, \code{aHull}
#' also returns a third element from the resulting list:
#' \itemize{
#' \item A 'aHull' object corresponding to a list of species-specific cropped alpha hulls.
#' }
#'
#' @details Based on a set of occurrences records of multiple-species, the
#' function generate alpha hull polygons ('SpatialPolygons' class) by sequentially
#' increasing the value of α parameter (starting from 0 in steps of defined
#' \emph{alphaIncrement}) until find the smallest value that met the \emph{partCount}
#' condition and encompass the \emph{fraction} of occurrences assigned by the user.
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
#' ahull_plants<-aHull(occ = occ_plants, crs= "+proj=longlat +datum=WGS84 +ellps=WGS84
#'  +towgs84=0,0,0", fraction = 1, partCount = 1, alphaIncrement = 1, buff = 0,
#'  distOcc = 0.25, poly = poly, cropToPoly = TRUE, rasOut = TRUE, ras = ras)
#'
#' @encoding UTF-8
#'
#' @references
#' 1. Brugman M.A., and Fox J.C. (2003). Bias in species range estimates from minimum
#' convex polygons: implications for conservation and options for improved planning.
#' Animal Conservation 6:19-28.
#'
#' 2. Capinha C., and Pateiro-López B. (2014). Predicting species distributions in
#' new areas or time periods with alpha-shapes. Ecological Informatics, 24:231–237.
#'
#' 3. Rabosky A.R.D., Cox C.L., Rabosky D.L., Title P.O., Holmes I.A., Feldman A.,
#' and McGuire J.A. (2016). Coral snakes predict the evolution of mimicry across
#' New World snakes. Nature Communications 7:11484.
#'
#' 4. Meyer L., Diniz-Filho J.A.F., and Lohmann L.G. (2018). A comparison of
#' hull methods for estimating species ranges and richness maps. Plant Ecology &
#' Diversity, 10(5-6):389-401.
#
#' @author Thaís Dória & Daniel Gonçalves-Souza
#' @export aHull
#' @import rangeBuilder
#' @import raster
#' @import sp

aHull <- function(occ, crs = NULL, fraction = NULL, partCount = NULL, alphaIncrement = NULL,
                   buff = 0, distOcc = NULL, poly = NULL, cropToPoly = FALSE,
                  rasOut = FALSE, ras = NULL){

  # Checking list / data conversion / warning messages
  {
  if (missing(occ))
    stop('occ is missing')
    # Occ as 'SpatialPoints' will be converted into a 'spOcc' object
    if(class(occ) == "list" & class(occ[[1]]) =="SpatialPoints"){
      class(occ) <- "spOcc"
    }
  if (is.null(crs) & (class(occ) != "spOcc"))
      stop('a crs must be informed')
  if (is.null(poly) & (cropToPoly == TRUE))
    stop('cropToPoly can only be true when poly is provided')
  if ((rasOut == TRUE) & is.null(ras))
      stop('rasOut can only be true when ras is provided')
    }

  # Pre-processing of data
  # Converting input data into a 'spOcc' object
  if (class(occ) != "spOcc"){

       # 1. ocurrences records as .csv files of multiple species:
       # To read the files and create a list of data.frames
       if (is.character(occ)){
          occ<-readOcc(occ, crs)
    }

       # 2. Ocurrences records of multiple species as a list of 'data.frames'
       # To convert data.frame occurrences into 'SpatialPoints'
       if (class(occ) == "list" & class(occ[[1]]) =="data.frame"){
            # Warning message
            if (is.null(distOcc))
            warning('distOcc is not provided, so default (zero) is used')
          occ <- lapply(occ, f.clean1) # a 'spOcc' object
          for (i in 1:length(occ)){
             colnames(occ[[i]]@coords) <- c("long", "lat")
             }
          class(occ) <- "spOcc"
  }
}

  # ANALYSIS BASED ON 'spOcc' OBJECT
  # GENERATING THE ALPHA HULL

      # If 'poly' is provided
      # Filtering the species occurrences based on a specified area
      if (class(occ) == "spOcc" & !is.null(poly)){
      spcheck<-checkOcc(occ, poly, SpOcc=TRUE)
      occ<-spcheck[[2]]
       }

      # Removing from dataset those species with less than 3 records
      occ.ahul <- list.clean(occ, fun = f.clean2, recursive = TRUE) # List with SpatialPoints of species with, at least, 3 occurrences records not duplicated
      spp.names <- names(occ.ahul)

      # Data frame of results
      df <- data.frame (matrix(ncol = 3, nrow = length(occ.ahul)))
      names(df) <- c('Species', 'Ocurrences', 'Alpha')
      df[, 1] <- spp.names

      # Counting and summarazing the occurrences the will be used to build alpha hulls
      for (i in 1:length(occ.ahul)){
      df[i,2] <- length(occ.ahul[[i]]@coords[,1])}

      # Building the alpha hull for each species
      sp.ahull <- mapply(f.ahull, occ.ahul, MoreArgs = list(fraction, partCount,
                                                            buff, alphaIncrement))
      ahulls <- sp.ahull[1,] # a 'SpatialPolygonsDataFrame' object
      class(ahulls) <- "aHull"
      alphas <- matrix(unlist(sp.ahull[2,]))
      alphas <- gsub("alpha", "", alphas[,1])
      df[,3]<- alphas

      # OUTPUTS

          # Shapefile output
            if (rasOut == FALSE){

               # Without crop to poly
               if(cropToPoly == FALSE){ # default
         # Output
         ahull.result <- list(AlphaValues_OccRecords = df, AhullShps = ahulls)
         return(ahull.result)
         }
               # Cropping to poly
               if(cropToPoly == TRUE){
               crop.ahulls <- list()
                 for (i in 1:length(ahulls)){
                 crop.ahul <- crop(ahulls[[i]], poly, filename=spp.names[i])
                 crop.ahulls[[i]] <- crop.ahul
                   }
              names(crop.ahulls)<-spp.names
              class(crop.ahulls) <- "aHull"

      # OUTPUT
      ahull.result <- list(AlphaValues_OccRecords = df, AhullShps = ahulls,
                           CroppedAhullShps=crop.ahulls)
      return(ahull.result)
         }

        }

          # Raster output
            if(rasOut == TRUE){

               # Without crop to poly
               if(cropToPoly == FALSE){ # default
               # Rasterizing the ahulls
               ahulls.r <- mapply(rasterize, ahulls, MoreArgs =
                                    list(ras, background = 0, mask=FALSE))
               for (i in 1:length(ahulls.r)){
               ahulls.r[[i]][ahulls.r[[i]] > 1] <- 1
                }

      # OUTPUT
      ahull.result <- list(AlphaValues_OccRecords = df, AhullShps = ahulls,
                             AhullRas = ahulls.r)
        return(ahull.result)
      }
               # Cropping to poly
               if(cropToPoly == TRUE){
               crop.ahulls <- list()
               for (i in 1:length(ahulls)){
               crop.ahul <- crop(ahulls[[i]], poly, filename=spp.names[i])
               crop.ahulls[[i]] <- crop.ahul
                }
               names(crop.ahulls)<-spp.names
               class(crop.ahulls) <- "aHull"
               # Rasterizing the results
                   # Original ahulls
                   ahulls.r <- mapply(rasterize, ahulls, MoreArgs = list(ras,
                                                  background = 0, mask=FALSE))
                  for (i in 1:length(ahulls.r)){
                  ahulls.r[[i]][ahulls.r[[i]] > 1] <- 1
                  }
                   # Cropped ahulls
                   crop.ahulls.r <- mapply(rasterize, crop.ahulls, MoreArgs =
                                             list(ras,background = 0, mask=FALSE))
                   for (i in 1:length(crop.ahulls.r)){
                   crop.ahulls.r[[i]][crop.ahulls.r[[i]] > 1] <- 1
            }

       # OUTPUT
       ahull.result <- list(AlphaValues_OccRecords = df, AhullShps = ahulls,
                             CroppedAhullShps = crop.ahulls, AhullRas = ahulls.r,
                             CroppedAhullRas = crop.ahulls.r)
       return(ahull.result)
      }
    }
       }











