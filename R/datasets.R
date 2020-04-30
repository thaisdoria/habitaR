#### habpref ####
#' Habitat preference for Amphibians
#'
#' A fictitious dataset exemplifying how the data frame for the argument \code{matrix.hab.pref}
#' of the function \code{aoh} should be structured.

#' @docType data
#' @keywords datasets
#' @name habpref
#' @usage data(habpref)
#' @format A data frame with 129 rows and 5 variables
NULL

#### alpref ####
#' Altitude preference for Amphibians
#'
#' A fictitious dataset exemplifying how the data frame for the argument \code{matrix.alt.pref}
#' of the function \code{aoh} should be structured.

#' @docType data
#' @keywords datasets
#' @name alpref
#' @usage data(alpref)
#' @format A data frame with 99 rows and 3 variables
NULL

#### lc ####
#' Land Cover
#'
#' A fictitious dataset exemplifying how the raster for the argument \code{lc.rec}
#' of the function \code{aoh} should be structured.

#' @docType data
#' @keywords datasets
#' @name lc
#' @usage data(lc)
#' @format A RasterLayer with 4790 rows, 3361 columns and resolution of
#' 0.0028 x 0.0028 degrees
NULL

#### al ####
#' Altitude Map
#'
#' A fictitious dataset exemplifying how the raster for the argument \code{alt.map}
#' of the function \code{aoh} should be structured.

#' @docType data
#' @keywords datasets
#' @name al
#' @usage data(al)
#' @format A RasterLayer with 2395 rows, 1681 columns and resolution of
#' 0.005 x 0.005 degrees
NULL

#### class_ref ####
#' Habitat classification reference
#'
#' A dataset to reference how the user could reclassify the
#' IUCN's Habitats Classification Scheme for the argument \code{cat}
#' of the function \code{prefHab}.

#' @docType data
#' @keywords datasets
#' @name class_ref
#' @usage data(class_ref)
#' @format A data frame with 6 rows and 125 variables
NULL

if (getRversion() >= "2.15.1") utils::globalVariables('class_ref')

#### AMPHIBIANS DATASET ####

#### eoo_amphShp ####
#'  Extent of Occurrence' (EOO) corresponding to the original (i.e. not refined)
#'  spatial distribution of Amphibians in class of 'SpatialPolygonsDataFrame'.
#'
#' A fictitious dataset exemplifying how the raster file for the argument \code{eoo.sp}
#' of the function \code{aohVal} could be structured. This dataset was generated
#' based on available data from the IUCN database. To provide an example dataset
#' for this package, the original maps were thinned and lightly custom modified.


#' @docType data
#' @keywords datasets
#' @name eoo_amphShp
#' @usage data(eoo_amphShp)
#' @format A SpatialPolygonDataFrame with 3 features.
NULL

#### eoo_amphRas ####
#'  Extent of Occurrence' (EOO) corresponding to the original (i.e. not refined)
#'  spatial distribution of Amphibians in class of 'SpatialPolygonsDataFrame'.
#'
#' A fictitious dataset exemplifying how the raster file for the argument \code{eoo.sp}
#' of the function \code{aohVal} could be structured. This dataset was generated
#' based on available data from the IUCN database. To provide an example dataset
#' for this package, the original maps were thinned and lightly custom modified.

#' @docType data
#' @keywords datasets
#' @name eoo_amphRas
#' @usage data(eoo_amphRas)
#' @format A RasterStack with 3 layers.
NULL

#### aoh_amphShp ####
#' Spatial distribution corresponding to the 'Area of Habitat' (AOH) for Amphibians as
#' SpatialPolygons in ESRI shapefile (.shp)
#'
#' A fictitious dataset exemplifying how the spatial distribution file for
#' the argument \code{aoh} of the function \code{aohVal} could be structured.

#' @docType data
#' @keywords datasets
#' @name aoh_amphShp
#' @usage data(aoh_amphShp)
#' @format A SpatialPolygons with 3 features.
NULL

#### aoh_amphRas ####
#' Spatial distribution corresponding to 'Area of Habitat' (AOH) for Amphibians as
#' raster file format (.asc).
#'
#' A fictitious dataset exemplifying how the raster file for the argument \code{aoh}
#' of the function \code{aohVal} could be structured.

#' @docType data
#' @keywords datasets
#' @name aoh_amphRas
#' @usage data(aoh_amphRas)
#' @format A RasterStack with 3 layers.
NULL

#### BIRDS DATASET ####
