#### habpref ####
#' Habitat preference for Birds
#'
#' A fictitious dataset exemplifying how the data frame for the argument \code{habPref}
#' of the function \code{aoh} should be structured.

#' @docType data
#' @keywords datasets
#' @name habpref_bird
#' @usage data(habpref_bird)
#' @format A data frame with 4 rows and 4 variables
NULL

#### alpref_bird ####
#' Altitude preference for Birds
#'
#' A fictitious dataset exemplifying how the data frame for the argument \code{altPref}
#' of the function \code{aoh} should be structured.

#' @docType data
#' @keywords datasets
#' @name alpref_bird
#' @usage data(alpref_bird)
#' @format A data frame with 4 rows and 4 variables
NULL

#### lc_map ####
#' Land Cover
#'
#' A fictitious dataset exemplifying how the raster for the argument \code{lc}
#' of the function \code{aoh} should be structured.

#' @docType data
#' @keywords datasets
#' @name lc_map
#' @usage data(lc_map)
#' @format A RasterLayer with 1198 rows, 841 columns and resolution of
#' 0.01 x 0.01 degrees
NULL

#### alt_map ####
#' Altitude Map
#'
#' A fictitious dataset exemplifying how the raster for the argument \code{alt.map}
#' of the function \code{aoh} should be structured.

#' @docType data
#' @keywords datasets
#' @name alt_map
#' @usage data(alt_map)
#' @format A RasterLayer with 266 rows, 187 columns and resolution of
#' 0.05 x 0.05 degrees
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

#### BIRDS DATASET ####

#### eoo_birdShp ####
#' EOO shapefile for Birds
#'
#' Extent of Occurrence' (EOO) corresponding to the original (i.e. not refined)
#' spatial distribution of Birds in class of 'SpatialPolygonsDataFrame'.
#'  A fictitious dataset exemplifying how the raster file for the argument \code{eoo.sp}
#' of the functions \code{aoh} and \code{aohVal} could be structured. This
#' dataset was generated based on available data from the IUCN database.
#' To provide an example dataset for this package, the original maps were
#' thinned and lightly custom modified.


#' @docType data
#' @keywords datasets
#' @name eoo_birdShp
#' @usage data(eoo_birdShp)
#' @format A SpatialPolygonDataFrame with 4 features.
NULL

#### eoo_birdRas ####
#' EOO Raster for Birds
#'
#'  Extent of Occurrence' (EOO) corresponding to the original (i.e. not refined)
#'  spatial distribution of Birds in class of 'SpatialPolygonsDataFrame'.
#' A fictitious dataset exemplifying how the raster file for the argument \code{eoo.sp}
#' of the function \code{aohVal} could be structured. This dataset was generated
#' based on available data from the IUCN database. To provide an example dataset
#' for this package, the original maps were thinned and lightly custom modified.

#' @docType data
#' @keywords datasets
#' @name eoo_birdRas
#' @usage data(eoo_birdRas)
#' @format A RasterStack with 4 layers.
NULL


#### aoh_birdShp ####
#' AOH shapefile for Birds
#'
#' Spatial distribution corresponding to the 'Area of Habitat' (AOH) for Birds as
#' SpatialPolygons in ESRI shapefile (.shp)
#' A fictitious dataset exemplifying how the spatial distribution file for
#' the argument \code{aoh} of the function \code{aohVal} could be structured.

#' @docType data
#' @keywords datasets
#' @name aoh_birdShp
#' @usage data(aoh_birdShp)
#' @format A SpatialPolygons with 4 features.
NULL


#### aoh_birdRas ####
#' AOH raster for Birds
#'
#' Spatial distribution corresponding to 'Area of Habitat' (AOH) for Birds as
#' raster file format (.asc).
#' A fictitious dataset exemplifying how the raster file for the argument \code{aoh}
#' of the function \code{aohVal} could be structured.

#' @docType data
#' @keywords datasets
#' @name aoh_birdRas
#' @usage data(aoh_birdRas)
#' @format A RasterStack with 3 layers.
NULL


#### climSuit_bird ####
#'  Climatic suitability map for Birds
#'
#' A fictitious dataset exemplifying how the list file for the argument \code{climSuit}
#' of the function \code{aoh} should be structured.

#' @docType data
#' @keywords datasets
#' @name climSuit_bird
#' @usage data(climSuit_bird)
#' @format A list with 2 Rasterlayers.
NULL

#### PLANTS DATASET ####

#### df_plantOcc ####
#'  Occurrence records for Plants
#'
#' Dataset with geographical coordinates exemplifying how the list file for the
#' argument \code{occ} of the function \code{aHull} should be structured.

#' @docType data
#' @keywords datasets
#' @name df_plantOcc
#' @usage data(df_plantOcc)
#' @format A list with 12 data frames.
NULL

#### spOcc_plants ####
#'  A 'spOcc' object corresponding to a list with SpatialPoints for Plants
#'
#' Dataset with SpatialPoints exemplifying how the list file for the
#' argument \code{occ} of the function \code{aHull} should be structured.

#' @docType data
#' @keywords datasets
#' @name spOcc_plants
#' @usage data(spOcc_plants)
#' @format A list with 12 SpatialPoints.
NULL

#### ahull_plantShp ####
#'  An 'spOcc' object corresponding to a list with SpatialPoints for Plants
#'
#' Dataset with SpatialPoints exemplifying how the list file for the
#' argument \code{occ} of the function \code{aHull} should be structured.

#' @docType data
#' @keywords datasets
#' @name spOcc_plants
#' @usage data(spOcc_plants)
#' @format A list with 12 SpatialPoints.

#### ahull_plantRas ####

#### sdm_plantRas5spp ####

#### sdm_plantSta5spp ####

#### plants_thresholds ####

#### df_plantOcc5spp ####

#### Sp_plantOcc5spp ####

#### spdf_plantShp5spp ####

#### spOcc_plantOcc5spp ####
