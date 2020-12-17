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
#' EOO raster for Birds
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
#' Dataset with geographical coordinates exemplifying how the occurrences data file
#' for the argument \code{occ} of the function \code{aHull} should be structured.

#' @docType data
#' @keywords datasets
#' @name df_plantOcc
#' @usage data(df_plantOcc)
#' @format A list with 12 data frames.
NULL

#### spOcc_plants ####
#'  Occurrence records for Plants
#'
#' Dataset with SpatialPoints exemplifying how the occurrences data file for the
#' argument \code{occ} of the function \code{aHull} should be structured.

#' @docType data
#' @keywords datasets
#' @name spOcc_plants
#' @usage data(spOcc_plants)
#' @format A spOcc' object corresponding to a list with 12 SpatialPoints.
NULL

#### poly ####
#' Shapefile of Caatinga
#'
#' Polygon of an region exemplifying how the polygon file for the
#' argument \code{poly} of the functions \code{checkOcc}, \code{aHull} and
#' \code{aohMixS} should be structured.

#' @docType data
#' @keywords datasets
#' @name poly
#' @usage data(poly)
#' @format A SpatialPolygonsDataFrame object.
NULL

#### ras ####
#' Raster model
#'
#' A fictitious dataset exemplifying how the raster file for the
#' argument \code{ras} of the function \code{aHull} should be structured.

#' @docType data
#' @keywords datasets
#' @name ras
#' @usage data(ras)
#' @format A RasterLayer.
NULL

#### ahull_plantShp ####
#' EOO shapefile for Plants (ahull)
#'
#' Dataset with SpatialPolygons exemplifying how the distribution file for the
#' argument \code{eooSp} of the function \code{aohMixS} should be structured.

#' @docType data
#' @keywords datasets
#' @name ahull_plantShp
#' @usage data(ahull_plantShp)
#' @format An 'aHull' object corresponding to a list with 5 SpatialPolygons.
NULL

#### ahull_plantRas ####
#'  EOO raster for Plants (ahull)
#'
#' Dataset with RasterLayer exemplifying how the distribution file for the
#' argument \code{eooSp} of the function \code{aohMixS} should be structured.

#' @docType data
#' @keywords datasets
#' @name ahull_plantRas
#' @usage data(ahull_plantRas)
#' @format An 'aHull' object corresponding to a list with 5 RasterLayers.
NULL

#### spdf_plantShp5spp ####
#' EOO shapefile for Plants
#'
#' Dataset with SpatialPolygonsDataFrame exemplifying how the distribution file
#' for the argument \code{eooSp} of the function \code{aohMixS} should be
#' structured.

#' @docType data
#' @keywords datasets
#' @name spdf_plantShp5spp
#' @usage data(spdf_plantShp5spp)
#' @format A SpatialPolygonsDataFrame with 5 Polygons.
NULL

#### sdm_plantRas5spp ####
#' SDM raster for Plants
#'
#' Dataset with RasterLayers exemplifying how the distribution model file for
#' the argument \code{modSp} of the functions \code{aohMixS} and \code{aohMixK}
#' should be structured.

#' @docType data
#' @keywords datasets
#' @name sdm_plantRas5spps
#' @usage data(sdm_plantRas5spp)
#' @format A list with 5 RasterLayers.
NULL

#### sdm_plantSta5spp ####
#' SDMs stack for Plants
#'
#' Dataset with RasterStack exemplifying how the distribution model file for
#' the argument \code{modSp} of the functions \code{aohMixS} and \code{aohMixK}
#' should be structured.

#' @docType data
#' @keywords datasets
#' @name sdm_plantSta5spp
#' @usage data(sdm_plantSta5spp)
#' @format A RasterStack with 5 RasterLayers.
NULL

#### plants_thresholds ####
#' Thresholds for Plants' SDMs
#'
#' A fictitious dataset exemplifying how the data frame for the argument
#' \code{threshold} of the function \code{aohMixK} should be structured.

#' @docType data
#' @keywords datasets
#' @name plants_thresholds
#' @usage data(plants_thresholds)
#' @format A data frame with 5 rows and 1 variables
NULL

#### df_plantOcc5spp ####
#'  Occurrence records for Plants
#'
#' Dataset with geographical coordinates exemplifying how the occurrences data file
#' for the argument \code{occ} of the function \code{aohMixK} should be structured.

#' @docType data
#' @keywords datasets
#' @name df_plantOcc5spp
#' @usage data(df_plantOcc5spp)
#' @format A list with 5 data frames.
NULL

#### Sp_plantOcc5spp ####
#' Occurrence records for Plants
#'
#' Dataset with SpatialPoints exemplifying how the occurrences data file for the
#' argument \code{occ} of the function \code{aohMixK} should be structured.

#' @docType data
#' @keywords datasets
#' @name Sp_plantOcc5spp
#' @usage data(Sp_plantOcc5spp)
#' @format A spOcc' object corresponding to a list with 5 SpatialPoints.
NULL

#### spOcc_plantOcc5spp ####
#'  Occurrence records for Plants
#'
#' Dataset with SpatialPoints exemplifying how the occurrences data file for the
#' argument \code{occ} of the function \code{aohMixK} should be structured.

#' @docType data
#' @keywords datasets
#' @name spOcc_plantOcc5spp
#' @usage data(spOcc_plantOcc5spp)
#' @format A spOcc' object corresponding to a list with 5 SpatialPoints.
NULL
