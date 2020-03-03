#'  AOH function
#'
#' Provide the area of habitat (AOH) of a given species through refinement of its known geographic distribution
#'
#' @param sd Spatial distribution shapefile. The name of the species must be on the second column of the attribute table of the shapefile.
#' @param lc.rec Land use map reclassified for the categories of habitat
#' @param matrix.hab.pref Data frame 0/1 of habitat preference of the species. First #column must be the species name. The posterior columns must be named after the categories of habitat as folowing the lc.rec classification
#' @param alt.map Elevation map
#' @param matrix.alt.pref Data frame with altitudinal range of species. First column must be the species name, second column the min value of altitude and the third column the max value of altitude
#' @param shp.out (logical) Whether the output should be a shapefile as opposed to a raster
#' @import raster
#' @return The result is a RasterLayer or RasterBrick; or SpatialPolygons object
#' @details The function map the area of habitat within the geographical distribution (SpatialPolygon) given as the input data. as the refined distribution of a given species. This refinement is made considering the specific preference for habitats of a given species.
#' @author Daniel Gonçalves-Souza & Thaís Dória
#' @export aoh

aoh <- function(sd, lc.rec, matrix.hab.pref, alt.map = NULL,
                matrix.alt.pref, shp.out = FALSE){
  lc.crop <- mask(crop(lc.rec, sd), sd)
  sp.habpref <- matrix.hab.pref[sd@data[, 2] == matrix.hab.pref[, 1],
                                2:ncol(matrix.hab.pref)]
  hab.cat <- as.numeric(colnames(sp.habpref)[as.vector(sp.habpref[1, ] == 1)])
  hab.ref <- lc.crop %in% hab.cat
  hab.ref <- mask(hab.ref, sd)

  if (is.null(alt.map) == FALSE){
    pol.ref <- rasterToContour(hab.ref)
    alt.crop <- mask(crop(alt.map, pol.ref), pol.ref)
    sp.altpref <- matrix.alt.pref[sd@data[, 2] == matrix.alt.pref[, 1], 2:3]
    alt.ref <- alt.crop >= sp.altpref[1] & alt.crop <= sp.altpref[2]
    ifelse(shp.out == TRUE, return(rasterToPolygons(alt.ref, dissolve = TRUE)),
           return(alt.ref))
  }

  if (is.null(alt.map)) {
    warning('No altitude map was provided, therefore only the refined is based
      on vegetation preference only ')
    ifelse(shp.out == TRUE, return(rasterToPolygons(hab.ref, dissolve = TRUE)),
           return(hab.ref))

  }
}
