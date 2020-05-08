#' aHull function
#'
#' Provide the alpha hull polygon from a occurrence records of species to represent
#' the extent of occurrence (EOO) of species for a provided list of species with
#' their respective occurrence data (i.e. geo-referenced coordinates).
#'
#' @usage
#' aHull ( aoh, resolution, plot=TRUE, progress=FALSE)
#'
#' @param eoo Spatial distribution data of the species representing the original
#' (i.e. not refined) extent of occurrence (EOO). It might correspond to
#' SpatialPolygons in ESRI shapefile (.shp), to RasterStack file (.asc or .tif) or to a
#' path for a folder with EOO shapefiles (.shp). If the shapefile is used, the
#' name of the species must be on the second column of the attribute table
#' of the shapefile.
#' @param aoh Spatial distribution data of the species representing the area
#' of habitat (AOH). It might correspond to SpatialPolygons in ESRI shapefile (.shp),
#' to RasterStack file (.asc or .tif), created or not with function
#' \code{\link[habitaR]{aoh}}, or might correspond to a path for a folder with EOO shapefiles (.shp).
#' If the shapefile is used, the name of the species must be on the second column of the attribute table.
#' @param resolution Numeric value to indicate the resolution prefered to generate
#' the raster files. Only used if both spatial data ('eoo' and 'aoh') are in shapefile format.
#' If 'eoo' is a shapefile and the 'aoh' is a raster, the resolution of latter is considered.
#' @param spplist Species list (character type) in the same order as the Rasterstack of
#' distribution data.
#' @param plot (logical) Whether the output should also return a plot representing the
#' graphical result of validation (see Rondinini et al. 2011). Default is TRUE.
#' @param progress (logical) a bar showing the progress of the function. Default is FALSE.
#' @import dismo
#' @import sp
#' @import ggplot2
#' @importFrom utils txtProgressBar setTxtProgressBar
#' @return \code{aohVal} If plot=TRUE, the function return a list with two objects.
#' The first is a data.frame with a summary of results for each species which aoh was evaluated.
#' It gives the number of records matching with the EOO ("MATCH.EOO) and with the AOH ("MATCH.AOH)
#' and the values of prevalence of points ("PP", Rondinini et al. 2011) through the proportion of
#' occurrences spatially congruent with suitable cells, and of model prevalence ("MP",
#' Rondinini et al. 2011), which represents EOOs proportion assigned as suitable). AOH predict
#' species occurrences correctly when all available records match with its suitable cells (PP = 1).
#' The evaluation of AOHs performance is based on difference between PP and MP. If PP > MP, AOH
#' performs better than EOO in predict presences (Ficetola et al. 2015).
#' The second object is a ggplot with a graphical representation of validation results
#' (see Rondinini et al. 2011).
#' If plot=FALSE, the return only the data.frame with the summary of results.
#' @details The function give the summary of validation steps followed to evaluate
#' the quality of models as described and performed by Rondidini et al. (2011).
#' If plot=FALSE, the function return a data.frame
#' and Ficetola et al. (2015)
#' @examples
#' # Example for signature 'SpatialPolygonsDataFrame' (eoo and aoh)
#' val_data1 <- aohVal(eoo = eoo_amphShp, aoh = aoh_amphShp, resolution = 0.05, plot=TRUE, progress = TRUE)
#' # Example for signature 'SpatialPolygonsDataFrame' (eoo) and 'RasterLayer' (aoh)
#' val_data2 <- aohVal(eoo = eoo_amphShp, aoh = aoh_amphRas, plot=TRUE, progress = TRUE)
#' # Example for signature 'RasterLayer' (eoo and aoh)
#' val_data3 <- aohVal(eoo = eoo_amphRas, aoh = aoh_amphRas, spplist = spplist_amph, plot = TRUE, progress = TRUE)
#' @references
#' 1. Rondinini, C., Di Marco, M., Chiozza, F., Santulli, G., Baisero, D., Visconti, P., Boitani, L. (2011). Global habitat suitability models of terrestrial mammals. Philosophical Transactions of the Royal Society B, 366, 2633–2641.
#' 2. Brooks, T. M, Fonseca, S.L. Pimm, Akçakaya, H.R., Buchanan, G.M., …, Rondinini C. (2019). Measuring Terrestrial Area of Habitat (AOH) and Its Utility for the IUCN Red List. Trends in Ecology &amp; Evolution, 34(11), 977–986.
#' 3. Ficetola, G. F., Rondinini, C., Bonardi, A., Baisero, D., &amp; Padoa-Schippa, E. (2015).Habitat availability for amphibians and extinction threat: a global analysis. Diversity and Distributions, 21(3), 302–311.
#' @author Thaís Dória & Daniel Gonçalves-Souza
#'
#' @export aohVal

aohVal <- function (eoo, aoh, resolution=NULL, spplist=NULL, plot=TRUE){






gapAnalysis
