#' aoh: mapping the AOH of species
#'
#' Provide the area of habitat (AOH) of a given species through refinement of its
#' known geographic distribution
#' @usage aoh(eoo_sp = NULL, lc = NULL, alt = NULL, alt_pref = NULL,
#' hab_pref = NULL, clim_suit = NULL, threshold = 0.5, resolution = NULL,
#' continuous = FALSE, shpOut = FALSE, extentOut = NULL,
#' progress = FALSE)
#' @param eoo_sp SpatialPolygons of the spatial distribution of the species or path
#'  for a folder with spatial distribution shapefiles (ESRI shapefile format).
#'  The name of the species must be on the second column of the attribute table
#'  of the shapefile.
#' @param lc 'RasterLayer', 'RasterBrick' or 'RasterStack' object of the
#' land use map reclassified for the categories of habitat.
#' @param hab_pref Data.frame 0/1 of habitat preference of the species.
#'  First column must be the species name. The posterior columns must be named
#'  after the categories of habitat as following the lc classification.
#'  \code{\link[habitaR]{prefHab}} can help obtain this data.
#' @param alt 'RasterLayer' object of the elevation map. Optional.
#' @param alt_pref Data.frame with altitudinal range of species. First
#' column must be the species name, second column the minimum value of altitude
#' and the third column the maximum value of altitude
#' @param clim_suit List containing RasterLayers objects of the climatic suitability
#' of the species. The itens of the list must be named after their
#' corresponding species. Optional.
#' @param shpOut (logical) Whether the output should be a shapefile as opposed
#' to a raster.
#' @param resolution Numeric value indicating the preferred resolution for the
#' rasters. Resolution must coarser than the resolution of lc and alt.
#' @param continuous (logical) Whether the output should be continuous instead binary
#' for the rasters. Only used if resolution provided or when the resolution of
#' lc and alt are different.
#' @param threshold Numeric value indicating the threshold of the cell coverage
#' by the species to the species be considered present (values between 0 and 1).
#' Only used if continuous = \code{FALSE} and a coarser resolution is provided or
#' the resolution of lc and alt are different or shpOut is
#' \code{TRUE}.
#' @param extentOut Extent object or a vector of four numbers indicating the
#' preferred output for the rasters. Optional.
#' @param progress (logical) A bar showing the progress of the function.
#' @return By default, \code{aoh} returns 'aoh' object corresponding to a list with two
#' elements:
#' \itemize{
#'   \item A data.frame detailing if the function was able (1) or not (0) to refinate
#'   the species distribution.
#'   \item A list of 'RasterLayer' or 'SpatialPolygons' object representing the refined
#' distribution of the species. For the 'RasterLayer' the value of 0 indicates the
#' cells where the species were considered as present before the refinement.
#' Values higher than 0 indicates the coverage of cell by the spatial distribution
#' after the species habitat's mapping.
#' }
#' @details The function map the 'area of habitat' (AOH) within the polygon of
#' geographical distribution (SpatialPolygon) representing the 'extent of occurrence'
#' (EOO). This mapping is made considering the specific preference for habitats
#' of a given species.
#' @examples
#'
#' ### Raster Output ###
#'
#' aoh_ras <- aoh(eoo_sp = eoo_birdShp, lc = lc_map, alt = alt_map,
#' alt_pref = alpref_bird, hab_pref = hab_pref_bird, clim_suit = clim_suit_bird,
#' continuous = TRUE, shpOut = FALSE, progress = FALSE)
#'
#' ### Shapefile Output ###
#'
#' aoh_shp <- aoh(eoo_sp = eoo_birdShp, lc = lc_map, alt = alt_map,
#' alt_pref = alpref_bird,hab_pref = hab_pref_bird, clim_suit = clim_suit_bird,
#' resolution = 0.05, shpOut = TRUE, progress = FALSE)
#'
#' @encoding UTF-8
#'
#' @references
#'
#' 1. Rondinini C., Di Marco M., Chiozza F., Santulli G., Baisero D., Visconti P.,
#' and Boitani L. (2011). Global habitat suitability models of terrestrial mammals.
#' Philosophical Transactions of the Royal Society B, 366:2633–2641.
#'
#' 2. Ficetola G.F., Rondinini C., Bonardi A., Baisero D., and Padoa-Schippa E.
#' (2015). Habitat availability for amphibians and extinction threat: a global
#' analysis. Diversity and Distributions, 21(3):302–311.
#'
#' 3. Brooks T.M., Pimm S.L., Akçakaya H.R., Buchanan G.M., Butchart S.H.M, Foden W.,
#' Hilton-Taylor C., Hoffmann M., Jenkins C.N., Joppa L., Li B.V., Menon V.,
#' Ocampo-Peñuela N., and Rondinini C. (2019). Measuring Terrestrial Area of Habitat
#' (AOH) and Its Utility for the IUCN Red List. Trends in Ecology & Evolution,
#' 34(11):977–986.
#'
#' @author Daniel Gonçalves-Souza & Thaís Dória
#' @export aoh
#' @import raster
#' @import rgdal
#' @import rgeos
#' @importFrom utils txtProgressBar setTxtProgressBar

aoh <- function(eoo_sp = NULL, lc = NULL, alt = NULL, alt_pref = NULL,
                hab_pref = NULL, clim_suit = NULL, threshold = 0.5, resolution = NULL,
                continuous = FALSE, shpOut = FALSE, extentOut = NULL,
                progress = FALSE){

  # Data frame of results
  df <- data.frame(matrix(ncol = 4, nrow = length(eoo_sp)))
  names(df) <- c('Species', 'Vegetation', 'Altitude', 'Climatic suitability')
  df[, 1] <- eoo_sp@data[, 2]
  df[, 2:3] <- 1
  df[, 4] <- 0

    # Checklist
  {
    if (missing(eoo_sp))
      stop('eoo_sp is missing')
    if (is.null(lc) & is.null(alt))
      stop('You have to provide at least lc or alt')
    if (is.null(lc))
      df[, 2] <- 0
    if (!is.null(hab_pref) & is.null(lc))
      stop('lc is missing')
    if (is.null(hab_pref) & !is.null(lc))
      stop('hab_pref is missing')
    if (is.null(alt))
      df[, 3] <- 0
    if (!is.null(alt_pref) & is.null(alt))
      stop('alt is missing')
    if (is.null(alt_pref) & !is.null(alt))
      stop('alt_pref is missing')
    if (!is.null(extentOut) & shpOut == TRUE)
      stop('extentOut can only be used when shpOut = FALSE')
    if (continuous == TRUE & shpOut == TRUE)
      stop('shpOut can only be true when continuous = FALSE')
  }

  # Read shapes in directory
  if (is.character(eoo_sp)) {
    eoo_sp <- readShp(eoo_sp)
  }

  # List of maps
  maps_names <- c('lc', 'alt')
  present <- c(!is.null(lc), !is.null(alt))
  maps <- mget(maps_names[present])

  # Looping for refinament
  result <- list()
  if (progress) {
    pb <- txtProgressBar(min = 0, max = length(eoo_sp), style = 3)
  }
  for (i in 1:length(eoo_sp)) {
    maps_eoo <- maps

    if (!is.null(clim_suit)) {
      if (eoo_sp@data[i, 2] %in% names(clim_suit)) {
        clim_suit_eoo <- clim_suit[names(clim_suit) == eoo_sp@data[i, 2]]
        maps_eoo <- c(clim_suit_eoo, maps_eoo)
        names(maps_eoo) <- c('clim_suit', names(maps_eoo)[2:3])
        df[i, 4] <- 1
      }
    }

    maps_eoo <- lapply(maps_eoo, function(x) crop(x, eoo_sp[i, ]))

    # Refinament of lc
    if (any(names(maps_eoo) == 'lc')) {
      sp_hab_pref <- hab_pref[as.character(eoo_sp[i, ]@data[, 2]) == as.character(hab_pref[, 1]),
                            2:ncol(hab_pref)]
      sp_hab_pref <- cbind(as.numeric(names(sp_hab_pref)), t(sp_hab_pref))
      if (nrow(sp_hab_pref) > 0) {
        hab.cat <- as.numeric(colnames(sp_hab_pref)[as.vector(sp_hab_pref[1, ] == 1)])
        hab.ref <- reclassify(maps_eoo[[which(names(maps_eoo) == 'lc')]],
                              sp_hab_pref)
        maps_eoo[[which(names(maps_eoo) == 'lc')]] <- mask(hab.ref, eoo_sp[i, ])
      }
      if (nrow(sp_hab_pref) == 0) {
        hab.ref <- maps_eoo[[which(names(maps_eoo) == 'lc')]] > 0
        maps_eoo[[which(names(maps_eoo) == 'lc')]] <- mask(hab.ref, eoo_sp[i, ])
        df[i, 2] <- 0
      }
    }

    # Refinament of al
    if (any(names(maps_eoo) == 'alt')) {
      sp_alt_pref <- alt_pref[as.character(eoo_sp[i, ]@data[, 2]) == as.character(alt_pref[, 1]), 2:3]
      alt_crop <- maps_eoo[[which(names(maps_eoo) == 'alt')]]
      if (nrow(sp_alt_pref) > 0) {
        if (sum(is.na(sp_alt_pref)) != 2) {
          if (sum(is.na(sp_alt_pref)) == 1) {
            if (is.na(sp_alt_pref[1])) {
              alt.ref <- alt_crop <= sp_alt_pref[1, 2]          }
            if (is.na(sp_alt_pref[2])) {
              alt.ref <- alt_crop >= sp_alt_pref[1, 1]
            }
          }
          if (sum(is.na(sp_alt_pref)) == 0) {
            alt_crop <- alt_crop >= sp_alt_pref[1, 1] & alt_crop <= sp_alt_pref[1, 2]
          }
          maps_eoo[[which(names(maps_eoo) == 'alt')]] <- mask(alt_crop, eoo_sp[i, ])
        }
      }
      if (nrow(sp_alt_pref) == 0){
        df[i, 3] <- 0
      }
    }

    # Overlay
    if(length(maps_eoo) > 1){
      # Resample maps
      res_all <- sapply(maps_eoo, xres)
      maps_eoo_mod <- maps_eoo[which(res_all != max(res_all))]
      temp <- list()
      for(j in 1:length(maps_eoo_mod)){
        temp[[j]] <- resolucao(x = maps_eoo_mod[[j]], y = maps_eoo[[which.max(res_all)]],
                               lbl = names(maps_eoo_mod[j]))
      }
      maps_eoo_mod <- c(maps_eoo[which(res_all == max(res_all))], temp)
      names(maps_eoo_mod) <- NULL

      # Refinament
      ref <- suppressWarnings(do.call(overlay,
                                      c(maps_eoo_mod,
                                        fun =  function(x) Reduce('*', x))))
    }

    if(length(maps_eoo) == 1){
      ref <- maps_eoo[[1]]
    }

    # Continuous
    if(!continuous){
      ref <- ref > threshold
    }

    # Change resolution
    if(!is.null(resolution)){
      if (resolution < xres(ref) & isFALSE(all.equal(resolution, xres(ref)))){
        stop('Chosen resolution is smaller than the maps provided')
      }
      base <- raster()
      extent(base) <- extent(ref)
      res(base) <- resolution
      {
        if(!continuous)
          ref <- resolucao(ref, base, type = 2)
        ref <- ref > threshold
        if(continuous)
          ref <- resolucao(ref, base, type = 1)
      }
    }

    # Shapefile output
    if(shpOut){
      if(minValue(ref) == 0 & maxValue(ref) == 0){
        warning(paste('Cannot return shapefile of', eoo_sp[i, ]@data[, 2],
                      'because there is no cells left after the refinement'))
        ref <- NA
      } else {
        if(class(ref) %in% c('RasterBrick', 'RasterStack')){
          ref <- lapply(as.list(ref),
                        function(x) rasterToPolygons(x,
                                                     fun = function(y) y > 0,
                                                     dissolve = T))
          ref <- do.call(bind, ref)
        } else {
          ref <- rasterToPolygons(ref, fun = function(x) x > 0,
                                  dissolve = T)
          names(ref@data) <- eoo_sp[i, ]@data[, 2]
        }
      }
    }

    if(!shpOut){
      names(ref) <- eoo_sp@data[i, 2]
    }

    result[[i]] <- ref
    if(progress == TRUE){
      setTxtProgressBar(pb, i)
    }
  }

  # extentOut
  if(!is.null(extentOut)){
    r <- raster()
    extent(r) <- extentOut
    res(r) <- res(result[[i]])
    result <- lapply(result, resample, r, method = 'ngb')
  }

  # Gathering data
  names(result) <- eoo_sp@data[, 2]
  result_full <- list(Summary = df, Data = result)
  class(result_full) <- "aoh"
  return(result_full)

}
