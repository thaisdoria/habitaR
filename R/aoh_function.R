#'  AOH function
#'
#' Provide the area of habitat (AOH) of a given species through refinement of its
#' known geographic distribution
#' @usage aoh(eoo.sp = NULL, lc = NULL, alt = NULL, altpref = NULL,
#' habpref = NULL, adq = NULL, threshold = 0.5, resolution = NULL,
#' continuous = FALSE, shp.out = FALSE, extent.out = NULL,
#' progress = FALSE)
#' @param eoo.sp SpatialPolygons of the spatial distribution of the species or path
#'  for a folder with spatial distribution shapefiles (ESRI shapefile format) .
#'  The name of the species must be on the second column of the attribute table
#'  of the shapefile.
#' @param lc RasterLayer, RasterBrick or RasterStack object of the land use map reclassified for the
#' categories of habitat.
#' @param habpref Data frame 0/1 of habitat preference of the species.
#'  First column must be the species name. The posterior columns must be named
#'  after the categories of habitat as following the lc classification.
#'  \code{\link[habitaR]{prefHab}} can help obtain this data.
#' @param alt RasterLayer object of the elevation map. Optional.
#' @param altpref Data frame with altitudinal range of species. First
#' column must be the species name, second column the minimum value of altitude
#' and the third column the maximum value of altitude
#' @param adq List containing RasterLayers objects of the climatic adequability
#' of the species. The itens of the list must be named after their
#' corresponding species. Optional.
#' @param shp.out (logical) Whether the output should be a shapefile as opposed
#' to a raster.
#' @param resolution Numeric value indicating the preferred resolution for the
#' rasters. Resolution must coarser than the resolution of lc and alt.map.
#' @param continuous (logical) Whether the output should be binary or continuous
#' for the rasters. Only used if resolution provided or when the resolution of
#' lc and alt.map are different.
#' @param threshold Numeric value indicating the threshold of the cell coverage
#' by the species to the species be considered present (values between 0 and 1).
#' Only used if continuous = \code{FALSE} and a coarser resolution is provided or
#' the resolution of lc and alt.map are different or shp.out is
#' \code{TRUE}.
#' @param extent.out Extent object or a vector of four numbers indicating the
#' preferred output for the rasters. Optional.
#' @param progress (logical) A bar showing the progress of the function.
#'
#' @import raster
#' @return \code{aoh} returns a list with two elements. The first element is a
#' data.frame detailing if the function was able (1) or not (0) to refinate the
#' species distribution. The second element is a list of 'RasterLayer' or
#' 'SpatialPolygons' object representing the refined distribution of the species.
#' For the 'RasterLayer' the value of 0 indicates the cells where the species
#' were considered as present before the refinement. Values higher than 0 indicates
#' the coverage of cell by the spatial distribution after the species habitat's
#' mapping.
#' @details The function map the 'area of habitat' (AOH) within the polygon of
#' geographical distribution (SpatialPolygon) representing the 'extent of occurrence'
#' (EOO). This mapping is made considering the specific preference for habitats
#' of a given species.
#' @examples
#' ref_data <- aoh(eoo.sp = eoo_amphShp, lc = lc , habpref = habpref,
#' alt.map = al, altpref = alpref)
#' @encoding UTF-8
#' @author Daniel Gonçalves-Souza & Thaís Dória
#' @export aoh
#' @import rgdal
#' @import rgeos
#' @importFrom utils txtProgressBar setTxtProgressBar

aoh <- function(eoo.sp = NULL, lc = NULL, alt = NULL, altpref = NULL,
                habpref = NULL, adq = NULL, threshold = 0.5, resolution = NULL,
                continuous = FALSE, shp.out = FALSE, extent.out = NULL,
                progress = FALSE){

  # Data frame of resuts
  df <- data.frame(matrix(ncol = 3, nrow = length(eoo.sp)))
  names(df) <- c('Species', 'Vegetation', 'Altitude')
  df[, 1] <- eoo.sp@data[, 2]
  df[, 2:3] <- 1

  # Checklist
  {
    if (missing(eoo.sp))
      stop("eoo.sp is missing")
    if(is.null(lc) & is.null(alt))
      stop('You have to provide at least lc or alt')
    if(is.null(lc))
      df[, 2] <- 0
    if(!is.null(habpref) & is.null(lc))
      stop('lc is missing')
    if(is.null(habpref) & !is.null(lc))
      stop('habpref is missing')
    if(is.null(alt))
      df[, 3] <- 0
    if(!is.null(altpref) & is.null(alt))
      stop('alt is missing')
    if(is.null(altpref) & !is.null(alt))
      stop('altpref is missing')
    if(!is.null(extent.out) & shp.out == TRUE)
      stop('extent.out can be only used when shp.out = FALSE')
    if(continuous == TRUE & shp.out == TRUE)
      stop('shp.out can be only be true when continuous = FALSE')
  }

  # Read shapes in directory
  if(is.character(eoo.sp)){
    eoo.sp <- readShp(eoo.sp)
  }

  # List of maps
  maps.names <- c('adq', 'lc', 'alt')
  present <- c(!is.null(adq), !is.null(lc), !is.null(alt))
  maps <- mget(maps.names[present])

  if(diff.crs(maps)){
    warning('CRS of the maps are different')
  }

  # Looping for refinament
  result <- list()
  if(progress){
    pb <- txtProgressBar(min = 0, max = length(eoo.sp), style = 3)
  }
  for(i in 1:length(eoo.sp)){
    maps.eoo <- lapply(maps, function(x) crop(x, eoo.sp[i, ]))

    # Refinament of lc
    if(any(names(maps.eoo) == 'lc')){
      sp.habpref <- habpref[as.character(eoo.sp[i, ]@data[, 2]) == as.character(habpref[, 1]),
                            2:ncol(habpref)]
      sp.habpref <- cbind(as.numeric(names(sp.habpref)), t(sp.habpref))
      if (nrow(sp.habpref) > 0){
        hab.cat <- as.numeric(colnames(sp.habpref)[as.vector(sp.habpref[1, ] == 1)])
        hab.ref <- reclassify(maps.eoo[[which(names(maps.eoo) == 'lc')]],
                              sp.habpref)
        maps.eoo[[which(names(maps.eoo) == 'lc')]] <- mask(hab.ref, eoo.sp[i, ])
      }
      if (nrow(sp.habpref) == 0){
        hab.ref <- maps.eoo[[which(names(maps.eoo) == 'lc')]] > 0
        maps.eoo[[which(names(maps.eoo) == 'lc')]] <- mask(hab.ref, eoo.sp[i, ])
        df[i, 2] <- 0
      }
    }

    # Refinament of al
    if(any(names(maps.eoo) == 'alt')){
      sp.altpref <- altpref[as.character(eoo.sp[i, ]@data[, 2]) == as.character(altpref[, 1]), 2:3]
      alt.crop <- maps.eoo[[which(names(maps.eoo) == 'alt')]]
      if (nrow(sp.altpref) > 0){
        if(sum(is.na(sp.altpref)) != 2){
          if(sum(is.na(sp.altpref)) == 1){
            if(is.na(sp.altpref[1])){
              alt.ref <- alt.crop <= sp.altpref[1, 2]          }
            if(is.na(sp.altpref[2])){
              alt.ref <- alt.crop >= sp.altpref[1, 1]
            }
          }
          if(sum(is.na(sp.altpref)) == 0){
            alt.crop <- alt.crop >= sp.altpref[1, 1] & alt.crop <= sp.altpref[1, 2]
          }
          maps.eoo[[which(names(maps.eoo) == 'alt')]] <- mask(alt.crop, eoo.sp[i, ])
        }
      }
      if (nrow(sp.altpref) == 0){
        df[i, 3] <- 0
      }
    }

    # Overlay
    if(length(maps.eoo) > 1){
      # Resample maps
      res.all <- sapply(maps.eoo, xres)
      maps.eoo.mod <- maps.eoo[which(res.all != max(res.all))]
      maps.eoo.mod <- lapply(maps.eoo.mod, resolucao,
                             y = maps.eoo[[which.max(res.all)]])
      maps.eoo.mod <- c(maps.eoo[which(res.all == max(res.all))], maps.eoo.mod)
      names(maps.eoo.mod) <- NULL

      # Refinament
      ref <- suppressWarnings(do.call(overlay,
                                      c(maps.eoo.mod,
                                        fun =  function(x) Reduce('*', x))))
      }

    if(length(maps.eoo) == 1){
      ref <- maps.eoo[[1]]
    }

    # Continuous
    if(!continuous){
      ref <- ref > threshold
    }

    # Change resolution
    if(!is.null(resolution)){
      base <- raster()
      extent(base) <- extent(ref)
      res(base) <- resolution
      {
      if(!continuous)
        ref <- resolucao(ref, base, mode = 2)
        ref <- ref > threshold
        if(continuous)
        ref <- resolucao(ref, base, mode = 1)
      }
    }

    # Shapefile output
    if(shp.out){
      if(minValue(ref) == 0 & maxValue(ref) == 0){
        warning(paste('Cannot return shapefile of', eoo.sp[i, ]@data[, 2],
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
          names(ref@data) <- eoo.sp[i, ]@data[, 2]
        }
      }
    }

    if(!shp.out){
      names(ref) <- eoo.sp@data[i, 2]
    }

    result[[i]] <- ref
    if(progress == TRUE){
      setTxtProgressBar(pb, i)
    }
  }

  # Extent.out
  if(!is.null(extent.out)){
    r <- raster()
    extent(r) <- extent.out
    res(r) <- res(result[[i]])
    result <- lapply(result, resample, r, method = 'ngb')
  }

  # Gathering data
  result.full <- list(Summary = df, Data = result)
  class(result.full) <- "aoh"
  return(result.full)

}
