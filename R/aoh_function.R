#'  AOH function
#'
#' Provide the area of habitat (AOH) of a given species through refinement of its
#' known geographic distribution
#' @usage aoh(eoo, lc.rec, matrix.hab.pref, alt.map = NULL, matrix.alt.pref = NULL,
#' shp.out = FALSE, resolution = NULL, continuous = FALSE, threshold = 0.5,
#' extent.out = NULL, progress = FALSE)
#' @param eoo SpatialPolygons of the spatial distribution of the species or path
#'  for a folder with spatial distribution shapefiles (ESRI shapefile format) .
#'  The name of the species must be on the second column of the attribute table
#'  of the shapefile.
#' @param lc.rec RasterLayer object of the land use map reclassified for the
#' categories of habitat
#' @param matrix.hab.pref Data frame 0/1 of habitat preference of the species.
#'  First column must be the species name. The posterior columns must be named
#'  after the categories of habitat as following the lc.rec classification.
#'  \code{\link[habitaR]{prefHab}} can help obtain this data.
#' @param alt.map RasterLayer object of the elevation map. Optional.
#' @param matrix.alt.pref Data frame with altitudinal range of species. First
#' column must be the species name, second column the minimum value of altitude
#' and the third column the maximum value of altitude
#' @param shp.out (logical) Whether the output should be a shapefile as opposed
#' to a raster.
#' @param resolution numeric value indicating the preferred resolution for the
#' rasters. Resolution must coarser than the resolution of lc.rec and alt.map.
#' Only used if alt.map provided.
#' @param continuous (logical) Whether the output should be binary or continuous
#' for the rasters. Only used if resolution provided.
#' @param threshold numeric value indicating the threshold of the cell coverage
#' by the species to the species be considered present. Only used if continuous = FALSE.
#' @param extent.out Extent object or a vector of four numbers indicating the
#' preferred output for the rasters. Optional.
#' @param progress (logical) a bar showing the progress of the function.
#'
#' @import raster
#' @return \code{aoh} returns a list with two elements. The first element is a
#' data.frame detailing if the function was able (1) or not (0) to refinate the
#' species distribution. The second element is a list of RasterLayer or
#' SpatialPolygons object representing the refinated distribution of the species.
#' For the RasterLayer the value of 0 indicates the cells where the the species
#' were considerated present before the refinement. Values higher than 0 indicates
#' the coverage of cell by the spatial distribution after the refinement.
#' @details The function map the area of habitat within the geographical
#' distribution (SpatialPolygon) given as the input data. as the refined
#' distribution of a given species. This refinement is made considering the
#' specific preference for habitats of a given species.
#' @examples
#' ref_data <- aoh(eoo = sd_amph, lc.rec = lc , matrix.hab.pref = habpref,
#' alt.map = al, matrix.alt.pref = alpref)
#' @author Daniel Gonçalves-Souza & Thaís Dória
#' @export aoh
#' @import rgdal
#' @import rgeos
#' @importFrom utils txtProgressBar setTxtProgressBar


aoh <- function(eoo, lc.rec, matrix.hab.pref, alt.map = NULL,
                matrix.alt.pref = NULL, shp.out = FALSE, resolution = NULL,
                continuous = FALSE, threshold = 0.5, extent.out = NULL,
                progress = FALSE){

  if(substr(eoo, nchar(eoo), nchar(eoo)) == '/'){
    eoo <- substr(eoo, 1, nchar(eoo) - 1)
  }

  if(is.character(eoo)){
    files.sp <- list.files(eoo, pattern = ".shp$")
    files.sp <- gsub(".shp","", files.sp)
    sps <- list()
    for (i in 1:length(files.sp)){
      sps[[i]] <- readOGR(dsn = eoo,
                          layer = files.sp[i])
    }

    if(length(sps) > 1){
      eoo <- do.call(bind, sps)
    }
    if(length(sps) == 1){
      eoo <- sps[[1]]
    }
  }

  #Summary data frame
  df <- data.frame(matrix(ncol = 3, nrow = length(eoo)))
  names(df) <- c('Species', 'Vegetation', 'Altitude')
  df[, 1] <- eoo@data[, 2]
  df[, 2:3] <- 1

  if (is.null(alt.map)) {
    warning('No altitude map was provided, therefore the refined shapes
                will be based on vegetation preference only')
    df[, 3] <- 0
  }

  # Looping para sd
  if(progress == TRUE){
    pb <- txtProgressBar(min = 0, max = length(eoo), style = 3)
  }
  result <- list()
  for (i in 1:length(eoo)){
    sd <- eoo[i, ]
    sd <- crop(sd, lc.rec)
    lc.crop <- crop(lc.rec, sd)
    lc.crop <- mask(lc.crop, sd)
    sp.habpref <- matrix.hab.pref[as.character(sd@data[, 2]) == as.character(matrix.hab.pref[, 1]),
                                  2:ncol(matrix.hab.pref)]
    if (nrow(sp.habpref) > 0){
      hab.cat <- as.numeric(colnames(sp.habpref)[as.vector(sp.habpref[1, ] == 1)])
      hab.ref <- lc.crop %in% hab.cat
      hab.ref <- mask(hab.ref, sd)
    }
    if (nrow(sp.habpref) == 0){
      hab.ref <- lc.crop > 0
      hab.ref <- mask(hab.ref, sd)
    }
    if (is.null(alt.map) == FALSE){
      if(is.null(matrix.alt.pref)){
        stop('No matrix of altitude preference provided')
      }
      alt.crop <- crop(alt.map, sd)
      alt.crop <- mask(alt.crop, sd)
      sp.altpref <- matrix.alt.pref[as.character(sd@data[, 2]) == as.character(matrix.alt.pref[, 1]), 2:3]
      if (nrow(sp.altpref) > 0){
        if(sp.altpref[, 2] != 0 & sum(is.na(sp.altpref[1, ])) == 0){
          alt.ref <- alt.crop >= sp.altpref[1, 1] & alt.crop <= sp.altpref[1, 2]
          alt.ref <- crop(alt.ref, sd)
          alt.ref <- mask(alt.ref, sd)
          # In case of different resolutions (not defined)
          if(res(lc.rec)[1] > res(alt.map)[1]){
            #factor <- round(res(lc.rec)[1] / res(alt.map)[1])
            #alt.ref <- aggregate(alt.ref, fact = factor, fun = mean)
            alt.ref <- resample(alt.ref, hab.ref)
            new.res <- res(lc.rec)[1]
          }
          if(res(lc.rec)[1] <= res(alt.map)[1]){
            #factor <- round(res(alt.map)[1] / res(lc.rec)[1])
            #hab.ref <- aggregate(hab.ref, fact = factor, fun = modal)
            hab.ref <- resample(hab.ref, alt.ref, method = 'ngb')
            new.res <- res(alt.ref)[1]
          }
          # Overlay refinement by altitude and by lc
          over <- overlay(hab.ref, alt.ref, fun = function(x, y) x * y)
          # Custom resolution
          if (is.null(resolution) == FALSE) {
            r <- raster()
            extent(r) <- extent(over)
            res(r) <- resolution
            if (resolution > (new.res)[1]){
              factor <- round(resolution / res(over)[1])
              over <- aggregate(over, fact = factor, fun = sum)
              over <- over / (factor^2)
              if (continuous == FALSE){
                over <- over >= threshold
                over <- resample(over, r, method = 'ngb')
              }
              if (continuous == TRUE){
                over <- resample(over, r)
              }
            }
            if (resolution < new.res){
              stop('Chosen resulution is smaller than the maps provided')
            }
          }
          if(shp.out == TRUE) {result[[i]] <- rasterToPolygons(over,
                                                               fun = function(x) x > 0,
                                                               dissolve = T)}
          if(shp.out == FALSE){
            result[[i]] <- over
          }
        }
        }
    }
    if (nrow(sp.habpref) == 0 & nrow(sp.altpref) != 0){
      df[i, 2] <- 0
      warning(paste('No vegetation preference found for',
                    as.character(sd@data[, 2]),
                    'therefore the refined shape is based on altitude
                    preference only'))
    }
    if (nrow(sp.habpref) == 0 & nrow(sp.altpref) == 0){
      df[i, 2:3] <- 0
      warning(paste('No vegetation or altitude preference found for',
                    as.character(sd@data[, 2]),
                    'therefore, the shape was not refined'))
    }

    if (nrow(sp.habpref) == 0 & nrow(sp.altpref) != 0){
      if(sp.altpref[, 2] == 0 | sum(is.na(sp.altpref[1, ])) == 0){
      df[i, 2:3] <- 0
      warning(paste('No vegetation or altitude preference found for',
                    as.character(sd@data[, 2]),
                    'therefore, the shape was not refined'))
      }
    }

    if(nrow(sp.habpref) > 0 & nrow(sp.altpref) == 0){
      df[i, 3] <- 0
      warning(paste('No altitude preference found for',
                    as.character(sd@data[, 2]),
                    'therefore, the shape was refined based only on
                      the land cover'))
    }

    if(nrow(sp.habpref) > 0 & length(sp.altpref[, 2]) != 0){
      if(sp.altpref[, 2] == 0 | sum(is.na(sp.altpref[1, ])) == 0){
      df[i, 3] <- 0
      warning(paste('No altitude preference found for',
                    as.character(sd@data[, 2]),
                    'therefore, the shape was refined based only on
                      the land cover'))
      }
    }
    if (is.null(alt.map) | nrow(sp.altpref) == 0) {
      # Custom resolution
      if (is.null(resolution) == FALSE) {
        r <- raster()
        extent(r) <- extent(hab.ref)
        res(r) <- resolution
        if (resolution > new.res){
          factor <- round((resolution / res(hab.ref)[1]))
          hab.ref <- aggregate(hab.ref, fact = factor, fun = sum)
          hab.ref <- hab.ref / (factor^2)
          if (continuous == FALSE){
            hab.ref <- hab.ref >= threshold
          }
          hab.ref <- resample(hab.ref, r, method ='ngb')
        }
        if (resolution < new.res){
          stop('Chosen resulution is smaller than the maps provided')
        }
      }
      if(shp.out == TRUE){
        result[[i]] <- rasterToPolygons(hab.ref, fun = function(x) x > 0,
                                        dissolve = T)
      }
      if(shp.out == FALSE){
        result[[i]] <- hab.ref
      }
    }
    if(progress == TRUE){
      setTxtProgressBar(pb, i)
    }
  }

  if(is.null(extent.out) == FALSE){
    r <- raster()
    extent(r) <- extent.out
    res(r) <- res(hab.ref)

    if(continuous == FALSE){
      result <- lapply(result, resample, r, method = 'ngb')
    }
    if(continuous == TRUE){
      result <- lapply(result, resample, r)
    }
  }
  names(result) <- eoo@data[, 2]
  result.full <- list(Summary = df, Data = result)
  return(result.full)
}
