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
#' categories of habitat.
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
#' @param resolution Numeric value indicating the preferred resolution for the
#' rasters. Resolution must coarser than the resolution of lc.rec and alt.map.
#' @param continuous (logical) Whether the output should be binary or continuous
#' for the rasters. Only used if resolution provided or when the resolution of
#' lc.rec and alt.map are different.
#' @param threshold Numeric value indicating the threshold of the cell coverage
#' by the species to the species be considered present (values between 0 and 1).
#' Only used if continuous = \code{FALSE} and a coarser resolution is provided or
#' the resolution of lc.rec and alt.map are different or shp.out is
#' \code{TRUE}.
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
  {
    if (missing(eoo))
      stop("eoo is missing")
    if (missing(lc.rec))
      stop("lc.rec is missing")
    if (missing(matrix.hab.pref))
      stop("matrix.hab.pref is missing")
    if(!is.null(extent.out) & shp.out == TRUE)
      stop('extent.out can be only used when shp.out = FALSE')
    if(continuous == TRUE & shp.out == TRUE)
      stop('shp.out can be only be true when continuous = FALSE')
    if(!is.null(alt.map))
    if(compareCRS(alt.map, lc.rec) == FALSE)
      warning('CRS from lc.rec e alt.map are different')
    if(!is.null(matrix.alt.pref) & is.null(alt.map))
      stop('alt.map is missing')
    if(is.null(matrix.alt.pref) & !is.null(alt.map))
      stop('matrix.alt.pref is missing')
    if(is.null(matrix.alt.pref))
      sp.altpref <- matrix(nrow = 0, ncol = 1)
  }

  if(is.character(eoo)){
    if(substr(eoo, nchar(eoo), nchar(eoo)) == '/'){
      eoo <- substr(eoo, 1, nchar(eoo) - 1)
    }
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
      df[i, 2] <- 0
    }
    names(hab.ref) <- sd@data[, 2]
    # Refinamento de altitude
    if (!is.null(alt.map)){
      if(is.null(matrix.alt.pref)){
        stop('No matrix of altitude preference provided')
      }
      alt.crop <- crop(alt.map, sd)
      alt.crop <- mask(alt.crop, sd)

      if(is.factor(matrix.alt.pref[, 2])){
        matrix.alt.pref[, 2] <- as.numeric(levels(matrix.alt.pref[, 2]))[matrix.alt.pref[, 2]]
      }
      if(is.factor(matrix.alt.pref[, 3])){
        matrix.alt.pref[, 3] <- as.numeric(levels(matrix.alt.pref[, 3]))[matrix.alt.pref[, 3]]
      }
      if(is.character(matrix.alt.pref[, 2])){
        matrix.alt.pref[, 2] <- as.numeric(matrix.alt.pref[, 2])
      }
      if(is.character(matrix.alt.pref[, 3])){
        matrix.alt.pref[, 3] <- as.numeric(matrix.alt.pref[, 3])
      }

      if(sum(matrix.alt.pref[, 3] < matrix.alt.pref[, 2], na.rm = T) > 0){
        stop(paste('Some maximum values are smaller than minimum in the matrix.alt.pref'))
      }

      sp.altpref <- matrix.alt.pref[as.character(sd@data[, 2]) == as.character(matrix.alt.pref[, 1]), 2:3]
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
            alt.ref <- alt.crop >= sp.altpref[1, 1] & alt.crop <= sp.altpref[1, 2]
          }

          alt.ref <- crop(alt.ref, sd)
          alt.ref <- mask(alt.ref, sd)
          # In case of different resolutions (not defined)
          if(res(lc.rec)[1] > res(alt.map)[1]){
            factor <- round(res(lc.rec)[1] / res(alt.map)[1])
            alt.ref <- aggregate(alt.ref, fact = factor, fun = sum)
            alt.ref <- alt.ref / (factor^2)
            alt.ref <- resample(alt.ref, hab.ref)
          }
          if(res(lc.rec)[1] <= res(alt.map)[1]){
            factor <- round(res(alt.map)[1] / res(lc.rec)[1])
            hab.ref <- aggregate(hab.ref, fact = factor, fun = sum)
            hab.ref <- hab.ref / (factor^2)
            hab.ref <- resample(hab.ref, alt.ref)
          }
          # Overlay refinement by altitude and by land cover
          over <- overlay(hab.ref, alt.ref, fun = function(x, y) x * y)
          names(over) <- sd@data[, 2]

          # Custom resolution
          if (!is.null(resolution)) {
            r <- raster()
            extent(r) <- extent(over)
            res(r) <- resolution
            if (resolution < res(over)[1]){
              stop('Chosen resulution is smaller than the maps provided')
            }
            if (resolution > res(over)[1]){
              factor <- round(resolution / res(over)[1])
              over <- aggregate(over, fact = factor, fun = sum)
              over <- over / (factor^2)
              over <- resample(over, r)
            }
            if (continuous == FALSE){
              over <- over >= threshold
            }

          }

          if(shp.out == TRUE) {
            if(sum(values(over > 0), na.rm = T) == 0){
              warning(paste('Cannot return shapefile of', sd@data[, 2],
                            'because there is no cells left after the refinement'))
              result[[i]] <- NULL
            }
            if(sum(values(over > 0), na.rm = T) > 0){
              over <- over >= threshold
              result[[i]] <- rasterToPolygons(over, fun = function(x) x > 0,
                                              dissolve = T)
            }
          }
          if(shp.out == FALSE){
            if(continuous == FALSE){
              over <- over >= threshold
            }
            result[[i]] <- over
          }
        }
      }
    }

    if (is.null(alt.map) | nrow(sp.altpref) == 0 | sum(is.na(sp.altpref)) == 2) {
      df[i, 3] <- 0
      # Custom resolution
      if (!is.null(resolution)) {
        r <- raster()
        extent(r) <- extent(hab.ref)
        res(r) <- resolution
        if (resolution < res(hab.ref)[1]){
          stop('Chosen resulution is smaller than the maps provided')
        }
        if (resolution > res(hab.ref)[1]){
          factor <- round((resolution / res(hab.ref)[1]))
          hab.ref <- aggregate(hab.ref, fact = factor, fun = sum)
          hab.ref <- hab.ref / (factor^2)
          hab.ref <- resample(hab.ref, r)
          if (continuous == FALSE){
            hab.ref <- hab.ref >= threshold
          }
        }
      }
      if(shp.out == TRUE) {
        if(sum(values(hab.ref > 0), na.rm = T) == 0){
          warning(paste('Cannot return shapefile of', sd@data[, 2],
                        'because there is no cells left after the refinement'))
          result[[i]] <- NULL
        }
        if(sum(values(hab.ref > 0), na.rm = T) > 0){
          result[[i]] <- rasterToPolygons(hab.ref, fun = function(x) x > 0,
                                          dissolve = T)
        }
      }
      if(shp.out == FALSE){
        result[[i]] <- hab.ref
      }
    }
    if(progress == TRUE){
      setTxtProgressBar(pb, i)
    }
  }

  if(!is.null(extent.out)){
    r <- raster()
    extent(r) <- extent.out
    res(r) <- res(result[[i]])

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
