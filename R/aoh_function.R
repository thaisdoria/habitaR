#'  AOH function
#'
#' Provide the area of habitat (AOH) of a given species through refinement of its known geographic distribution
#'
#' @param sd Spatial distribution shapefile (ESRI shapefile format). The name of the species must be on the second column of the attribute table of the shapefile.
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


aoh <- function(shapes, lc.rec, matrix.hab.pref, alt.map = NULL,
                matrix.alt.pref, shp.out = FALSE, resolution = NULL,
                continuous = FALSE, threshold = 0.5){
  
  if(is.character(shapes)){
    files.sp <- list.files(shapes, pattern = ".shp$")
    files.sp <- gsub(".shp","", files.sp)
    sps <- list()
    for (i in 1:length(files.sp)){
      sps[[i]] <- readOGR(dsn = shapes,
                          layer = files.sp[i])
    }

    shapes <- do.call(bind, sps)
  }

  # Looping para sd
  pb <- txtProgressBar(min = 0, max = length(shapes), style = 3)
  for (i in 1:length(shapes)){
    sd <- shapes[i,]
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
      hab.ref <- lc.crop
    }
    if (is.null(alt.map) == FALSE){
      alt.crop <- crop(alt.map, sd)
      alt.crop <- mask(alt.crop, sd)
      sp.altpref <- matrix.alt.pref[as.character(sd@data[, 2]) == as.character(matrix.alt.pref[, 1]), 2:3]
      if (nrow(sp.altpref) > 0){
        alt.ref <- alt.crop >= sp.altpref[1, 1] & alt.crop <= sp.altpref[1, 2]
        alt.ref <- crop(alt.ref, sd)
        alt.ref <- mask(alt.ref, sd)
        # In case of different resolutions (not defined)
        if(res(lc.rec)[1] > res(alt.map)[1]){
          alt.ref <- resample(alt.ref, hab.ref, method = 'ngb')
          new.res <- res(lc.rec)[1]
        }
        if(res(lc.rec)[1] <= res(alt.map)[1]){
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
            factor <- round((resolution / res(over)[1]))
            over <- aggregate(over, fact = factor, fun = sum)
            over <- over / (factor^2)
            if (continuous == FALSE){
              over <- over >= threshold
            }
            over <- resample(over, r, method = 'ngb')
          }
          if (resolution < new.res){
            over <- disaggregate(over, fact = (res(over)[1] / resolution))
            over <- resample(over, r, method = 'ngb')
          }
        }
        result <- list()
        if(shp.out == TRUE) {result[[i]] <- rasterToPolygons(over,
                                                             fun = function(x) x > 0,
                                                             dissolve = T)}
        if(shp.out == FALSE){
          result[[i]] <- over
        }
      }
    }
    if (nrow(sp.habpref) == 0 & nrow(sp.altpref) != 0){
      warning(paste('No vegetation preference found for',
                    as.character(sd@data[, 2]),
                    'therefore the refined shape is based on altitude
                    preference only'))
    }
    if (nrow(sp.habpref) == 0 & nrow(sp.altpref) == 0){
      warning(paste('No vegetation or altitude preference found for',
                    as.character(sd@data[, 2]),
                    'therefore, the shape was not refined'))
    }
    if (is.null(alt.map) | nrow(sp.altpref) == 0) {
      if(is.null(alt.map)){
        warning('No altitude map was provided, therefore the refined shape
                is based on vegetation preference only')
      }
      if(nrow(sp.habpref) > 0 & nrow(sp.altpref) == 0){
        warning(paste('No altitude preference found for',
                      as.character(sd@data[, 2]),
                      'therefore, the shape was refined based only on
                      the land cover'))
      }
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
          hab.ref <- disaggregate(hab.ref, fact = (res(hab.ref)[1] / resolution))
          hab.ref <- resample(hab.ref, r, method = 'ngb')
        }
      }
      result <- list()
      if(shp.out == TRUE){
        result[[i]] <- rasterToPolygons(hab.ref, fun = function(x) x > 0,
                                        dissolve = T)
      }
      if(shp.out == FALSE){
        result[[i]] <- hab.ref
      }
    }
    setTxtProgressBar(pb, i)
  }
  names(result) <- shapes@data[, 2]
  return(result)
}
