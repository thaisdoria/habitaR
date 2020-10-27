#' aohMix: mapping the AOH of species by mixing data from SDM and EOO following Syfert et al. (2014).
#'
#' Based on a mixing approach to combine the geographical species distribution
#' derived from spatial-informed methods and also modelling techniques, mapping the specie's
#' area of habitat (AOH) from a mixing approach
#'
#' mapping the AOH by thresholding distribution models (SDM) based on the EOO geometry.
#'
#'@usage aohMix1 (eooSp, modSp, thresInitial = NULL , thresIncrement = NULL,
#'continuous = TRUE, poly = NULL, cropToPoly = FALSE, progress = TRUE,
#'removeTempFile = TRUE, stack = TRUE )
#'
#'@param eooSp Spatial distribution data of the species representing the
#' original (i.e. not refined) extent of occurrence (EOO). It might correspond to
#' \itemize{
#'   \item path for a folder with spatial distribution files (.shp, .asc or .tif
#'   format)
#'   \item SpatialPolygonDataFrame (see \code{\link[aoh]{readShp}} to obtain such
#' class of object)
#'   \item aHull object created from occurrences records (see \code{\link[aoh]{aHull}}
#'   to obtain such class of object)
#'   \item 'aHull' Raster
#'   \item 'RasterLayer' object
#'   \item 'RasterStack' object
#'   \item 'RasterBrick' object
#'  }
#'@param modSp Species distribution derived from modelling techniques (eg. SDMs).
#'It might correspond to
#'#'\itemize{
#'   \item path for a folder with species distribution models files (.asc or .tif
#'   format)
#'   \item 'RasterLayer' object
#'   \item 'RasterStack' object
#'   \item 'RasterBrick' object
#'   \item 'list' with 'RasterLayer' class of features (see \code{\link[aoh]{readRas}}
#'   to obtain such object).
#'@param thresInitial teste
#'@param thresIncrement teste
#'
#'
#'@examples
#'
#' ### Shapefile as Input ('aHull' class) ###
#'
#' # Binary Output #
#'
#' aohmixS_bin <- aohMix (eooSp = aHull_plantShp, modSp = sdm_plantRas,
#' thresInitial = 0.05, thresIncrement = 0.25, continuous = FALSE, cropToPoly = TRUE,
#' poly = poly, progress = TRUE, stack = TRUE)
#'
#' # Continuous Output #
#'
#' aohmixS_con <- aohMix (eooSp = aHull_plantShp, modSp = sdm_plantRas,
#' thresInitial = 0.05, thresIncrement = 0.25, continuous = TRUE, cropToPoly = TRUE,
#' poly = poly, progress = TRUE, stack = TRUE)
#'
#'
#' ### Raster as Input ###
#'
#' # Binary Output #
#'
#' aohmixR_bin <- aohMix (eooSp = aHull_plantRas, modSp = sdm_plantRas,
#' thresInitial = 0.05, thresIncrement = 0.25, continuous = FALSE, cropToPoly = TRUE,
#' poly = poly, progress = TRUE, stack = TRUE)
#'
#' # Continuous Output #
#'
#' aohmixR_con <- aohMix (eooSp = aHull_plantRas, modSp = sdm_plantRas,
#' thresInitial = 0.05, thresIncrement = 0.25, continuous = TRUE, cropToPoly = TRUE,
#' poly = poly, progress = TRUE, stack = TRUE)
#'
#'@encoding UTF-8
#'
#'@references
#' 1. Syfert M.M., Joppa L.N., Smith M.J, Coomes D., Bachman S.P. & Brummitt N.A. (2014).
#' Using species distribution models to inform IUCN Red List assessments. Biological
#' Conservation. 177.
#'
#' 2. Sangermano F., Eastman J.R. (2012). A GIS framework for the refinement of species
#' geographic ranges. International Journal of Geographical Information Science 26, 39-55.
#'
#' @author Thaís Dória & Daniel Gonçalves-Souza
#' @export aohMix
#' @import raster
#' @import sp


aohMix <- function(eooSp, modSp, thresInitial = NULL , thresIncrement = NULL,
                   continuous = TRUE, poly = NULL, cropToPoly = FALSE,
                   progress = TRUE, removeTempFile = TRUE, stack = TRUE){

  # Checking list and warning messages
  {
    if (missing(eooSp))
      stop("eooSp is missing")
    if (missing(modSp))
      stop("modSp is missing")
    if (is.null(poly) & (cropToPoly == TRUE))
      stop('cropToPoly can only be true when poly is provided')
    }

  # Data.frame of results
  dfres <- data.frame(matrix(ncol = 5, nrow = length(eooSp)))
  names(dfres) <- c('Species', 'Threshold', 'MaxJsi', 'DifSizes', 'Prop')
  spp.nm<-gsub("[.]", " ", names(eooSp))
  dfres[,1] <- spp.nm

  # Creating the threshold (spectrum of values)
  threshold <- seq(thresInitial,1,thresIncrement)

  # Pre-processing of data: converting 'eooSp' input data into a 'Raster'
  {
  # 1. Input as a path for folders
    if (is.character(eooSp)) {
    eooSp <- readShp(eooSp)
  }
  # 2. Input as 'SpatialPolygons' or 'SpatialPolygonsDataFrame' family of class
    # Rasterize them
    if(class(eooSp) == "aHull" | class(eooSp) == "SpatialPolygonsDataFrame"
       | class(eooSp) == "SpatialPolygons"){
    eooSp <- mapply(rasterize, aHull_plantShp, MoreArgs =
                      list(sdm_plantRas[[1]], background = 0, mask=FALSE))
    for (i in 1:length(eooSp)){
      eooSp[[i]][eooSp[[i]] > 1] <- 1
    }
  }
  }

  # 3. Input data as 'Raster' family of classes
    if((class(eooSp) == "list" & class(eooSp[[1]]) == "RasterLayer")
       | class(eooSp) == "RasterLayer" | class(eooSp) == "RasterStack"){

  # Looping analysis to each feature from a list of rasters, a 'RasterStack', or a 'RasterBrick':
    # Enabling the progress bar
    if(progress == TRUE){
    pb <- txtProgressBar(min = 0, max = length(sp.names), style = 3)
      }

    list.r <- list()
    for (j in 1:length(eooSp)){
    rasEoo<-eooSp[[j]] #eoos (e.g. alpha hull)
    rasMod<-modSp[[j]] #models (e.g. sdms)
    # Align the extent to compare both set of distribution maps

    if((extent(rasMod[[1]]) == extent(rasEoo[[1]])) == FALSE){
        rasMod<-setExtent(rasMod, extent(rasEoo), keepres=FALSE, snap=FALSE)}

    if((extent(rasMod[[1]]) == extent(rasEoo[[1]])) == TRUE){
       # Get the values above the threshold
       nt<-length(threshold)
       l<-list()
       # Looping to get all maps corresponding to each value from threshold spectrum of probabilities
       for (i in 1:nt){
       ras2 <- rasMod >= threshold[i]
       l[i] <- raster::stack(ras2)
        }
    rasMod.bin <- raster::stack(l) # set of binary models

    # Get the intersection of two rasters (considering the stack with all maps derived
    # from the spectrum of threshold values):
    comb <- raster::stack(rasEoo + rasMod.bin) # comb == combining two rasters (2 indicates intersection)
    both <- raster::stack(comb == 2) # both == intersection (value 2) between both maps
    # Get the union (all the area covered by both rasters)
    all <- raster::stack(comb >= 1) # all == total area covered by both maps

      # JACCARD SIMILARITY INDEX (JSI)
      jsi<-cellStats(both, sum)/cellStats(all,sum)
      jsi[is.nan(jsi)] <- 0

      # Summary of Results (jsi for all threshold spectrum)
      dfjac <- data.frame(matrix(ncol = 2, nrow = nt))
      names(dfjac) <- c('Threshold', 'Jsi')
      rownames(dfjac)<-names(jsi)
      dfjac[,1]<- threshold
      dfjac[,2]<- jsi # one of the outputs (species-specific)

      # Getting the maxThreshold and maxJSI
      dfjacord <-dfjac[order(dfjac$Jsi),]

      # Generating aohMixed model based on maximal similarity through comparision of eoo and mods
      layermax<-rownames(dfjacord[(length(threshold)),])
      aohMax.bin<-subset(rasMod.bin,layermax) # extracting only the layer corresponding to the max threshold
      aohMax.bin[aohMax.bin == 0]<-NA


      ### Measures of performance/evaluation
      # DIF SIZES (difference, in area - number of cells - between the modeljaccard and eoo)
       difSize<-cellStats(aohMax.bin, sum)/cellStats(rasEoo,sum)
       plot(rasEoo) # ploting eoo original
       comb.eval <- aohMax.bin + rasEoo
       plot(comb.eval) # ploting union between eoo and modeljaccard (all the area covered by the both rasters)
       both.eval <- comb.eval == 2
       plot(both.eval) # ploting only the intersection between both maps

       # PROP (proportion of EOO that was maintained in the resulting modeljaccard)
       prop <- cellStats(both.eval, sum) / cellStats(rasEoo, sum)

    # SUMMARIZING THE RESULTS OF MODEL JACCARD
    dfres[j,2] <- dfjacord[length(threshold),1] # thresholdMAX
    dfres[j,3] <- dfjacord[length(threshold),2] # maxJSI
    dfres[j,4] <- difSize
    dfres[j,5] <- prop

    # Removing temp files and displaying progress bar
    if (removeTempFile == TRUE) {
      removeTmpFiles(h=0.0)
    } # default

    # OUTPUT FORMAT OF MAPS

    # Raster Continuos
    if (continuous){
      aohMix<-mask(rasMod, aohMax.bin) # crop the continuous model based on geometry from aohMax.bin
      aohMix[is.na(aohMix[])]<- 0
          }
    else{
      aohMix<-aohMax.bin
      aohMix[is.na(aohMix[])]<- 0
              }

    if (cropToPoly == TRUE){
        aohMix<-crop(mask(aohMix, poly),poly)
        aohMix<-extend(aohMix, c(10,10))
                 }
    if(progress == TRUE){
      setTxtProgressBar(pb, i)

    list.r[[j]]<-aohMix
    }
    }

    }

   names(list.r)<-spp.nm

   # FINAL OUTPUT
   if(stack){
     list.r <- raster::stack(list.r)
     aohMix.Res <- list(SpecificMeasures = dfres, aohMix = list.r)
     return(aohMix.Res)
   }
   else{
     aohMix.Res <- list(SpecificMeasures = dfres, aohMix = list.r)
     return(aohMix.Res)
   }
   }
}

