#' aohMixS: mapping the species' AOH by mixing data from EOO and species'
#' distribution models following Syfert's method.
#'
#' Through a mix approach combining the species' spatial-informed distribution with
#' modelling techniques, thresholds the distribution models (e.g. SDMs) based on EOO's
#' geometry to generate models-derived EOOs. Here, EOOs might correspond to the alpha
#' hulls (ahull) or Minimum Polygon Convex (MCP) and a geographical similarity approach
#' is used to estimate the shape of the species' distribution based on models. It is
#' develop by adapting the approach described in Syfert et al. (2014), so that the
#' species' area of habitat (AOH) can be interpreted as the SDMs-derived EOOs.
#'
#'@usage aohMixS (eooSp = eooSp, modSp = modSp, thresInitial = NULL , thresIncrement = NULL,
#'continuous = TRUE, poly = NULL, cropToPoly = FALSE, progress = TRUE,
#'removeTempFile = TRUE, stack = TRUE )
#'
#'@param eooSp Spatial distribution data of the species representing the
#' original (i.e. not refined) extent of occurrence (EOO). It might correspond to:
#' \itemize{
#'   \item path for a folder with spatial distribution files (ESRI shapefile format).
#'   \item SpatialPolygonsDataFrame (see \code{\link[habitaR]{readShp}} to obtain such
#'   class of object).
#'   \item aHull object with elements of 'SpatialPolygons' class created from
#'   occurrences records (see \code{\link[habitaR]{aHull}} to obtain such class
#'   of object).
#'   \item a list of 'RasterLayer' objects created from occurrences records
#'   (see \code{\link[habitaR]{aHull}} to obtain such class of object).
#'   \item 'RasterLayer', 'RasterStack' or 'RasterBrick' objects.
#'  }
#'NOTE: Input data for EOOs might correspond to Alpha Hull or to Minimum Polygon
#'Convex (MCP), which can be generated, for example, with the \code{\link[dismo]{convaHull}}
#'or \code{\link[adehabitatHR]{mcp}} functions.
#'@param modSp Species distribution derived from modelling techniques (eg. SDMs).
#'It might correspond to:
#' \itemize{
#'   \item path for a folder with species distribution models files (.asc or .tif
#'   format).
#'    \item a list of 'RasterLayer' objects (see \code{\link[habitaR]{readRas}}
#'   to obtain such class of object).
#'   \item 'RasterLayer', 'RasterStack' or 'RasterBrick' objects.
#'   }
#'
#'@param thresInitial The minimum value of threshold to indicate the presence of the
#'species in the cell. NOTE: From this initial value all threshold spectrum will be
#'considered up to maximum value of 1.0 based on the increment value defined
#'by \code{\link[habitaR]{threIncrement}}.
#'@param thresIncrement The amount to sequentially increase the initial threshold with each
#'iteration until find the value that maximizes the geographical overlap (maxJSI) between
#'EOO and distribution models. See details.
#'@param poly Optional. A polygon (ESRI shapefile in a 'SpatialPolygonsDataFrame'
#' class) of a given area to be used as a mask to crop and restrict the models-derived
#' EOOs to the area corresponding to provided polygon.
#'@param cropToPoly (logical) Whether the output should be cropped by the provided poly.
#'Only used if 'poly' is provided. If \code{TRUE}, the resulting maps will be restricted
#'to the area of the poly. Default is \code{FALSE}.
#'@param progress (logical) A bar showing the progress of the function.
#' Default is \code{TRUE}.
#'@param removeTempFile (logical) Whether the temporary files generated in each iteration
#'should be deleted. It is recommended if the user wants to analyse a high volume of data at
#'the same time. Default is \code{TRUE}.
#'@param stack (logical) Whether the output maps should be returned as a stack of 'RasterLayer'
#'files. Default is \code{TRUE}.
#'
#'@return By default, \code{aohMixS} returns a list with two elements:
#'  \itemize{
#'   \item A 'RasterStack' (if \code{stack = TRUE}) or list of 'RasterLayer'
#'   objects (if \code{stack = FALSE}) of maps representing SDMs-derived EOOs.
#'    \item A data.frame of species and measures relative to SDM-derived EOO's
#'    construction and evaluation, as follow:
#'      \itemize{
#'         \item Threshold - Threshold value maximizing the geographical similarity
#'         between distribution from EOO and from model.
#'         \item MaxJSI - Value of Jaccard Similarity Index (JSI) measured from
#'         comparison between the EOO and the model returning the maximum geographical
#'         similarity.
#'         \item DifSizes - Difference (in number of cells) between the EOO and
#'         the SDMs-derived EOOs. This measure can be used to evaluate the performance
#'         of this analysis (see Syfert et al. 2014 for more details).
#'         \item Prop - Proportion of the EOO maintained in the resulting SDMs-derived EOOs.
#'         This measure can be used to evaluate the performance of this analysis
#'         (see Syfert et al. 2014 for more details).
#'         }
#'      }
#'
#'@details
#' Models-derived EOOs are generated through an adapted approach presented by Syfert
#' et al. (2014) to setting thresholds by maximizing (across all spectrum of
#' probabilities) the geographical similarity between the EOO (i.e. a polygon drawn
#' based on the presence records, such as alpha hull or minimum polygon convex) and
#' the polygon derived from models (e.g. SDMs predicted presences). In each iteration,
#' based on the increment value defined by \code{\link[habitaR]{threIncrement}}, is
#' searched the threshold that return the most similar distribution between
#' the provided model and the EOO through the Jaccard Similarity Index (JSI),
#' calculated as:
#' \itemize{
#'         \item JSI = C / (A + B) - C
#'         }
#'
#'A: area of alpha hull / B: area of SDM / C: overlapped area between A and B.
#'
#'@examples
#'
#' ### Path for folder as Input ###
#'
#' \dontrun{
#'
#' # Binary Output #
#' aohmix_bin <- aohMixS (eooSp = path_eoo, modSp = path_mod,
#' thresInitial = 0.05, thresIncrement = 0.25, continuous = FALSE, cropToPoly = TRUE,
#' poly = poly, progress = TRUE, stack = TRUE)
#'
#' # Continuous Output #
#' aohmix_con <- aohMixS (eooSp = path_eoo, modSp = path_mod,
#' thresInitial = 0.05, thresIncrement = 0.25, continuous = TRUE, cropToPoly = TRUE,
#' poly = poly, progress = TRUE, stack = TRUE)
#'
#' }
#'
#'
#' ### Shapefile as Input ('aHull' or 'SpatialPolygonsDataFrame' class) ###
#'
#' # Binary Output #
#'
#' aohmixS_bin <- aohMixS (eooSp = aHull_plantShp, modSp = sdm_plantRas,
#' thresInitial = 0.05, thresIncrement = 0.25, continuous = FALSE, cropToPoly = TRUE,
#' poly = poly, progress = TRUE, stack = TRUE)
#'
#' aohmixS_bin <- aohMixS (eooSp = spdf_plantShp, modSp = sdm_plantRas,
#' thresInitial = 0.05, thresIncrement = 0.25, continuous = FALSE, cropToPoly = TRUE,
#' poly = poly, progress = TRUE, stack = TRUE)
#'
#' # Continuous Output #
#'
#' aohmixS_con <- aohMixS (eooSp = aHull_plantShp, modSp = sdm_plantRas,
#' thresInitial = 0.05, thresIncrement = 0.25, continuous = TRUE, cropToPoly = TRUE,
#' poly = poly, progress = TRUE, stack = TRUE)
#'
#' aohmixS_con <- aohMixS (eooSp = spdf_plantShp, modSp = sdm_plantRas,
#' thresInitial = 0.05, thresIncrement = 0.25, continuous = TRUE, cropToPoly = TRUE,
#' poly = poly, progress = TRUE, stack = TRUE)
#'
#'
#' ### Raster as Input ###
#'
#' # Binary Output #
#'
#' aohmixR_bin <- aohMixS (eooSp = aHull_plantRas, modSp = sdm_plantRas,
#' thresInitial = 0.05, thresIncrement = 0.25, continuous = FALSE, cropToPoly = TRUE,
#' poly = poly, progress = TRUE, stack = TRUE)
#'
#' # Continuous Output #
#'
#' aohmixR_con <- aohMixS (eooSp = aHull_plantRas, modSp = sdm_plantRas,
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
#' @export aohMixS
#' @import raster
#' @import rlist
#' @import rgdal
#' @import sp


aohMixS <- function(eooSp = NULL, modSp = NULL, thresInitial = NULL , thresIncrement = NULL,
                   continuous = TRUE, poly = NULL, cropToPoly = FALSE,
                   progress = TRUE, removeTempFile = TRUE, stack = TRUE) {

  # Checking list and warning messages
  {
    if (missing(eooSp))
      stop("eooSp is missing")
    if (missing(modSp))
      stop("modSp is missing")
    if (is.null(poly) & (cropToPoly == TRUE))
      stop('cropToPoly can only be true when poly is provided')
    }

  # Creating the threshold (spectrum of values)
  threshold <- seq(thresInitial,1,thresIncrement)

  # Pre-processing of data:
  # Converting 'modSp' input data into a 'Raster'
  if (is.character(modSp)) {
    modSp <- readRas(modSp)
  }

  # Converting 'eooSp' input data into a 'Raster'
    # 1. Input as a path for folders
    if (is.character(eooSp)) {
       if(substr(eooSp, nchar(eooSp), nchar(eooSp)) == '/'){
           eooSp <- substr(eooSp, 1, nchar(eooSp) - 1)
         }
       files.sp <- list.files(eooSp, pattern = ".shp$")
       files.sp <- gsub(".shp","", files.sp)
       names <- list.files(eooSp, pattern = ".shp$", full.names = F)
       sps <- list()
       for (i in 1:length(files.sp)){
         sps[[i]] <- readOGR(dsn = eooSp,
                             layer = files.sp[i])
       }
       names(sps)<- files.sp
       eooSp <- sps
                    }

   # 2. Input as 'SpatialPolygons' or 'SpatialPolygonsDataFrame' family of class
    # Rasterize them
    if((class(eooSp) == "list" & class(eooSp[[1]]) == "SpatialPolygonsDataFrame") |
    class(eooSp) == "aHull"){
    eooSp <- mapply(rasterize, eooSp, MoreArgs =
                      list(modSp[[1]], background = 0, mask=FALSE))
    for (i in 1:length(eooSp)){
      eooSp[[i]][eooSp[[i]] > 1] <- 1
    }
    }

    if(class(eooSp) == "SpatialPolygonsDataFrame" | class(eooSp) == "SpatialPolygons"){
      lsp<-list()
      for (i in 1:length(eooSp)){
        lsp[[i]]<-eooSp[i,1]
      }
      names(lsp)<-names(modSp)
      eooSp<-lsp
      eooSp <- mapply(rasterize, eooSp, MoreArgs =
                        list(modSp[[1]], background = 0, mask=FALSE))
      for (i in 1:length(eooSp)){
        eooSp[[i]][eooSp[[i]] > 1] <- 1
      }

    }

  # Data.frame of results
  dfres <- data.frame(matrix(ncol = 5, nrow = length(eooSp)))
  names(dfres) <- c('Species', 'Threshold', 'MaxJsi', 'DifSizes', 'Prop')
  spp.nm<-gsub("[.]", " ", names(eooSp))

  # 3. Input data as 'Raster' family of classes
    if((class(eooSp) == "list" & class(eooSp[[1]]) == "RasterLayer")
       | class(eooSp) == "RasterLayer" | class(eooSp) == "RasterStack"){

  # Looping analysis to each feature from a list of rasters, a 'RasterStack', or a 'RasterBrick':
    # Enabling the progress bar
    list.r <- list()
    if(progress){
    pb <- txtProgressBar(min = 0, max = length(eooSp), style = 3)
      }

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
       dfres[,1] <- spp.nm
       dfres[j,2] <- dfjacord[length(threshold),1] # thresholdMAX
       dfres[j,3] <- dfjacord[length(threshold),2] # maxJSI
       dfres[j,4] <- difSize
       dfres[j,5] <- prop


    # Removing temp files
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

     # Displaying progress bar
    if(progress == TRUE){
      setTxtProgressBar(pb, j)
    }

    list.r[[j]]<-aohMix
    }
    }

  # FINAL OUTPUT
   if(stack){
     list.r <- raster::stack(list.r)
     names(list.r)<-spp.nm
     aohMix.Res <- list(aohMix = list.r, SpecificMeasures = dfres)
     return(aohMix.Res)
   }
   else{
     names(list.r)<-spp.nm
     aohMix.Res <- list(aohMix = list.r, SpecificMeasures = dfres)
     return(aohMix.Res)
   }
   }
}

