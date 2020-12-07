#' aohMixK: mapping the species' AOH by mixing data from EOO and species'
#' distribution models following Kremen's method.
#'
#' Based on a mix approach combining species' distribution models with the
#' occurrences records, draws species' spatial-informed distribution
#' (e.g. alpha hull) to correcting for over-prediction from modeling techniques
#' (e.g. SDMs). It is develop by adapting the approach described in Kremen et al.
#' (2008), so that the species' area of habitat (AOH) can be interpreted as the
#' models corrected for over-prediction.
#'
#'@usage aohMixK(modSp = modSp, occ = occ, crs = NULL, distOcc = NULL,
#'threshold = 0.4, fraction = NULL, partCount = NULL, alphaIncrement = NULL,
#'buff = NULL,  continuous=TRUE, progress = TRUE, removeTempFile = TRUE, saveRas = FALSE,
#'stack = TRUE, validation = TRUE)
#'
#'@param modSp Species distribution derived from modelling techniques (eg. SDMs).
#'It might correspond to:
#' \itemize{
#'   \item path for a folder with species distribution models files (.asc or .tif
#'   format).
#'    \item a list of 'RasterLayer' objects (see \code{\link[habitaR]{readRas}}
#'   to obtain such class of object).
#'   \item 'RasterLayer', 'RasterStack' or 'RasterBrick' objects.
#'   }
#'@param occ Occurrences records of the species (coordinates in decimal degrees).
#' 'It might correspond to
#'\itemize{
#'   \item path for a folder with the species occurrences files (.csv format)
#'   \item a list of 'data.frames' with the occurrences from multiple species (see data
#' examples)
#'   \item a list of of 'SpatialPoints' from multiple species (see data
#' examples)
#'   \item 'spOcc' object corresponding to a list of 'SpatialPoints' from
#' multiple species (see \code{\link[habitaR]{readOcc}} to obtain such object).
#' }
#' NOTE: If path is provided, each .csv file should correspond to only one species
#' and the file must be named with the corresponding species names. The files must
#' have 3 columns identified as "species" (species names or other identification of taxa),
#' "long" (longitude), "lat" (latitude). Longitude must be in a column before the
#' latitude column.
#' @param crs The Coordinate Reference System (CRS) specifing the projection and
#' datum of dataset. Could be a CRS object or a character string. Only used if
#' 'occ' not correspond to a object from 'SpatialPoints' class.
#' @param distOcc A value corresponding to the minimum distance to
#' consider two coordinates as not duplicate. Values up to this distance will
#' correspond to duplicates and removed. Units of this value must be in km.
#' Default is zero (i.e. only exactly coincindent coordinates will be removed).
#' Optional and only used if 'occ' is a path for .csv files or a list of data.frames.
#' If 'occ' correspond to 'SpatialPoints', this argument should be ignored
#' (i.e., distOcc = NULL). For more details, see \code{\link[sp:remove.duplicates]{remove.duplicates}}
#' in the \pkg{sp} package.
#' @param threshold Numeric value (between 0 and 1)indicating the threshold of
#' species'suitability habitat from models that will be used to filter the
#' set of occurrences records and then construct the species alpha hull.
#' From all occurrences, only those records coinciding with cells whose
#' suitability values are equal or above this threshold will be used to
#' build the alpha hull. If multiple species are considered, this object
#' might correspond to:
#' \itemize{
#'   \item A general numeric value (e.g. 0.2, 0.6) to be applied for
#'   all species, or
#'   \item A data.frame with specific threshold values for each species.
#'   }
#'   NOTE: Default is 0.40 (following Kremen et al. 2008). See details.
#' @param fraction The minimum fraction of occurrences that must be included in
#' polygon.
#' @param partCount The maximum number of disjunct polygons that are allowed.
#' @param alphaIncrement The amount to increase alpha with each iteration.
#' @param buff A buffer distance in meters to increase boundaries of alpha hull.
#' Default is zero (0).
#' @param continuous (logical) Whether the output should continuous instead
#' binary for the rasters. Default is \code{TRUE}
#' @param progress (logical) A bar showing the progress of the function.
#' Default is \code{TRUE}.
#' @param removeTempFile (logical) Whether the temporary files generated in
#' each iteration should be deleted. It is recommended if the user wants to
#' analyse a high volume of data at the same time. Default is \code{TRUE}.
#' @param saveRas (logical) Whether the output should be saved in the working
#' directory as raster files (.asc format). Default is \code{FALSE}
#' @param stack (logical) Whether the output mapsshould be returned as a stack
#' of 'RasterLayer' files. Default is \code{TRUE}.
#' @param validation (logical) Whether the output should also return a data.frame
#' with the measures used to evaluate the model's performance. Default
#' is \code{TRUE}
#'
#'@return By default, \code{aohMixK} returns a list with three elements:
#'  \itemize{
#'   \item A 'RasterStack' (if \code{stack = TRUE}) or list of 'RasterLayer'
#'   objects (if \code{stack = FALSE}) of maps representing the AOHs generated
#'   from correction for over-prediction of models.
#'   \item A data.frame of species and the respective alpha values and number of
#'   occurrences (after the removal of duplicate coordinates) used to construct
#'   the alpha hull. If the conditions assigned by the user to build the alpha hulls
#'   cannot be satisfied, is returned the minimum convex hull (MCH) and the
#'   alpha value is identified as 'MCH'.
#'    \item A data.frame of species and the respective measures relative to construction
#'    and evaluation of distribution maps obtained from the correction for
#'    over-prediction of modelS, as follow:
#'      \itemize{
#'        \item CellsEOO - Alpha Hull size measured in number of cells
#'        \item CellsMOD - Model size measured in number of cells
#'         \item (MOD-EOO) - Difference, in number of cells, between the model
#'         and the alpha hull. This measure can be used to evaluate the performance
#'         of this analysis.
#'         \item DifSizes - A value representing how bigger is the EOO comparing with the MOD.
#'         This measure can be used to evaluate the performance of this analysis.
#'         \item Prop - Proportion of the EOO (AlphaHull) maintained in the corrected
#'         for over-prediction SDMs. This measure can be used to evaluate the performance
#'         of this analysis.
#'         }
#'      }
#'
#' @details Species distribution models (SDMs) are corrected for over-prediction
#' through an adapted approach presented by Kremen et al. (2008). From the
#' SDMs and a specified threshold value, a distribution polygon is drawn based on
#' alpha hull method. Only the occurrences records that are above the suitability
#' threshold defined by \code{\link[habitaR]{threshold}} are used to construct
#' alpha hull. By default, the threshold is 0.40 (following Kremen et al. 2008),
#' but disctinct values can also be assigned by the user. For multiple-species
#' analysis, specific threshold values can also be informed to be considered for
#' each species.
#'
#' @seealso The dynamic alpha hulls drawn from the sequentially increasing of alpha
#' values are created with \code{\link[rangeBuilder:getDynamicAlphaHull]{getDynamicAlphaHull}}
#' from \pkg{rangeBuilder} package.
#'
#' @examples
#'
#' ### Path for folder to models and occurrences as Input ###
#'
#' \dontrun{
#'
#' # Binary Output #
#' aohmixK_bin <- aohMixK (modSp = path_mod, occ = path_occ,
#' crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0", distOcc = NULL,
#' threshold = 0.4, fraction = 1, partCount = 1, alphaIncrement = 1.0,
#' buff = 10,  continuous = FALSE, progress = TRUE, removeTempFile = TRUE, saveRas = FALSE,
#' stack = FALSE, validation = TRUE)
#'
#' # Continuous Output #
#'
#' aohmixK_con <- aohMixK (modSp = path_mod, occ = path_occ,
#' crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0", distOcc = NULL,
#' threshold = 0.4, fraction = 1, partCount = 1, alphaIncrement = 1.0,
#' buff = 10,  continuous = TRUE, progress = TRUE, removeTempFile = TRUE, saveRas = FALSE,
#' stack = FALSE, validation = TRUE)
#'
#' }
#'
#' ### RasterLayer of models and DataFrame of occurrences as Input ###
#'
#' # Binary Output #
#'
#' aohmixK_bin <- aohMixK (modSp = sdm_plantRas5spp, occ = df_plantOcc5spp,
#' crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0", distOcc = NULL,
#' threshold = 0.4, fraction = 1, partCount = 1, alphaIncrement = 1.0,
#' buff = 10,  continuous = FALSE, progress = TRUE, removeTempFile = TRUE, saveRas = FALSE,
#' stack = FALSE, validation = TRUE)
#'
#' # Continuous Output #
#'
#' aohmixK_con <- aohMixK (modSp = sdm_plantRas5spp, occ = df_plantOcc5spp,
#' crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0", distOcc = NULL,
#' threshold = 0.4, fraction = 1, partCount = 1, alphaIncrement = 1.0,
#' buff = 10,  continuous = TRUE, progress = TRUE, removeTempFile = TRUE, saveRas = FALSE,
#' stack = FALSE, validation = TRUE)
#'
#' ### RasterStack of models and SpatialPoints of occurrences as Input ###
#'
#' # Binary Output #
#'
#' aohmixK_bin <- aohMixK (modSp = sdm_plantSta5spp, occ = Sp_plantOcc5spp,
#' crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0", distOcc = NULL,
#' threshold = 0.4, fraction = 1, partCount = 1, alphaIncrement = 1.0,
#' buff = 10,  continuous = FALSE, progress = TRUE, removeTempFile = TRUE, saveRas = FALSE,
#' stack = FALSE, validation = TRUE)
#'
#' # Continuous Output #
#'
#' aohmixK_con <- aohMixK (modSp = sdm_plantSta5spp, occ = Sp_plantOcc5spp,
#' crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0", distOcc = NULL,
#' threshold = 0.4, fraction = 1, partCount = 1, alphaIncrement = 1.0,
#' buff = 10,  continuous = TRUE, progress = TRUE, removeTempFile = TRUE, saveRas = FALSE,
#' stack = FALSE, validation = TRUE)
#'
#' ### RasterStack of models and SpatialPoints of occurrences (lista of 'SpatialPoints'
#' or 'spOcc' class) as Input ###
#'
#' # Binary Output #
#'
#' aohmixK_bin <- aohMixK (modSp = sdm_plantSta5spp, occ = sp_plantOcc5spp,
#' crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0", distOcc = NULL,
#' threshold = 0.4, fraction = 1, partCount = 1, alphaIncrement = 1.0,
#' buff = 10,  continuous = FALSE, progress = TRUE, removeTempFile = TRUE, saveRas = FALSE,
#' stack = FALSE, validation = TRUE)
#'
#' aohmixK_bin <- aohMixK (modSp = sdm_plantSta5spp, occ = spOcc_plantOcc5spp,
#' crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0", distOcc = NULL,
#' threshold = 0.4, fraction = 1, partCount = 1, alphaIncrement = 1.0,
#' buff = 10,  continuous = FALSE, progress = TRUE, removeTempFile = TRUE, saveRas = FALSE,
#' stack = FALSE, validation = TRUE)
#'
#' # Continuous Output #
#'
#' aohmixK_con <- aohMixK (modSp = sdm_plantSta5spp, occ = Sp_plantOcc5spp,
#' crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0", distOcc = NULL,
#' threshold = 0.4, fraction = 1, partCount = 1, alphaIncrement = 1.0,
#' buff = 10,  continuous = TRUE, progress = TRUE, removeTempFile = TRUE, saveRas = FALSE,
#' stack = FALSE, validation = TRUE)
#'
#' #' aohmixK_con <- aohMixK (modSp = sdm_plantSta5spp, occ = spOcc_plantOcc5spp,
#' crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0", distOcc = NULL,
#' threshold = 0.4, fraction = 1, partCount = 1, alphaIncrement = 1.0,
#' buff = 10,  continuous = TRUE, progress = TRUE, removeTempFile = TRUE, saveRas = FALSE,
#' stack = FALSE, validation = TRUE)
#'
#' ### Species-specific threshold for multiple-species ###
#'
#' # Continuous Output #
#'
#' aohmixK_con <- aohMixK (modSp = sdm_plantRas5spp, occ = df_plantOcc5spp,
#' crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0", distOcc = NULL,
#' threshold = plants_thresholds, fraction = 1, partCount = 1, alphaIncrement = 1.0,
#' buff = 10,  continuous = TRUE, progress = TRUE, removeTempFile = TRUE, saveRas = FALSE,
#' stack = FALSE, validation = TRUE)
#'
#' @encoding UTF-8
#'
#'@references
#' 1. Kremen, C., Cameron, A., Moilanen, A., Phillips, S. J., Thomas, C. D.,
#' Beentje, H., ... Zjhra, M. L. (2008). Aligning Conservation Priorities Across
#' Taxa in Madagascar with High-Resolution Planning Tools. Science, 320(5873),
#' 222-226.
#'
#' 2. VER QUEM MAIS
#'
#' @author Thaís Dória & Daniel Gonçalves-Souza
#' @export aohMixK
#' @import raster
#' @import rlist
#' @import rgdal
#' @import sp


aohMixK <- function(modSp = modSp, occ = NULL, crs = NULL, distOcc = NULL, threshold = 0.4,
                    fraction = NULL, partCount = NULL, alphaIncrement = NULL, buff = NULL,
                    continuous=TRUE, progress = TRUE, removeTempFile = TRUE, saveRas = FALSE,
                    stack = TRUE, validation = TRUE){


# Checking list and warning messages
{
  if (missing(occ))
    stop("occ is missing")
  if (missing(modSp))
    stop("modSp is missing")
  }


  # Pre-processing of data: reading occurrences and models

    # Models
  {
    # 1. Mod as a path
    if (is.character(modSp)) {
      modSp <- readRas(modSp)
    }

    # 2. Mod as a stack of rasters
    if(class(modSp) == "RasterStack" | class(modSp) == "RasterBrick"){
      modSp <- unstack(modSp)
    }
  }

    # Occurrences (will be conveted into a "spOcc" objetc)
  {
    # 1. Occ is list of 'SpatialPoints'
    if(class(occ)== "spOcc" | class(occ) == "list" & class(occ[[1]]) == "SpatialPoints"){
      class(occ) <- "spOcc" # Converting input data into a 'spOcc' object
      # Warnings
      if (!is.null(crs) & class(crs) != "numeric")
        warning("'occ' already correspond to 'SpatialPoints', so 'crs' is not used")
      if (!is.null(distOcc) & class(distOcc) == "numeric")
        warning("'occ' already correspond to 'SpatialPoints', so 'distOcc' is not used")
    }

    # 2. Occ is NOT a 'spOcc' object or a list of 'SpatialPoints'
    if (class(occ) != "spOcc" | class(occ) == "list" & class(occ[[1]]) != "SpatialPoints"){
      # I. ocurrences as path for folder with .csv files of multiple species will be converted into a 'spOcc' object:
      if (is.character(occ)){
      occ <- readOcc(occ, crs, distOcc) # a 'spOcc' object
      }
      # II. Ocurrences of multiple species as a list of 'data.frames'
      if (class(occ) == "list" & class(occ[[1]]) == "data.frame"){
        # Error and Warning messages
        {
          if (is.null(crs))
            stop("a crs must be informed")
          if (class(crs)=="numeric")
            stop("crs as 'numeric' is not valid for slot ‘proj4string’ in an object of class “Spatial”. See usage and examples to check the arguments.")
          if (!is.null(distOcc) & class(distOcc) != "numeric")
            stop("distOcc must be provided as 'numeric' or NULL")
          if (is.null(distOcc))
            warning("distOcc is not provided, so default (zero) is used")
        }

        # To get coords from data.frame
        l<-list()
        for (i in 1:length(occ)){
          long<-as.numeric(as.character(occ[[i]]$long))
          lat<-as.numeric(as.character(occ[[i]]$lat))
          l[[i]]<-as.matrix(cbind(long, lat))
        }
        names(l) <- names(occ) # list of coords

        # To convert occurrences into 'SpatialPoints'
        # CRS is required
        sp.clean<-list()
        for (i in 1:length(l)){
          if (is.character(crs)){
            spcc<-SpatialPoints(l[[i]], proj4string = CRS(crs))
          }
          if (class(crs) == "CRS"){
            spcc<-SpatialPoints(l[[i]], proj4string = crs)
          }
          sp.clean[[i]]<-spcc
        }
        names(sp.clean) <- names(l)

        # Removing duplicates (distOcc)
        sp.clean2<-list()
        for (i in 1:length(sp.clean)){
          if (!is.null(distOcc)){
            sp<-remove.duplicates(sp.clean[[i]], zerodist(sp.clean[[i]], zero=as.numeric(distOcc)))
          }
          if (is.null(distOcc)){
            sp<-remove.duplicates(sp.clean[[i]])
          }
          sp.clean2[[i]]<-sp
        }
        names(sp.clean2) <- names(sp.clean)

        # To convert object in a "spOcc" class
        for (i in 1:length(sp.clean2)){
          colnames(sp.clean2[[i]]@coords) <- c("long", "lat")
        }
        class(sp.clean2) <- "spOcc" # a 'spOcc' object
        occ<-sp.clean2
      }
    }

    # 2. Occ as a list of 'SpatialPoints' that will be converted into a 'spOcc' object
    if(class(occ) == "list" & class(occ[[1]]) =="SpatialPoints"){
      class(occ) <- "spOcc"
    }


      }

   # MAIN STEP: Building a distribution map based on SpatialPoints and Threshold of Models
   {
    # To identify probability values of the models' cells matching with the spatial points

     match.occmod<-t(mapply(extract, modSp, occ))


  # To filter the spatial points based on a threshold
   # Default (threshold=0.40) or other numeric value general for all species

  if(class(threshold) == "numeric"){
    thre <- threshold
    for (i in 1:length(occ)){
      fil<-occ[[i]][match.occmod[[i]] >= thre,]
      occ.thre[[i]]<-fil
    }
    names(occ.thre)<-names(occ)
  }
  else{ # Threshold informed by the user (a data.frame with specific values for each spp.)
    occ.thre<-list()
    thre<-threshold
    for (i in 1:length(occ)){
    fil<-occ[[i]][match.occmod[[i]] >= as.numeric(thre[i,2]),]
    occ.thre[[i]]<-fil
    }
    names(occ.thre)<-names(occ)
  }


   # To build the Ahull maps based on the cleaning spatial points (Distribution map drawn based on spatial points filtering by the threshold of model)
   occ.ahul <- occ.thre
   # Data frame of alpha hull construction results
   df <- data.frame (matrix(ncol = 3, nrow = length(occ.ahul)))
   names(df) <- c('Species', 'Ocurrences', 'Alpha')
   df[, 1] <- names(occ)

   # To count/summarize occurrences that will be used to build ahulls after cleaning by the threshold of models
   for (i in 1:length(occ.ahul)){
     df[i,2] <- length(occ.ahul[[i]]@coords[,1])} # number of occurrences filtered

   # To build alpha hull based on cleaned spatial points by the informed threshold of models
   sp.ahull <- mapply(f.ahull, occ.ahul, MoreArgs = list(fraction, partCount,
                                                         buff, alphaIncrement))
   ahulls <- sp.ahull[1,] # a 'SpatialPolygonsDataFrame' object
   alphas <- matrix(unlist(sp.ahull[2,]))
   alphas <- gsub("alpha", "", alphas[,1])
   df[,3]<- alphas

   # Rasterizing the shapes of ahulls
    mod.ahulls.r <- mapply(rasterize, ahulls, MoreArgs =
                       list(modSp[[1]], background = 0, mask=FALSE)) # Raster of distribution drawn based on spatial points filtered by the threshold


   list.r<-list()
   # Enabling the progress bar
   if(progress){
     pb <- txtProgressBar(title="Models Correction for Over-prediction", min = 0, max = length(mod.ahulls.r), style = 3)
   }

   if(continuous){
   for (i in 1:length(mod.ahulls.r)){
    mod.ahulls.r[[i]][mod.ahulls.r[[i]] == 0]<-NA
    mod.ahulls.r.cont<-mask(modras[[i]], mod.ahulls.r[[i]])
    mod.ahulls.r.cont[is.na(mod.ahulls.r.cont[])]<- 0
    mod.ahulls.r.cont<-mask(mod.ahulls.r.cont, modras[[i]])
    plot(mod.ahulls.r.cont)
    list.r[[i]]<-mod.ahulls.r.cont
    # Displaying progress bar
    if(progress == TRUE){
      setTxtProgressBar(pb, i)
    } # default
   }
     names(list.r)<-names(occ)
   }
   else{
     {
     l[[i]]<-mod.ahulls.r
   }
      names(l)<-names(occ)
   }

     if (saveRas == TRUE){
       for (i in 1:length(list.r)){
       writeRaster(list.r[[i]], paste(names(occ[i]),"AOH_MK.asc",sep=""), format="ascii", bylayer=T, overwrite=T, progress="text")
       }
}
      if(stack){
         list.r <- raster::stack(list.r)
         names(list.r)<-names(occ)
           }


   # Removing temp files
   if (removeTempFile == TRUE) {
     removeTmpFiles(h=0.0)
   } # default
}

   # OUTPUTS AND VALIDATION STEP: Building ahulls based on all SpatialPoints to compare with the ahulls generated with the filtered occurrences (the resulting maps)
   {
   if (validation == TRUE){ # default
     # Data frame of evaluation results
  val <- data.frame (matrix(ncol = 6, nrow = length(occ)))
  colnames(val) <- c("Species", "CellsEOO", "CellsMOD", "MOD-EOO", "DifSizes", "Prop")
  val[, 1] <- names(occ)

  # To build alpha hull based on original spatial points (all points) without the filtering (extracting) step
  occ.ahul<-occ
  eoos.ahull <- mapply(f.ahull, occ.ahul, MoreArgs = list(fraction, partCount,
                                                        buff, alphaIncrement))
  eoos.ahulls <- eoos.ahull[1,] # a 'SpatialPolygonsDataFrame' object

  if(progress){
    pb <- txtProgressBar(title="Building Ahulls for Validation", min = 0, max = length(occ.ahul), style = 3)

  }

  # Rasterizing
  eoos.ahulls.r <- mapply(rasterize, eoos.ahulls, MoreArgs =
                       list(modras[[1]], background = 0, mask=FALSE))

  for (i in 1:length(eoos.ahulls.r)){
      r1<-eoos.ahulls.r[[i]] #ahull (EOOs)
      r2<-mod.ahulls.r[[i]] # models (SDMs)
      r2[r2>0] <- 1
      c1<-cellStats(r1, sum) # ahull size in number of cells
      c2<-cellStats(r2, sum) # model size in number of cells
      sub<-c2-c1
      if (c1>c2){
        dif<-c1/c2
      }
      if (c1<c2){
        dif<-c2/c1
      }
      combination <- r1 + r2
      plot(combination)
      intersection <- combination == 2
      plot(intersection)
      plot(r1)
      # Union is all the area covered by the both rasters
      prop <- cellStats(intersection, sum) / c1
      prop
      spnm<-df[i,1]
      res<-c(spnm, c1, c2, sub, dif, prop)
      val[i,2] <- c1
      val[i,3] <- c2
      val[i,4] <- sub
      val[i,5] <- dif
      val[i,6] <- prop

      # Removing temp files
      if (removeTempFile == TRUE) {
        removeTmpFiles(h=0.0)
      } # default

      # Displaying progress bar
      if(progress == TRUE){
        setTxtProgressBar(pb, i)
      }
  }

  # OUTPUTS
  res<-list(ModCorMaps=list.r, AhullModData=df, ValidationData=val)
  # l = raster of distribution maps (ahulls) constructed based on filtered SpatialPoints by the threshold of models
  # df = data.frame of the ahull construction based on models
  # val = data.frame of the validation step
  return(res)
   }
     else{
       # OUTPUTS
       res<-list(ModCorMaps=list.r, AhullModData=df)
       # l = raster of distribution maps (ahulls) constructed based on filtered SpatialPoints by the threshold of models
       # df = data.frame of the ahull construction based on models
       return(res)
     }
   }
   }



