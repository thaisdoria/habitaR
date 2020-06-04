#' aohVal: evaluate the AOH mapping
#'
#' Provide the summary of validation results based on comparison between an
#' independent occurrence records of species and the extent of occurrence (EOO)
#' and area of habitat (AOH) following Rondinini et al. (2011)
#'
#' @usage
#' # method for signature 'SpatialPolygonsDataFrame', 'RasterLayer',
#' 'RasterStack', a 'list' of 'RasterLayer' or 'aoh' object (see
#'  \code{\link[aoh]{aoh}} to generate the latter object)
#'
#' aohVal(eooSp, aohSp, plot=TRUE, progress=FALSE)
#'
#' @param eooSp Spatial distribution data of the species representing the
#' original (i.e. not refined) extent of occurrence (EOO). It might correspond to
#' 'SpatialPolygonsDataFrame' (see \code{\link[aoh]{readShp}} to obtain such
#' class of object), a 'RasterLayer', a 'RasterStack', or a 'list' with 'RasterLayer'
#' class of features (see \code{\link[aoh]{readRas}} to obtain such object).
#'
#' @param aohSp Spatial distribution data of the species representing the area
#' of habitat (AOH). It might correspond to 'SpatialPolygonsDataFrame'
#' (see \code{\link[aoh]{readShp}} to obtain such class of object), a
#' 'RasterLayer', a 'RasterStack', a 'list' with 'RasterLayer' class of features
#' (see \code{\link[aoh]{readRas}} to obtain such object), or a 'aoh'
#' (see \code{\link[aoh]{aoh}} to obtain such class of object).
#'
#' @param plot (logical) Whether the output should also return a plot
#' representing the graphical result of validation (see Rondinini et al. 2011).
#' Default is \code{TRUE}.
#'
#' @param progress (logical) A bar showing the progress of the function.
#' Default is \code{FALSE}.
#'
#' @return \code{aohVal} returns a data.frame with a summary of results for each species which aoh was evaluated.
#' It gives the number of records matching with the EOO ("MATCH.EOO) and with the
#' AOH ("MATCH.AOH) and the values of prevalence of points ("PP", Rondinini et al.
#' 2011) through the proportion of occurrences spatially congruent with suitable
#' cells, and of model prevalence ("MP", Rondinini et al. 2011), which represents
#' EOOs proportion assigned as suitable). AOH predict species occurrences
#' correctly when all available records match with its suitable cells (PP = 1).
#' The evaluation of AOHs performance is based on difference between PP and MP.
#' If PP > MP, AOH performs better than EOO in predict presences (Ficetola et al.
#' 2015).
#'
#' If plot is \code{TRUE}, the function return also a plot with a graphical representation
#' of validation results (see Rondinini et al. 2011). If plot \code{FALSE}, the function
#' return a data.frame
#'
#' @details The function gives the summary of validation steps followed to evaluate
#' the quality of models as described and performed by Rondidini et al. (2011) and
#' Ficetola et al. (2015).
#'
#' @examples
#'
#' ### Fictitious birds data
#'
#' # Example for signature 'SpatialPolygonsDataFrame' (eoo) and 'aoh' (aoh) with
#' a 'RasterLayer' class of features.
#'
#' birdVal1<-aohVal(eoo = eoo_birdShp, aoh = aoh_birdRas, plot = TRUE, progress = TRUE)

#' # Example for signature 'RasterLayer' or 'RasterStack' (eoo) and 'aoh' (aoh) with
#' a 'RasterLayer' class of features.
#'
#' birdVal2<-aohVal(eoo = eoo_birdRas, aoh = aoh_birdRas, plot = TRUE, progress = TRUE)
#'
#' # Example for signature 'SpatialPolygonsDataFrame' (eoo) and 'aoh' (aoh) with
#' a 'SpatialPolygonsDataFrame' class of features.
#'
#' birdVal3<-aohVal(eoo = eoo_birdShp, aoh = aoh_birdShp, plot = TRUE, progress = TRUE)
#'
#' # Example for signature 'RasterLayer' or 'RasterStack' (eoo) and 'aoh' (aoh) with
#' a 'SpatialPolygonsDataFrame' class of features.
#'
#' birdVal4<-aohVal(eoo = eoo_birdRas, aoh = aoh_birdShp, plot = TRUE, progress = TRUE)
#'
#' @encoding UTF-8
#' @references
#' 1. Rondinini C., Di Marco M., Chiozza F., Santulli G., Baisero D., Visconti P.,
#' and Boitani L. (2011). Global habitat suitability models of terrestrial mammals.
#' Philosophical Transactions of the Royal Society B, 366:2633–2641.
#'
#' 2. Ficetola G.F., Rondinini C., Bonardi A., Baisero D., and Padoa-Schippa E.
#' (2015).Habitat availability for amphibians and extinction threat: a global
#' analysis. Diversity and Distributions, 21(3):302–311.
#'
#' 3. Brooks T.M., Pimm S.L., Akçakaya H.R., Buchanan G.M., Butchart S.H.M, Foden W.,
#' Hilton-Taylor C., Hoffmann M., Jenkins C.N., Joppa L., Li B.V., Menon V.,
#' Ocampo-Peñuela N., and Rondinini C. (2019). Measuring Terrestrial Area of Habitat
#' (AOH) and Its Utility for the IUCN Red List. Trends in Ecology & Evolution,
#' 34(11):977–986.
#'
#' @author Thaís Dória & Daniel Gonçalves-Souza
#' @export aohVal
#' @import dismo
#' @import sp
#' @import GISTools
#' @importFrom utils txtProgressBar setTxtProgressBar

aohVal <- function (eooSp, aohSp, plot = TRUE, progress = TRUE){

  # Checklist and warning messages
  {
    if (missing(eooSp))
      stop("eooSp is missing")
    if (missing(aohSp))
      stop("aohSp is missing")
  }

  # Extracting the name of species from input data
  if(class(eooSp) == "SpatialPolygonsDataFrame"){ # can be generated with 'readShp' function)
    sp.names <- eooSp@data[,2]
  }
  if(class(eooSp) %in% c("RasterLayer", "RasterStack")
  | (class(eooSp) == "list" & class(eooSp[[1]]) == "RasterLayer")) { # a 'list' of rasters can be generated with 'readRas' function)
    sp.names <- gsub("[.]", " ", names(eooSp))
  }

  # Summary data frame of results
  dfRes <- data.frame(matrix(ncol = 6, nrow = length(sp.names)))
  colnames(dfRes) <- c("Species", "MatchEoo", "MatchAoh", "PP", "MP", "PP-MP")
  dfRes[,1] <- sp.names

  # Enabling the progress bar
  if(progress == TRUE){
    pb <- txtProgressBar(min = 0, max = length(sp.names), style = 3)
  }

  # Lopping the validation steps for each-species
  for (i in 1:length(sp.names)){

    if(progress == TRUE){
      setTxtProgressBar(pb, i)
    }

    # Input data based on shape analysis - it is not the best recomended
    {
      # Input eoo and aoh as SpatialPolygonsDataFrame
      if(class(eooSp) == "SpatialPolygonsDataFrame" # can be generated with 'readShp' function)
         & (class(aohSp) == "SpatialPolygonsDataFrame" | class(aohSp) == "aoh" &
            class(aohSp$Data[[1]]) == "SpatialPolygonsDataFrame")){ # can be generated with 'readShp' or 'aoh' function)

        sp.e <- eooSp[i, ]
        plot(sp.e)
        occ <- gbif(as.character(sp.e@data[,2]), ext=sp.e@bbox, geo=T)

        if (is.null(occ)){
          warning(paste('No occurrence records found inside of the searched extent for',sp.names[i]))
          match.eoo <- NA
          match.aoh <- NA
          pp <- NA
          mp <- NA
        }

        if (is.null(occ) == FALSE){
          pts <- as.data.frame(cbind(occ$lon, occ$lat)) # coordinates corresponding to specified extent
          coordinates(pts) <- ~ V1 + V2
          proj4string(pts)<-crs(sp.e)
          plot(pts, add=T)
          match.eoo <- poly.counts (pts, sp.e)
        }

        if(class(aohSp) == "SpatialPolygonsDataFrame"){
          sp.a <- aohSp[i, ]
          match.aoh <- poly.counts(pts, sp.a)
        }

        if(class(aohSp) == "aoh" & class(aohSp$Data[[1]]) == "SpatialPolygonsDataFrame"){
          sp.a <- aohSp$Data[[i]]
          match.aoh <- poly.counts(pts, sp.a)
        }

        pp <- as.numeric(match.aoh) / as.numeric(match.eoo)
        mp <- poly.areas(sp.a) / poly.areas(sp.e)
      }
    }

    # Input data based on raster analysis
    {
    # Input 'eoo' and 'aoh' as Raster objects
    if(class(eooSp) == "RasterLayer" | (class(eooSp) == "list" & # a 'list' of rasters can be generated with 'readRas' function)
                                         class(eooSp[[1]]) == "RasterLayer") | class(eooSp) == "RasterStack"
       & (class(aohSp) == "RasterLayer" | (class(aohSp) == "list" & # a 'list' of rasters can be generated with 'readRas' function)
                                            class(aohSp[[1]]) == "RasterLayer") | class(aohSp) == "RasterStack" |
          (class(aohSp) == "aoh" & class(aohSp$Data[[1]]) == "RasterLayer")))
    {
      sp.re <- eooSp[[i]]

      if(class(aohSp) %in% c("RasterLayer", "RasterStack")){
        sp.ra <- aohSp[[i]]

        if(res(sp.ra) > res(sp.re)){
          factor <- round((res(sp.ra) / res(sp.re)))
          sp.re <- aggregate(sp.re, fact = factor, fun = modal)
          sp.ra <- resample(sp.ra, sp.re)
          sp.ra <- mask(sp.ra, sp.re)
          sp.ra [sp.ra > 0] <- 1
        }

        ext <- extent(sp.re)
        sp.ra <- setExtent(sp.ra, ext, keepres=FALSE, snap=FALSE)
        sp.ra <- resample(sp.ra, sp.re)
        sp.ra <- mask(sp.ra, sp.re)
        sp.ra [sp.ra > 0] <- 1
      }

      if(class(aohSp) == "aoh" & class(aohSp$Data[[1]]) == "RasterLayer"){
        sp.ra <- aohSp$Data[[i]]

        if(res(sp.ra) > res(sp.re)){
          factor <- round((res(sp.ra) / res(sp.re)))
          sp.re <- aggregate(sp.re, fact = factor, fun = modal)
          sp.ra <- resample(sp.ra, sp.re)
          sp.ra <- mask(sp.ra, sp.re)
          sp.ra [sp.ra > 0] <- 1
        }

        ext<-extent(sp.re)
        sp.ra<-setExtent(sp.ra, ext, keepres=FALSE, snap=FALSE)
        sp.ra<-resample(sp.ra, sp.re)
        sp.ra <- mask(sp.ra, sp.re)
        sp.ra [sp.ra > 0] <- 1
      }
      plot(sp.re)
    }

    # Input 'eoo' object as Raster and 'aoh' object as SpatialPolygonsDataFrame
    if(class(eooSp) == "RasterLayer" | (class(eooSp) == "list" & # a 'list' of rasters can be generated with 'readRas' function)
                                         class(eooSp[[1]]) == "RasterLayer") | class(eooSp) == "RasterStack"
       & (class(aohSp) == "SpatialPolygonsDataFrame" | (class(aohSp) == "aoh"
                                                         & class(aohSp$Data[[1]]) == "SpatialPolygonsDataFrame")))
    {
      sp.re <- eooSp[[i]]


      if(class(aohSp) == "SpatialPolygonsDataFrame"){
        # rasterize the aoh based on resolution of eoo
        sp.a <- aohSp[i,]
        r <- raster()
        extent(r) <- extent(sp.re)
        res(r) <- res(sp.re)
        sp.ra <- rasterize(sp.a, r)
        sp.ra<-resample(sp.ra, sp.re)
        sp.ra <- mask(sp.ra, sp.re)
        sp.ra [sp.ra > 0] <- 1
      }

      if(class(aohSp) == "aoh" & class(aohSp$Data[[1]]) == "SpatialPolygonsDataFrame"){
        # rasterize the aoh based on resolution of eoo
        sp.a <- aohSp$Data[[i]]
        r <- raster()
        extent(r) <- extent(sp.re)
        res(r) <- res(sp.re)
        sp.ra <- rasterize(sp.a, r)
        sp.ra<-resample(sp.ra, sp.re)
        sp.ra <- mask(sp.ra, sp.re)
        sp.ra [sp.ra > 0] <- 1
      }
      plot(sp.re)
       }

    # Input 'eoo' object as SpatialPolygonsDataFrame and 'aoh' object as Raster
    if(class(eooSp) == "SpatialPolygonsDataFrame" & (class(aohSp) %in% c("RasterLayer", "RasterStack")
      | (class(aohSp) == "list" & class(aohSp[[1]]) == "RasterLayer") | # a 'list' of rasters can be generated with 'readRas' function)
      (class(aohSp) == "aoh" & class(aohSp$Data[[1]]) == "RasterLayer")))
      {
      # rasterize the eoo based on resolution of aoh
      sp.e <- eooSp[i, ]
      r <- raster()
      extent(r) <- extent(sp.e)

      if(class(aohSp) %in% c("RasterLayer", "RasterStack") | (class(aohSp) == "list" &
        class(aohSp[[1]]) == "RasterLayer")) {
        res(r) <- res(aohSp[[i]])
        sp.re <- rasterize(sp.e, r)
        sp.re [sp.re > 1] <- 1
        sp.re <- mask(sp.re, sp.e)
        sp.ra <- aohSp[[i]]
        sp.ra [sp.ra > 0] <- 1
        sp.ra <- mask(sp.ra, sp.e)
      }

      if(class(aohSp) == "aoh" & class(aohSp$Data[[1]]) == "RasterLayer"){
        res(r) <- res(aohSp$Data[[i]])
        sp.re <- rasterize(sp.e, r)
        sp.re [sp.re > 1] <- 1
        sp.re <- mask(sp.re, sp.e)
        sp.ra <- aohSp$Data[[i]]
        sp.ra [sp.ra > 0] <- 1
        sp.ra <- mask(sp.ra, sp.e)
      }
      plot(sp.re)
       }

    # download occurrences from gbif based on extent of EOO (to restrict the search to inside of original distribution)
    ex <- extent(sp.re)
    occ <- gbif(as.character(sp.names[i]), ext=ex, geo=T)

    if (is.null(occ)){
      warning(paste('No occurrence records found inside of the searched extent for',sp.names[i]))
      match.eoo <- NA
      match.aoh <- NA
      pp <- NA
      mp <- NA
    }

    if (is.null(occ) == FALSE){
      pts <- as.data.frame(cbind(occ$lon, occ$lat)) # coordinates corresponding to specified extent
      coordinates(pts) <- ~ V1 + V2
      plot(pts, add=T)
      match.eoo <- extract (sp.re, pts)
      match.eoo <- sum(match.eoo, na.rm=T)
      match.aoh <- extract (sp.ra, pts)
      match.aoh <- sum(match.aoh, na.rm=T)
      pp <- as.numeric(match.aoh) / as.numeric(match.eoo)
      mp <- cellStats(sp.ra, sum) /cellStats(sp.re, sum)
    }
  }

    # Summarizing the validation results
    if (sum(match.eoo, na.rm=T) != 0){
      dfRes[i,2] <- match.eoo
      dfRes[i,3] <- match.aoh
      dfRes[i,4] <- pp
      dfRes[i,5] <- mp
      dfRes[i,6] <- pp - mp
    }
    if (sum(match.eoo, na.rm=T) == 0) {
      warning (paste('No occurrence records found inside of the eoo raster for', as.character(sp.names[i])))
      dfRes[i,2:6] <- NA
    }
  }

  class(dfRes) <- "aohVal"
  if (plot == FALSE){
    return(dfRes)}

  if (plot == TRUE){
    plot(as.numeric(dfRes$MP), as.numeric(dfRes$PP), type="p", ylim=c(0,1),
         xlim=c(0,1), xlab="Model Prevalence (MP)", ylab="Point Prevalence (PP)",
         cex=sqrt(dfRes$MATCH.EOO)/4)
    abline(c(0,1), lwd=0.8, lty=5, col="dark grey")
    return(dfRes)
  }
}

