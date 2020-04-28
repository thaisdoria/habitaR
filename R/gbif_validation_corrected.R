#' Validation function
#'
#' Provide the summary of validation results based on comparison between an
#' independent occurrence records of species and the extent of occurrence (EOO)
#' and area of habitat (AOH) following Rondinini et al. (2011)
#'
#' @usage
#' # method for signature 'SpatialPolygonsDataFrame', 'RasterLayer',
#' 'RasterStack', a 'list' of 'RasterLayer' or 'habitaR' object (see
#'  \code{\link[habitaR]{aoh}} to generate the latter object)
#'
#' aohVal(eoo.sp, aoh.sp, plot=TRUE, progress=FALSE)
#'
#' @param eoo.sp Spatial distribution data of the species representing the
#' original (i.e. not refined) extent of occurrence (EOO). It might correspond to
#' 'SpatialPolygonsDataFrame' (see \code{\link[habitaR]{readShp}} to obtain such
#' object), a 'RasterLayer', a 'RasterStack', or a 'list' with 'RasterLayer'
#' features (see \code{\link[habitaR]{readRas}} to obtain such object).
#'
#' @param aoh.sp Spatial distribution data of the species representing the area
#' of habitat (AOH). It might correspond to 'SpatialPolygonsDataFrame'
#' (see \code{\link[habitaR]{readShp}} to obtain such #' object), a
#' 'RasterLayer', a 'RasterStack', a 'list' with 'RasterLayer'
#' features (see \code{\link[habitaR]{readRas}} to obtain such object), or a
#' 'habitaR' (see \code{\link[habitaR]{aoh}} to obtain such object).
#'
#' @param plot (logical) Whether the output should also return a plot
#' representing the graphical result of validation (see Rondinini et al. 2011).
#' Default is TRUE.
#'
#' @param progress (logical) a bar showing the progress of the function.
#' Default is FALSE.
#'
#' @return \code{aohVal}
#'
#' A data.frame with a summary of results for each species which aoh was evaluated.
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
#' If plot=TRUE, the function return also a plot with a graphical representation
#' of validation results (see Rondinini et al. 2011).
#'
#' @details The function gives the summary of validation steps followed to evaluate
#' the quality of models as described and performed by Rondidini et al. (2011).
#' If plot=FALSE, the function return a data.frame
#' and Ficetola et al. (2015)
#'
#' @examples
#'
#' ### Fictitious amphibians data
#'
#' # Example for signature 'SpatialPolygonsDataFrame' (eoo) and 'habitaR' (aoh) with
#' a 'RasterLayer' class of features.
#'
#' val_data1 <- aohVal(eoo = eoo_amphShp, aoh = aoh_amphRas, plot=TRUE, progress = TRUE)
#'
#' # Example for signature 'SpatialPolygonsDataFrame' (eoo) and 'habitaR' (aoh) with
#' a 'SpatialPolygonsDataFrame' class of features.
#'
#' val_data2 <- aohVal(eoo = eoo_amphShp, aoh = aoh_amphShp, resolution = 0.05, plot=TRUE, progress = TRUE)
#'
#' @references
#' 1. Rondinini, C., Di Marco, M., Chiozza, F., Santulli, G., Baisero, D., Visconti,
#' P., Boitani, L. (2011). Global habitat suitability models of terrestrial mammals.
#' Philosophical Transactions of the Royal Society B, 366, 2633–2641.
#'
#' 2. Brooks, T. M, Fonseca, S.L. Pimm, Akçakaya, H.R., Buchanan, G.M., …,
#' Rondinini C. (2019). Measuring Terrestrial Area of Habitat (AOH) and Its
#' Utility for the IUCN Red List. Trends in Ecology &amp; Evolution, 34(11),
#' 977–986.
#'
#' 3. Ficetola, G. F., Rondinini, C., Bonardi, A., Baisero, D., &amp;
#' Padoa-Schippa, E. (2015).Habitat availability for amphibians and extinction
#' threat: a global analysis. Diversity and Distributions, 21(3), 302–311.
#'
#' @author Thaís Dória & Daniel Gonçalves-Souza
#' @export aohVal
#' @import dismo
#' @import sp
#' @importFrom utils txtProgressBar setTxtProgressBar

aohVal <- function (eoo.sp, aoh.sp, plot = TRUE, progress = TRUE){

  {
    if (missing(eoo.sp))
      stop("eoo is missing")
    if (missing(aoh.sp))
      stop("aoh is missing")
     }

  # Extracting the name of species from input data
  if(class(eoo.sp) == "SpatialPolygonsDataFrame"){ # can be generated with 'readShp' function)
    sp.names <- eoo.sp@data[,2]
  }
  if(class(eoo.sp) == "RasterLayer" | class(eoo.sp) == "RasterStack"
     | class(eoo.sp) == "list" & class(eoo.sp[[1]]) == "RasterLayer"){ # a 'list' of rasters can be generated with 'readRas' function)
    sp.names <- gsub("//.", " ", names(eoo.sp))
  }

  # Summary data frame of results
    dfRes <- data.frame(matrix(ncol = 6, nrow = length(sp.names)))
    colnames(dfRes) <- c("Species", "MATCH.EOO", "MATCH.AOH", "PP", "MP", "PP-MP")
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

    # Input 'eoo' and 'aoh' as Raster objects
    if(class(eoo.sp) == "RasterLayer" | class(eoo.sp) == "list" & # a 'list' of rasters can be generated with 'readRas' function)
       class(eoo.sp[[1]]) == "RasterLayer" | class(eoo.sp) == "RasterStack"
       & class(aoh.sp) == "RasterLayer" | class(aoh.sp) == "list" & # a 'list' of rasters can be generated with 'readRas' function)
       class(aoh.sp[[1]]) == "RasterLayer" | class(aoh.sp) == "RasterStack" |
       class(aoh.sp) == "habitaR")
      {
     sp.re <- eoo.sp[[i]]

     if(class(aoh.sp) == "RasterLayer" | class(aoh.sp) ==  "RasterStack"){
     sp.ra <- aoh.sp[[i]]}

     if(class(aoh.sp) == "habitaR" & class(aoh.sp$Data[[1]]) == "RasterLayer"){
       sp.ra <- aoh.sp$Data[[i]]
     }


     if(class(aoh.sp) == "habitaR" & class(aoh.sp$Data[[1]]) == "SpatialPolygonsDataFrame"){
       # rasterize the aoh based on resolution of eoo
       sp.a <- aoh.sp$Data[[i]]
       r <- raster()
       extent(r) <- extent(sp.a)
       res(r) <- res(eoo.sp[[i]])
       sp.ra <- rasterize(sp.a, r)
       sp.ra [sp.ra > 1] <- 1
          }
      }

   # Input 'eoo' object as SpatialPolygonsDataFrame and 'aoh'  object as Raster
    if(class(eoo.sp) == "SpatialPolygonsDataFrame" & class(aoh.sp) == "RasterLayer"
     | class(aoh.sp) == "list" & class(aoh.sp[[1]]) == "RasterLayer" | # a 'list' of rasters can be generated with 'readRas' function)
     class(aoh.sp) == "RasterStack"| class(aoh.sp) == "habitaR" &
     class(aoh.sp$Data[[1]]) == "RasterLayer") {
       # rasterize the eoo based on resolution of aoh
       sp.e <- eoo.sp[i, ]
       r <- raster()
       extent(r) <- extent(sp.e)

       if(class(aoh.sp) == "RasterLayer" | class(aoh.sp) == "list" &
          class(aoh.sp[[1]]) == "RasterLayer" | class(aoh.sp) == "RasterStack"){
       res(r) <- res(aoh.sp[[i]])
       sp.re <- rasterize(sp.e, r)
       sp.re [sp.re > 1] <- 1
       sp.ra <- aoh.sp[[i]]
       }
       if(class(aoh.sp) == "habitaR" & class(aoh.sp$Data[[1]]) == "RasterLayer"){
         res(r) <- res(aoh.sp[[i]] | aoh.sp$Data[[i]])
         sp.re <- rasterize(sp.e, r)
         sp.re [sp.re > 1] <- 1
         sp.ra <- aoh.sp$Data[[i]]
       }
     }

# download occurrences from gbif based on extent of EOO (to restrict the search to inside of original distribution)
       plot(sp.re)
       ex <- extent(sp.re)
       occ <- gbif(as.character(sp.names[i]), ext=ex, geo=T)
       pts <- as.data.frame(cbind(occ$lon, occ$lat)) # coordinates corresponding to specified extent
       coordinates(pts) <- ~ V1 + V2
       plot(pts, add=T)
       close(pb)
       match.eoo <- extract (sp.re, pts)
       match.aoh <- extract (sp.ra, pts)
       pp <- as.numeric(match.aoh) / as.numeric(match.eoo)
       mp <- cellStats(sp.ra, sum) /cellStats(sp.re, sum)

       # Input eoo and aoh as SpatialPolygonsDataFrame
       if(class(eoo.sp) == "SpatialPolygonsDataFrame" # can be generated with 'readShp' function)
          & class(aoh.sp) ==
          "SpatialPolygonsDataFrame" | class(aoh.sp) == "habitaR" &
          class(aoh.sp$Data[[1]]) == "SpatialPolygonsDataFrame"){ # can be generated with 'readShp' or 'aoh' function)

         sp.e <- eoo.sp[i, ]
         plot(sp.e)
         occ <- gbif(as.character(sp.e@data[,2]), ext=sp.e@bbox, geo=T)
         pts <- as.data.frame(cbind(occ$lon, occ$lat)) # coordinates corresponding to specified extent
         coordinates(pts) <- ~ V1 + V2
         plot(pts, add=T)
         match.eoo <- over (pts, sp.e)
         if(class(aoh.sp) == "SpatialPolygonsDataFrame"){
           sp.a <- aoh.sp[i, ]
           match.aoh <- sum(over(pts, sp.a))
         }

         if(class(aoh.sp) == "habitaR" & class(aoh.sp$Data[[1]]) == "SpatialPolygonsDataFrame"){
         sp.a <- aoh.sp$Data[[i]]
         match.aoh <- sum(over(pts, sp.a))
         }

         pp <- as.numeric(match.aoh) / as.numeric(match.eoo)
         mp <- area(sp.a) /area(sp.e)
         }


          if (sum(match.eoo, na.rm=T) != 0){
          dfRes[i,2] <- match.eoo
          dfRes[i,3] <- match.aoh
          dfRes[i,4] <- pp
          dfRes[i,5] <- mp
          dfRes[i,6] <- pp - mp
        }

          if (sum(match.eoo, na.rm=T) == 0) {
          warning (paste('No occurrence record found inside the eoo raster for', as.character(nm)))
          dfRes[i,2:6] <- NA
        }
            }

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

