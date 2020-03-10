#'  Validation function
#'
#' Provide the area of habitat (AOH) of a given species through refinement of its known geographic distribution
#'
#' @param eoos Directory where are the species 'Extent of Occurrence' (EOO) in format ESRI shapefile or raster. 
#' @param aohs Species 'Area of habitat' (AOH) in format ESRI shapefile or raster. 
#' @param matrix.alt.pref Data frame with altitudinal range of species. First column must be the species name, second column the min value of altitude and the third column the max value of altitude
#' @param shp.out (logical) Whether the output should be a shapefile as opposed to a raster
#' @import raster
#' @return The result is a RasterLayer or RasterBrick; or SpatialPolygons object
#' @details The function map the area of habitat within the geographical distribution (SpatialPolygon) given as the input data. as the refined distribution of a given species. This refinement is made considering the specific preference for habitats of a given species.
#' @author Daniel Gonçalves-Souza & Thaís Dória
#' @export validation.aoh

validation.aoh <- function (eoo, aoh, resolution){
 
  if(is.character(eoo)){
    files.sp <- list.files(eoo, pattern = ".shp$")
    files.sp <- gsub(".shp","", files.sp)
    sps <- list()
    for (i in 1:length(files.sp)){
      sps[[i]] <- readOGR(dsn = eoo,
                          layer = files.sp[i])
    }
    eoo <- do.call(bind, sps)
  }
  
  if(class(eoo) == "SpatialPolygonsDataFrame" & class(aoh) == "SpatialPolygonsDataFrame"){
    # rasterize the shapefiles
    sp <- data.frame(matrix(ncol = 6, nrow = length(eoo)))
    colnames(sp) <- c("Species", "MATCH.EOO", "MATCH.AOH", "PP", "MP", "PP-MP")
    sp[,1] <- eoo@data[,2]
    for (i in 1:length(eoo)){
      sp.e <- eoo[i, ]
      r <- raster()
      extent(r) <- extent(sp.e)
      res(r) <- resolution
      sp.re <- rasterize(sp.e, r)
      sp.re [sp.re > 1] <- 1
      plot(sp.re)
      # download occurrences from gbif based on extent of EOO (to restrict the search to inside of original distribution)
      ex <- extent(sp.re)
      names(sp.re) <- eoo@data[1,2]
      occ <- gbif(names(sp.re), ext=ex, geo=T)
      pts <- as.data.frame(cbind(occ$lon, occ$lat)) # coordinates corresponding to specified extent
      coordinates(pts) <- ~ V1 + V2
      plot(pts, add=T)
      match.eoo <- extract (sp.re, pts)
      
      if (sum(match.eoo, na.rm=T) == 0) {
        warning (paste('No occurrence record found inside the eoo raster for', as.character(spp[i])))
        sp[i,2] <- NA
      }
      
      if (sum(match.eoo, na.rm=T) != 0){
        match.eoo <- length(match.eoo)
        sp.a <- a[i, ]
        sp.ra <- rasterize(sp.a, r)
        match.aoh <- extract (sp.ra, pts)
        match.aoh <- length(match.aoh)
        pp <- as.numeric(match.aoh) / as.numeric(match.eoo)
        mp <- cellStats(sp.ra, sum) /cellStats(sp.re, sum)
        sp[i,2] <- match.eoo 
        sp[i,3] <- match.aoh
        sp[i,4] <- pp 
        sp[i,5] <- mp
        sp[i,6] <- pp - mp
      }}  
    return(sp)
  plot(as.matrix(sp[,5]), as.matrix(sp[,4]), xlab = "Model Prevalence", ylab = "Point Prevalence", xlim = c(0, 1.0), ylim = c(0, 1), pch=1, cex=sqrt(as.matrix(sp[,2])))
  abline(a = -0.0, b =1, lwd=1.0, lty=2)
}

  if(class(eoo) == "SpatialPolygonsDataFrame" & class(aoh) == "RasterLayer"){
    sp <- data.frame(matrix(ncol = 6, nrow = length(eoo)))
    colnames(sp) <- c("Species", "MATCH.EOO", "MATCH.AOH", "PP", "MP", "PP-MP")
    sp[,1] <- eoo@data[,2]
    for (i in 1:length(eoo)){
      sp.e <- eoo[1, ]
      r <- raster()
      extent(r) <- extent(sp.e)
      res(r) <- resolution
      sp.re <- rasterize(sp.e, r)
      sp.re [sp.re > 1] <- 1
      ex <- extent(sp.re)
      names(sp.re) <- eoo@data[1,2]
      occ <- gbif(names(sp.re), ext=ex, geo=T)
      pts <- as.data.frame(cbind(occ$lon, occ$lat)) # coordinates corresponding to specified extent
      coordinates(pts) <- ~ V1 + V2
      match.eoo <- as.matrix(extract (sp.re, pts))
      
      if (sum(match.eoo, na.rm=T) == 0) {
        warning (paste('No occurrence record found inside the eoo raster for', as.character(spp[i])))
        sp[i,2] <- NA
      }
      
      if (sum(match.eoo, na.rm=T) != 0){
        match.eoo <- length(match.eoo)
        sp.a <- a[i, ]
        sp.ra <- rasterize(sp.a, r)
        match.aoh <- extract (sp.ra, pts)
        match.aoh <- length(match.aoh)
        pp <- as.numeric(match.aoh) / as.numeric(match.eoo)
        mp <- cellStats(sp.ra, sum) /cellStats(sp.re, sum)
        sp[i,2] <- match.eoo 
        sp[i,3] <- match.aoh
        sp[i,4] <- pp 
        sp[i,5] <- mp
        sp[i,6] <- pp - mp
      }}  
    return(sp)
plot(as.matrix(sp[,5]), as.matrix(sp[,4]), xlab = "Model Prevalence", ylab = "Point Prevalence", xlim = c(0, 1.0), ylim = c(0, 1), pch=1, cex=sqrt(as.matrix(sp[,2])))
abline(a = -0.0, b =1, lwd=1.0, lty=2)
}
  
  if(class(eoo) == "RasterLayer" & class(aoh) == "RasterLayer"){
    list <- list()
    for (i in 1:length(eoo)){
      sp.re <- eoo[i, ]
      ex <- extent(sp.re)
      occ <- gbif(names(sp.re), ext=ex, geo=T)
      pts <- as.data.frame(cbind(occ$lon, occ$lat)) # coordinates corresponding to specified extent
      coordinates(pts) <- ~ V1 + V2
      match.eoo <- extract (sp.re, pts)
      match.eoo <- as.data.frame(table(match.eoo))
      if (sum(match.eoo, na.rm=T) == 0) {
        warning (paste('No occurrence record found inside the eoo raster for', as.character(spp[i])))
        sp[i,2] <- NA
      }
      
      if (sum(match.eoo, na.rm=T) != 0){
        match.eoo <- leght(match.eoo)
        sp.a <- a[i, ]
        sp.ra <- rasterize(sp.a, r)
        match.aoh <- extract (sp.ra, pts)
        match.aoh <- length(match.aoh)
        pp <- as.numeric(match.aoh) / as.numeric(match.eoo)
        mp <- cellStats(sp.ra, sum) /cellStats(sp.re, sum)
        sp[i,2] <- match.eoo 
        sp[i,3] <- match.aoh
        sp[i,4] <- pp 
        sp[i,5] <- mp
        sp[i,6] <- pp - mp
      }}  
    return(sp)
    plot(as.matrix(sp[,5]), as.matrix(sp[,4]), xlab = "Model Prevalence", ylab = "Point Prevalence", xlim = c(0, 1.0), ylim = c(0, 1), pch=1, cex=sqrt(as.matrix(sp[,2])))
    abline(a = -0.0, b =1, lwd=1.0, lty=2)
  }
}


