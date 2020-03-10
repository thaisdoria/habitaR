#' Validation function
#'
#' Provide the summary of validation results based on comparison between an independent occurrence records of species and the extent of occurrence (EOO) and area of habitat (AOH) following Rondinini et al. (2011)
#'
#' @param eoo Species distribution data corresponding to original (or not refined) extent of occurrence (EOO) in formats ESRI shapefile (.shp) or raster file (.asc or .tif), or path for a folder to the shapes of 'Extent of Occurrence' (EOO) files. 
#' @param aoh Species 'Area of habitat' (AOH) in format ESRI shapefile or raster (created or not with function 'aoh'). 
#' @param resolution Numeric value to indicate the resolution prefered to generate the raster files.
#' @return The result is a data frame with a species-specific summary of informations about the number of records matching with the EOO ("MATCH.EOO) and with the AOH ("MATCH.AOH), as well the values of prevalence of points ("PP", Rondinini et al. 2011) through the proportion of occurrences spatially congruent with suitable cells, and of model prevalence ("MP", Rondinini et al. 2011), which represents EOOs proportion assigned as suitable). AOH predict species occurrences correctly when all available records match with its suitable cells (PP = 1). The evaluation of quality of AOHs is based on difference between PP and MP. If PP > MP, AOH performs better than EOO in predict presences (Ficetola et al. 2015).
#' @details The function gives the summary of data necessary to evaluate the quality of models as described and performed by Rondidini et al. (2011) and Ficetola et al. (2015) map the area of habitat within the geographical distribution (SpatialPolygon) given as the input data. as the refined distribution of a given species. This refinement is made considering the specific preference for habitats of a given species.
#' @references Rondinini, C., Di Marco, M., Chiozza, F., Santulli, G., Baisero, D., Visconti, P., Boitani, L. (2011). Global habitat suitability models of terrestrial mammals. Philosophical Transactions of the Royal Society B, 366, 2633–2641. Brooks, T. M, Fonseca, S.L. Pimm, Akçakaya, H.R., Buchanan, G.M., …, Rondinini C. (2019). Measuring Terrestrial Area of Habitat (AOH) and Its Utility for the IUCN Red List. Trends in Ecology &amp; Evolution, 34(11), 977–986. Ficetola, G. F., Rondinini, C., Bonardi, A., Baisero, D., &amp; Padoa-Schippa, E. (2015).Habitat availability for amphibians and extinction threat: a global analysis. Diversity and Distributions, 21(3), 302–311.
#' @author Thaís Dória & Daniel Gonçalves-Souza 
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


