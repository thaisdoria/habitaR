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
  
  if(class(eoo) =="SpatialPolygonsDataFrame" & class (aoh) == "SpatialPolygonsDataFrame"){
    # rasterize the shapefiles
    for (i in 1:length(eoo)){
      sp.e <- eoo[i, ]
      r <- raster()
      extent(r) <- extent(sp.e)
      res(r) <- resolution
      sp.re <- rasterize(sp.e, r)
      sp.re [sp.re > 1] <- 1
      sp.a <- aoh[i, ]
      sp.ra <- rasterize(sp.a, r)
      { 
        # download occurrences from gbif based on extent of EOO (to restrict the search to inside of original distribution)
        ex <- extent(sp.re)
        names(sp.re) <- eoo@data[1,2]
        occ <- gbif(names(sp.re), ext=ex, geo=T)
        pts <- as.data.frame(cbind(occ$lon, occ$lat)) # coordinates corresponding to specified extent
        coordinates(pts) <- ~ V1 + V2
        match.eoo <- extract (sp.re, pts)
        match.eoo <- as.data.frame(table(match.eoo))
        match.aoh <- extract (sp.ra, pts)
        match.aoh <- as.data.frame(table(match.aoh))
        pp <- as.numeric(match.aoh) / as.numeric(match.eoo)
        mp <- cellStats(sp.ra, sum) /cellStats(sp.re, sum)
        val <- pp - mp
      }}
  }
  
  if(class(eoo) =="RasterLayer" & class (aoh) =="RasterLayer"){
    for (i in 1:length(aoh)){
      sp.re <- eoo[1, ]
      ex <- extent(sp.re)
      occ <- gbif(names(sp.re), ext=ex, geo=T)
      pts <- as.data.frame(cbind(occ$lon, occ$lat)) # coordinates corresponding to specified extent
      coordinates(pts) <- ~ V1 + V2
      match.eoo <- extract (sp.re, pts)
      match.eoo <- as.data.frame(table(match.eoo))
      match.aoh <- extract (sp.ra, pts)
      match.aoh <- as.data.frame(table(match.aoh))
      pp <- as.numeric(match.aoh) / as.numeric(match.eoo)
      mp <- cellStats(sp.ra, sum) /cellStats(sp.re, sum)
      val <- pp - mp
    }}
  
  result <- NULL
  result[i,] <- cbind(result, val)
}

  
  