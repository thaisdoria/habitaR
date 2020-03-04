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

sps <- list()

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
  
  if(class(eoo)=="SpatialPolygonsDataFrame"){
        for (i in 1:length(eoo)){
        sp <- eoo[i, ]
        r <- raster()
        extent(r) <- extent(sp)
        res(r) <- resolution
        sp.r <- rasterize(sp, r, background = 0)
        sp.r[sp.r > 1] <- 1
        names(sp.r) <- eoo[1,]$binomial
        ex <- extent(sp.r)
        occ <- gbif(names(sp.r), ext=ex, geo=T)
                        }}

  
  
  
  
if(class(eoo)=="RasterLayer"){
  for (i in 1:length(eoo)){
    for (i in 1:length(eoo)){
      sp <- eoo[i, ]
   
    occ=gbif(eoo.r, ext=extent(eoo), geo=T, download=T)
  return(as.data.frame(cbind(p.occ$species, p.occ$lon, p.occ$lat))) # returning only coordinates and name of species}
  