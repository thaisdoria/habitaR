#'  readSHP function
#'
#' Read the spatial distribution shapefiles (ESRI shapefile format) of species
#' from a specified folder and provide a SpatialPolygonDataFrame object.
#'
#' @usage readShp (path)
#' @param path Path for a folder with spatial distribution shapefiles
#' (ESRI shapefile format). The name of the species must be on the second
#' column of the attribute table of the shapefile.
#' @return A object from a class of 'SpatialPolygonDataFrame'
#' @author Thaís Dória & Daniel Gonçalves-Souza
#' @export readShp

readShp <- function(path){
  if(substr(path, nchar(path), nchar(path)) == '/'){
    path <- substr(path, 1, nchar(path) - 1)
  }
  files.sp <- list.files(path, pattern = ".shp$")
  files.sp <- gsub(".shp","", files.sp)
  sps <- list()
  for (i in 1:length(files.sp)){
    sps[[i]] <- readOGR(dsn = path,
                        layer = files.sp[i])
  }
  if(length(sps) > 1){
    sd <- do.call(bind, sps)
    return(sd)
  }
  if(length(sps) == 1){
    sd <- sps[[1]]
    return(sd)
  }
}


