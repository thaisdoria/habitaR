#'  readSHP function
#'
#' Read the spatial distribution shapefiles (ESRI shapefile format) of species
#' from a specified folder and provide a SpatialPolygonDataFrame object.
#'
#' @usage aoh(eoo, lc.rec, matrix.hab.pref, alt.map = NULL, matrix.alt.pref = NULL,
#' shp.out = FALSE, resolution = NULL, continuous = FALSE, threshold = 0.5,
#' extent.out = NULL, progress = FALSE)
#' @param path path for a folder with spatial distribution shapefiles
#' (ESRI shapefile format). The name of the species must be on the second
#' column of the attribute table of the shapefile.

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

t<-readShp("G:/Backup_DELLUFBA_ABRIL2020/Documents/TESTE")
