#' readRAS: read raster files
#'
#' Read the spatial distribution raster files (.tif or .asc format) of species
#' from a specified folder and provide a list with the raster files.
#'
#' @usage readRas (path)
#' @param path Path for a folder with spatial distribution raster files
#' (.asc or .tif format).
#' @return A list of raster files or a raster file
#' @encoding UTF-8
#' @author Thaís Dória & Daniel Gonçalves-Souza
#' @export readRas

readRas <- function(path){
  if(substr(path, nchar(path), nchar(path)) == '/'){
    path <- substr(path, 1, nchar(path) - 1)
  }

   files.sp <- list.files(path, pattern = ".tif$|.asc$")

   if(length(files.sp) > 1){
  sps <- list()
  for (i in 1:length(files.sp)){
    sps[[i]] <- raster(files.sp[i])
  }
  names(sps)<-sub("(\\w+\\s+\\w+).*", "\\1", files.sp)
  return(sps)
  }

  if(length(files.sp) == 1){
    sps <- raster(files.sp[1])
    names(sps)<-sub("(\\w+\\s+\\w+).*", "\\1", files.sp)
    return(sps)
  }
}

