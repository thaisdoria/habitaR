#' readRas: read raster files
#'
#' Read the spatial distribution raster files (.tif or .asc format) of species
#' from a specified folder and provide a list with the raster files.
#'
#' @usage readRas (path)
#' @param path Path for a folder with spatial distribution raster files
#' (.asc or .tif format).
#' @return \code{readRas} retrurns a list of raster files or a raster file.
#' @encoding UTF-8
#' @author Thaís Dória & Daniel Gonçalves-Souza
#' @export readRas

readRas <- function(path){

  if(substr(path, nchar(path), nchar(path)) == '/'){
    path <- substr(path, 1, nchar(path) - 1)
  }

  files.sp <- list.files(path, pattern = ".tif$|.asc$",
                         full.names = T)

  sps <- lapply(files.sp, raster)
  names.sp <- list.files(path, pattern = ".tif$|.asc$")
  names(sps) <- gsub("*\\.asc|*\\.tif", '', names.sp)

  return(sps)
}

