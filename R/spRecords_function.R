#' spRecords
#'
#'Gets occurrence data of species from databases and turn into a
#'SpatialPoints object#'
#'@usage spRecords(sp, ex = NULL, dataBase = c('GBIF', 'VertNet', 'BISON'),
#' progress = FALSE)
#' @param sp a vector containing the names of the species.
#' @param ex The extent of the output Spatial points. Should be an extent object
#'  or a list containing extent objects.
#' @param dataBase vector cortaining which databases the function should search
#' for occurrence data.
#' @return \code{spRecords} returns a list containing SpatialPoints objects of
#' the occurrence of the species.
#' @examples
#'name <- c('Orcinus orca', 'Bothrops jararaca',)
#'area <- list(extent(c(-179.8733, 79.3333, -8.6333, 8.65083 )), NULL)
#'occ.shps <- spRecords(sp = name, ex = area, dataBase = c('GBIF', 'BISON'))
#'
#' @encoding UTF-8
#' @author Daniel Gonçalves-Souza & Thaís Dória
#' @export spRecords
#' @import raster
#' @import rbison
#' @import rvertnet
#' @import dismo
#' @importFrom utils txtProgressBar setTxtProgressBar


spRecords <- function(sp, ex = NULL, dataBase = c('GBIF', 'VertNet', 'BISON'),
                      progress = FALSE) {
  {
    if (missing(sp))
      stop('sp is missing')
    if (any(!dataBase %in% c('GBIF', 'VertNet', 'BISON')))
      stop('dataBase name is invalid')
    if (class(ex) != 'list')
      ex <- list(ex)
  }

  result <- list()
  if (progress) {
    pb <- txtProgressBar(min = 0, max = length(sp), style = 3)
  }

  for (i in 1:length(sp)) {
    occ.all <- list()
    gen <- unlist(strsplit(sp[i], ' '))[1]
    es <- unlist(strsplit(sp[i], ' '))[2]

    # GBIF
    if (any(dataBase %in% 'GBIF')) {
    occ.gb <- suppressMessages(gbif(gen, es, ext = ex[[i]], geo = T))
    occ.all[[1]] <- data.frame(lon = occ.gb$lon, lat = occ.gb$lat)
    }

    # VetNet
    if (any(dataBase %in% 'VertNet')) {
      occ.vn <- suppressMessages(searchbyterm(genus = gen, specificepithet = es))
      occ.vn <- occ.vn$data
      occ.all[[2]] <- data.frame(lon = as.numeric(occ.vn$decimallongitude),
                      lat = as.numeric(occ.vn$decimallatitude),
                      stringsAsFactors = FALSE)
    }

    # BISON
    if (any(dataBase %in% 'BISON')) {
      occ.bs <- suppressMessages(bison(sp[i]))
      occ.all[[3]] <- data.frame(lon = occ.bs$points$decimalLongitude,
                      lat = occ.bs$points$decimalLatitude,
                      stringsAsFactors = FALSE)
    }

    occ.all <- do.call(rbind, occ.all)
    occ.all <- na.omit(occ.all)
    occ.all <- occ.all[!duplicated(paste(occ.all[, 1], occ.all[, 2])), ]
    coordinates(occ.all) <- ~ lon + lat

    if (!is.null(ex[[i]])) {
    occ.all <- crop(occ.all, ex[[i]])
    }
    result[[i]] <- occ.all

    if (progress) {
      setTxtProgressBar(pb, i)
    }
  }
  names(result) <- sp
  return(result)
}



