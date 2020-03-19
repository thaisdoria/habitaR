#' Habitat preference
#'
#' Provide the area of habitat (AOH) of a given species through refinement of its
#' known geographic distribution
#' @usage prefHab(sp, key, cat = NULL, progress = F)
#' @param sp
#' @param key
#' @param cat
#' @param progress
#'
#' @import rredlist
#' @return The result is a list with two elements. The first element is a
#' data.frame detailing if the function was able (1) or not (0) to refinate the
#' species distribution. The second element is a list of RasterLayer or
#' SpatialPolygons object representing the refinated distribution of the species.
#' For the RasterLayer the value of 0 indicates the cells where the the species
#' were considerated present before the refinement. Values higher than 0 indicates
#' the coverage of cell by the spatial distribution after the refinement.
#' @details The function map the area of habitat within the geographical
#' distribution (SpatialPolygon) given as the input data. as the refined
#' distribution of a given species. This refinement is made considering the
#' specific preference for habitats of a given species.
#' @examples
#' data(sd_amph)
#' data(lc)
#' data(habpref)
#' data(al)
#' data(alpref)
#' ref_data <- aoh(eoo = sd_amph, lc.rec = lc , matrix.hab.pref = habpref,
#' alt.map = al, matrix.alt.pref = alpref)
#' @author Daniel Gonçalves-Souza & Thaís Dória
#' @export prefHab
#'

prefHab <- function(sp, key, cat = NULL, progress = FALSE){
  if(is.null(cat) == TRUE){
    cat <- class_ref
  }
  df <- data.frame(matrix(nrow = length(sp), ncol = length(unique(cat$User_class)) + 1))
  colnames(df) <- c('species', unique(cat$User_class))
  pb <- txtProgressBar(min = 0, max = length(sp), style = 3)

  for (i in 1:length(sp)){
    hab.spp <- rl_habitats(sp[i], key = key)
    if(length(hab.spp$result) == 0){
      warning(paste('No habitat information was found for', sp[i]))
    }
    code <- hab.spp$result$code
    yes.hab <- cat[, 1] %in% code
    cat.hab <- names(df)[2:ncol(df)] %in% unique(cat$User_class[yes.hab])
    df[i, 2:ncol(df)] <- as.numeric(cat.hab)
    df[i, 1] <- sp[i]
    if(progress == TRUE) {
      setTxtProgressBar(pb, i)
    }
  }
  df <- df[, colnames(df) != 0]
  return(df)
}
