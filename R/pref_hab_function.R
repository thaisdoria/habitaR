#' Habitat preference
#'
#' Obtains the habitat preference of species in IUCN's database and put in the
#' format required to the function \code{aoh}, based on the classification scheme of
#' the user's land use map.
#' @usage prefHab(sp, key = NULL, cat = NULL,
#' suitability = c('Suitable', 'Marginal', 'Unknown'),
#' mi = FALSE,
#' season = c('Resident', 'Breeding','Non-breeding', 'Passage',
#' 'Unknown'), progress = FALSE)
#' @param sp a vector containing the names or IUCN's IDs of the species.
#' @param key A IUCN API token. You can request a key in the website
#' http://apiv3.iucnredlist.org/api/v3/token
#' @param cat a two columns data frame. The first column must have the codes
#' from IUCN's Habitats Classification Scheme (Version 3.1). The second column
#' must have the numeric code of the user's land use map that is equivalent to
#' the IUCN's Habitats Classification Scheme. When there is no land use equivalent
#' to the IUCN's classification, use the value 0. If no data frame is provided the
#' function uses the dataset obtained through \code{data(class_ref)} as reference.
#' @param suitability a vector specifying which levels of habitat suitability
#' must be considered.
#' @param mi (logical) only suitable habitats of major importance must be
#' included?
#' @param season a vector specifying which seasons of habitat importance
#' must be considered.
#' @param progress (logical) a bar showing the progress of the function.
#'
#' @import rredlist
#' @importFrom utils txtProgressBar setTxtProgressBar
#' @return \code{prefHab} returns a data frame in which the first column is the
#' name of the species and the posterior columns are the codes of the user's
#' classification scheme. 1 indicates that the species has preference for a
#' given class of land use while 0 indicates that it has not.
#'
#' @examples
#' \donttest{
#' ## Not run:
#' sp <- c('Bradypus torquatus', 'Adelophryne maranguapensis', 'Dermatonotus muelleri')
#' prefHab(sp)
#' ## End
#' }
#' @author Daniel Gonçalves-Souza & Thaís Dória
#' @export prefHab
#'

prefHab <- function(sp, key = NULL, cat = NULL,
                    suitability = c('Suitable', 'Marginal', 'Unknown'),
                    mi = FALSE,
                    season = c('Resident', 'Breeding','Non-breeding', 'Passage',
                                           'Unknown'), progress = FALSE){
  if(mi == TRUE & sum(suitability %in% c('Marginal', 'Unknown')) > 0){
    stop('It is not possible to simultaneously include Marginal e Unknown habitat
         suitability and major importance habitats only')
  }
  if(is.null(cat) == TRUE){
    cat <- class_ref
  }
  if(ncol(cat) == 2){
    colnames(cat)[2] <- 'User_code'
  }

  df <- data.frame(matrix(nrow = length(sp), ncol = length(unique(cat$User_code)) + 1))
  colnames(df) <- c('species', unique(cat$User_code))
  if(progress == TRUE) {
    pb <- txtProgressBar(min = 0, max = length(sp), style = 3)
  }

  for (i in 1:length(sp)){
    hab.spp <- rl_habitats(sp[i], key = key)
    if(length(hab.spp$result) == 0){
      warning(paste('No habitat information was found for', sp[i]))
      df[i, 2:ncol(df)] <- rep(0, length(2:ncol(df)))
      df[i, 1] <- sp[i]
    }
    if(length(hab.spp$result) > 0){
      pref.tab <- hab.spp$result
      pref.tab <- pref.tab[pref.tab$suitability %in% suitability, ]
      pref.tab <- pref.tab[pref.tab$season %in% season, ]
      ifelse(mi, pref.tab <- pref.tab[is.na(pref.tab$majorimportance) == FALSE, ],
             pref.tab <- pref.tab)
      code <- hab.spp$result$code
      yes.hab <- cat[, 1] %in% code
      cat.hab <- names(df)[2:ncol(df)] %in% unique(cat$User_code[yes.hab])
      df[i, 2:ncol(df)] <- as.numeric(cat.hab)
      df[i, 1] <- sp[i]
    }
    if(progress == TRUE) {
      setTxtProgressBar(pb, i)
    }
  }
  df <- df[, colnames(df) != 0]
  return(df)
}
