#' Internal function to identify the species with less than 3 records.
#'
#' @param occ A 'spOcc' object with 'SpatialPoints' features.
#' @noRd

f.clean2 <- function (occ){
  options(digits=4)
  sp.clean <- nrow(coordinates(occ@coords)) < 3
}
