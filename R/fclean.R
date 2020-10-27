#' Internal function to identify the species with less than 3 records.
#'
#' @param occ A 'spOcc' object with 'SpatialPoints' features.
#' @noRd

f.clean <- function (occ){
  options(digits=4)
  sp.clean <- nrow(coordinates(occ@coords)) < 3
}
