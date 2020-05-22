#' Internal function to apply the point.in.polygon function based on boundaries of polygon
#' provided by user to check if the occurrences from its dataset are inside of this area.
#'
#' @param occ A list of dataframes with occurrences records in coordinates degrees
#' (longitude and latitude).
#' @noRd

f.cf.occ<-function(occ){
      cf.occ<-point.in.polygon(occ@coords[,1], occ@coords[,2], pol.x, pol.y,
                          mode.checked=FALSE)
}
