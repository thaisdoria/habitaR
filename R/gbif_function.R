
f.gbif <- function (spp){
  for (i in 1:length(spp)) {
    p.occ=gbif(spp, ext=ex, geo=T, download=T)
    return(as.data.frame(cbind(p.occ$species, p.occ$lon, p.occ$lat))) # returning only coordinates and name of species
  }}
