f.ras <- function (list.clean){
  for (i in 1:length(list.clean)){
    r <- rasterize(as.data.frame(list.clean[[i]][,2:3]), r0.05, background=0, fun='count') # each coordinate point present in each cell is counted
    r[r > 1] <- 1 # to remove duplicates considering as one all coordinates that is present in the same cell
    c <- crop(r, caatinga)
    m <- mask(c, caatinga)
    e <- extent(-44.8, -34.8, -16.6, -2.2)
    r.occu.caat <- extend(m,e)
    nome <- names(list.clean[i])
    writeRaster(r.occu.caat,paste(nome,"GBIFPoints.asc",sep = ""),format="ascii", overwrite=T)
  }}
