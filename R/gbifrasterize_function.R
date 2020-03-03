f.ras <- function (x=list.clean, r=r0.05, write=TRUE){
  for (i in 1:length(x)){
    r <- rasterize(as.data.frame(x[[i]][,2:3]), r, background=0, fun='count') # each coordinate point present in each cell is counted
    r[r > 1] <- 1 # to remove duplicates considering as one all coordinates that is present in the same cell
    c <- crop(r, caatinga)
    m <- mask(c, caatinga)
    e <- extent(-44.8, -34.8, -16.6, -2.2)
    r.occu.caat <- extend(m,e)
    nome <- names(x[i])
    if (write==TRUE)
      {
        writeRaster(r.occu.caat,paste(nome,"GBIFPoints.asc",sep = ""),format="ascii", overwrite=T)
      } else {
      return(r.occu.caat)
      }
    
  }}
