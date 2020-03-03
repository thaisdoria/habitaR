f.ras <- function (x=list.clean, r=r0.05, shape=caatinga, e=extent(-44.8, -34.8, -16.6, -2.2), write=TRUE){
  rasterized<-list()
   for (i in 1:length(x)){
    xy<-as.data.frame(x[[i]][,2:3]) 
    r <- rasterize(xy, r, background=0, fun='count') # each coordinate point present in each cell is counted
    r[r > 1] <- 1 # to remove duplicates considering as one all coordinates that is present in the same cell
    c <- crop(r, shape)
    m <- mask(c, shape)
    r.occu <- extend(m,e)
    id <- names(x[i])
    if (write==TRUE)
      {
        writeRaster(r.occu,paste(id,"GBIFPoints.asc",sep = ""),format="ascii", overwrite=T)
      } else {
      rasterized[[i]]<-r.occu
      }
    }
 return(rasterized)
}
