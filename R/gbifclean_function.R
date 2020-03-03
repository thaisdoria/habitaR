f.clean <- function(lista.gbif){
  if (length(lista.gbif[[1]]) != 0){
    DF=NULL
    for (i in 1:length(lista.gbif)) {
      DF=rbind(DF, lista.gbif[[i]])
      x=as.numeric(as.matrix(DF[,2]))
      y=as.numeric(as.matrix(DF[,3]))
      coord=cbind(x, y)
    }
    spp=as.data.frame(DF[,1])
    all=as.data.frame(cbind(spp, coord))
    list.clean=split(all, all$`DF[, 1]`)
    return(list.clean)}

  if (length(lista.gbif[[1]]) == 0) {
    DF=NULL
    for (i in 2:length(lista.gbif)){
      DF=rbind(DF, lista.gbif[[i]])
      x=as.numeric(as.matrix(DF[,2]))
      y=as.numeric(as.matrix(DF[,3]))
      coord=cbind(x, y)
    }
    spp=as.data.frame(DF[,1])
    all=as.data.frame(cbind(spp, coord))
    list.clean=split(all, all$`DF[, 1]`)
    return(list.clean)}}
