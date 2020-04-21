#' Validation function
#'
#' Provide the summary of validation results based on comparison between an
#' independent occurrence records of species and the extent of occurrence (EOO)
#' and area of habitat (AOH) following Rondinini et al. (2011)
#' @usage
#' # method for signature 'SpatialPolygonsDataFrame' (eoo and aoh)
#' aohVal(eoo, aoh, resolution, plot=TRUE, progress=FALSE)
#' # method for signature 'SpatialPolygonsDataFrase' (eoo) and 'RasterStack' (aoh)
#' aohVal(eoo, aoh, plot=TRUE, progress=FALSE)
#' # method for signature 'RasterStack' (eoo and aoh)
#' aohVal(eoo, aoh, spplist, plot=TRUE, progress=FALSE)
#' #' @param eoo Spatial distribution data of the species representing the original
#' (i.e. not refined) extent of occurrence (EOO). It might correspond to
#' SpatialPolygons in ESRI shapefile (.shp), to RasterStack file (.asc or .tif) or to a
#' path for a folder with EOO shapefiles (.shp). If the shapefile is used, the
#' name of the species must be on the second column of the attribute table
#' of the shapefile.
#' @param aoh Spatial distribution data of the species representing the area
#' of habitat (AOH). It might correspond to SpatialPolygons in ESRI shapefile (.shp),
#' to RasterStack file (.asc or .tif), created or not with function
#' \code{\link[habitaR]{aoh}}, or might correspond to a path for a folder with EOO shapefiles (.shp).
#' If the shapefile is used, the name of the species must be on the second column of the attribute table.
#' @param resolution Numeric value to indicate the resolution prefered to generate
#' the raster files. Only used if both spatial data ('eoo' and 'aoh') are in shapefile format.
#' If 'eoo' is a shapefile and the 'aoh' is a raster, the resolution of latter is considered.
#' @param spplist Species list (character type) in the same order as the Rasterstack of
#' distribution data.
#' @param plot (logical) Whether the output should also return a plot representing the
#' graphical result of validation (see Rondinini et al. 2011). Default is TRUE.
#' @param progress (logical) a bar showing the progress of the function. Default is FALSE.
#' @import dismo
#' @import sp
#' @import ggplot2
#' @importFrom utils txtProgressBar setTxtProgressBar
#' @return \code{aohVal} If plot=TRUE, the function return a list with two objects.
#' The first is a data.frame with a summary of results for each species which aoh was evaluated.
#' It gives the number of records matching with the EOO ("MATCH.EOO) and with the AOH ("MATCH.AOH)
#' and the values of prevalence of points ("PP", Rondinini et al. 2011) through the proportion of
#' occurrences spatially congruent with suitable cells, and of model prevalence ("MP",
#' Rondinini et al. 2011), which represents EOOs proportion assigned as suitable). AOH predict
#' species occurrences correctly when all available records match with its suitable cells (PP = 1).
#' The evaluation of AOHs performance is based on difference between PP and MP. If PP > MP, AOH
#' performs better than EOO in predict presences (Ficetola et al. 2015).
#' The second object is a ggplot with a graphical representation of validation results
#' (see Rondinini et al. 2011).
#' If plot=FALSE, the return only the data.frame with the summary of results.
#' @details The function give the summary of validation steps followed to evaluate
#' the quality of models as described and performed by Rondidini et al. (2011).
#' If plot=FALSE, the function return a data.frame
#' and Ficetola et al. (2015)
#' @examples
#' # Example for signature 'SpatialPolygonsDataFrame' (eoo and aoh)
#' val_data1 <- aohVal(eoo = eoo_amphShp, aoh = aoh_amphShp, resolution = 0.05, plot=TRUE, progress = TRUE)
#' # Example for signature 'SpatialPolygonsDataFrame' (eoo) and 'RasterLayer' (aoh)
#' val_data2 <- aohVal(eoo = eoo_amphShp, aoh = aoh_amphRas, plot=TRUE, progress = TRUE)
#' # Example for signature 'RasterLayer' (eoo and aoh)
#' val_data3 <- aohVal(eoo = eoo_amphRas, aoh = aoh_amphRas, spplist = spplist_amph, plot = TRUE, progress = TRUE)
#' @references
#' 1. Rondinini, C., Di Marco, M., Chiozza, F., Santulli, G., Baisero, D., Visconti, P., Boitani, L. (2011). Global habitat suitability models of terrestrial mammals. Philosophical Transactions of the Royal Society B, 366, 2633–2641.
#' 2. Brooks, T. M, Fonseca, S.L. Pimm, Akçakaya, H.R., Buchanan, G.M., …, Rondinini C. (2019). Measuring Terrestrial Area of Habitat (AOH) and Its Utility for the IUCN Red List. Trends in Ecology &amp; Evolution, 34(11), 977–986.
#' 3. Ficetola, G. F., Rondinini, C., Bonardi, A., Baisero, D., &amp; Padoa-Schippa, E. (2015).Habitat availability for amphibians and extinction threat: a global analysis. Diversity and Distributions, 21(3), 302–311.
#' @author Thaís Dória & Daniel Gonçalves-Souza
#'
#' @export aohVal

aohVal <- function (eoo, aoh, resolution=NULL, spplist=NULL, plot=TRUE){

  {
    if (missing(eoo))
      stop("eoo is missing")
    if (missing(aoh))
      stop("aoh is missing")
    if(class(eoo) == "SpatialPolygonsDataFrame" & class(aoh) == "SpatialPolygonsDataFrame" & is.null(resolution))
      stop("resolution is required if 'eoo' and 'aoh' are in shapefile format")
    if(class(eoo) == "SpatialPolygonsDataFrame" & class(aoh) == "RasterStack" & !is.null(resolution))
      stop("resolution is NOT required if 'aoh' is a rasterstack (resolution will be the same as that from 'aoh')")
    if(class(eoo) == "RasterStack" & class(aoh) == "RasterStack" & missing(spplist))
      stop("spplist is required if eoo and aoh are both in RasterStack format")
    if(class(eoo) == "RasterStack" & class(aoh) == "RasterStack" & !is.null(resolution))
      stop("resolution will be the same than 'eoo' and 'aoh'")
  }

  if(is.character(eoo)){
    files.sp.e <- list.files(eoo, pattern = ".shp$")
    files.sp.e <- gsub(".shp","", files.sp.e)
    spse <- list()
    for (i in 1:length(files.sp.e)){
      spse[[i]] <- readOGR(dsn = eoo,
                           layer = files.sp.e[i])
    }
    eoo <- do.call(bind, spse)
  }

  if(is.character(aoh)){
    files.sp.a <- list.files(aoh, pattern = ".shp$")
    files.sp.a <- gsub(".shp","", files.sp.a)
    spsa <- list()
    for (i in 1:length(files.sp.a)){
      spsa[[i]] <- readOGR(dsn = aoh,
                           layer = files.sp.a[i])
    }
    aoh <- do.call(bind, spsa)
  }

  if(class(eoo) == "SpatialPolygonsDataFrame" & class(aoh) == "SpatialPolygonsDataFrame"){
    # rasterize the shapefiles
    dfRes <- data.frame(matrix(ncol = 6, nrow = length(eoo)))
    colnames(dfRes) <- c("Species", "MATCH.EOO", "MATCH.AOH", "PP", "MP", "PP-MP")
    dfRes[,1] <- eoo@data[,2]
    if(progress == TRUE){
      pb <- txtProgressBar(min = 0, max = length(eoo), style = 3)
    }
    for (i in 1:length(eoo)){
      sp.e <- eoo[i, ]
      r <- raster()
      extent(r) <- extent(sp.e)
      res(r) <- resolution
      sp.re <- rasterize(sp.e, r)
      sp.re [sp.re > 1] <- 1
      plot(sp.re)
      # download occurrences from gbif based on extent of EOO (to restrict the search to inside of original distribution)
      ex <- extent(sp.re)
      nm <- sp.e@data[1,2]
      occ <- gbif(as.character(nm), ext=ex, geo=T)
      pts <- as.data.frame(cbind(occ$lon, occ$lat)) # coordinates corresponding to specified extent
      coordinates(pts) <- ~ V1 + V2
      plot(pts, add=T)
      match.eoo <- extract (sp.re, pts)

      if (sum(match.eoo, na.rm=T) != 0){
        match.eoo <- sum(match.eoo, na.rm=T)
        sp.a <- aoh[i, ]
        sp.ra <- rasterize(sp.a, r)
        match.aoh <- extract (sp.ra, pts)
        match.aoh <- sum(match.aoh, na.rm=T)
        pp <- as.numeric(match.aoh) / as.numeric(match.eoo)
        mp <- cellStats(sp.ra, sum) /cellStats(sp.re, sum)
        dfRes[i,2] <- match.eoo
        dfRes[i,3] <- match.aoh
        dfRes[i,4] <- pp
        dfRes[i,5] <- mp
        dfRes[i,6] <- pp - mp
      }

      if (sum(match.eoo, na.rm=T) == 0) {
        warning (paste('No occurrence record found inside the eoo raster for', as.character(nm)))
        dfRes[i,2:6] <- NA
      }
      if(progress == TRUE){
        setTxtProgressBar(pb, i)
      }
    }

  if (plot == FALSE){
  return(dfRes)}

  if (plot == TRUE){
    gg <- ggplot(data=dfRes, aes(x=MP, y=PP)) +
      xlab("Model Prevalence (MP)") +
      ylab("Point Prevalence (PP)") +
      xlim(c(0, 1.0)) +
      ylim(c(0, 1.0)) +
      geom_point(
        bg="white",
        shape=21,
        alpha=1.0,
        size=sqrt(dfRes$MATCH.EOO),
        stroke = 1.0) +
      geom_abline(
        a=0.0,
        b=1,
        lwd=0.8,
        lty=2,
        color="dark grey") +
      theme_bw() +
      theme(panel.grid.major.y = element_blank(), # argumentos para remover linhas de grade
            panel.grid.minor.y = element_blank(),
            panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"),
            axis.title.y = element_text(size = rel(1.2), angle = 90, margin=margin(t = 0, r = 20, b = 0, l = 0)),
            axis.title.x = element_text(size = rel(1.2), margin=margin(t = 20, r = 0, b = 0, l = 0))
      )
    plot(gg)
    return(list(dfRes, gg))
  }
 }

  if(class(eoo) == "SpatialPolygonsDataFrame" & class(aoh) == "RasterStack"){
    # rasterize the shapefiles
    dfRes <- data.frame(matrix(ncol = 6, nrow = length(eoo)))
    colnames(dfRes) <- c("Species", "MATCH.EOO", "MATCH.AOH", "PP", "MP", "PP-MP")
    dfRes[,1] <- eoo@data[,2]
    if(progress == TRUE){
      pb <- txtProgressBar(min = 0, max = length(eoo), style = 3)
    }
    for (i in 1:length(eoo)){
      sp.e <- eoo[i, ]
      r <- raster()
      extent(r) <- extent(sp.e)
      res(r) <- res(aoh[[i]])
      sp.re <- rasterize(sp.e, r)
      sp.re [sp.re > 1] <- 1
      plot(sp.re)
      # download occurrences from gbif based on extent of EOO (to restrict the search to inside of original distribution)
      ex <- extent(sp.re)
      nm <- sp.e@data[1,2]
      occ <- gbif(as.character(nm), ext=ex, geo=T)
      pts <- as.data.frame(cbind(occ$lon, occ$lat)) # coordinates corresponding to specified extent
      coordinates(pts) <- ~ V1 + V2
      plot(pts, add=T)
      match.eoo <- extract (sp.re, pts)

      if (sum(match.eoo, na.rm=T) != 0){
        match.eoo <- sum(match.eoo, na.rm=T)
        sp.ra <- aoh[[i]]
        match.aoh <- extract (sp.ra, pts)
        match.aoh <- sum(match.aoh, na.rm=T)
        pp <- as.numeric(match.aoh) / as.numeric(match.eoo)
        mp <- cellStats(sp.ra, sum) /cellStats(sp.re, sum)
        dfRes[i,2] <- match.eoo
        dfRes[i,3] <- match.aoh
        dfRes[i,4] <- pp
        dfRes[i,5] <- mp
        dfRes[i,6] <- pp - mp
      }

      if (sum(match.eoo, na.rm=T) == 0) {
        warning (paste('No occurrence record found inside the eoo raster for', as.character(nm)))
        dfRes[i,2:6] <- NA
      }
      if(progress == TRUE){
        setTxtProgressBar(pb, i)
      }
    }

    if (plot == FALSE){
      return(dfRes)}

    if (plot == TRUE){
      gg <- ggplot(data=dfRes, aes(x=MP, y=PP)) +
        xlab("Model Prevalence (MP)") +
        ylab("Point Prevalence (PP)") +
        xlim(c(0, 1.0)) +
        ylim(c(0, 1.0)) +
        geom_point(
          bg="white",
          shape=21,
          alpha=1.0,
          size=sqrt(dfRes$MATCH.EOO),
          stroke = 1.0) +
        geom_abline(
          a=0.0,
          b=1,
          lwd=0.8,
          lty=2,
          color="dark grey") +
        theme_bw() +
        theme(panel.grid.major.y = element_blank(), # argumentos para remover linhas de grade
              panel.grid.minor.y = element_blank(),
              panel.grid.major.x = element_blank(),
              panel.grid.minor.x = element_blank(),
              plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"),
              axis.title.y = element_text(size = rel(1.2), angle = 90, margin=margin(t = 0, r = 20, b = 0, l = 0)),
              axis.title.x = element_text(size = rel(1.2), margin=margin(t = 20, r = 0, b = 0, l = 0))
        )
      plot(gg)
      return(list(dfRes, gg))
    }
  }

  if(class(eoo) == "RasterLayer" & class(aoh) == "RasterLayer"){
    # rasterize the shapefiles
    dfRes <- data.frame(matrix(ncol = 6, nrow = nlayers(eoo)))
    colnames(dfRes) <- c("Species", "MATCH.EOO", "MATCH.AOH", "PP", "MP", "PP-MP")
    dfRes[,1] <- spplist
    if(progress == TRUE){
      pb <- txtProgressBar(min = 0, max = length(eoo), style = 3)
    }
    for (i in 1:nlayers(eoo)){
      sp.re <- eoo[[i]]
      plot(sp.re)
      # download occurrences from gbif based on extent of EOO (to restrict the search to inside of original distribution)
      ex <- extent(sp.re)
      nm <- spplist[i]
      occ <- gbif(as.character(nm), ext=ex, geo=T)
      pts <- as.data.frame(cbind(occ$lon, occ$lat)) # coordinates corresponding to specified extent
      coordinates(pts) <- ~ V1 + V2
      plot(pts, add=T)
      match.eoo <- extract (sp.re, pts)

      if (sum(match.eoo, na.rm=T) != 0){
        match.eoo <- sum(match.eoo, na.rm=T)
        sp.ra <- aoh[[i]]
        match.aoh <- extract (sp.ra, pts)
        match.aoh <- sum(match.aoh, na.rm=T)
        pp <- as.numeric(match.aoh) / as.numeric(match.eoo)
        mp <- cellStats(sp.ra, sum) /cellStats(sp.re, sum)
        dfRes[i,2] <- match.eoo
        dfRes[i,3] <- match.aoh
        dfRes[i,4] <- pp
        dfRes[i,5] <- mp
        dfRes[i,6] <- pp - mp
      }

      if (sum(match.eoo, na.rm=T) == 0) {
        warning (paste('No occurrence record found inside the eoo raster for', as.character(nm)))
        dfRes[i,2:6] <- NA
      }
      if(progress == TRUE){
        setTxtProgressBar(pb, i)
      }
    }

    if (plot == FALSE){
      return(dfRes)}

    if (plot == TRUE){
      gg <- ggplot(data=dfRes, aes(x=MP, y=PP)) +
        xlab("Model Prevalence (MP)") +
        ylab("Point Prevalence (PP)") +
        xlim(c(0, 1.0)) +
        ylim(c(0, 1.0)) +
        geom_point(
          bg="white",
          shape=21,
          alpha=1.0,
          size=sqrt(dfRes$MATCH.EOO),
          stroke = 1.0) +
        geom_abline(
          a=0.0,
          b=1,
          lwd=0.8,
          lty=2,
          color="dark grey") +
        theme_bw() +
        theme(panel.grid.major.y = element_blank(), # argumentos para remover linhas de grade
              panel.grid.minor.y = element_blank(),
              panel.grid.major.x = element_blank(),
              panel.grid.minor.x = element_blank(),
              plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"),
              axis.title.y = element_text(size = rel(1.2), angle = 90, margin=margin(t = 0, r = 20, b = 0, l = 0)),
              axis.title.x = element_text(size = rel(1.2), margin=margin(t = 20, r = 0, b = 0, l = 0))
        )
      plot(gg)
      return(list(dfRes, gg))
    }
  }
}

