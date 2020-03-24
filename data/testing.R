devtools::install_github("klutometis/roxygen")
library(roxygen2)
library(available)
available("aoh")
available("habitaR")
available("trial")

library(habitaR)

devtools::create("D:/DOC/trial3")
devtools::document("D:/DOC/trial3")
warnings()
?aoh
devtools::check("C:/Thais/trial")
setwd("C:/Thais/trial")

sd[27,]

lc=raster("landcoverReclass.tif")
mat=read.csv("HabMat_Amphs_TRIAL.csv", sep=";", header=T)
colnames(mat)=c("spp",0,1,2,3,4)

aoh(sd, lc, mat, alt.map = NULL, shp.out = FALSE)
