
library("rgdal")
library("raster")
library(ncdf4)
library(sp)
library(rasterVis)
library(maptools)
library(maps)
setwd("C:/users/HAC809/Documents/Hector/Project/HI evaluation/dates")
#Reading the coordinates for crops and creating a spatialpoint object
coord<-list()
coord2<-list()
coord_pts<-list()
coordinates<-list.files(pattern=glob2rx("Coordinates*.csv"))

for (i in 1:length(coordinates)){
  coord[[i]]<-read.csv(coordinates[[i]],header =T)
  coord
  coord2[[i]]<-cbind(coord[[i]]$Longitude, coord[[i]]$Latitude)
  coord_pts[[i]]<-SpatialPointsDataFrame(coords=coord2[[i]],data.frame(coord[[i]]$HI),proj4string = CRS("+proj=longlat +datum=WGS84"))
}
setwd("Z:/general_inputs/planting_dates")
#Rasterizing the tif files
var<-list("harvest day","planting day","growing season length")
rasters<-list()
for(i in 1:length(var)){
rasters[[i]]<-brick("Wheat_ir_growing_season_dates_v1.23.nc4",varname=var[[i]])
rast<-stack(rasters)
}
summary(rast1)
rast1<-extract(rast,coord_pts[[4]],method='simple')
cropdf1<-data.frame(coord[[4]],rast1)
names(cropdf1)<-c("Lon","Lat", "Cont","type","HI", "dontknow","hdate","sdate", "growseason")
write.csv(cropdf1,"wheatharvest.csv")
summary(cropdf1)
head(cropdf1)

