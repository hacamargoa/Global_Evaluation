
library("rgdal")
library(raster)
library(ncdf4)
library(sp)
library(rasterVis)
library(maptools)
library(maps)

setwd("C:/Users/Hector/Desktop/hsdates")
getwd
#inspect the netcdf

wwheati<-nc_open('wwh_ir_growing_season_dates_v2.nc4')
w<-"wwh_ir_growing_season_dates_v2.nc4"
print(wwheati)
summary(wwheati)

#Rasterizing the tif files winter wheat irrigated
raster_wir<-list()
var<-list("harvest day","planting day","growing season length")
for(i in 1:length(var)){
raster_wir[[i]]<-brick("wwh_ir_growing_season_dates_v2.nc4",varname=var[[i]])
rast_wir<-stack(raster_wir)
}

plot(raster_wir[[3]])
dfwwi<-as.data.frame(rast_wir, xy=T)
colnames(dfwwi)<-c("Lon", "Lat","harvest_day","planting_day","grow_seas_length")
summary(dfwwi)

#Rasterizing the tif files winter wheat rainfed
raster_wrf<-list()
var<-list("harvest day","planting day","growing season length")
for(i in 1:length(var)){
  raster_wrf[[i]]<-brick("wwh_rf_growing_season_dates_v2.nc4",varname=var[[i]])
  rast_wrf<-stack(raster_wrf)
}

plot(rast_wrf[[3]])
dfwwr<-as.data.frame(rast_wrf, xy=T)
colnames(dfwwr)<-c("Lon", "Lat","harvest_day","planting_day","grow_seas_length")
summary(dfwwr)

#Rasterizing the tif files spring wheat irrigated
raster_swir<-list()
var<-list("harvest day","planting day","growing season length")
for(i in 1:length(var)){
  raster_swir[[i]]<-brick("swh_ir_growing_season_dates_v2.nc4",varname=var[[i]])
  rast_swir<-stack(raster_swir)
}

plot(rast_swir[[3]])
dfswi<-as.data.frame(rast_swir, xy=T)
colnames(dfswi)<-c("Lon", "Lat","harvest_day","planting_day","grow_seas_length")
summary(dfswi)

#Rasterizing the tif files spring wheat rainfed
raster_swrf<-list()
var<-list("harvest day","planting day","growing season length")
for(i in 1:length(var)){
  raster_swrf[[i]]<-brick("swh_rf_growing_season_dates_v2.nc4",varname=var[[i]])
  rast_swrf<-stack(raster_swrf)
}

plot(rast_swrf[[3]])
dfswr<-as.data.frame(rast_swrf, xy=T)
colnames(dfswr)<-c("Lon", "Lat","harvest_day","planting_day","grow_seas_length")
summary(dfswr)


#Rasterizing the tif files
raster_mir<-list()
for(i in 1:length(var)){
  raster_mir[[i]]<-brick("Maize_ir_growing_season_dates_v1.25.nc4",varname=var[[i]])
  rast_mir<-stack(raster_mir)
}

plot(rast_mir[[3]])
dfmir<-as.data.frame(rast_mir, xy=T)
colnames(dfmir)<-c("Lon", "Lat","harvest_day","planting_day","grow_seas_length")
summary(dfmir)

#Rasterizing the tif files
raster_mrf<-list()
for(i in 1:length(var)){
  raster_mrf[[i]]<-brick("Maize_rf_growing_season_dates_v1.25.nc4",varname=var[[i]])
  rast_mrf<-stack(raster_mrf)
}
plot(rast_mrf[[3]])
dfmrf<-as.data.frame(rast_mrf, xy=T)
colnames(dfmrf)<-c("Lon", "Lat","harvest_day","planting_day","grow_seas_length")
summary(dfmrf)

#/////Sewing date/////
sdates<-data.frame(dfwwi$Lon,dfwwi$Lat,dfwwr$planting_day,dfwwi$planting_day,dfswr$planting_day,dfswi$planting_day,dfmrf$planting_day,dfmir$planting_day)
head(sdates)
colnames(sdates)<-c("Lon","Lat","TeWW","TeWWi","TeSW","TeSWi","TeCo","TeCoi")
write.csv(sdates,"sdates.csv")

#//////Harvest dates///////
hdates<-data.frame(dfwwi$Lon,dfwwi$Lat,dfwwr$harvest_day,dfwwi$harvest_day,dfswr$harvest_day,dfswi$harvest_day,dfmrf$harvest_day,dfmir$harvest_day)
head(hdates)
colnames(hdates)<-c("Lon","Lat","TeWW","TeWWi","TeSW","TeSWi","TeCo","TeCoi")
write.csv(hdates,"hdates.csv")


