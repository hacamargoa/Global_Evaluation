
library("rgdal")
library("raster")
library(ncdf4)
library(sp)
library(rasterVis)
library(maptools)
library(maps)
setwd("C:/users/HAC809/Documents/Hector/Project/HI evaluation/dates")

#Rasterizing the tif files
raster_wir<-list()
var<-list("harvest day","planting day","growing season length")
for(i in 1:length(var)){
raster_wir[[i]]<-brick("Wheat_ir_growing_season_dates_v1.23.nc4",varname=var[[i]])
rast_wir<-stack(raster_wir)
}

df1<-as.data.frame(rast_wir, xy=T)
colnames(df1)<-c("Lon", "Lat","harvest_day","planting_day","grow_seas_length")

#Rasterizing the tif files
raster_wrf<-list()
var<-list("harvest day","planting day","growing season length")
for(i in 1:length(var)){
  raster_wrf[[i]]<-brick("Wheat_rf_growing_season_dates_v1.23.nc4",varname=var[[i]])
  rast_wrf<-stack(raster_wrf)
}

df2<-as.data.frame(rast_wrf, xy=T)
colnames(df2)<-c("Lon", "Lat","harvest_day","planting_day","grow_seas_length")

#Rasterizing the tif files
raster_cir<-list()
var<-list("harvest day","planting day","growing season length")
for(i in 1:length(var)){
  raster_cir[[i]]<-brick("Maize_ir_growing_season_dates_v1.23.nc4",varname=var[[i]])
  rast_cir<-stack(raster_cir)
}
df3<-as.data.frame(rast_cir, xy=T)
colnames(df3)<-c("Lon", "Lat","harvest_day","planting_day","grow_seas_length")

#Rasterizing the tif files
raster_crf<-list()
var<-list("harvest day","planting day","growing season length")
for(i in 1:length(var)){
  raster_crf[[i]]<-brick("Maize_rf_growing_season_dates_v1.23.nc4",varname=var[[i]])
  rast_crf<-stack(raster_crf)
}
df4<-as.data.frame(rast_crf, xy=T)
colnames(df4)<-c("Lon", "Lat","harvest_day","planting_day","grow_seas_length")

#/////Sewing date/////
sdates<-data.frame(df2$Lon,df2$Lat,df2$planting_day,df4$planting_day,df1$planting_day,df3$planting_day)
head(sdates)
colnames(sdates)<-c("Lon","Lat","TeWW","TeCo","TeWWi","TeCoi")
sdates<-sdates[c(-3,-5)]
sw=read.table("sdates.txt",h=T)
head(sw)
sw<-sw[c(-5,-8,-9,-10,-11)]
sdate<-merge(sdates,sw,all=TRUE)
head(sdate)
write.csv(sdate,"sdates.csv")

#//////Harvest dates///////
hdates<-data.frame(df2$Lon,df2$Lat,df2$harvest_day,df4$harvest_day,df1$harvest_day,df3$harvest_day)
head(hdates)
colnames(hdates)<-c("Lon","Lat","TeWW","TeCo","TeWWi","TeCoi")
hdates<-hdates[c(-3,-5)]
hw=read.table("hdates.txt",h=T)
head(hw)
hw<-hw[c(-5,-8,-9,-10,-11)]
hdate<-merge(hdates,hw,all=TRUE)
head(hdate)
write.csv(hdate,"hdates.csv")


