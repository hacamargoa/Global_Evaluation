setwd("C:/Users/Hector/Documents/Pughtam-cropozone/Global_evaluation_inputs/Ray gridded/")
library(sp)
library(ncdf4)
library(raster)

#reading the Ray files and stacking them in raster files
crop<-c("Wheat","Maize","Rice")
list.crops<-list()
crop_rayR<-list()
for (j in 1:length(crop)){
  list.crops[[j]]<-list.files(pattern=crop[j])
  crop_ray<-list()
  for(i in 1:length(list.crops[[j]])){
    crop_ray[[i]]<-raster(list.crops[[j]][[i]],varname="Data")
    }
  crop_rayR[[j]]<-stack(crop_ray)
  crop_rayR[[j]]<-subset(crop_rayR[[j]],1:41)
  crop_rayR[[j]]<-extend(crop_rayR[[j]],bb)
  writeRaster(crop_rayR[[j]],paste0("C:/Users/Hector/Documents/Pughtam-cropozone/Global_evaluation_outputs/Ray_Yield_",crop[j],"_newcrops_1970-2010.nc"),"CDF")
  }
names(crop_rayR)<-paste0("Ray",crop)

prod_LPJR<-list()
area_LPJR<-list()
yield_LPJR<-list()
for (i in 1:length(ProdBG)){
  prod_LPJR[[i]]<-subset(ProdBG[[i]],10:50)
  area_LPJR[[i]]<-subset(ArNew[[i]],10:50)
  yield_LPJR[[i]]<-prod_LPJR[[i]]/area_LPJR[[i]]
  #writeRaster(ArNew[[i]],paste0("C:/Users/Hector/Documents/Pughtam-cropozone/Global_evaluation_outputs/SPAMest_",crop[i],"_1960_2010.nc"),"CDF")
  #writeRaster(ArNewi[[i]],paste0("C:/Users/Hector/Documents/Pughtam-cropozone/Global_evaluation_outputs/SPAMest_",crop[i],"irr_1960_2010.nc"),"CDF")
  #writeRaster(yield_LPJR[[i]],paste0("C:/Users/Hector/Documents/Pughtam-cropozone/Global_evaluation_outputs/LPJYield_",crop[i],"_newcrops_1970-2010.nc"),"CDF")
  #writeRaster(prod_LPJR[[i]],paste0("C:/Users/Hector/Documents/Pughtam-cropozone/Global_evaluation_outputs/LPJprod_",crop[i],"_newcrops_1970-2010.nc"),"CDF")
}




