
library(ncdf4)
library(raster)
library(rworldmap)
library(dplyr)
library(plyr)
library(forecast)
library(zoo)


crop<-c("SPAMest_Wheat_","SPAMest_Maize_","SPAMest_Rice_")
cropirr<-c("SPAMest_Wheatirr_","SPAMest_Maizeirr_","SPAMest_Riceirr_")
ArNew<-list()
ArNewi<-list()
ArNewr<-list()
setwd("C:/Users/hac809/Documents/Pughtam-cropozone/Global_evaluation_outputs/Area")
for (j in 1:length(crop)){
  ArNew[[j]]<-list.files(pattern=crop[j])
  ArNewi[[j]]<-list.files(pattern=cropirr[j])
  ArNew[[j]]<-brick(ArNew[[j]])
  ArNewi[[j]]<-brick(ArNewi[[j]])*(ArNew[[j]]/ArNew[[j]])
  ArNewr[[j]]<-ArNew[[j]]-ArNewi[[j]]
}

#Calling the yield from only climate output of LPJ-GUESS
setwd("C:/Users/hac809/Documents/output/Only_climate")
yield=read.table('yield.out', h=TRUE)
#Selecting cultivar by country based on FAO yield for 1960 all countries with cultivar 2
yield2=yield
yield2=subset(yield2,Year>1960 & Year<2011)
yield2$TeCo<-yield2$TeCG
yield2$TeCoi<-yield2$TeCGi
yield2$IndexW<-ifelse(yield2$Year==1961,ifelse(yield2$TeWW>=yield2$TeSW,1,2),NA)
yield2<-na.locf(yield2)
yield2$TeW<-ifelse(yield2$IndexW==1,yield2$TeWW,yield2$TeSW)
yield2$TeWi<-ifelse(yield2$IndexW==1,yield2$TeWWi,yield2$TeSWi)
yield2<-yield2[,c(1,2,3,19,18,16,15,13,14,8)]

vect<-c(1961:2010)
li<-list()
for (i in 1:length(vect)){
  li[[i]]<-subset(yield2,Year==vect[i])
  li[[i]]<-li[[i]][c(-3)]
}

crops<-names(li[[i]])[c(-1,-2)]
bb <- extent(-180, 180, -90, 90)
rasterize<-function(coord,value){
  areas<-SpatialPointsDataFrame(coords=coord,data.frame(value),proj4string = CRS("+proj=longlat +datum=WGS84"))
  areas2<-SpatialPixelsDataFrame(areas,tolerance=0.0192077,areas@data) 
  areas3<-as(areas2,"SpatialGridDataFrame")
  Rstr<-raster(areas3)
}
rastcrop<-list()
for(j in 1:length(crops)){
  crop_r<-list()
  for (i in 1:length(vect)){
    yield_coords<-cbind(li[[i]]$Lon,li[[i]]$Lat)
    crop_r[[i]]<-rasterize(yield_coords,li[[i]][crops[j]])
  }
  names(crop_r)<-paste0("Yi_",vect)
  rastcrop[[j]]<-stack(crop_r)
  rastcrop[[j]]<-extend(rastcrop[[j]],bb)
}
names(rastcrop)<-paste0(crops)

#multiplied by 10 to convert yield from kg/m2 to Ton/ha production(ha)
Wheatprod<-((rastcrop[[2]]*ArNewr[[1]]+rastcrop[[1]]*ArNewi[[1]])*10/0.88)
Maizprod<-((rastcrop[[4]]*ArNewr[[2]]+rastcrop[[3]]*ArNewi[[2]])*10/0.88)
Riceprod<-((rastcrop[[6]]*ArNew[[3]])*10/0.87)
ProdBG1<-list(Wheatprod,Maizprod,Riceprod)

ProdDf<-list()
AreaDf<-list()
for(j in 1:length(ProdBG1)){
  ProdDf[[j]]<-as.data.frame(ProdBG1[[j]],xy=TRUE)
  AreaDf[[j]]<-as.data.frame(ArNew[[j]],xy=TRUE)
}

#Global aggregation
Global<-list()
Df<-list()
for(j in 1:length(ProdDf)){
  Prod<-as.data.frame(unlist(ProdDf[[j]][,c(-1,-2)]))
  Area<-as.data.frame(unlist(AreaDf[[j]][,c(-1,-2)]))
  Lon<-as.data.frame(rep(ProdDf[[j]]$x,50))
  Lat<-as.data.frame(rep(ProdDf[[j]]$y,50))
  Year<-as.data.frame(rep(c(1961:2010),ea=259200))
  Df[[j]]<-cbind(Lon,Lat,Year,Prod,Area)
  colnames(Df[[j]])<-c("Lon","Lat","Year","Prod","Area")
  Global[[j]]<-na.omit(Df[[j]])
  Global[[j]]<-aggregate(Global[[j]],by=list(Global[[j]]$Year),FUN=sum)
  Global[[j]]$Yield<-Global[[j]]$Prod/Global[[j]]$Area
  colnames(Global[[j]])[1]<-c("Year")
  Global[[j]]<-subset(Global[[j]],Year>1960&Year<2011)
 }



#Calling the yield from only climate + CO2 output of LPJ-GUESS
setwd("C:/Users/hac809/Documents/output/Climate_CO2/")
yield=read.table('yield.out', h=TRUE)
#Selecting cultivar by country based on FAO yield for 1960 all countries with cultivar 2
yield2=yield
yield2=subset(yield2,Year>1960 & Year<2011)
yield2$TeCo<-yield2$TeCG
yield2$TeCoi<-yield2$TeCGi
yield2$IndexW<-ifelse(yield2$Year==1961,ifelse(yield2$TeWW>=yield2$TeSW,1,2),NA)
yield2<-na.locf(yield2)
yield2$TeW<-ifelse(yield2$IndexW==1,yield2$TeWW,yield2$TeSW)
yield2$TeWi<-ifelse(yield2$IndexW==1,yield2$TeWWi,yield2$TeSWi)
yield2<-yield2[,c(1,2,3,19,18,16,15,13,14,8)]

vect<-c(1961:2010)
li<-list()
for (i in 1:length(vect)){
  li[[i]]<-subset(yield2,Year==vect[i])
  li[[i]]<-li[[i]][c(-3)]
}

crops<-names(li[[i]])[c(-1,-2)]
bb <- extent(-180, 180, -90, 90)
rasterize<-function(coord,value){
  areas<-SpatialPointsDataFrame(coords=coord,data.frame(value),proj4string = CRS("+proj=longlat +datum=WGS84"))
  areas2<-SpatialPixelsDataFrame(areas,tolerance=0.0192077,areas@data) 
  areas3<-as(areas2,"SpatialGridDataFrame")
  Rstr<-raster(areas3)
}
rastcrop<-list()
for(j in 1:length(crops)){
  crop_r<-list()
  for (i in 1:length(vect)){
    yield_coords<-cbind(li[[i]]$Lon,li[[i]]$Lat)
    crop_r[[i]]<-rasterize(yield_coords,li[[i]][crops[j]])
  }
  names(crop_r)<-paste0("Yi_",vect)
  rastcrop[[j]]<-stack(crop_r)
  rastcrop[[j]]<-extend(rastcrop[[j]],bb)
}
names(rastcrop)<-paste0(crops)

#multiplied by 10 to convert yield from kg/m2 to Ton/ha production(ha)
Wheatprod<-((rastcrop[[2]]*ArNewr[[1]]+rastcrop[[1]]*ArNewi[[1]])*10/0.88)
Maizprod<-((rastcrop[[4]]*ArNewr[[2]]+rastcrop[[3]]*ArNewi[[2]])*10/0.88)
Riceprod<-((rastcrop[[6]]*ArNew[[3]])*10/0.87)
ProdBG2<-list(Wheatprod,Maizprod,Riceprod)

ProdDf2<-list()
for(j in 1:length(ProdBG2)){
  ProdDf2[[j]]<-as.data.frame(ProdBG2[[j]],xy=TRUE)
 }


#Global aggregation
Global2<-list()
Df<-list()
for(j in 1:length(ProdDf2)){
  Prod<-as.data.frame(unlist(ProdDf2[[j]][,c(-1,-2)]))
  Area<-as.data.frame(unlist(AreaDf[[j]][,c(-1,-2)]))
  Lon<-as.data.frame(rep(ProdDf2[[j]]$x,50))
  Lat<-as.data.frame(rep(ProdDf2[[j]]$y,50))
  Year<-as.data.frame(rep(c(1961:2010),ea=259200))
  Df[[j]]<-cbind(Lon,Lat,Year,Prod,Area)
  colnames(Df[[j]])<-c("Lon","Lat","Year","Prod","Area")
  Global2[[j]]<-na.omit(Df[[j]])
  Global2[[j]]<-aggregate(Global2[[j]],by=list(Global2[[j]]$Year),FUN=sum)
  Global2[[j]]$Yield<-Global2[[j]]$Prod/Global2[[j]]$Area
  colnames(Global2[[j]])[1]<-c("Year")
  Global2[[j]]<-subset(Global2[[j]],Year>1960&Year<2011)
}

#Calling the yield from only climate + Nfert output of LPJ-GUESS
setwd("C:/Users/hac809/Documents/output/Climate_Nfert/")
yield=read.table('yield.out', h=TRUE)
#Selecting cultivar by country based on FAO yield for 1960 all countries with cultivar 2
yield2=yield
yield2=subset(yield2,Year>1960 & Year<2011)
yield2$TeCo<-yield2$TeCG
yield2$TeCoi<-yield2$TeCGi
yield2$IndexW<-ifelse(yield2$Year==1961,ifelse(yield2$TeWW>=yield2$TeSW,1,2),NA)
yield2<-na.locf(yield2)
yield2$TeW<-ifelse(yield2$IndexW==1,yield2$TeWW,yield2$TeSW)
yield2$TeWi<-ifelse(yield2$IndexW==1,yield2$TeWWi,yield2$TeSWi)
yield2<-yield2[,c(1,2,3,19,18,16,15,13,14,8)]

vect<-c(1961:2010)
li<-list()
for (i in 1:length(vect)){
  li[[i]]<-subset(yield2,Year==vect[i])
  li[[i]]<-li[[i]][c(-3)]
}

crops<-names(li[[i]])[c(-1,-2)]
bb <- extent(-180, 180, -90, 90)
rasterize<-function(coord,value){
  areas<-SpatialPointsDataFrame(coords=coord,data.frame(value),proj4string = CRS("+proj=longlat +datum=WGS84"))
  areas2<-SpatialPixelsDataFrame(areas,tolerance=0.0192077,areas@data) 
  areas3<-as(areas2,"SpatialGridDataFrame")
  Rstr<-raster(areas3)
}
rastcrop<-list()
for(j in 1:length(crops)){
  crop_r<-list()
  for (i in 1:length(vect)){
    yield_coords<-cbind(li[[i]]$Lon,li[[i]]$Lat)
    crop_r[[i]]<-rasterize(yield_coords,li[[i]][crops[j]])
  }
  names(crop_r)<-paste0("Yi_",vect)
  rastcrop[[j]]<-stack(crop_r)
  rastcrop[[j]]<-extend(rastcrop[[j]],bb)
}
names(rastcrop)<-paste0(crops)

#multiplied by 10 to convert yield from kg/m2 to Ton/ha production(ha)
Wheatprod<-((rastcrop[[2]]*ArNewr[[1]]+rastcrop[[1]]*ArNewi[[1]])*10/0.88)
Maizprod<-((rastcrop[[4]]*ArNewr[[2]]+rastcrop[[3]]*ArNewi[[2]])*10/0.88)
Riceprod<-((rastcrop[[6]]*ArNew[[3]])*10/0.87)
ProdBG3<-list(Wheatprod,Maizprod,Riceprod)

ProdDf3<-list()
for(j in 1:length(ProdBG3)){
  ProdDf3[[j]]<-as.data.frame(ProdBG3[[j]],xy=TRUE)
}

#Global aggregation
Global3<-list()
Df<-list()
for(j in 1:length(ProdDf3)){
  Prod<-as.data.frame(unlist(ProdDf3[[j]][,c(-1,-2)]))
  Area<-as.data.frame(unlist(AreaDf[[j]][,c(-1,-2)]))
  Lon<-as.data.frame(rep(ProdDf3[[j]]$x,50))
  Lat<-as.data.frame(rep(ProdDf3[[j]]$y,50))
  Year<-as.data.frame(rep(c(1961:2010),ea=259200))
  Df[[j]]<-cbind(Lon,Lat,Year,Prod,Area)
  colnames(Df[[j]])<-c("Lon","Lat","Year","Prod","Area")
  Global3[[j]]<-na.omit(Df[[j]])
  Global3[[j]]<-aggregate(Global3[[j]],by=list(Global3[[j]]$Year),FUN=sum)
  Global3[[j]]$Yield<-Global3[[j]]$Prod/Global3[[j]]$Area
  colnames(Global3[[j]])[1]<-c("Year")
  Global3[[j]]<-subset(Global3[[j]],Year>1960&Year<2011)
}

#Calling the yield from only climate + Nfert + CO2 output of LPJ-GUESS
setwd("C:/Users/hac809/Documents/output")
yield=read.table('yield.out', h=TRUE)
#Selecting cultivar by country based on FAO yield for 1960 all countries with cultivar 2
yield2=yield
yield2=subset(yield2,Year>1960 & Year<2011)
yield2$TeCo<-yield2$TeCG
yield2$TeCoi<-yield2$TeCGi
yield2$IndexW<-ifelse(yield2$Year==1961,ifelse(yield2$TeWW>=yield2$TeSW,1,2),NA)
yield2<-na.locf(yield2)
yield2$TeW<-ifelse(yield2$IndexW==1,yield2$TeWW,yield2$TeSW)
yield2$TeWi<-ifelse(yield2$IndexW==1,yield2$TeWWi,yield2$TeSWi)
yield2<-yield2[,c(1,2,3,19,18,16,15,13,14,8)]

vect<-c(1961:2010)
li<-list()
for (i in 1:length(vect)){
  li[[i]]<-subset(yield2,Year==vect[i])
  li[[i]]<-li[[i]][c(-3)]
}

crops<-names(li[[i]])[c(-1,-2)]
bb <- extent(-180, 180, -90, 90)
rasterize<-function(coord,value){
  areas<-SpatialPointsDataFrame(coords=coord,data.frame(value),proj4string = CRS("+proj=longlat +datum=WGS84"))
  areas2<-SpatialPixelsDataFrame(areas,tolerance=0.0192077,areas@data) 
  areas3<-as(areas2,"SpatialGridDataFrame")
  Rstr<-raster(areas3)
}
rastcrop<-list()
for(j in 1:length(crops)){
  crop_r<-list()
  for (i in 1:length(vect)){
    yield_coords<-cbind(li[[i]]$Lon,li[[i]]$Lat)
    crop_r[[i]]<-rasterize(yield_coords,li[[i]][crops[j]])
  }
  names(crop_r)<-paste0("Yi_",vect)
  rastcrop[[j]]<-stack(crop_r)
  rastcrop[[j]]<-extend(rastcrop[[j]],bb)
}
names(rastcrop)<-paste0(crops)

#multiplied by 10 to convert yield from kg/m2 to Ton/ha production(ha)
Wheatprod<-((rastcrop[[2]]*ArNewr[[1]]+rastcrop[[1]]*ArNewi[[1]])*10/0.88)
Maizprod<-((rastcrop[[4]]*ArNewr[[2]]+rastcrop[[3]]*ArNewi[[2]])*10/0.88)
Riceprod<-((rastcrop[[6]]*ArNew[[3]])*10/0.87)
ProdBG4<-list(Wheatprod,Maizprod,Riceprod)

ProdDf4<-list()
for(j in 1:length(ProdBG4)){
  ProdDf4[[j]]<-as.data.frame(ProdBG4[[j]],xy=TRUE)
}

#Global aggregation
Global4<-list()
Df<-list()
for(j in 1:length(ProdDf4)){
  Prod<-as.data.frame(unlist(ProdDf4[[j]][,c(-1,-2)]))
  Area<-as.data.frame(unlist(AreaDf[[j]][,c(-1,-2)]))
  Lon<-as.data.frame(rep(ProdDf4[[j]]$x,50))
  Lat<-as.data.frame(rep(ProdDf4[[j]]$y,50))
  Year<-as.data.frame(rep(c(1961:2010),ea=259200))
  Df[[j]]<-cbind(Lon,Lat,Year,Prod,Area)
  colnames(Df[[j]])<-c("Lon","Lat","Year","Prod","Area")
  Global4[[j]]<-na.omit(Df[[j]])
  Global4[[j]]<-aggregate(Global4[[j]],by=list(Global4[[j]]$Year),FUN=sum)
  Global4[[j]]$Yield<-Global4[[j]]$Prod/Global4[[j]]$Area
  colnames(Global4[[j]])[1]<-c("Year")
  Global4[[j]]<-subset(Global4[[j]],Year>1960&Year<2011)
}


#Calling the yield from only climate + Nfert + CO2+ Cultivar output of LPJ-GUESS
countries<-as.data.frame(gridCountriesDegreesHalf)
names(countries)<-c("UN","Lon","Lat")
yield<-join(yield,countries)
#Selecting cultivar by country based on HDI (Human Development Index)
#MaizCult<-read.csv("C:/Users/hac809/Documents/Pughtam-cropozone/Global_evaluation_inputs/FAO/MaizCultBC.csv",h=T)
#MaizCult<-MaizCult[c(2,4)]
MaizCult<-read.csv("C:/Users/hac809/Documents/Pughtam-cropozone/Global_evaluation_inputs/FAO/FAO_MaizBC.csv",h=T)
#MaizCult$Cultivar<-ifelse(MaizCult$FYield>5,1,2)
MaizCult$Cultivar<-ifelse(MaizCult$FYield>0.055*(MaizCult$Year-1960)+4.15,1,2)
MaizCult<-MaizCult[c(1,3,7)]
MaizCult<-subset(MaizCult,!is.na(UN)&UN!=-99)
MaizCult<-distinct(MaizCult)
#yield<-yield[,-16]
yield<-join(yield,MaizCult)
yield2=yield
yield2=subset(yield2,Year>1960 & Year<2011)
yield2=na.locf(yield2,fromLast = TRUE)
yield2$TeCo<-ifelse(yield2$Cultivar==2,yield2$TeCG,yield2$TeCS)
yield2$TeCoi<-ifelse(yield2$Cultivar==2,yield2$TeCGi,yield2$TeCSi)
yield2<-yield2[,c(1,2,3,15,4,5,17,8,9,10,18,13,14)]
#yield2$TeW<-pmax(yield2$TeWW,yield2$TeSW)
#yield2$TeWi<-pmax(yield2$TeWWi,yield2$TeSWi)


vect<-c(1961:2010)
li<-list()
for (i in 1:length(vect)){
  li[[i]]<-subset(yield2,Year==vect[i])
  li[[i]]<-li[[i]][c(-3)]
}

#crops<-names(li[[i]])[c(11,10,5,3,7,6,4)]
crops<-names(li[[i]])[c(8,4,9,5,10,6,11,12,7)]
bb <- extent(-180, 180, -90, 90)
rasterize<-function(coord,value){
  areas<-SpatialPointsDataFrame(coords=coord,data.frame(value),proj4string = CRS("+proj=longlat +datum=WGS84"))
  areas2<-SpatialPixelsDataFrame(areas,tolerance=0.0192077,areas@data) 
  areas3<-as(areas2,"SpatialGridDataFrame")
  Rstr<-raster(areas3)
}
rastcrop<-list()
for(j in 1:length(crops)){
  crop_r<-list()
  for (i in 1:length(vect)){
    yield_coords<-cbind(li[[i]]$Lon,li[[i]]$Lat)
    crop_r[[i]]<-rasterize(yield_coords,li[[i]][crops[j]])
  }
  names(crop_r)<-paste0("Yi_",vect)
  rastcrop[[j]]<-stack(crop_r)
  rastcrop[[j]]<-extend(rastcrop[[j]],bb)
}
names(rastcrop)<-paste0(crops)

#Calling Ray files, if not available see the end of this script to call the original files
crop<-c("Wheat","Maize","Rice")
cropR<-c("Ray_Yield_Wheat","Ray_Yield_Maize_","Ray_Yield_Rice_")
crop_rayR<-list()
Prod_ray<-list()
setwd("C:/Users/hac809/Documents/Pughtam-cropozone/Global_evaluation_outputs")
for (j in 1:length(crop)){
  crop_rayR[[j]]<-list.files(pattern=cropR[j])
  crop_rayR[[j]]<-brick(crop_rayR[[j]])
  Prod_ray[[j]]<-subset(ArNew[[j]],10:50)*crop_rayR[[j]]
}

names(crop_rayR)<-paste0("Ray",crop)
names(Prod_ray)<-paste0("RayP",crop)


#multiplied by 10 to convert yield from kg/m2 to Ton/ha production(ha)
#Divided by 0.88, 0.87, 0.91 since FAO production is assumed to have 12% water content
#Wheatprod<-((rastcrop[[2]]*ArNewr[[1]]+rastcrop[[1]]*ArNewi[[1]])*10/0.88)
WWprod<-((rastcrop[[2]]*ArNewr[[1]]+rastcrop[[1]]*ArNewi[[1]])*10/0.88)
SWprod<-((rastcrop[[4]]*ArNewr[[1]]+rastcrop[[3]]*ArNewi[[1]])*10/0.88)
#Corr by GR for wheat
WWcorr<- stack(WWprod[[c(10:50)]],crop_rayR[[1]])
WWcorr1<- calc(WWcorr, fun=function(x) cor(x[1:41], x[42:82], method='pearson'))
SWcorr<- stack(SWprod[[c(10:50)]],crop_rayR[[1]])
SWcorr1<- calc(SWcorr, fun=function(x) cor(x[1:41], x[42:82], method='pearson'))
Scomp<-overlay(SWcorr1,WWcorr1,fun=function(x,y) {ifelse(x>y,1,0)})
Wcomp<-overlay(WWcorr1,SWcorr1,fun=function(x,y) {ifelse(x>y,1,0)})
Wheatprod<-WWprod*Wcomp+SWprod*Scomp
Maizprod<-((rastcrop[[6]]*ArNewr[[2]]+rastcrop[[5]]*ArNewi[[2]])*10/0.88)
Riceprod<-((rastcrop[[8]]*ArNew[[3]])*10/0.87)
ProdBG5<-list(Wheatprod,Maizprod,Riceprod)


ProdDf5<-list()
for(j in 1:length(ProdBG5)){
  ProdDf5[[j]]<-as.data.frame(ProdBG5[[j]],xy=TRUE)
  }

#Global aggregation
Global5<-list()
Df<-list()
for(j in 1:length(ProdDf5)){
  Prod<-as.data.frame(unlist(ProdDf5[[j]][,c(-1,-2)]))
  Area<-as.data.frame(unlist(AreaDf[[j]][,c(-1,-2)]))
  Lon<-as.data.frame(rep(ProdDf5[[j]]$x,50))
  Lat<-as.data.frame(rep(ProdDf5[[j]]$y,50))
  Year<-as.data.frame(rep(c(1961:2010),ea=259200))
  Df[[j]]<-cbind(Lon,Lat,Year,Prod,Area)
  colnames(Df[[j]])<-c("Lon","Lat","Year","Prod","Area")
  Global5[[j]]<-na.omit(Df[[j]])
  Global5[[j]]<-aggregate(Global5[[j]],by=list(Global5[[j]]$Year),FUN=sum)
  Global5[[j]]$Yield<-Global5[[j]]$Prod/Global5[[j]]$Area
  colnames(Global5[[j]])[1]<-c("Year")
  Global5[[j]]<-subset(Global5[[j]],Year>1960&Year<2011)
}

GLOBAL<-list(Global,Global2,Global3,Global4,Global5)
Avg<-list()
avg<-list()
label<-c("Only Climate", "Climate+CO2","Climate+Nfert","Climate+CO2+Nfert","All Drivers")
for (i in 1:length(GLOBAL)){
  for (j in 1:length(GLOBAL[[i]])){
    temp1<-GLOBAL[[i]][[j]][c(1:10,41:50),c(1,2,3,7)]
    temp1_1<-mean(temp1[c(1:10),4])
    temp1_2<-mean(temp1[c(11:20),4])
    temp1_3<-temp1_2-temp1_1
    avg[[j]]<-as.data.frame(temp1_3)
    avg[[j]]$crop<-crop[j]
    avg[[j]]$simu<-label[i]
    }
  Avg[[i]]<-do.call("rbind",avg)
}
Avg<-do.call("rbind",Avg)
Avg<-Avg[order(Avg$crop),]

#Graph 1 red color is FAO, blue LPJG
X11(width=8,height=8)
par(mfrow=c(3,2))
cropn<-crop
for(i in 1:3){
  plot(Global[[i]]$Year,Global[[i]]$Yield, type="l",col="blue", xlab="", ylab="",
       main=crop[i], font=2, font.lab=2, ylim=c(1.0,4.5),cex.lab=2,cex.main=2,cex.axis=1.5,ylbias=0.5,lwd=2)#Only Climate
  lines(Global2[[i]]$Year,Global2[[i]]$Yield,col="red",lwd=2)#Climate+CO2
  lines(Global3[[i]]$Year,Global3[[i]]$Yield,col="green",lwd=2)#Climate+Nfert
  lines(Global4[[i]]$Year,Global4[[i]]$Yield,col="orange",lwd=ifelse(i==3,5,2))#Climate+CO2+Nfert
  lines(Global5[[i]]$Year,Global5[[i]]$Yield,col="purple",lwd=2)#Alldrivers
  title( ylab="Yield (t/ha)", line=2.5, cex.lab=2,font=2, font.lab=2)
  if(i==3){legend('bottom', legend=c("C","C+CO2","C+N","C+N+CO2","AD"),col = c("blue", "red","green", "orange", "purple"), 
         lwd = 3, xpd = TRUE, horiz = TRUE, cex = 1, seg.len=1,inset=-0.4,bty="n",text.font = 2)}
         
  temp1<-subset(Avg,crop==cropn[i])
  barplot(temp1$temp1_3,main=cropn[i],names.arg=c("C","C,CO2","C,N","C,N,Co2","AD"),col="blue",cex.axis=1.5,lwd=2,cex.main=2,cex.names=1.4,font=2)
  }

