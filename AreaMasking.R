#option 1 to call areas
source ("C:/Users/Hector/Documents/Pughtam-cropozone/Global_Evaluation/mergingFAOandRay")

#option 2 call SPAMest_[Crop]_1960_2010.nc
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

#Calling the yield output of LPJ-GUESS
setwd("C:/Users/hac809/Documents/output")
yield=read.table('yield.out', h=TRUE)
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
ProdBG1<-list(Wheatprod,Maizprod,Riceprod)

#Selecting year lag based on correlation
detrend<-function(x){
      dt<-ma(x,order=5,centre=TRUE)
      dt_res<-x-dt
      return(dt_res)
}

for (i in 1:length(ProdBG)){
  temp1<-calc(ProdBG[[i]],detrend)
  temp1<-temp1[[-c(1,2,nlayers(temp1)-1,nlayers(temp1))]]
  raytest<-calc(crop_rayR[[i]],detrend)
  raytest<-raytest[[-c(1,2,nlayers(raytest)-1,nlayers(raytest))]]
  temp1<-stack(temp1[[c(10:46)]],raytest[[c(1:37)]])
  Corr<-calc(temp1, fun=function(x) cor(x[1:37], x[38:74], method='pearson'))
  Corr1<-calc(temp1, fun=function(x) cor(x[2:37], x[38:73], method='pearson'))
  Corr2<-calc(temp1, fun=function(x) cor(x[1:36], x[39:74], method='pearson'))
  Map1<-overlay(Corr,Corr1,Corr2, fun=function(x,y,z) {ifelse(x>y&x>z,1,0)})
  Map2<-overlay(Corr1,Corr2,Corr, fun=function(x,y,z) {ifelse(x>y&x>z,1,0)})
  Map3<-overlay(Corr2,Corr,Corr1, fun=function(x,y,z) {ifelse(x>y&x>z,1,0)})
  ProdBG[[i]]<-ProdBG1[[i]][[c(2:49)]]*Map1+ProdBG1[[i]][[c(3:50)]]*Map2+ProdBG1[[i]][[c(1:48)]]*Map3
}

ProdDf<-list()
AreaDf<-list()
for(j in 1:length(ProdBG)){
  ProdDf[[j]]<-as.data.frame(ProdBG[[j]],xy=TRUE)
  AreaDf[[j]]<-as.data.frame(ArNew[[j]][[-c(1,50)]],xy=TRUE)
}

#Global aggregation
Global<-list()
Df<-list()
for(j in 1:length(ProdDf)){
  Prod<-as.data.frame(unlist(ProdDf[[j]][,c(-1,-2)]))
  Area<-as.data.frame(unlist(AreaDf[[j]][,c(-1,-2)]))
  Lon<-as.data.frame(rep(ProdDf[[j]]$x,48))
  Lat<-as.data.frame(rep(ProdDf[[j]]$y,48))
  Year<-as.data.frame(rep(c(1962:2009),ea=259200))
  Df[[j]]<-cbind(Lon,Lat,Year,Prod,Area)
  colnames(Df[[j]])<-c("Lon","Lat","Year","Prod","Area")
  Global[[j]]<-na.omit(Df[[j]])
  Global[[j]]<-aggregate(Global[[j]],by=list(Global[[j]]$Year),FUN=sum)
  Global[[j]]$Yield<-Global[[j]]$Prod/Global[[j]]$Area
  colnames(Global[[j]])[1]<-c("Year")
  Global[[j]]<-subset(Global[[j]],Year>1961&Year<2010)
  write.csv(Global[[j]],paste0("C:/Users/hac809/Documents/Pughtam-cropozone/Global_evaluation_outputs/DataGlobal",crop[j],".csv"), row.names = FALSE)
}
FGlobal<- read.csv("C:/Users/hac809/Documents/Pughtam-cropozone/Global_evaluation_inputs/FAO/FAOGl.csv", h=T)
FGlobal<-subset(FGlobal,year<2011)
Lin_yield<-list()
Lin_prod<-list()
Lin_Fyield<-list()
Lin_Fprod<-list()
for (i in 1:3){
  Lin_yield[[i]]<-lm(Global[[i]]$Yield~Global[[i]]$Year)
  Lin_prod[[i]]<-lm(Global[[i]]$Prod~Global[[i]]$Year)
  Lin_Fyield[[i]]<-lm(FGlobal[,3*i+1]~FGlobal$year)
  Lin_Fprod[[i]]<-lm(FGlobal[,3*i-1]~FGlobal$year)
}



#Graph 1 red color is FAO, blue LPJG
X11(width=8,height=8)
par(mfrow=c(3,2))
for(i in 1:3){
  # plot(FGlobal$year,FGlobal[,i*3-1]/1000000, type="l",col="red", xlab="", ylab="",
  #      main=cropn[i], font=2, font.lab=2, ylim=c(100,1000),cex.lab=2,cex.main=2,cex.axis=1.5,ylbias=0.5,lwd=2)
  # title( ylab="Prod. (Million Tons)", line=2.5, cex.lab=2,font=2, font.lab=2)
  # lines(Global[[i]]$Year,Global[[i]]$Prod/1000000,col="blue",,lwd=2)
  # abline(Lin_prod[[i]]$coefficients[1]/1000000,Lin_prod[[i]]$coefficients[2]/1000000,lwd=2)
  # abline(Lin_Fprod[[i]]$coefficients[1]/1000000,Lin_Fprod[[i]]$coefficients[2]/1000000,lwd=2,col="gray30",lty=6)
  
  plot(FGlobal$year,FGlobal[,i*3-1]/1000000, type="l",col="red", xlab="", ylab="",
       main=cropn[i], font=2, font.lab=2, ylim=c(100,1000),cex.lab=2,cex.main=2,cex.axis=1.5,ylbias=0.5,lwd=2)
  title( ylab="Prod. (Million Tons)", line=2.5, cex.lab=2,font=2, font.lab=2)
  lines(Global[[i]]$Year,Global[[i]]$Prod/1000000,col="blue",lwd=2)
  abline(Lin_prod[[i]]$coefficients[1]/1000000,Lin_prod[[i]]$coefficients[2]/1000000,lwd=2)
  abline(Lin_Fprod[[i]]$coefficients[1]/1000000,Lin_Fprod[[i]]$coefficients[2]/1000000,lwd=2,col="gray30",lty=6)
  
  plot(FGlobal$year,FGlobal[,i*3+1], type="l",col="red", xlab="", ylab="",
       main=cropn[i], font=2, font.lab=2, ylim=c(1.0,6.0),cex.lab=2,cex.main=2,cex.axis=1.5,ylbias=0.5,lwd=2)
  title( ylab="Yield (t/ha)", line=2.5, cex.lab=2,font=2, font.lab=2)
  lines(Global[[i]]$Year,Global[[i]]$Yield,col="blue",lwd=2)
  abline(Lin_yield[[i]]$coefficients[1],Lin_yield[[i]]$coefficients[2],lwd=2)
  abline(Lin_Fyield[[i]]$coefficients[1],Lin_Fyield[[i]]$coefficients[2],lwd=2,col="gray30",lty=6)
}



#Country aggregation
year<-rep(1962:2009, each=243)
# aggregation production
agg_listc<-list()
agg_list<-list()
for(j in 1:length(ProdBG)){
  for(i in 1:48){ 
    agg_listc[[i]]<-as(ProdBG[[j]][[i]], 'SpatialGridDataFrame')
    agg_listc[[i]]<-aggregateHalfDegreeGridToCountries(agg_listc[[i]])
    agg_listc[[i]]<-joinCountryData2Map(agg_listc[[i]],nameJoinColumn ='UN',joinCode = "UN")
    agg_listc[[i]]<-as.data.frame(agg_listc[[i]])
    agg_listc[[i]]<-agg_listc[[i]][c(22,50,51)]
    names(agg_listc[[i]])[3]<-'Prod'
  }
  agg_list[[j]]<-do.call("rbind", agg_listc)
  agg_list[[j]]<-cbind(year,agg_list[[j]])
}
# aggregation area from area correction
AreaBCc<-list()
AreaBC<-list()
ArNew_<-list()
for(j in 1:length(ArNew)){
  ArNew_[[j]]<-ArNew[[j]][[-c(1,50)]]
  for(i in 1:48){
    AreaBCc[[i]]<-as(ArNew_[[j]][[i]], 'SpatialGridDataFrame')
    AreaBCc[[i]]<-aggregateHalfDegreeGridToCountries(AreaBCc[[i]])
    AreaBCc[[i]]<-joinCountryData2Map(AreaBCc[[i]],nameJoinColumn ='UN',joinCode = "UN")
    AreaBCc[[i]]<-as.data.frame(AreaBCc[[i]])
    AreaBCc[[i]]<-AreaBCc[[i]][c(22,50,51)]
    names(AreaBCc[[i]])[3]<-'Area'
  }
  AreaBC[[j]]<-do.call("rbind", AreaBCc)
  AreaBC[[j]]<-cbind(year,AreaBC[[j]])
}

aggdf<-list()
for(j in 1:length(agg_list)){
  aggdf[[j]]<-merge(agg_list[[j]],AreaBC[[j]][-2],by=c("year","UN"))
  aggdf[[j]]<-subset(aggdf[[j]],!is.na(UN)& UN!=-99)
  aggdf[[j]]$LYield<-ifelse(aggdf[[j]]$Area==0,0,aggdf[[j]]$Prod/aggdf[[j]]$Area)
  aggdf[[j]]<-subset(aggdf[[j]],NAME_SORT!="Ashmore and Cartier Islands"& NAME_SORT!="French Guiana"& NAME_SORT!="West Bank")
  names(aggdf[[j]])<-c("year","UN","Country","LProd","LArea","LYield")
  write.csv(aggdf[[j]],paste0("DataNational",crop[j],".csv"), row.names = FALSE)
}

#producing Netcdf for Yield BG from LPJ
prod_LPJR<-list()
area_LPJR<-list()
yield_LPJR<-list()
for (i in 1:length(ProdBG)){
  prod_LPJR[[i]]<-subset(ProdBG[[i]],9:48)
  area_LPJR[[i]]<-subset(ArNew[[i]],10:49)
  yield_LPJR[[i]]<-prod_LPJR[[i]]/area_LPJR[[i]]
  #writeRaster(yield_LPJR[[i]],paste0("C:/Users/hac809/Documents/Pughtam-cropozone/Global_evaluation_outputs/LPJYield_",crop[i],"_newcrops_1970-2009.nc"),"CDF",overwrite=TRUE)
  #writeRaster(prod_LPJR[[i]],paste0("C:/Users/hac809/Documents/Pughtam-cropozone/Global_evaluation_outputs/LPJprod_",crop[i],"_newcrops_1970-2009.nc"),"CDF" ,overwrite=TRUE)
}


#Input Ray data from original files
#setwd("C:/Users/hac809/Documents/Pughtam-cropozone/Global_evaluation_inputs/Ray_gridded/")

#reading the Ray files and stacking them in raster files
#Option 1 call it from Ray files
# crop<-c("Wheat","Maize","Rice")
# list.crops<-list()
# crop_rayR<-list()
# for (j in 1:length(crop)){
#   list.crops[[j]]<-list.files(pattern=crop[j])
#   list.crops[[j]]<-raster(list.crops[[j]][1],varname="Data")
#   crop_rayR[[j]]<-subset(list.crops[[j]],1:41)
#   crop_rayR[[j]]<-extend(crop_rayR[[j]],bb)
#   #writeRaster(crop_rayR[[j]],paste0("C:/Users/Hector/Documents/Pughtam-cropozone/Global_evaluation_outputs/Ray_Yield_",crop[j],"_newcrops_1970-2010.nc"),"CDF")
# }
# test1<-nc_open(test[1])
# test1<-raster(test[1],varname="Data",origin=-89.75)
# raster
# startDate=1970-01-01
# names(crop_rayR)<-paste0("Ray",crop)

