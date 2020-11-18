library(ncdf4)
library(raster)
library(rworldmap)
library(dplyr)
library(plyr)

#spin up until 1960

yield60<-read.table("C:/Users/hac809/Documents/output/1960/yield.out",h=T)
countries<-as.data.frame(gridCountriesDegreesHalf)
names(countries)<-c("UN","Lon","Lat")
yield60<-join(yield60,countries)
yield60<-join(yield60,MaizCult)
yield60=subset(yield60,Year>1960 & Year<2011)
yield60=na.locf(yield60,fromLast = TRUE)
yield60$TeCo<-ifelse(yield60$Cultivar==2,yield60$TeCG,yield60$TeCS)
yield60$TeCoi<-ifelse(yield60$Cultivar==2,yield60$TeCGi,yield60$TeCSi)
yield60_=yield60
yield60_$TeW<-pmax(yield60_$TeWW,yield60_$TeSW)
yield60_$TeWi<-pmax(yield60_$TeWWi,yield60_$TeSWi)
yield60_<-yield60_[-c(4,5,6,7,9,10,11,12,16)]
vect<-c(1961:2010)
li60_<-list()
for (i in 1:length(vect)){
  li60_[[i]]<-subset(yield60_,Year==vect[i])
  li60_[[i]]<-li60_[[i]][c(-3)]
}

crops<-names(li60_[[i]])[c(10,9,8,7,5,4,3)]
bb <- extent(-180, 180, -90, 90)
rasterize<-function(coord,value){
  areas<-SpatialPointsDataFrame(coords=coord,data.frame(value),proj4string = CRS("+proj=longlat +datum=WGS84"))
  areas2<-SpatialPixelsDataFrame(areas,tolerance=0.0192077,areas@data) 
  areas3<-as(areas2,"SpatialGridDataFrame")
  Rstr<-raster(areas3)
}
rastcrop60_<-list()
for(j in 1:length(crops)){
  crop_r60_<-list()
  for (i in 1:length(vect)){
    yield_coords<-cbind(li60_[[i]]$Lon,li60_[[i]]$Lat)
    crop_r60_[[i]]<-rasterize(yield_coords,li60_[[i]][crops[j]])
  }
  names(crop_r60_)<-paste0("Yi_",vect)
  rastcrop60_[[j]]<-stack(crop_r60_)
  rastcrop60_[[j]]<-extend(rastcrop60_[[j]],bb)
}
names(rastcrop60_)<-paste0(crops)

#multiplied by 10 to convert yield from kg/m2 to Ton/ha production(ha)
#Divided by 0.88, 0.87, 0.91 since FAO production is assumed to have 12% water content
Wprod60_<-((rastcrop60_[[2]]*ArNewr[[1]]+rastcrop60_[[1]]*ArNewi[[1]])*10/0.88)
Mprod60_<-((rastcrop60_[[4]]*ArNewr[[2]]+rastcrop60_[[3]]*ArNewi[[2]])*10/0.88)
Rprod60_<-((rastcrop60_[[5]]*ArNew[[3]])*10/0.87)  
ProdBG60_<-list(Wprod60_,Mprod60_,Rprod60_)
ProdDf60_<-list()
AreaDf<-list()
for(j in 1:length(ProdBG)){
  ProdDf60_[[j]]<-as.data.frame(ProdBG60_[[j]],xy=TRUE)
  AreaDf[[j]]<-as.data.frame(ArNew[[j]],xy=TRUE)
}
Global60_<-list()
Df60_<-list()
for(j in 1:length(ProdDf60_)){
  Prod<-as.data.frame(unlist(ProdDf60_[[j]][,c(-1,-2)]))
  Area<-as.data.frame(unlist(AreaDf[[j]][,c(-1,-2)]))
  Lon<-as.data.frame(rep(ProdDf60_[[j]]$x,50))
  Lat<-as.data.frame(rep(ProdDf60_[[j]]$y,50))
  Year<-as.data.frame(rep(c(1961:2010),ea=259200))
  Df60_[[j]]<-cbind(Lon,Lat,Year,Prod,Area)
  colnames(Df60_[[j]])<-c("Lon","Lat","Year","Prod","Area")
  Global60_[[j]]<-na.omit(Df60_[[j]])
  Global60_[[j]]<-aggregate(Global60_[[j]],by=list(Global60_[[j]]$Year),FUN=sum)
  Global60_[[j]]$Yield<-Global60_[[j]]$Prod/Global60_[[j]]$Area
  Global60_[[j]]<-Global60_[[j]][,-4]
  colnames(Global60_[[j]])[1]<-c("Year")
  Global60_[[j]]<-subset(Global60_[[j]],Yield>=0.5)
  write.csv(Global60_[[j]],paste0("C:/Users/hac809/Documents/Pughtam-cropozone/Global_evaluation_outputs/DataGlobal60",crop[j],".csv"), row.names = FALSE)
}

Lin_yield60_<-list()
Lin_prod60_<-list()
for (i in 1:3){
  Lin_yield60_[[i]]<-lm(Global60_[[i]]$Yield~Global60_[[i]]$Year)
  Lin_prod60_[[i]]<-lm(Global60_[[i]]$Prod~Global60_[[i]]$Year)
}

#soiln spinup60
soiln60<-read.table("C:/Users/hac809/Documents/output/1960/soil_npool.out",h=T)
soiln60=subset(soiln60,Year>1960 & Year<2011)
soiln60$N=rowSums(soiln60[,c("NH4", "NO3", "NO2", "NO",  "N2O", "N2")])
soiln60_<-soiln60[-c(4:9)]
vect<-c(1961:2010)
Si60_<-list()
rast_sn60_<-list()
for (i in 1:length(vect)){
  Si60_[[i]]<-subset(soiln60_,Year==vect[i])
  Si60_[[i]]<-Si60_[[i]][c(-3)]
  soiln_coords<-cbind(Si60_[[i]]$Lon,Si60_[[i]]$Lat)
  rast_sn60_[[i]]<-rasterize(soiln_coords,Si60_[[i]][,3])
}
  names(rast_sn60_)<-paste0("Sn_",vect)
  rast_sn60_<-stack(rast_sn60_)
  rast_sn60_<-extend(rast_sn60_,bb)

#masking the soiln according to area per crop
Wsn60_<-rast_sn60_*ArNew[[1]]
Msn60_<-rast_sn60_*ArNew[[2]]
Rsn60_<-rast_sn60_*ArNew[[3]]
soilnBG60_<-list(Wsn60_,Msn60_,Rsn60_)
soilnDf60_<-lapply(soilnBG60_,as.data.frame,xy=TRUE)
GlobSn60_<-list()
Dfsn60_<-list()
for(j in 1:length(soilnDf60_)){
  soiln<-as.data.frame(unlist(soilnDf60_[[j]][,c(-1,-2)]))
  Lon<-as.data.frame(rep(soilnDf60_[[j]]$x,50))
  Lat<-as.data.frame(rep(soilnDf60_[[j]]$y,50))
  Year<-as.data.frame(rep(c(1961:2010),ea=259200))
  Dfsn60_[[j]]<-cbind(Lon,Lat,Year,soiln)
  colnames(Dfsn60_[[j]])<-c("Lon","Lat","Year","N")
  GlobSn60_[[j]]<-na.omit(Dfsn60_[[j]])
  GlobSn60_[[j]]<-aggregate(GlobSn60_[[j]],by=list(GlobSn60_[[j]]$Year),FUN=sum)
}
for(j in 1:length(soilnDf60_)){  
  GlobSn60_[[j]]<-GlobSn60_[[j]][,-4]
  colnames(GlobSn60_[[j]])[1]<-c("Year")
  }

#Spin-up until 2000
yield00<-read.table("C:/Users/hac809/Documents/output/2000/yield.out",h=T)
yield00<-join(yield00,countries)
yield00<-join(yield00,MaizCult)
yield00=subset(yield00,Year>1960 & Year<2011)
yield00=na.locf(yield00,fromLast = TRUE)
yield00$TeCo<-ifelse(yield00$Cultivar==2,yield00$TeCG,yield00$TeCS)
yield00$TeCoi<-ifelse(yield00$Cultivar==2,yield00$TeCGi,yield00$TeCSi)
yield00_=yield00
yield00_$TeW<-pmax(yield00_$TeWW,yield00_$TeSW)
yield00_$TeWi<-pmax(yield00_$TeWWi,yield00_$TeSWi)
yield00_=subset(yield00_,Year>=2000 & Year<2011)
yield00_<-yield00_[-c(4,5,6,7,9,10,11,12,16)]
vect00<-c(2000:2010)
li00_<-list()
for (i in 1:length(vect00)){
  li00_[[i]]<-subset(yield00_,Year==vect00[i])
  li00_[[i]]<-li00_[[i]][c(-3)]
}

rastcrop00_<-list()
for(j in 1:length(crops)){
  crop_r00_<-list()
  for (i in 1:length(vect00)){
    yield_coords<-cbind(li00_[[i]]$Lon,li00_[[i]]$Lat)
    crop_r00_[[i]]<-rasterize(yield_coords,li00_[[i]][crops[j]])
  }
  names(crop_r00_)<-paste0("Yi_",vect00)
  rastcrop00_[[j]]<-stack(crop_r00_)
  rastcrop00_[[j]]<-extend(rastcrop00_[[j]],bb)
}
names(rastcrop00_)<-paste0(crops)

#multiplied by 10 to convert yield from kg/m2 to Ton/ha production(ha)
#Divided by 0.88, 0.87, 0.91 since FAO production is assumed to have 12% water content
Wprod00_<-((rastcrop00_[[2]]*subset(ArNewr[[1]],c(40:50))+rastcrop00_[[1]]*subset(ArNewi[[1]],c(40:50)))*10/0.88)
Mprod00_<-((rastcrop00_[[4]]*subset(ArNewr[[2]],c(40:50))+rastcrop00_[[3]]*subset(ArNewi[[2]],c(40:50)))*10/0.88)
Rprod00_<-((rastcrop00_[[5]]*subset(ArNew[[3]],c(40:50)))*10/0.87)  
ProdBG00_<-list(Wprod00_,Mprod00_,Rprod00_)
ProdDf00_<-list()
for(j in 1:length(ProdBG)){
  ProdDf00_[[j]]<-as.data.frame(ProdBG00_[[j]],xy=TRUE)
}
Global00_<-list()
Df00_<-list()
for(j in 1:length(ProdDf00_)){
  Prod<-as.data.frame(unlist(ProdDf00_[[j]][,c(-1,-2)]))
  Area<-as.data.frame(unlist(AreaDf[[j]][,c(42:52)]))
  Lon<-as.data.frame(rep(ProdDf00_[[j]]$x,11))
  Lat<-as.data.frame(rep(ProdDf00_[[j]]$y,11))
  Year<-as.data.frame(rep(c(2000:2010),ea=259200))
  Df00_[[j]]<-cbind(Lon,Lat,Year,Prod,Area)
  colnames(Df00_[[j]])<-c("Lon","Lat","Year","Prod","Area")
  Global00_[[j]]<-na.omit(Df00_[[j]])
  Global00_[[j]]<-aggregate(Global00_[[j]],by=list(Global00_[[j]]$Year),FUN=sum)
  Global00_[[j]]$Yield<-Global00_[[j]]$Prod/Global00_[[j]]$Area
  colnames(Global00_[[j]])[1]<-c("Year")
  Global00_[[j]]<-subset(Global00_[[j]],Year>=2000&Year<=2010&Yield>=0.5)
  write.csv(Global00_[[j]],paste0("C:/Users/hac809/Documents/Pughtam-cropozone/Global_evaluation_outputs/DataGlobal00",crop[j],".csv"), row.names = FALSE)
}

Lin_yield00_<-list()
Lin_prod00_<-list()
for (i in 1:3){
  Lin_yield00_[[i]]<-lm(Global00_[[i]]$Yield~Global00_[[i]]$Year)
  Lin_prod00_[[i]]<-lm(Global00_[[i]]$Prod~Global00_[[i]]$Year)
}

#soiln spinup00
soiln00<-read.table("C:/Users/hac809/Documents/output/2000/soil_npool.out",h=T)
soiln00=subset(soiln00,Year>2000 & Year<2011)
soiln00$N=rowSums(soiln00[,c("NH4", "NO3", "NO2", "NO",  "N2O", "N2")])
soiln00_<-soiln00[-c(4:9)]
vect<-c(2001:2010)
Si00_<-list()
rast_sn00_<-list()
for (i in 1:length(vect)){
  Si00_[[i]]<-subset(soiln00_,Year==vect[i])
  Si00_[[i]]<-Si00_[[i]][c(-3)]
  soiln_coords<-cbind(Si00_[[i]]$Lon,Si00_[[i]]$Lat)
  rast_sn00_[[i]]<-rasterize(soiln_coords,Si00_[[i]][,3])
}
names(rast_sn00_)<-paste0("Sn_",vect)
rast_sn00_<-stack(rast_sn00_)
rast_sn00_<-extend(rast_sn00_,bb)

#masking the soiln according to area per crop
Wsn00_<-rast_sn00_*ArNew[[1]][[41:50]]
Msn00_<-rast_sn00_*ArNew[[2]][[41:50]]
Rsn00_<-rast_sn00_*ArNew[[3]][[41:50]]
soilnBG00_<-list(Wsn00_,Msn00_,Rsn00_)
soilnDf00_<-lapply(soilnBG00_,as.data.frame,xy=TRUE)
GlobSn00_<-list()
Dfsn00_<-list()
for(j in 1:length(soilnDf00_)){
  soiln<-as.data.frame(unlist(soilnDf00_[[j]][,c(-1,-2)]))
  Lon<-as.data.frame(rep(soilnDf00_[[j]]$x,10))
  Lat<-as.data.frame(rep(soilnDf00_[[j]]$y,10))
  Year<-as.data.frame(rep(c(2001:2010),ea=259200))
  Dfsn00_[[j]]<-cbind(Lon,Lat,Year,soiln)
  colnames(Dfsn00_[[j]])<-c("Lon","Lat","Year","N")
  GlobSn00_[[j]]<-na.omit(Dfsn00_[[j]])
  GlobSn00_[[j]]<-aggregate(GlobSn00_[[j]],by=list(GlobSn00_[[j]]$Year),FUN=sum)
}
for(j in 1:length(soilnDf00_)){  
  GlobSn00_[[j]]<-GlobSn00_[[j]][,-4]
  colnames(GlobSn00_[[j]])[1]<-c("Year")
}


#soiln spinup 1910
soiln<-read.table("C:/Users/hac809/Documents/output/soil_npool.out",h=T)
soiln<-subset(soiln,Year>1960 & Year<2011)
soiln$N=rowSums(soiln[,c("NH4", "NO3", "NO2", "NO",  "N2O", "N2")])
soiln_<-soiln[-c(4:9)]
vect<-c(1961:2010)
Si_<-list()
rast_sn_<-list()
for (i in 1:length(vect)){
  Si_[[i]]<-subset(soiln_,Year==vect[i])
  Si_[[i]]<-Si_[[i]][c(-3)]
  soiln_coords<-cbind(Si_[[i]]$Lon,Si_[[i]]$Lat)
  rast_sn_[[i]]<-rasterize(soiln_coords,Si_[[i]][,3])
}
names(rast_sn_)<-paste0("Sn_",vect)
rast_sn_<-stack(rast_sn_)
rast_sn_<-extend(rast_sn_,bb)

#masking the soiln according to area per crop
Wsn_<-rast_sn_*ArNew[[1]]
Msn_<-rast_sn_*ArNew[[2]]
Rsn_<-rast_sn_*ArNew[[3]]
soilnBG_<-list(Wsn_,Msn_,Rsn_)
soilnDf_<-lapply(soilnBG_,as.data.frame,xy=TRUE)
GlobSn_<-list()
Dfsn_<-list()
for(j in 1:length(soilnDf_)){
  soiln<-as.data.frame(unlist(soilnDf_[[j]][,c(-1,-2)]))
  Lon<-as.data.frame(rep(soilnDf_[[j]]$x,50))
  Lat<-as.data.frame(rep(soilnDf_[[j]]$y,50))
  Year<-as.data.frame(rep(c(1961:2010),ea=259200))
  Dfsn_[[j]]<-cbind(Lon,Lat,Year,soiln)
  colnames(Dfsn_[[j]])<-c("Lon","Lat","Year","N")
  GlobSn_[[j]]<-na.omit(Dfsn_[[j]])
  GlobSn_[[j]]<-aggregate(GlobSn_[[j]],by=list(GlobSn_[[j]]$Year),FUN=sum)
}
for(j in 1:length(soilnDf_)){  
  GlobSn_[[j]]<-GlobSn_[[j]][,-4]
  colnames(GlobSn_[[j]])[1]<-c("Year")
}

#Graph 1 red color is FAO, blue LPJG
cropn<-c("Wheat","Maize","Rice")
X11(width=8,height=8)
par(mfrow=c(3,2))
for(i in 1:3){
  # plot(Global[[i]]$Year,Global[[i]]$Yield, type="l",col="black", xlab="", ylab="",
  #      main=cropn[i], font=2, font.lab=2, ylim=c(1.0,6.0),cex.lab=2,cex.main=2,cex.axis=1.5,ylbias=0.5,lwd=2)
  # title( ylab="Yield (t/ha)", line=2.5, cex.lab=2,font=2, font.lab=2)
  # lines(Global60_[[i]]$Year,Global60_[[i]]$Yield,col="blue",lwd=2)
  # lines(Global00_[[i]]$Year,Global00_[[i]]$Yield,col="gray",lwd=2)
  
  plot(GlobSn_[[i]]$Year,GlobSn_[[i]]$N/1000000000,type="l",col="black", xlab="", ylab="",
       main=cropn[i], font=2, font.lab=2, ylim=c(1,20.0),cex.lab=2,cex.main=2,cex.axis=1.5,ylbias=0.5,lty=2, lwd=2)
  title( ylab="Soil Nitrogen ", line=2.5, cex.lab=2,font=2, font.lab=2)
  lines(GlobSn00_[[i]]$Year,GlobSn00_[[i]]$N/1000000000,col="gray",lty=2, lwd=2)
  lines(GlobSn60_[[i]]$Year,GlobSn60_[[i]]$N/1000000000,col="Blue",lty=2, lwd=2)
  #abline(Lin_yield[[i]]$coefficients[1],Lin_yield[[i]]$coefficients[2],lwd=2)
  #abline(Lin_yield60_[[i]]$coefficients[1],Lin_yield60_[[i]]$coefficients[2],lwd=2, col="green")
  #abline(Lin_yield00_[[i]]$coefficients[1],Lin_yield00_[[i]]$coefficients[2],lwd=2, col="gray")
}
