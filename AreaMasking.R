source("Z:/Scripts/Global_evaluation/Global_Evaluation/irrigMaizeAreaEstim.R")
source("Z:/Scripts/Global_evaluation/Global_Evaluation/irrigWheatAreaEstim.R")

#Calling the yield output of LPJ-GUESS
setwd("Z:/ozone/ozone_manure/Output")
yield=read.table("yield.out", h=TRUE)
summary(yield)
yield$TeW<-pmax(yield$TeWW,yield$TeSW)
yield$TeWi<-pmax(yield$TeWWi,yield$TeSWi)
head(yield)
yield2=subset(yield,Year>1959 & Year<2011)
summary(yield2)
yield3<-yield2[c(-4,-5,-7,-8)]
summary(subset(yield3,Year==2010))
dim(yield3)
vect<-c(1960:2010)
li<-list()
Corn_r<-list()
Corn_i<-list()
Wheat_r<-list()
Wheat_i<-list()

for (i in 1:length(vect)){
  li[[i]]<-subset(yield3,Year==vect[i])
  li[[i]]<-li[[i]][c(-3)]
}

yield_coords<-cbind(li[[1]]$Lon,li[[1]]$Lat)

rasterize<-function(coord,value){
  areas<-SpatialPointsDataFrame(coords=coord,data.frame(value),proj4string = CRS("+proj=longlat +datum=WGS84"))
  areas2<-SpatialPixelsDataFrame(areas,tolerance=0.0192077,areas@data) 
  areas3<-as(areas2,"SpatialGridDataFrame")
  Rstr<-raster(areas3)
}

for (i in 1:length(vect)){
  Corn_r[[i]]<-rasterize(yield_coords,li[[i]]$TeCo)
  Corn_i[[i]]<-rasterize(yield_coords,li[[i]]$TeCoi)
  Wheat_r[[i]]<-rasterize(yield_coords,li[[i]]$TeW)
  Wheat_i[[i]]<-rasterize(yield_coords,li[[i]]$TeWi)
 
  }
YieldM_i<-stack(Corn_i)
YieldM_r<-stack(Corn_r)
YieldW_i<-stack(Wheat_i)
YieldW_r<-stack(Wheat_r)
names(YieldM_i)<-paste0("YieldM",xout,"_i")
names(YieldM_r)<-paste0("YieldM",xout,"_r")
names(YieldW_i)<-paste0("YieldW",xout,"_i")
names(YieldW_r)<-paste0("YieldW",xout,"_r")
bb <- extent(-180, 180, -90, 90)
yieldM_i<-extend(YieldM_i,bb)
yieldM_r<-extend(YieldM_r,bb)
yieldW_i<-extend(YieldW_i,bb)
yieldW_r<-extend(YieldW_r,bb)

#multiplied by 10 to convert yield from kg/m2 to Ton/ha production(ha)
#Divided by 0.88 since FAO production is assumed to have 12% water content
Maizprod<-((yieldM_r*out.rast_r+yieldM_i*out.rast_i)*10/0.88)
names(Maizprod)<-xout
Wheatprod<-((yieldW_r*out.rastW_r+yieldW_i*out.rastW_i)*10/0.88)
names(Wheatprod)<-xout

#correcting the coordinates rounding to one decimal(resourse use bcse did not know how to do it)
out.rast<-(yieldM_i/yieldM_i)*out.rast
out.rastW<-(yieldW_i/yieldW_i)*out.rastW

#global aggregation 
WheatGlobalProd <-cellStats(Wheatprod, 'sum')
MaizGlobalProd <-cellStats(Maizprod, 'sum')
WheatGlobalArea<-cellStats(out.rastW, 'sum')
MaizGlobalArea <-cellStats(out.rast, 'sum')
Global<-cbind.data.frame(c(1960:2010),WheatGlobalProd, MaizGlobalProd,WheatGlobalArea,MaizGlobalArea)
write.csv(Global,'Z:/Other simulations/First paper/DataGlobal.csv', row.names = TRUE)

#Country aggregation
library(rworldmap)

#rast_list<-list(Wheatprod, Maizprod, out.rastW, out.rast)

year<-rep(1960:2010, each=243)

# Wheat production
agg_listw<-list()
 for(i in 1:51){ 
  agg_listw[[i]]<-as(Wheatprod[[i]], 'SpatialGridDataFrame')
  agg_listw[[i]]<-aggregateHalfDegreeGridToCountries(agg_listw[[i]])
  agg_listw[[i]]<-joinCountryData2Map(agg_listw[[i]],nameJoinColumn ='UN',joinCode = "UN")
  agg_listw[[i]]<-as.data.frame(agg_listw[[i]])
  agg_listw[[i]]<-agg_listw[[i]][c(22,50,51)]
    names(agg_listw[[i]])[3]<-"WProd"
     }

Wprod<- do.call("rbind", agg_listw) 
head(Wprod)
Wprod<- cbind(year,Wprod)
tail(Wprod)
# maize production
agg_listm<-list()
for(i in 1:51){ 
  agg_listm[[i]]<-as(Maizprod[[i]], 'SpatialGridDataFrame')
  agg_listm[[i]]<-aggregateHalfDegreeGridToCountries(agg_listm[[i]])
  agg_listm[[i]]<-joinCountryData2Map(agg_listm[[i]],nameJoinColumn='UN',joinCode = 'UN')
  agg_listm[[i]]<-as.data.frame(agg_listm[[i]])
  agg_listm[[i]]<-agg_listm[[i]][c(50,51)]
  names(agg_listm[[i]])[2]<-"MProd"
}
Mprod<- do.call("rbind", agg_listm) 
Mprod<- cbind(year,Mprod)

# Wheat Area
agg_listwa<-list()
for(i in 1:51){ 
  agg_listwa[[i]]<-as(out.rastW[[i]], 'SpatialGridDataFrame')
  agg_listwa[[i]]<-aggregateHalfDegreeGridToCountries(agg_listwa[[i]])
  agg_listwa[[i]]<-joinCountryData2Map(agg_listwa[[i]],nameJoinColumn='UN',joinCode = 'UN')
  agg_listwa[[i]]<-as.data.frame(agg_listwa[[i]])
  agg_listwa[[i]]<-agg_listwa[[i]][c(50,51)]
  names(agg_listwa[[i]])[2]<-"WArea"
}
Warea<- do.call("rbind", agg_listwa) 
Warea<- cbind(year,Warea)
head(Warea)
# maize Area
agg_listma<-list()
for(i in 1:51){
  agg_listma[[i]]<-as(out.rast[[i]], 'SpatialGridDataFrame')
  agg_listma[[i]]<-aggregateHalfDegreeGridToCountries(agg_listma[[i]])
  agg_listma[[i]]<-joinCountryData2Map(agg_listma[[i]],nameJoinColumn='UN',joinCode = 'UN')
  agg_listma[[i]]<-as.data.frame(agg_listma[[i]])
  agg_listma[[i]]<-agg_listma[[i]][c(50,51)]
  names(agg_listma[[i]])[2]<-"MProd"
}
Marea<- do.call("rbind", agg_listma)
Marea<- cbind(year,Marea)
head(Marea)
By_country<-cbind(Wprod,Mprod[3],Warea[3],Marea[3])
colnames(By_country)[2:7]<-c("Country","UN","Wheat_prod","Maize_prod","Wheat_area", "Maize_area")
head(By_country)
#remiving repeated entries
By_country<-subset(By_country,Country != "Ashmore and Cartier Islands" & Country != "French Guiana" )
summary(By_country)

#exporting the final dataframe
write.csv(By_country,'Z:/Other simulations/First paper/DataByCountry.csv', row.names = TRUE)
