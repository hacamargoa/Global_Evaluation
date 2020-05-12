#source("Z:/Scripts/Global_evaluation/Global_Evaluation/irrigMaizeAreaEstim.R")
#source("Z:/Scripts/Global_evaluation/Global_Evaluation/irrigWheatAreaEstim.R")

#Calling the yield output of LPJ-GUESS
setwd("C:/Users/Hector/Documents/Pughtam-cropozone/Global_evaluation_inputs")
yield=read.table("yield.out", h=TRUE)
yield$TeW<-pmax(yield$TeWW,yield$TeSW)
yield$TeWi<-pmax(yield$TeWWi,yield$TeSWi)
yield2=subset(yield,Year>1960 & Year<2011)
yield2<-yield2[c(-4,-5,-8,-9)]

vect<-c(1961:2010)
li<-list()
for (i in 1:length(vect)){
  li[[i]]<-subset(yield2,Year==vect[i])
  li[[i]]<-li[[i]][c(-3)]
}

yield_coords<-cbind(li[[1]]$Lon,li[[1]]$Lat)
crops<-names(li[[i]])[c(9,8,5,3,7,6,4)]
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
  crop_r[[i]]<-rasterize(yield_coords,li[[i]][crops[j]])
  names(crop_r[[i]])<-paste0("Yi_",vect[i])
}
names(crop_r)<-paste0("Yi_",vect)
rastcrop[[j]]<-stack(crop_r)
rastcrop[[j]]<-extend(rastcrop[[j]],bb)
}
names(rastcrop)<-paste0(crops)

#multiplied by 10 to convert yield from kg/m2 to Ton/ha production(ha)
#Divided by 0.88, 0.87, 0.91 since FAO production is assumed to have 12% water content
Maizprod<-((rastcrop[[4]]*ArNewr[[2]]+rastcrop[[3]]*ArNewi[[2]])*10/0.88)
Wheatprod<-((rastcrop[[2]]*ArNewr[[1]]+rastcrop[[1]]*ArNewi[[1]])*10/0.88)
Riceprod<-(rastcrop[[5]]*ArNew[[3]]*10/0.87)
#Soyprod<-((rastcrop[[7]]*ArNewr[[4]]+rastcrop[[6]]*ArNewi[[4]])*10/0.91)
ProdBG<-list(Wheatprod,Maizprod,Riceprod)


#global aggregation
GlobalProd<-list()
GlobalArea<-list()
Global<-list()
for(i in 1:length(ProdBG)){
GlobalProd[[i]]<-cellStats(ProdBG[[i]],'sum')
GlobalArea[[i]]<-cellStats(ArNew[[i]], 'sum')
Global[[i]]<-cbind.data.frame(year=c(1961:2010),Prod=GlobalProd[[i]],Area=GlobalArea[[i]])
Global[[i]]$Yield<-Global[[i]]$Prod/Global[[i]]$Area
write.csv(Global[[i]],paste0("C:/Users/Hector/Documents/Pughtam-cropozone/Global_evaluation_outputs/DataGlobal",crop[i],".csv"), row.names = FALSE)
}

#Country aggregation
#install.packages("rworldmap")
library(rworldmap)
year<-rep(1961:2010, each=243)
# aggregation production
agg_listc<-list()
agg_list<-list()
  for(j in 1:length(ProdBG)){
     for(i in 1:50){ 
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
aggdf<-list()
for(j in 1:length(agg_list)){
  aggdf[[j]]<-merge(agg_list[[j]],AreaBC[[j]],by=c("year","UN"))
  aggdf[[j]]<-subset(aggdf[[j]],!is.na(UN)& UN!=-99)
  aggdf[[j]]$LYield<-ifelse(aggdf[[j]]$Area==0,0,aggdf[[j]]$Prod/aggdf[[j]]$Area)
  aggdf[[j]]<-subset(aggdf[[j]],NAME_SORT!="Ashmore and Cartier Islands"& NAME_SORT!="French Guiana"& NAME_SORT!="West Bank")
  names(aggdf[[j]])<-c("year","UN","Country","LProd","LArea","LYield")
  write.csv(aggdf[[j]],paste0("C:/Users/Hector/Documents/Pughtam-cropozone/Global_evaluation_outputs/DataNational",crop[j],".csv"), row.names = FALSE)
}
