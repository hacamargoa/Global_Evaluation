#Country correction by country
#install.packages("rworldmap")
library(rworldmap)
library(raster)
year<-rep(1960:2010, each=185)
agg_list<-list()
for(j in 1:length(out.rast)){
agg_listc<-list()
for(i in 1:51){ 
  agg_listc[[i]]<-as(out.rast[[j]][[i]], 'SpatialGridDataFrame')
  agg_listc[[i]]<-aggregateHalfDegreeGridToCountries(agg_listc[[i]])
  agg_listc[[i]]<-joinCountryData2Map(agg_listc[[i]],nameJoinColumn ='UN',joinCode = "UN")
  agg_listc[[i]]<-as.data.frame(agg_listc[[i]])
  agg_listc[[i]]<-agg_listc[[i]][c(22,50,51)]
  names(agg_listc[[i]])[3]<-"Area"
}
agg_list[[j]]<-lapply(agg_listc,FUN=subset,(UN!=-99 & NAME_SORT!="Ashmore and Cartier Islands" &
                                           NAME_SORT!="French Guiana" & NAME_SORT!="West Bank"))
agg_list[[j]]<-do.call("rbind", agg_list[[j]])
agg_list[[j]]<-cbind(year,agg_list[[j]])
agg_list[[j]]<-subset(agg_list[[j]],year!=1960)
}

# reported Area
FWhe<- read.csv("C:/Users/Hector/Documents/Pughtam-cropozone/Global_evaluation_inputs/FAO/FAOWheaBC.csv", h=T)
FMai<- read.csv("C:/Users/Hector/Documents/Pughtam-cropozone/Global_evaluation_inputs/FAO/FAOMaizBC.csv", h=T)
FRic<- read.csv("C:/Users/Hector/Documents/Pughtam-cropozone/Global_evaluation_inputs/FAO/FAORiceBC.csv", h=T)
FSoy<- read.csv("C:/Users/Hector/Documents/Pughtam-cropozone/Global_evaluation_inputs/FAO/FAOSoybBC.csv", h=T)
FAO<-list(FWhe,FMai,FRic,FSoy)
FAre<-lapply(FAO,subset,select=c(1,2,3,4))
FAre<-lapply(FAre,setNames,nm=c("UN","Country","year","FArea"))
FAre<-lapply(FAre,subset,year<2011)

# countries<-as.data.frame(gridCountriesDegreesHalf)
# names(countries)<-c("UN","x","y")
# dim(countries)
# countries2<-cbind(year=rep(c(1961:2010),each=85700),do.call("rbind",replicate(50,countries,simplify = FALSE)))

Ar<-list()
for (i in 1:length(FAre)){
Ar[[i]]<-merge(agg_list[[i]],FAre[[i]],by=c("year","UN"),all=TRUE)
Ar[[i]]$perc<-Ar[[i]]$Area/Ar[[i]]$FArea
Ar[[i]]$perc<-ifelse(is.na(Ar[[i]]$perc),1,Ar[[i]]$perc)
Ar[[i]]<-Ar[[i]][c(1,2,7)]
Ar[[i]]<-merge(countries2,Ar[[i]],by=c("year","UN"),all=TRUE)
Ar[[i]]$perc<-ifelse(is.na(Ar[[i]]$perc),1,Ar[[i]]$perc)
Ar[[i]]$perc<-ifelse(Ar[[i]]$perc==0,1,Ar[[i]]$perc)
Ar[[i]]$perc<-ifelse(Ar[[i]]$perc==Inf,1,Ar[[i]]$perc)
}

Rf_corr<-function(a){
  ifelse(is.na(a),0,a)
}

percr<-list()
ArNew<-list()
ArNewi<-list()
ArNewr<-list()
for(i in 1:length(Ar)){
perc<-list()
for(j in 1:50){
perc[[j]]<-subset(Ar[[i]],year==j+1960)
perc[[j]]<-perc[[j]][c(-1,-2)]
coordinates(perc[[j]])<- ~ x+y
perc[[j]]<-SpatialPixelsDataFrame(perc[[j]],tolerance=0.0125,perc[[j]]@data)
perc[[j]]<-raster(perc[[j]])
perc[[j]]<-extend(perc[[j]],bb)
}
percr[[i]]<-stack(perc)
ArNew[[i]]<-subset(out.rast[[i]],c(2:51))
ArNewi[[i]]<-subset(out.rasti[[i]],c(2:51))
ArNew[[i]]<-ArNew[[i]]/percr[[i]]
ArNewi[[i]]<-ArNewi[[i]]/percr[[i]]
ArNewi[[i]][is.na(ArNewi[[i]])]<-0
ArNewr[[i]]<-ArNew[[i]]-ArNewi[[i]]
}

# Testing Areas
year<-rep(1961:2010, each=243)
AreaBC<-list()
for(i in 1:length(ArNew)){
agg_listwa<-list()
for(j in 1:50){ 
  agg_listwa[[j]]<-as(ArNew[[i]][[j]], 'SpatialGridDataFrame')
  agg_listwa[[j]]<-aggregateHalfDegreeGridToCountries(agg_listwa[[j]])
  agg_listwa[[j]]<-joinCountryData2Map(agg_listwa[[j]],nameJoinColumn='UN',joinCode = 'UN')
  agg_listwa[[j]]<-as.data.frame(agg_listwa[[j]])
  agg_listwa[[j]]<-agg_listwa[[j]][c(50,51)]
  names(agg_listwa[[j]])[2]<-"Area"
}
AreaBC[[i]]<- do.call("rbind", agg_listwa)
AreaBC[[i]]<- cbind(year,AreaBC[[i]])
}
test.Areas<-list()
par(mfrow=c(2,2))
crop<-c("Wheat","Maize","Rice","Soybean")
for(i in 1:length(ArNew)){
test.Areas[[i]]<-merge(FAre[[i]],AreaBC[[i]],by=c("year","UN"), all=TRUE)
test.Areas[[i]]<-subset(test.Areas[[i]],!is.na(Area))
plot(test.Areas[[i]]$FArea,test.Areas[[i]]$Area, ylab="FAO", xlab="Estimation", main=paste(crop[i],"Harvested Area"))
abline(0,1)
}

