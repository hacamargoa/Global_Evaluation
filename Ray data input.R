getwd()
setwd("C:/Users/HAC809/Documents/Hector/Project/DataBases/Yield/Ray/Ray gridded/")
library(sp)
library(ncdf4)
library(raster)
#example to read the info
ex<-nc_open("Maize_areaweightedyield_2003_ver12b.nc")
ex
#reading the Ray files and stacking them in raster files
MRay<-list.files(pattern="Maize")
RRay<-list.files(pattern="Rice")
WRay<-list.files(pattern="Wheat")
Maize_ray<-list()
Rice_ray<-list()
Wheat_ray<-list()
for(i in 1:length(MRay)){
  Maize_ray[[i]]<-raster(MRay[[i]],varname="Data")
  Rice_ray[[i]]<-raster(RRay[[i]],varname="Data")
  Wheat_ray[[i]]<-raster(WRay[[i]],varname="Data")
  }

Rayrice<-stack(Rice_ray)
Raymaize<-stack(Maize_ray)
Raywheat<-stack(Wheat_ray)
plot(Raymaize[[33]])
Rayrice<-subset(Rayrice,1:41)
Raymaize<-subset(Raymaize,1:41)
Raywheat<-subset(Raywheat,1:41)
xoutRay<-c(1970:2010)
names(Rayrice)<-paste0("RayYieldR",xoutRay)
names(Raymaize)<-paste0("RayYieldM",xoutRay)
names(Raywheat)<-paste0("RayYieldW",xoutRay)
bb <- extent(-180, 180, -90, 90)
Rayrice<-extend(Rayrice,bb)
Raymaize<-extend(Raymaize,bb)
Raywheat<-extend(Raywheat,bb)
plot(Raymaize[[41]])
#calling the model output and calculating average gridded yield acording to irrigated and rainfed areas
# yieldLPJ=subset(yield,Year>1969 & Year<2014)
# coordinates(yieldLPJ)=~Lon+Lat
# gridded(yieldLPJ)=TRUE
# yieldLPJ<-yieldLPJ[c(-2,-3,-5,-6)]
# head(yieldLPJ)
# yieldLPJ1<-subset(yieldLPJ,Year==1970)
# yieldLPJ1<-yieldLPJ1[1,2]
# head(yieldLPJ)
# summary(yieldLPJ1)
# 
# test<-raster(yieldLPJ1)
# plot(test)
# Rasterstack<-function(x){
#   pp<-list()
#   for(i in 1:length(xoutRay)){
#     pp[[i]]<-subset(yieldLPJ,Year==1969+i)
#     pp[[i]]<-pp[[i]][match(x,names(yieldLPJ))]
#     pp[[i]]<-raster(pp[[i]])
#     }
#   return(list(stack(pp)))
# }
# LPJoutlist<-list("TeCo","TeCoi","TeW","TeWi")
# ppp<-lapply(LPJoutlist,FUN=Rasterstack)

#calling the already created output from LPJ
LPJmaize<-subset(Maizprod,11:51)
LPJwheat<-subset(Wheatprod,11:51)
gridareaM<-subset(out.rast,11:51)
gridareaW<-subset(out.rastW,11:51)
LPJmaizeY<-LPJmaize/gridareaM
LPJwheatY<-LPJwheat/gridareaW
names(LPJmaizeY)<-paste0("LPJYieldM",xoutRay)
names(LPJwheatY)<-paste0("LPJYieldW",xoutRay)
plot(LPJmaize[[1]])
plot(gridareaM[[1]])
plot(LPJmaizeY[[1]])
plot(LPJwheatY[[1]])
