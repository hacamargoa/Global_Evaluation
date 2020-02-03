getwd()
setwd("Z:/general_inputs/Irrigation")
library("ncdf4")
library("raster")
#Extracting dataframe from NetCDF
irarea<-nc_open("HID_aei_ha.nc")
print(irarea)
area<-brick("HID_aei_ha.nc")
#Using only the first set of estimated data (14 layers)
areas<-subset(area,1:14)
#flipping bcse somehow the data is upside down
area1<-t(flip(areas, direction='y'))
plot(area1[[1]])
res(area1)
#looping to transform the coordinates from image to lon/lat in all layers 
a<-nlayers(area1)
AEI<-list()
r<-list()
for (i in 1:a){
  AEI[[i]]<-values(area1[[i]])
  r[[i]]<-raster(nrows = 2160, ncols = 4320)
  values(r[[i]])<-AEI[[i]]
}
ar<-stack(r)
plot(ar[[14]])
crs(ar)<-"+proj=utm +zone=48 +datum=WGS84"

#agregate from 1/12 to 0.5 degrees
area0.5<-aggregate(ar, fact=6, fun=sum)
plot(area0.5[[14]])
area0.5

#convert in data.frame
areadf<-as.data.frame(area2[[1]],xy=T)
class(areadf)
summary(areadf)
head(areadf)
colnames(areadf)<-c("Lon","Lat","Area")
#creating a raster from the dataframe
dim(areadf)
coordinates(areadf)<- ~ Lon+Lat
class(areadf)
gridded(areadf)<-TRUE
areadf1<-raster(areadf)
plot(areadf1,main='AREA')
