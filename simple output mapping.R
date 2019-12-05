library(sp)
library("raster")

getwd()
setwd("C:/Users/Hector/Desktop/Ozone_manure/data/env")
hdate=read.table("hdate.txt", h=T)
summary(hdate)
hdate2=subset(hdate,Year>2000 & Year<2002)
summary(hdate2)
hdate3<-hdate[c(-3,-4,-5,-7,-8)]
summary(hdate3)

coordinates(hdate3)<- ~ Lon+Lat
class(hdate3)
gridded(hdate3)<-TRUE
hdate4<-raster(hdate3)
plot(hdate4,main='hdateCo')

