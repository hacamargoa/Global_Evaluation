library(sp)
library("raster")

setwd("Z:/ozone/ozone/Output")
HI=read.table("HI.out", h=T)
summary(HI)
HI2=subset(HI,Year>1997 & Year<2013)
summary(HI2)
HI3<-HI2[c(-4,-5,-7,-8,-6)]
summary(HI3)

coordinates(HI3)<- ~ Lon+Lat
class(HI3)
gridded(HI3)<-TRUE
years<-as.list(seq (1998,2012,1)) 
sub<-list()
sub1<-list()
sub2<-list()
for(i in 1:length(years)){
  sub[[i]]<-subset(HI3,Year==years[[i]])
  sub1[[i]]<-sub[[i]][c(-1)]
  sub2[[i]]<-raster(sub1[[i]])
 }
outrast<-stack(sub2)
plot(outrast[[1]],zlim=c(0,1),main='HI Maize 1998')
plot(outrast[[2]],zlim=c(0,1),main='HI Maize 1999')
plot(outrast[[3]],zlim=c(0,1),main='HI Maize 2000')
plot(outrast[[4]],zlim=c(0,1),main='HI Maize 2001')
plot(outrast[[5]],zlim=c(0,1),main='HI Maize 2002')
plot(outrast[[6]],zlim=c(0,1),main='HI Maize 2003')
plot(outrast[[7]],zlim=c(0,1),main='HI Maize 2004')
plot(outrast[[8]],zlim=c(0,1),main='HI Maize 2005')
plot(outrast[[9]],zlim=c(0,1),main='HI Maize 2006')
plot(outrast[[10]],zlim=c(0,1),main='HI Maize 2007')
plot(outrast[[11]],zlim=c(0,1),main='HI Maize 2008')
plot(outrast[[12]],zlim=c(0,1),main='HI Maize 2009')
plot(outrast[[13]],zlim=c(0,1),main='HI Maize 2010')
plot(outrast[[14]],zlim=c(0,1),main='HI Maize 2011')
plot(outrast[[15]],zlim=c(0,1),main='HI Maize 2012')



     