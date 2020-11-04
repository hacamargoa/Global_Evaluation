#source("Z:/Scripts/Global_evaluation/Global_Evaluation/SpamAreasInput.R")
library(ncdf4)
library(raster)
library(rlist)

#Subsetting irrigated areas from SPAM
Ir_Ar<-Areas[c(-2,-4,-6,-8)]

#Starting to use the HYDE cropland area
setwd("C:/Users/hac809/Documents/Pughtam-cropozone/Global_evaluation_inputs/TotIrrig")
irrfiles<-list.files(pattern="*.asc")
To<-list()
for(i in 1:length(irrfiles)){
  To[[i]]<-raster(irrfiles[[i]])
}

ToT<-stack(To)
ToT<-subset(ToT,c(1:5))
ToTi0.5<-aggregate(ToT, fact=6, fun=sum)*100# 100 to convert from Km2 to ha
crs(ToTi0.5)="+proj=longlat +datum=WGS84"

#Function to avoid higher irrigated than total area
ir_correc<-function(a,b){
  ifelse(a-b<0,a,b)
}
AreaCi<-list()
AreaCrli<-list()
for(j in 1:4){
ToTi0.5[[5]]<-overlay(Ir_Ar[[j]][[1]],ToTi0.5[[5]],fun=fix)
#Create the % of area from 1990 based on 2000
diff<-list()
AreaCi[[j]]<-list(NULL,NULL,NULL,NULL,Ir_Ar[[j]][[1]])
for(i in 1:4){
diff[[5-i]]<-(ToTi0.5[[6-i]]/ToTi0.5[[5-i]])
AreaCi[[j]][[5-i]]<-AreaCi[[j]][[6-i]]/diff[[5-i]]
}
names(AreaCi[[j]])<-c("1960","1970","1980","1990","2000")
AreaCrli[[j]]<-list.append(AreaCi[[j]],"2005"=Ir_Ar[[j]][[2]],"2010"=Ir_Ar[[j]][[3]])
AreaCrli[[j]]<-stack(AreaCrli[[j]])
AreaCrli[[j]]<-overlay(AreaCrl[[j]],AreaCrli[[j]],fun=ir_correc)
}
names(AreaCrli)<-c("wheat","maize","rice","soyb")
#AreaCrli tested changing as below
#Ir_Ar[[3]][[3]]-AreaCrli[[1]][[7]]#should be 0 for same crops and year (2000)
#plot(AreaCrl[[2]][[5]])#same pattern for same crop

#Interpolation for every year
xin<-c(1960,1970,1980,1990,2000,2005,2010)
xout<-c(1960:2010)
len <- diff(xin)
base <- findInterval(xout, xin)

out.rasti<-list()
for(i in 1:length(AreaCrli)){
lower <- unique(base[base < nlayers(AreaCrli[[i]])])
  #Creating the raster with the change rate between years
  s.change <- stack(sapply(if(length(lower) > 0) lower else nlayers(AreaCrli[[i]]) - 1, 
                  function(x) {
                  overlay(AreaCrli[[i]][[x]], AreaCrli[[i]][[x+1]], fun=function(x1, x2) (x2-x1)/len[x])
                  }))
  multi <- xout - xin[base]
  chg.ind <- ifelse(base > nlayers(s.change), nlayers(s.change), base)
  out.rasti[[i]]<- stack(sapply(seq_along(xout), function(x) {
      if(xout[x] %in% xin) {
      AreaCrli[[i]][[base[x]]]
      } else {
      overlay(AreaCrli[[i]][[base[x]]], s.change[[chg.ind[x]]],
              fun=function(x1, x2) x1 + (x2*multi[x]))
      }
    }))
  #Renaming the layers
  names(out.rasti[[i]])<-paste0(xout)
}
names(out.rasti)<-c("wheat","maize","rice","soyb")
#calculating the rainfed area
#out.rasti tested as below
#out.rasti[[4]][[31]]-AreaCrli[[4]][[4]]#should be 0
#plot(out.rasti[[4]][[31]])#same pattern for same crop

