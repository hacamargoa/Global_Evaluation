#source("Z:/Scripts/Global_evaluation/Global_Evaluation/SpamAreasInput.R")
library(ncdf4)
library(raster)
library(rlist)

#Calling the area database from SPAM and adding irrigated and rainfed per crop
Ye_Ar<-list()
for(i in 1:7){
Ye_Ar[[i]]<-list()
for(j in 1:3){
Ye_Ar[[i]][[j]]=Areas[[i]][[j]]+Areas[[i+1]][[j]]
}
names(Ye_Ar[[i]])<-c("2000","2005","2010")
}
Ye_Ar<-Ye_Ar[c(-2,-4,-6)]
names(Ye_Ar)<-c("wheat","maize","rice","soyb")

#Starting to use the HYDE cropland area
setwd("C:/Users/hac809/Documents/Pughtam-cropozone/Global_evaluation_inputs/Cropland")
cropfiles<-list.files(pattern="*.asc")
To<-list()
for(i in 1:length(cropfiles)){
  To[[i]]<-raster(cropfiles[[i]])
}

#aggregate to 0.5x0.5 and transf. to Ha
ToT<-stack(To)
ToT<-subset(ToT,c(1:5))
ToT0.5<-aggregate(ToT, fact=6, fun=sum)*100
crs(ToT0.5)="+proj=longlat +datum=WGS84"

#function to avoid higherSPAM values than HYDE
fix<-function(a,b){
  ifelse(a > b,a,b)
}

#calculation of the relative variation per decade from HYDE and application to SPAM
AreaCrl<-list()
AreaC<-list()
for(j in 1:4){
ToT0.5[[5]]<-overlay(Ye_Ar[[j]][[1]],ToT0.5[[5]],fun=fix)
#Create the % of area from 1990 based on 2000
diff<-list()
AreaC[[j]]<-list(NULL,NULL,NULL,NULL,Ye_Ar[[j]][[1]])
for(i in 1:4){
diff[[5-i]]<-ToT0.5[[6-i]]/ToT0.5[[5-i]]
AreaC[[j]][[5-i]]<-AreaC[[j]][[6-i]]/diff[[5-i]]
}
names(AreaC[[j]])<-c("1960","1970","1980","1990","2000")
AreaCrl[[j]]<-list.append(AreaC[[j]],"2005"=Ye_Ar[[j]][[2]],"2010"=Ye_Ar[[j]][[3]])
AreaCrl[[j]]<-stack(AreaCrl[[j]])
}
names(AreaCrl)<-c("wheat","maize","rice","soyb")
#AreaCrl tested changing as below
#Ye_Ar[[4]][[1]]-AreaCrl[[4]][[5]]#should be 0 for same crops and year (2000)
#plot(AreaCrl[[1]][[5]])#same pattern for same crop

#Interpolation for every year
xin<-c(1960,1970,1980,1990,2000,2005,2010)
xout<-c(1960:2010)
len <- diff(xin)
base <- findInterval(xout, xin)

out.rast<-list()
for(i in 1:length(AreaCrl)){
lower <- unique(base[base < nlayers(AreaCrl[[i]])])
  #Creating the raster with the change rate between years
  s.change <- stack(sapply(if(length(lower) > 0) lower else nlayers(AreaCrl[[i]]) - 1, 
                  function(x) {
                  overlay(AreaCrl[[i]][[x]], AreaCrl[[i]][[x+1]], fun=function(x1, x2) (x2-x1)/len[x])
                  }))
  multi <- xout - xin[base]
  chg.ind <- ifelse(base > nlayers(s.change), nlayers(s.change), base)
  out.rast[[i]]<- stack(sapply(seq_along(xout), function(x) {
      if(xout[x] %in% xin) {
      AreaCrl[[i]][[base[x]]]
      } else {
      overlay(AreaCrl[[i]][[base[x]]], s.change[[chg.ind[x]]],
              fun=function(x1, x2) x1 + (x2*multi[x]))
      }
    }))
  #Renaming the layers
  names(out.rast[[i]])<-paste0(xout)
}
names(out.rast)<-c("wheat","maize","rice","soyb")
#out.rast tested as below
#out.rast[[4]][[51]]-AreaCrl[[4]][[7]]#should be 0
#plot(out.rast[[4]][[51]])#same pattern for same crop
#out.rast[[4]][[31]]-out.rast[[4]][[32]]#continuous layers should be different
