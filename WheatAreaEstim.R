
library("ncdf4")
library("raster")
library(rgdal)
#Calling the wheat area database from SPAM
wheat0.5<-wheat_i0.5+wheat_r0.5
wheat0.5


#Starting to use the HYDE cropland area
setwd("Z:/general_inputs/HYDE/Cropland")

cropfiles<-list.files(pattern="*.asc")
To<-list()
for(i in 1:length(cropfiles)){
  To[[i]]<-raster(cropfiles[[i]])
}

# ToT<-stack(To)
# x<-c(1:5,15:21)
# ToT0.5<-subset(ToT,x)
# edTo<-aggregate(ToT0.5, fact=6, fun=sum)*100
# fix<-function(a,b){
#   ifelse(a > b,a,b)
# }
# #test<-overlay(wheat0.5,edTo[[5]],fun=fix)
# crs(edTo)="+proj=longlat +datum=WGS84"
# mask<-function(x1){
#   ifelse(x1=="-Inf",-1,x1)
# }

#Create the % of area from 1990 based on 2000
wedTo_f<-overlay(wheat0.5,edTo[[5]],fun=fix)
wdiff1990<-((wedTo_f-edTo[[4]])/wedTo_f)
wArea1990<-overlay(wdiff1990,fun=mask)
#Masking wheat2000 based on Area1990
wheat1990<-wheat0.5-(wArea1990*wheat0.5)


#Create the % of area from 1980 based on 1990
wdiff1980<-((edTo[[4]]-edTo[[3]])/edTo[[4]])
wdiff1980
wArea1980<-overlay(wdiff1980,fun=mask)
#Masking wheat1990 based on Area1980
wheat1980<-wheat1990-(wArea1980*wheat1990)


#Create the % of area from 1970 based on 1980
wdiff1970<-((edTo[[3]]-edTo[[2]])/edTo[[3]])
wdiff1970
wArea1970<-overlay(wdiff1970,fun=mask)
#Masking wheat1980 based on Area1970
wheat1970<-wheat1980-(wArea1970*wheat1980)


#Create the % of area from 1960 based on 1970
wdiff1960<-((edTo[[2]]-edTo[[1]])/edTo[[2]])
wdiff1960
wArea1960<-overlay(wdiff1960,fun=mask)
#Masking wheat1970 based on Area1960
wheat1960<-wheat1970-(wArea1960*wheat1970)

wheat2005<-wheat_i05ag+wheat_r05ag
wheat2010<-wheat_i10ag+wheat_r10ag
Areawheat<-stack(wheat1960, wheat1970, wheat1980, wheat1990, wheat0.5, wheat2005, wheat2010)

#Interpolation for every year
xin<-c(1960,1970,1980,1990,2000,2005,2010)
xout<-c(1960:2010)
  
len <- diff(xin)
base <- findInterval(xout, xin)
wlower <- unique(base[base < nlayers(Areawheat)])

  #Creating the raster with the change rate between years
  ws.change <- stack(sapply(if(length(wlower) > 0) wlower else nlayers(Areawheat) - 1, 
                  function(x) {
                  overlay(Areawheat[[x]], Areawheat[[x+1]], fun=function(x1, x2) (x2-x1)/len[x])
                  }))
  multi <- xout - xin[base]
  wchg.ind <- ifelse(base > nlayers(ws.change), nlayers(ws.change), base)
  out.rastW <- stack(sapply(seq_along(xout), function(x) {
      if(xout[x] %in% xin) {
      Areawheat[[base[x]]]
      } else {
      overlay(Areawheat[[base[x]]], ws.change[[wchg.ind[x]]],
              fun=function(x1, x2) x1 + (x2*multi[x]))
      }
 
    }))

  #Renaming the layers
  names(out.rastW)<-paste0("wheat",xout)
  plot(out.rastW[[5]])
