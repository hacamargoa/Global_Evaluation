source("Z:/Scripts/Global_evaluation/Global_Evaluation/SpamAreasInput.R")

library("ncdf4")
library("raster")
library(rgdal)
#Calling the Maize area database from SPAM
maize0.5<-maiz_i0.5+maiz_r0.5
maize0.5


#Starting to use the HYDE cropland area
setwd("Z:/general_inputs/HYDE/Cropland")

cropfiles<-list.files(pattern="*.asc")
To<-list()
for(i in 1:length(cropfiles)){
  To[[i]]<-raster(cropfiles[[i]])
}

ToT<-stack(To)
x<-c(1:5,15:21)
ToT0.5<-subset(ToT,x)
edTo<-aggregate(ToT0.5, fact=6, fun=sum)*100
fix<-function(a,b){
  ifelse(a > b,a,b)
}
#test<-overlay(maize0.5,edTo[[5]],fun=fix)
crs(edTo)="+proj=longlat +datum=WGS84"
mask<-function(x1){
  ifelse(x1=="-Inf",-1,x1)
}

#Create the % of area from 1990 based on 2000
edTo_f<-overlay(maize0.5,edTo[[5]],fun=fix)
diff1990<-((edTo_f-edTo[[4]])/edTo_f)
Area1990<-overlay(diff1990,fun=mask)
#Masking maiz2000 based on Area1990
maiz1990<-maize0.5-(Area1990*maize0.5)


#Create the % of area from 1980 based on 1990
diff1980<-((edTo[[4]]-edTo[[3]])/edTo[[4]])
diff1980
Area1980<-overlay(diff1980,fun=mask)
#Masking maiz1990 based on Area1980
maiz1980<-maiz1990-(Area1980*maiz1990)


#Create the % of area from 1970 based on 1980
diff1970<-((edTo[[3]]-edTo[[2]])/edTo[[3]])
diff1970
Area1970<-overlay(diff1970,fun=mask)
#Masking maiz1980 based on Area1970
maiz1970<-maiz1980-(Area1970*maiz1980)


#Create the % of area from 1960 based on 1970
diff1960<-((edTo[[2]]-edTo[[1]])/edTo[[2]])
diff1960
Area1960<-overlay(diff1960,fun=mask)
#Masking maiz1970 based on Area1960
maiz1960<-maiz1970-(Area1960*maiz1970)

maiz2005<-maiz_i05ag+maiz_r05ag
maiz2010<-maiz_i10ag+maiz_r10ag
Areamaiz<-stack(maiz1960, maiz1970, maiz1980, maiz1990, maize0.5, maiz2005, maiz2010)

#Interpolation for every year
xin<-c(1960,1970,1980,1990,2000,2005,2010)
xout<-c(1960:2010)
  
len <- diff(xin)
base <- findInterval(xout, xin)
lower <- unique(base[base < nlayers(Areamaiz)])

  #Creating the raster with the change rate between years
  s.change <- stack(sapply(if(length(lower) > 0) lower else nlayers(Areamaiz) - 1, 
                  function(x) {
                  overlay(Areamaiz[[x]], Areamaiz[[x+1]], fun=function(x1, x2) (x2-x1)/len[x])
                  }))
  multi <- xout - xin[base]
  chg.ind <- ifelse(base > nlayers(s.change), nlayers(s.change), base)
  out.rast <- stack(sapply(seq_along(xout), function(x) {
      if(xout[x] %in% xin) {
      Areamaiz[[base[x]]]
      } else {
      overlay(Areamaiz[[base[x]]], s.change[[chg.ind[x]]],
              fun=function(x1, x2) x1 + (x2*multi[x]))
      }
 
    }))
  #Renaming the layers
  names(out.rast)<-paste0("maiz",xout)

