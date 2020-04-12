source("Z:/Scripts/Global_evaluation/Global_Evaluation/WheatAreaEstim.R")

#Starting to use the HYDE cropland area
setwd("Z:/general_inputs/HYDE/Totirrig")

# irrifiles<-list.files(pattern="*.asc")
# To_i<-list()
# for(i in 1:length(irrifiles)){
#   To_i[[i]]<-raster(irrifiles[[i]])
# }
# ToI<-stack(To_i)
# ToI0.5<-subset(ToI,x)
# edTo_i<-aggregate(ToI0.5, fact=6, fun=sum)*100
# crs(edTo_i)="+proj=longlat +datum=WGS84"
# plot(ToI[[5]])
# plot(edTo_i[[5]])
# masnan<-function(x,y){
#   ifelse(x=="NaN" & y==0,0,x)
# }
#


#Create the % of area from 1990 based on 2000
wedTo_f_i<-overlay(wheat_i0.5,edTo_i[[5]],fun=fix)
wdiff1990_i<-((wedTo_f_i-edTo_i[[4]])/wedTo_f_i)
wdiff1990_i
wArea1990_i<-overlay(wdiff1990_i,fun=mask)
#Masking wheat2000 based on Area1990
wheat1990_i<-wheat_i0.5-(wArea1990_i*wheat_i0.5)
plot(wheat1990_i)
wheat1990_i<-overlay(wheat1990_i,edTo_i[[5]],fun=masnan)
wheat1990_i<-overlay(wheat1990,wheat1990_i,fun=ir_correc)


#Create the % of area from 1980 based on 1990
wdiff1980_i<-((edTo_i[[4]]-edTo_i[[3]])/edTo_i[[4]])
wdiff1980_i
wArea1980_i<-overlay(wdiff1980_i,fun=mask)
#Masking wheat1990 based on Area1980
wheat1980_i<-wheat1990_i-(wArea1980_i*wheat1990_i)
plot(wheat1980_i)
wheat1980_i<-overlay(wheat1980_i,edTo_i[[4]],fun=masnan)
wheat1980_i<-overlay(wheat1980,wheat1980_i,fun=ir_correc)


#Create the % of area from 1970 based on 1980
wdiff1970_i<-((edTo_i[[3]]-edTo_i[[2]])/edTo_i[[3]])
wdiff1970_i
wArea1970_i<-overlay(wdiff1970_i,fun=mask)
plot(wArea1970_i)
#Masking wheat1980 based on Area1970
wheat1970_i<-wheat1980_i-(wArea1970_i*wheat1980_i)
plot(wheat1970_i)
wheat1970_i<-overlay(wheat1970_i,edTo_i[[3]],fun=masnan)
wheat1970_i<-overlay(wheat1970,wheat1970_i,fun=ir_correc)

#Create the % of area from 1960 based on 1970
wdiff1960_i<-((edTo_i[[2]]-edTo_i[[1]])/edTo_i[[2]])
wdiff1960_i
wArea1960_i<-overlay(wdiff1960_i,fun=mask)
plot(wArea1960_i)
#Masking wheat1970 based on Area1960
wheat1960_i<-wheat1970_i-(wArea1960_i*wheat1970_i)
plot(wheat1960_i)
wheat1960_i<-overlay(wheat1960_i,edTo_i[[2]],fun=masnan)
wheat1960_i<-overlay(wheat1960,wheat1960_i,fun=ir_correc)
p<-wheat1960-wheat1960_i
plot(p)


Areawheat_i<-stack(wheat1960_i, wheat1970_i, wheat1980_i, wheat1990_i, wheat_i0.5, wheat_i05ag, wheat_i10ag)

#Interpolation for every year irrigated area
wlower_i <- unique(base[base < nlayers(Areawheat_i)])

#Creating the raster with the change rate between years
ws.change_i <- stack(sapply(if(length(wlower_i) > 0) wlower_i else nlayers(Areawheat_i) - 1, 
                         function(x) {
                           overlay(Areawheat_i[[x]], Areawheat_i[[x+1]], fun=function(x1, x2) (x2-x1)/len[x])
                         }))
wchg.ind_i <- ifelse(base > nlayers(ws.change_i), nlayers(ws.change_i), base)
out.rastW_i <- stack(sapply(seq_along(xout), function(x) {
  if(xout[x] %in% xin) {
    Areawheat_i[[base[x]]]
  } else {
    overlay(Areawheat_i[[base[x]]], ws.change_i[[wchg.ind_i[x]]],
            fun=function(x1, x2) x1 + (x2*multi[x]))
  }
  
}))
#Renaming the layers
names(out.rastW_i)<-paste0("wheat",xout,"_i")
out.rastW_r<- out.rastW-out.rastW_i
names(out.rastW_r)<-paste0("wheat",xout,"_r")

