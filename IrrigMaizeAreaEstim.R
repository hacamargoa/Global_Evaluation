source("Z:/Scripts/Global_evaluation/Global_Evaluation/MaizeAreaEstim.R")

#Starting to use the HYDE cropland area
setwd("Z:/general_inputs/HYDE/Totirrig")

irrifiles<-list.files(pattern="*.asc")
To_i<-list()
for(i in 1:length(irrifiles)){
  To_i[[i]]<-raster(irrifiles[[i]])
}
ToI<-stack(To_i)
ToI0.5<-subset(ToI,x)
edTo_i<-aggregate(ToI0.5, fact=6, fun=sum)*100
crs(edTo_i)="+proj=longlat +datum=WGS84"
plot(ToI[[5]])
plot(edTo_i[[5]])
masnan<-function(x,y){
  ifelse(x=="NaN" & y==0,0,x)
}
ir_correc<-function(a,b){
  ifelse(a-b<0,a,b)
}
#Create the % of area from 1990 based on 2000
edTo_f_i<-overlay(maiz_i0.5,edTo_i[[5]],fun=fix)
diff1990_i<-((edTo_f_i-edTo_i[[4]])/edTo_f_i)
diff1990_i
Area1990_i<-overlay(diff1990_i,fun=mask)
Area1990_i<-overlay(Area1990_i,edTo_f_i,fun=masnan)
#Masking maiz2000 based on Area1990
maiz1990_i<-maiz_i0.5-(Area1990_i*maiz_i0.5)
maiz1990_i<-overlay(maiz1990,maiz1990_i,fun=ir_correc)
plot(maiz1990_i)


#Create the % of area from 1980 based on 1990
diff1980_i<-((edTo_i[[4]]-edTo_i[[3]])/edTo_i[[4]])
diff1980_i
Area1980_i<-overlay(diff1980_i,fun=mask)
Area1980_i<-overlay(Area1980_i,edTo_i[[4]],fun=masnan)
#Masking maiz1990 based on Area1980
maiz1980_i<-maiz1990_i-(Area1980_i*maiz1990_i)
maiz1980_i<-overlay(maiz1980,maiz1980_i,fun=ir_correc)
plot(maiz1980_i)
p<-maiz1980-maiz1980_i
plot(p)
#Create the % of area from 1970 based on 1980
diff1970_i<-((edTo_i[[3]]-edTo_i[[2]])/edTo_i[[3]])
diff1970_i
Area1970_i<-overlay(diff1970_i,fun=mask)
Area1970_i<-overlay(Area1970_i,edTo_i[[3]],fun=masnan)
plot(Area1970_i)
#Masking maiz1980 based on Area1970
maiz1970_i<-maiz1980_i-(Area1970_i*maiz1980_i)
maiz1970_i<-overlay(maiz1970,maiz1970_i,fun=ir_correc)
plot(maiz1970_i)
p<-maiz1970-maiz1970_i
plot(p)

#Create the % of area from 1960 based on 1970
diff1960_i<-((edTo_i[[2]]-edTo_i[[1]])/edTo_i[[2]])
diff1960_i
Area1960_i<-overlay(diff1960_i,fun=mask)
Area1960_i<-overlay(Area1960_i,edTo_i[[2]],fun=masnan)
plot(Area1960_i)
#Masking maiz1970 based on Area1960
maiz1960_i<-maiz1970_i-(Area1960_i*maiz1970_i)
maiz1960_i<-overlay(maiz1960,maiz1960_i,fun=ir_correc)
plot(maiz1960_i)
p<-maiz1960-maiz1960_i
plot(p)


Areamaiz_i<-stack(maiz1960_i, maiz1970_i, maiz1980_i, maiz1990_i, maiz_i0.5, maiz_i05ag, maiz_i10ag)

#Interpolation for every year irrigated area
lower_i <- unique(base[base < nlayers(Areamaiz_i)])

#Creating the raster with the change rate between years
s.change_i <- stack(sapply(if(length(lower_i) > 0) lower_i else nlayers(Areamaiz_i) - 1, 
                         function(x) {
                           overlay(Areamaiz_i[[x]], Areamaiz_i[[x+1]], fun=function(x1, x2) (x2-x1)/len[x])
                         }))
chg.ind_i <- ifelse(base > nlayers(s.change_i), nlayers(s.change_i), base)
out.rast_i <- stack(sapply(seq_along(xout), function(x) {
  if(xout[x] %in% xin) {
    Areamaiz_i[[base[x]]]
  } else {
    overlay(Areamaiz_i[[base[x]]], s.change_i[[chg.ind_i[x]]],
            fun=function(x1, x2) x1 + (x2*multi[x]))
  }
  
}))
#Renaming the layers
names(out.rast_i)<-paste0("maiz",xout,"_i")
out.rast_r<- out.rast-out.rast_i
names(out.rast_r)<-paste0("maiz",xout,"_r")

