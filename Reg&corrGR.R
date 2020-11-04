#BY_GRIDCELL
#Correlation
library(raster)
require(plyr)
library(dplyr)
require(forecast)
library(rworldmap)
#Transforming LPJ and Ray data to data frames

Deraster<-function(x){
df<-list()
for(i in 1:nlayers(x)){
  df[[i]]<-as.data.frame(x[[i]],xy=TRUE)
  df[[i]]$year<-1969+i
}
df<-lapply(df, setNames, c("Lon","Lat","Yield","Years"))
return(bind_rows(df, .id = "label"))
}
crop_rayR1<-list()
for (i in 1:3){
  crop_rayR1[[i]]<-crop_rayR[[i]][[1:40]]
}
yield_LPJdf<-lapply(yield_LPJR,FUN=Deraster)
yield_Raydf<-lapply(crop_rayR1,FUN=Deraster)

prod_Raydf<-lapply(Prod_ray,FUN=Deraster)
prod_Raydf1<-list()
for (i in 1:3){
colnames(prod_Raydf[[i]])[4]<-"Prod"
prod_Raydf1[[i]]<-subset(prod_Raydf[[i]],Years==2010)
prod_Raydf1[[i]]$Prod2<-prod_Raydf1[[i]]$Prod/1000000
}
y_ext<-list()
yield_bothdf<-list()
yearsg_ma<-list()
for (i in 1:3){
  y_ext[[i]]<-cbind(yield_LPJdf[[i]][,-1],Ray=yield_Raydf[[i]][,4])
  y_ext[[i]]$label<-rep(1:259200,40 )
  yield_bothdf[[i]]<-na.omit(y_ext[[i]])
  yield_bothdf[[i]]<-yield_bothdf[[i]][yield_bothdf[[i]]$label %in% names(which(table(yield_bothdf[[i]]$label)>7)), ]
  yearsg_ma[[i]]<-yield_bothdf[[i]][,c(4,6)]
  colnames(yearsg_ma[[i]])<-c("year","label")
  yearsg_ma[[i]]<-ddply(yearsg_ma[[i]], .(label),cut)
  yield_bothdf[[i]]<-yield_bothdf[[i]][,c(6,4,1,2,3,5)]
}

#DETREND
dtcropbg<-list()
for (j in 1:3){
  dtbg<-list()
  for(i in 1:2){
    detrendbg<-function(x){
    dt<-ma(x[,i+4],order=5,centre=TRUE)
    dt_res<- x[,i+4]-dt
    dt_res<-subset(dt_res,!is.na(dt_res))
    return(data.frame(dt_res))
  }
    dtbg[[i]]<-ddply(yield_bothdf[[j]], .(label),detrendbg)
  }
  dtcropbg[[j]]<-join(yearsg_ma[[j]],do.call("cbind",dtbg)[c(-3)])
  colnames(dtcropbg[[j]])<-c("year","label","LPJ","Ray")
}
names(dtcropbg)<-crop

#CORRELATION
corrfunG<-function(x){
  COR=cor.test(x$LPJ,x$Ray)
  n1<-length(x$LPJ)
  n2<-length(x$Ray)
  #COR1=cor.test(x$LPJ[-1],x$Ray[-n2])#one year swift back in LPJ (LPJ2001vsRay2000)
  #COR2=cor.test(x$LPJ[-n1],x$Ray[-1])#one year swift back in LPJ (LPJ2000vsRay2001)
  return(data.frame(COR$estimate,COR$p.value))
 }
corrbg<-list()
corrbg1<-list()
#corrbg2<-list()
#corrindex<-list()
Cropc<-list()
y_exts<-list()
for ( i in 1:3){
corrbg[[i]]<-ddply(dtcropbg[[i]], .(label),corrfunG)
corrbg1[[i]]<-corrbg[[i]]
corrbg1[[i]]$COR.estimate<-ifelse(corrbg1[[i]]$COR.p.value>0.05,corrbg1[[i]]$COR.estimate==0,corrbg1[[i]]$COR.estimate)
#corrbg1[[i]]$COR1.estimate<-ifelse(corrbg1[[i]]$COR1.p.value>0.05,corrbg1[[i]]$COR1.estimate==0,corrbg1[[i]]$COR1.estimate)
#corrbg1[[i]]$COR2.estimate<-ifelse(corrbg1[[i]]$COR2.p.value>0.05,corrbg1[[i]]$COR2.estimate==0,corrbg1[[i]]$COR2.estimate)
#corrbg2[[i]]<-corrbg1[[i]]
#temp<-colnames(corrbg1[[i]][c(2,4,6)])[max.col(corrbg1[[i]][c(2,4,6)],ties.method="first")]
#corrbg2[[i]]$index<-ifelse(temp=="COR.estimate",0,ifelse(temp=="COR1.estimate",1,2))
#corrbg2[[i]]$CORy<-pmax(corrbg2[[i]]$COR.estimate,corrbg2[[i]]$COR1.estimate,corrbg2[[i]]$COR2.estimate)
#corrbg2[[i]]<-corrbg2[[i]][-c(2:7)]
y_exts[[i]]<-subset(y_ext[[i]],Years==1970)
for(j in 1:nrow(corrbg1[[i]])){
corrbg1[[i]]$Lon[j]<-y_exts[[i]][which(y_exts[[i]]$label==corrbg1[[i]]$label[j]),1]
corrbg1[[i]]$Lat[j]<-y_exts[[i]][which(y_exts[[i]]$label==corrbg1[[i]]$label[j]),2]
corrbg[[i]]$Lon[j]<-y_exts[[i]][which(y_exts[[i]]$label==corrbg[[i]]$label[j]),1]
corrbg[[i]]$Lat[j]<-y_exts[[i]][which(y_exts[[i]]$label==corrbg[[i]]$label[j]),2]
#corrbg2[[i]]$Lon[j]<-y_exts[[i]][which(y_exts[[i]]$label==corrbg2[[i]]$label[j]),1]
#corrbg2[[i]]$Lat[j]<-y_exts[[i]][which(y_exts[[i]]$label==corrbg2[[i]]$label[j]),2]
}}

scorrbg1<-list()
linesc<-list()
X11(width=4,height=9)
par(mfrow=c(3,1),omi=c(0.2,0.1,0,0),mai=c(0.2,0.1,0.1,0.1))
for (i in 1:3){
  scorrbg1[[i]]<-corrbg1[[i]][,c(4,5,2)]
  coordinates(scorrbg1[[i]])<-c("Lon","Lat")
  gridded(scorrbg1[[i]])<-TRUE
  scorrbg1[[i]]<-raster(scorrbg1[[i]])
  scorrbg1[[i]]<-extend(scorrbg1[[i]],extent(-180, 180, -90, 90))
  scorrbg1[[i]]<-as(scorrbg1[[i]],"SpatialGridDataFrame")
  mapYcorGR<-mapGriddedData(scorrbg1[[i]],catMethod = c(-1,-0.8,-0.6,-0.4,-0.2,-0.00001,0.00001,0.2,0.4,0.6,0.8,1),
                         colourPalette = c("red4","red","hotpink1","plum1","pink","white","deepskyblue1","dodgerblue","steelblue3","blue","blue4"),
                         borderCol = "gray",oceanCol="azure2",xlim=c(-180,180),ylim=c(57,90),landCol="gray",addLegend=FALSE)
  if(i==3){do.call(addMapLegend,c(mapYcorGR, legendLabels="all", legendWidth=1.1,digits=1,legendShrink=0.7,legendMar=2))}
  title( main =paste0("Corr. Yield ",cropn[i]), line=-0.5, cex.main=2,font.main=2)
}
scorrbg<-list()
X11(width=4,height=9)
par(mfrow=c(3,1),omi=c(0.0,0.0,0.0,0.0),mai=c(0.65,0.5,0.65,0.1))
for (i in 1:3){ 
    scorrbg[[i]]<-corrbg[[i]]
    #scorrbg[[i]]$CORy<-pmax(scorrbg[[i]]$COR.estimate,scorrbg[[i]]$COR1.estimate,scorrbg[[i]]$COR2.estimate)
    temp<-na.omit(scorrbg[[i]])
    # for(j in 1:nrow(temp)){
    # temp$CORy.p[j]<-temp[j,max.col(temp[j,c(2,4,6)],ties.method="first")*2+1]
    # }
    temp$CORy.sig<-as.factor(ifelse(temp$COR.p.value>0.05,0,1))
    scorrbg[[i]]<-temp
    linesc[[i]]<-join(scorrbg[[i]][,c(4,5,2,6)],prod_Raydf1[[i]][,c(2,3,6)],type = "inner")
    plot(linesc[[i]][,5]*1000,linesc[[i]][,3],xlab="", ylab="", main="",col=c("gray","black")[linesc[[i]]$CORy.sig])
    title( main=paste0(cropn[i]),line=2,font=2, font.lab=2,cex.lab=2,cex.main=2,cex.axis=1.5)
    title( ylab="Corr. Coeff (r)", xlab=if(i==3){"Production (Thousand Tons)"},line=2.5,cex.lab=1.5,font=2, font.lab=2)
}

#REGRESSION
regfunG<-function(x){
  GRY=lm(Yield~yearn*Dummy,data=x)
  return(data.frame(Slope=GRY$coefficients[4],pvi=summary(GRY)$coefficients[4, 4]))
}
yield_reg<-list()
regGRY<-list()
regGRY1<-list()
Cropr<-list()
y_exts<-list()
for (i in 1:3){
yieldsdf_<-na.omit(y_ext[[i]])
yieldsdf_<-yieldsdf_[yieldsdf_$label %in% names(which(table(yieldsdf_$label)>3)), ]
yieldsdf_<-yieldsdf_[,c(6,4,1,2,3,5)]
yieldsdf_$yearn<-yieldsdf_$Years-1969
yieldsdf_L<-yieldsdf_[,c(1,7,3,4,5)]#LPJ data
yieldsdf_R<-yieldsdf_[,c(1,7,3,4,6)]#Ray data
colnames(yieldsdf_R)<-names(yieldsdf_L)
yieldsdf_R$Dummy<-1
yieldsdf_L$Dummy<-0
yield_reg[[i]]<-rbind(yieldsdf_R,yieldsdf_L)
regGRY[[i]]<-ddply(yield_reg[[i]], .(label), regfunG)
regGRY1[[i]]<-regGRY[[i]]
regGRY1[[i]]$Slope<-ifelse(regGRY1[[i]]$pvi>0.05,0,regGRY1[[i]]$Slope)
y_exts[[i]]<-subset(y_ext[[i]],Years==1970)
for(j in 1:nrow(regGRY1[[i]])){
  regGRY1[[i]]$Lon[j]<-y_exts[[i]][which(y_exts[[i]]$label==regGRY1[[i]]$label[j]),1]
  regGRY1[[i]]$Lat[j]<-y_exts[[i]][which(y_exts[[i]]$label==regGRY1[[i]]$label[j]),2]
  regGRY[[i]]$Lon[j]<-y_exts[[i]][which(y_exts[[i]]$label==regGRY[[i]]$label[j]),1]
  regGRY[[i]]$Lat[j]<-y_exts[[i]][which(y_exts[[i]]$label==regGRY[[i]]$label[j]),2]
}
}

sregGRY1<-list()
sregGRP1<-list()
X11(width=4,height=9)
par(mfrow=c(3,1),omi=c(0.2,0.1,0,0),mai=c(0.2,0.1,0.1,0.1))
for (i in 1:3){
  sregGRY1[[i]]<-regGRY1[[i]][,c(4,5,2)]
  coordinates(sregGRY1[[i]])<-c("Lon","Lat")
  gridded(sregGRY1[[i]])<-TRUE
  sregGRY1[[i]]<-raster(sregGRY1[[i]])
  sregGRY1[[i]]<-extend(sregGRY1[[i]],extent(-180, 180, -90, 90))
  sregGRY1[[i]]<-as(sregGRY1[[i]],"SpatialGridDataFrame")
  mapYGR<-mapGriddedData(sregGRY1[[i]],catMethod = c(-0.6,-0.4,-0.2,-0.00001,0.00001,0.2,0.4,0.6),
                         colourPalette = c("red","hotpink1","plum1","white","steelblue1","steelblue3","blue"),
                         borderCol =,oceanCol="azure2",xlim=c(-180,180),ylim=c(57,90),landCol="gray",addLegend=FALSE)
  if(i==3){do.call(addMapLegend,c(mapYGR, legendLabels="all", legendWidth=0.9,digits=1,legendShrink=0.8,legendMar = 2))}
  title(main =paste0("Diff. ",crop[i]," (t/ha*yr)"), line=-6, cex.main=2,font.main=2)
}
X11(width=4,height=9)
par(mfrow=c(3,1),omi=c(0.0,0.0,0.0,0.0),mai=c(0.65,0.5,0.65,0.1))
lines<-list()
  for (i in 1:3){
  lines[[i]]<-join(regGRY1[[i]][,c(4,5,2)],prod_Raydf1[[i]][,c(2,3,6)],type = "inner")
  plot(lines[[i]][,4]*1000,lines[[i]][,3],xlab="", ylab="", main="")
  title( main=paste0(crop[i]),line=2,font=2, font.lab=2,cex.lab=2,cex.main=2,cex.axis=1.5)
  title( ylab="Slope Difference", xlab=if(i==3){"Production (Thousand Tons)"},line=2.5,cex.lab=1.5,font=2, font.lab=2)
  }

  # sregGRP1[[i]]<-prod_Raydf1[[i]][,c(2,3,6)]
  # coordinates(sregGRP1[[i]])<-c("Lon","Lat")
  # gridded(sregGRP1[[i]])<-TRUE
  # sregGRP1[[i]]<-raster(sregGRP1[[i]])
  # sregGRP1[[i]]<-extend(sregGRP1[[i]],extent(-180, 180, -90, 90))
  # sregGRP1[[i]]<-as(sregGRP1[[i]],"SpatialGridDataFrame")
  # mapPGR<-mapGriddedData(sregGRP1[[i]],catMethod = c(0,0.05,0.1,0.15,0.2,0.3,0.5,1),
  #                        colourPalette = c("white","plum1","hotpink1","tomato2","red","firebrick","red4"),
  #                        borderCol = "black",oceanCol="aquamarine",xlim=c(-180,180),ylim=c(57,90),landCol="gray",addLegend=FALSE)
  # if(i==3){do.call(addMapLegend,c(mapPGR, legendLabels="all", legendWidth=0.9,digits=2,legendShrink=0.8,legendMar = 0))}
  # title( main =paste0("Prod. ",crop[i]," (Mill. Tons)"), line=-6, cex.main=2,font.main=2)
  # }

  #counting the total grids used
  #test<-(!is.na(getValues(Maizprod[[50]])))
  #length(test[test==TRUE])