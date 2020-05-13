#BY_GRIDCELL
#Correlation
require(plyr)
require(dplyr)
library(dplyr)
library(ggplot2)
require(forecast)
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

#memory.size()
#memory.limit(size=100000)
yield_LPJdf<-lapply(yield_LPJR,FUN=Deraster)
yield_Raydf<-lapply(crop_rayR,FUN=Deraster)
#CORRELATION
y_ext<-list()
yield_bothdf<-list()
yearsg_ma<-list()
for (i in 1:3){
  y_ext[[i]]<-cbind(yield_LPJdf[[i]][,-1],Ray=yield_Raydf[[i]][,4])
  y_ext[[i]]$label<-rep(1:259200,41 )
  yield_bothdf[[i]]<-na.omit(y_ext[[i]])
  yield_bothdf[[i]]<-yield_bothdf[[i]][yield_bothdf[[i]]$label %in% names(which(table(yield_bothdf[[i]]$label)>7)), ]
  yearsg_ma[[i]]<-yield_bothdf[[i]][,c(4,6)]
  colnames(yearsg_ma[[i]])<-c("year","label")
  yearsg_ma[[i]]<-ddply(yearsg_ma[[i]], .(label),cut)
  yield_bothdf[[i]]<-yield_bothdf[[i]][,c(6,4,1,2,3,5)]
}

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
  dtcropbg[[j]]<-cbind(year=yearsg_ma[[j]][,2],do.call("cbind",dtbg)[c(-3)])
  colnames(dtcropbg[[j]])<-c("year","label","LPJ","Ray")
}
names(dtcropbg)<-crop

corrfun<-function(x){
  COR=cor.test(x$LPJ,x$Ray)
  return(data.frame(COR$estimate,COR$p.value))
}
corrbg<-list()
corrbg1<-list()
Cropc<-list()
y_exts<-list()
for ( i in 1:3){
corrbg[[i]]<-ddply(dtcropbg[[i]], .(label),corrfun)
corrbg1[[i]]<-corrbg[[i]]
corrbg1[[i]]$COR.estimate<-ifelse(corrbg1[[i]]$COR.p.value>0.05,0,corrbg1[[i]]$COR.estimate)
y_exts[[i]]<-subset(y_ext[[i]],Years==1970)
for(j in 1:nrow(corrbg1[[i]])){
corrbg1[[i]]$Lon[j]<-y_exts[[i]][which(y_exts[[i]]$label %in% corrbg1[[i]]$label[j]),2]
corrbg1[[i]]$Lat[j]<-y_exts[[i]][which(y_exts[[i]]$label %in% corrbg1[[i]]$label[j]),3]
##par(mfrow=c(3))
# corrgrM1_<-corrgrM1[,c(-1,-3)]
# corrgrM1_<-corrgrM1_[,c(2,3,1)]
# names(corrgrM1_)[3]<-"Correlation"
# coordinates(corrgrM1_)<-~Lon+Lat
# gridded(corrgrM1_)=TRUE
# Ras_corrM<-raster(corrgrM1_)
Crop_corr<-ggplot(corrbg1[[i]],aes(xmin=Lon,ymin=Lat,xmax=Lon+0.5, ymax=Lat+0.5, fill=Correlation))
Cropc[[i]]<-Crop_corr+geom_rect()+
  guides(fill=guide_colourbar(label=TRUE))+
  ggtitle("Wheat")+
  borders('world', colour='black',size = 0.00000001)+
  xlab("") +
  ylab("") +
  scale_fill_gradientn(breaks=c(-1,-0.8,-0.5,-0.2,-0.05,-0.001,0,0.001,0.05,0.2,0.5,0.8,1), labels=c(-1,"",-0.5,"","","",0,"","","",0.5,"",1),
                       colours = c("red4","red","pink4","pink2","pink1","pink","white","lightblue3",
                                  "lightblue3","steelblue1","steelblue3","blue","blue4"),values=NULL, na.value="gray50")+
  theme(plot.title = element_text(size = 25, face = "bold", hjust=0.5),
        legend.title = element_blank(),
        legend.key.width = unit(3,"cm"),
        legend.key.height = unit(2,"cm"),
        legend.position = "bottom",
        legend.direction = "horizontal",
        axis.text = element_text(size = 15),
        axis.title.x = element_text(size = 40, vjust = -0.5),
        axis.title.y = element_text(size = 40, vjust = 0.2),
        legend.text = element_text(size = 20,vjust = -0.3),panel.background = element_rect(fill = "#BFD5E3"))
}}
grid.arrange(Cropc[[i]], nrow=3)

#REGRESSION
regfun<-function(x){
  GRY=lm(Yield~yearn*Dummy,data=x)
  return(data.frame(Slope=GRY$coefficients[4],pvi=summary(GRY)$coefficients[4, 4]))
}
yield_reg<-list()
for (i in 1:3){
yieldsdf_<-na.omit(y_ext[[i]])
yieldsdf_<-yieldsdf_[yieldsdf_$label %in% names(which(table(yieldsdf_$label)>3)), ]
yieldsdf_<-yieldsdf_[,c(1,5,2,3,4,6)]
yieldsdf_<-yieldsdf_
#yieldsdf_$Yield<-ifelse(is.na(yieldsdf_$Yield)& yieldsdf_$Ray!="NaN",0,yieldsdfm_$Yield)
#yieldsdfm_$Ray<-ifelse(yieldsdfm_$Ray=="NaN"& yieldsdfm_$Yield!="NA",0,yieldsdfm_$Ray)
yieldsdf_$yearn<-yieldsdf$Years-1969
yieldsdf_L<-yieldsdfm_[,c(1,7,3,4,5)]#LPJ data
yieldsdf_R<-yieldsdfm_[,c(1,7,3,4,6)]#Ray data
colnames(yieldsdf_L)<-names(yieldsdf_R)
yieldsdf_R$Dummy<-1
yieldsdf_L$Dummy<-0
yield_reg[[i]]<-rbind(yieldsdf_R,yieldsdf_L)
regGRY[[i]]<-ddply(yield_reg[[i]], .(label), regfun)
regGRY1[[i]]<-regGRY[[i]]
regGRY1[[i]]$Slope<-ifelse(regGRY1[[i]]$pvi>0.05,0,regGRY1[[i]]$Slope)
for(j in 1:nrow(regGRY1)){
  regGRY1[[i]]$Lon[j]<-y_exts[[i]][which(y_exts[[i]]$label==regPGRM1$label[j]),2]
  regGRY1[[i]]$Lat[j]<-y_exts[[i]][which(y_exts[[i]]$label==regPGRM1$label[j]),3]
}
}
regPGRM1_<-regPGRM1[,c(-1,-3)]
regPGRM1_<-regPGRM1_[,c(2,3,1)]

for(j in 1:nrow(corrbg1[[i]])){
  corrbg1[[i]]$Lon[j]<-y_exts[[i]][which(y_exts[[i]]$label %in% corrbg1[[i]]$label[j]),2]
  corrbg1[[i]]$Lat[j]<-y_exts[[i]][which(y_exts[[i]]$label %in% corrbg1[[i]]$label[j]),3]
# Maizgr1<-subset(Maizgr1,!is.na(Yield))
# Maizgr2<-subset(Maizgr2,!is.na(Yield))
# Maizgr1<- Maizgr1[Maizgr1$label %in% names(which(table(Maizgr1$label)>1)),]
# Maizgr2<- Maizgr2[Maizgr2$label %in% names(which(table(Maizgr2$label)>1)),]

# coordinates(regPGRM1_)<-~Lon+Lat
# gridded(regPGRM1_)=TRUE
# Ras_regM<-raster(regPGRM1_)
# plot(Ras_regM)


Wreg<-ggplot(regPGRW1_,aes(xmin=Lon,ymin=Lat,xmax=Lon+0.5, ymax=Lat+0.5, fill=Slope))
Wheatr<-Wreg+geom_rect()+
  guides(fill=guide_colourbar(label=TRUE))+
  ggtitle("Wheat")+
  borders('world', colour='black',size = 0.00000001)+
  xlab("") +
  ylab("") +
  scale_fill_gradientn(breaks=c(0.7,-0.5,-0.2,-0.05,-0.001,0,0.001,0.05,0.2,0.5,0.7), labels=c("",-0.5,"","","",0,"","","",0.5,""),
                       colours = c("red4","red","pink4","pink2","pink1","pink","white","lightblue3",
                                   "lightblue3","steelblue1","steelblue3","blue","blue4"),values=NULL, na.value="gray50")+
  theme(plot.title = element_text(size = 25, face = "bold", hjust=0.5),
        legend.title = element_blank(),
        legend.key.width = unit(3,"cm"),
        legend.key.height = unit(2,"cm"),
        legend.position = "bottom",
        legend.direction = "horizontal",
        axis.text = element_text(size = 15),
        axis.title.x = element_text(size = 40, vjust = -0.5),
        axis.title.y = element_text(size = 40, vjust = 0.2),
        legend.text = element_text(size = 20,vjust = -0.3),panel.background = element_rect(fill = "#BFD5E3"))
}
grid.arrange(Maizer,Wheatr, nrow=1)

# mapCountryData
# regPGRM2<-regPGRM1_
# regPGRM2$Slope[regPGRM2$Slope< -1]<- -1
# regPGRM2$Slope[regPGRM2$Slope> 1]<- 1


