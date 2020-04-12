#BY_GRIDCELL
#Correlation
require(plyr)
require(dplyr)
library(dplyr)
library(ggplot2)
library(gridExtra)
#Transforming LPJ and Ray data to data frames

colnames<-c("Lon","Lat","Yield","Years")
Deraster<-function(x){
df<-list()
for(i in 1:nlayers(x)){
  df[[i]]<-as.data.frame(x[[i]],xy=TRUE)
  df[[i]]$Years<-1969+i
}
df<-lapply(df, setNames, colnames)
return(bind_rows(df, .id = "label"))
}
#memory.size()
#memory.limit(size=100000)
LPJoutput<-list(LPJmaizeY,LPJwheatY,Raymaize,Raywheat)
yieldsdf<-lapply(LPJoutput,FUN=Deraster)
#CORRELATION
yieldsdfm<-cbind(yieldsdf[[1]],Ray=yieldsdf[[3]][,4])
yieldsdfm$label<-rep(1:259200,41 )
yieldsdfw<-cbind(yieldsdf[[2]],Ray=yieldsdf[[4]][,4])
yieldsdfw$label<-rep(1:259200,41 )

yieldsdfm1<-yieldsdfm
yieldsdfm1$Yield<-ifelse(is.na(yieldsdfm1$Yield)& yieldsdfm1$Ray!="NaN",0,yieldsdfm1$Yield)
yieldsdfm1$Ray<-ifelse(yieldsdfm1$Ray=="NaN"& yieldsdfm1$Yield!="NA",0,yieldsdfm1$Ray)
yieldsdfm1<-subset(yieldsdfm,!is.na(Yield) & !is.na(Ray))
yieldsdfm2<-yieldsdfm1[yieldsdfm1$label %in% names(which(table(yieldsdfm1$label)>8)), ]
Yearsgm<-yieldsdfm2[c(1,5)]

yieldsdfw1<-yieldsdfw
yieldsdfw1$Yield<-ifelse(is.na(yieldsdfw1$Yield)& yieldsdfw1$Ray!="NaN",0,yieldsdfw1$Yield)
yieldsdfw1$Ray<-ifelse(yieldsdfw1$Ray=="NaN"& yieldsdfw1$Yield!="NA",0,yieldsdfw1$Ray)
yieldsdfw1<-subset(yieldsdfw1,!is.na(Yield) & !is.na(Ray))
yieldsdfw2<-yieldsdfw1[yieldsdfw1$label %in% names(which(table(yieldsdfw1$label)>8)), ]
Yearsgw<-yieldsdfw2[c(1,5)]

cut<-function(x){
  cu<-x[c(-1,-2,-(length(x$Years)),-(length(x$Years)-1)),]
  return(data.frame(cu))
}
yearsgm<-ddply(Yearsgm, .(label),cut)
yearsgw<-ddply(Yearsgw, .(label),cut)
require(forecast)
detrendbg<-function(x){
  dt<-ma(x$Ray,order=5,centre=TRUE)
  dt_res<- x$Ray-dt
  dt_res<-subset(dt_res,dt_res!="NA")
  return(data.frame(dt_res))
}

detgriLPJ<-ddply(yieldsdfm2, .(label),detrendbg)#change the name of the variable in the detrendbc function to Yield
detgriRay<-ddply(yieldsdfm2, .(label),detrendbg)#change the name of the variable in the detrendbc function to Ray
detgriM<-cbind(yearsgm,LPJ=detgriLPJ[,2],Ray=detgriRay[,2])
summary(detgriM)
detgriwLPJ<-ddply(yieldsdfw2, .(label),detrendbg)#change the name of the variable in the detrendbc function to Yield
detgriwRay<-ddply(yieldsdfw2, .(label),detrendbg)#change the name of the variable in the detrendbc function to Ray
detgriW<-cbind(yearsgw,LPJ=detgriwLPJ[,2],Ray=detgriwRay[,2])
summary(detgriW)

corrfun<-function(x){
  COR=cor.test(x$LPJ,x$Ray)
  return(data.frame(COR$estimate,COR$p.value))
}
#MAIZE
corrgrM<-ddply(detgriM, .(label),corrfun)
corrgrM1<-corrgrM
corrgrM1$COR.estimate<-ifelse(corrgrM1$COR.p.value>0.05,0,corrgrM1$COR.estimate)

yieldsdfms<-subset(yieldsdfm,Years==1970)
for(i in 1:nrow(corrgrM1)){
corrgrM1$Lon[i]<-yieldsdfm[which(yieldsdfms$label==corrgrM1$label[i]),2]
corrgrM1$Lat[i]<-yieldsdfm[which(yieldsdfms$label==corrgrM1$label[i]),3]
}
corrgrM1_<-corrgrM1[,c(-1,-3)]
corrgrM1_<-corrgrM1_[,c(2,3,1)]
names(corrgrM1_)[3]<-"Correlation"

# coordinates(corrgrM1_)<-~Lon+Lat
# gridded(corrgrM1_)=TRUE
# Ras_corrM<-raster(corrgrM1_)

#WHEAT
corrgrW<-ddply(detgriW, .(label),corrfun)
corrgrW1<-corrgrW
corrgrW1$COR.estimate<-ifelse(corrgrW1$COR.p.value>0.05,0,corrgrW1$COR.estimate)

yieldsdfws<-subset(yieldsdfw,Years==1970)
for(i in 1:nrow(corrgrW1)){
  corrgrW1$Lon[i]<-yieldsdfw[which(yieldsdfws$label==corrgrW1$label[i]),2]
  corrgrW1$Lat[i]<-yieldsdfw[which(yieldsdfws$label==corrgrW1$label[i]),3]
}
corrgrW1_<-corrgrW1[,c(-1,-3)]
corrgrW1_<-corrgrW1_[,c(2,3,1)]
dim(corrgrW1_)
names(corrgrW1_)[3]<-"Correlation"

# coordinates(corrgrW1_)<-~Lon+Lat
# gridded(corrgrW1_)=TRUE
# Ras_corrW<-raster(corrgrW1_)

Wcorr<-ggplot(corrgrW1_,aes(xmin=Lon,ymin=Lat,xmax=Lon+0.5, ymax=Lat+0.5, fill=Correlation))
Wheatc<-Wcorr+geom_rect()+
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

Mcorr<-ggplot(corrgrM1_,aes(xmin=Lon,ymin=Lat,xmax=Lon+0.5, ymax=Lat+0.5, fill=Correlation))
Maizec<-Mcorr+geom_rect()+
  guides(fill=guide_colourbar(label=TRUE))+
  ggtitle("Maize")+
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
grid.arrange(Maizec,Wheatc, nrow=1)

#REGRESSION
#Maize
yieldsdfm<-yieldsdfm[,c(1,5,2,3,4,6)]
summary(yieldsdfm)
yieldsdfm_<-yieldsdfm
yieldsdfm_$Yield<-ifelse(is.na(yieldsdfm_$Yield)& yieldsdfm_$Ray!="NaN",0,yieldsdfm_$Yield)
yieldsdfm_$Ray<-ifelse(yieldsdfm_$Ray=="NaN"& yieldsdfm_$Yield!="NA",0,yieldsdfm_$Ray)
yieldsdfm_$yearn<-yieldsdfm$Years-1969

Maizgr1<-yieldsdfm_[,c(1,7,3,4,5)]#LPJ data
Maizgr2<-yieldsdfm_[,c(1,7,3,4,6)]#Ray data

#Dummy Ray is 1 LPJ is 0
Maizgr1$Dummy<-0
Maizgr2$Dummy<-1
colnames(Maizgr2)<-names(Maizgr1)
Maizgr1<-subset(Maizgr1,!is.na(Yield))
Maizgr2<-subset(Maizgr2,!is.na(Yield))
Maizgr1<- Maizgr1[Maizgr1$label %in% names(which(table(Maizgr1$label)>1)),]
Maizgr2<- Maizgr2[Maizgr2$label %in% names(which(table(Maizgr2$label)>1)),]
Maizgr3<-rbind(Maizgr1,Maizgr2)


regfun<-function(x){
  L_GRY=lm(Yield~yearn*Dummy,data=x)
  return(data.frame(Slope=L_GRY$coefficients[4],pvi=summary(L_GRY)$coefficients[4, 4]))
}

regPGRM<-ddply(Maizgr3, .(label), regfun)#change the name of the variable to M_Yield in regfun function
regPGRM1<-regPGRM
regPGRM1$Slope<-ifelse(regPGRM1$pvi>0.05,0,regPGRM1$Slope)
for(i in 1:nrow(regPGRM1)){
  regPGRM1$Lon[i]<-yieldsdfms[which(yieldsdfms$label==regPGRM1$label[i]),2]
  regPGRM1$Lat[i]<-yieldsdfms[which(yieldsdfms$label==regPGRM1$label[i]),3]
}

regPGRM1_<-regPGRM1[,c(-1,-3)]
regPGRM1_<-regPGRM1_[,c(2,3,1)]

# coordinates(regPGRM1_)<-~Lon+Lat
# gridded(regPGRM1_)=TRUE
# Ras_regM<-raster(regPGRM1_)
# plot(Ras_regM)

#Wheat
yieldsdfw<-yieldsdfw[,c(1,5,2,3,4,6)]
summary(yieldsdfw)
yieldsdfw_<-yieldsdfw
yieldsdfw_$Yield<-ifelse(is.na(yieldsdfw_$Yield)& yieldsdfw_$Ray!="NaN",0,yieldsdfw_$Yield)
yieldsdfw_$Ray<-ifelse(yieldsdfw_$Ray=="NaN"& yieldsdfw_$Yield!="NA",0,yieldsdfw_$Ray)
yieldsdfw_$yearn<-yieldsdfw$Years-1969

Wheatgr1<-yieldsdfw_[,c(1,7,3,4,5)]#LPJ data
Wheatgr2<-yieldsdfw_[,c(1,7,3,4,6)]#Ray data

#Dummy Ray is 1 LPJ is 0
Wheatgr1$Dummy<-0
Wheatgr2$Dummy<-1
colnames(Wheatgr2)<-names(Wheatgr1)
Wheatgr1<-subset(Wheatgr1,!is.na(Yield))
Wheatgr2<-subset(Wheatgr2,!is.na(Yield))
Wheatgr1<- Wheatgr1[Wheatgr1$label %in% names(which(table(Wheatgr1$label)>1)),]
Wheatgr2<- Wheatgr2[Wheatgr2$label %in% names(which(table(Wheatgr2$label)>1)),]
Wheatgr3<-rbind(Wheatgr1,Wheatgr2)


regfun<-function(x){
  L_GRY=lm(Yield~yearn*Dummy,data=x)
  return(data.frame(Slope=L_GRY$coefficients[4],pvi=summary(L_GRY)$coefficients[4, 4]))
}

regPGRW<-ddply(Wheatgr3, .(label), regfun)#change the name of the variable to M_Yield in regfun function
regPGRW1<-regPGRW
regPGRW1$Slope<-ifelse(regPGRW1$pvi>0.05,0,regPGRW1$Slope)
for(i in 1:nrow(regPGRW1)){
  regPGRW1$Lon[i]<-yieldsdfws[which(yieldsdfws$label==regPGRW1$label[i]),2]
  regPGRW1$Lat[i]<-yieldsdfws[which(yieldsdfws$label==regPGRW1$label[i]),3]
}

regPGRW1_<-regPGRW1[,c(-1,-3)]
regPGRW1_<-regPGRW1_[,c(2,3,1)]

# coordinates(regPGRW1_)<-~Lon+Lat
# gridded(regPGRW1_)=TRUE
# Ras_regW<-raster(regPGRW1_)
# plot(Ras_regW)


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
mapCountryData
regPGRM2<-regPGRM1_
regPGRM2$Slope[regPGRM2$Slope< -1]<- -1
regPGRM2$Slope[regPGRM2$Slope> 1]<- 1
Mreg<-ggplot(regPGRM2,aes(xmin=Lon,ymin=Lat,xmax=Lon+0.5, ymax=Lat+0.5, fill=Slope))
Maizer<-Mreg+geom_rect()+
  guides(fill=guide_colourbar(label=TRUE))+
  ggtitle("Maize")+
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
grid.arrange(Maizer,Wheatr, nrow=1)
