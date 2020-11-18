#BY COUNTRY COMPARISON
library(ncdf4)
library(raster)

#copy table from FAOSTAT_Bycountry.xls in Global_evaluation inputs
setwd("C:/Users/hac809/Documents/Pughtam-cropozone")
FWhe<- read.csv("Global_evaluation_inputs/FAO/FAO_WheaBC.csv", h=T)
FMai<- read.csv("Global_evaluation_inputs/FAO/FAO_MaizBC.csv", h=T)
FRic<- read.csv("Global_evaluation_inputs/FAO/FAO_RiceBC.csv", h=T)
FAO<-list(FWhe,FMai,FRic)

DataBC<-list()
DataBC1<-list()
for (i in 1:length(aggdf)){
  DataBC[[i]]<-join(aggdf[[i]],FAO[[i]][-2])
  DataBC1[[i]]<-na.omit(DataBC[[i]])
  DataBC1[[i]]<-DataBC1[[i]][!(DataBC1[[i]]$LYield<0.10 & DataBC1[[i]]$FYield>2),]
}

#1:1 regression including all the countries and years
RegunoY<-list()
RegunoP<-list()
X11(width=8,height=8)
par(mfrow=c(3,2))
for (i in 1:3){
  RegunoY[[i]]<-lm(DataBC1[[i]]$FYield~DataBC1[[i]]$LYield+0)
  RegunoP[[i]]<-lm(DataBC1[[i]]$FProd~DataBC1[[i]]$LProd+0)
  plot(DataBC1[[i]]$LProd,DataBC1[[i]]$FProd,xlab="", ylab="", main=paste0("Prod_",crop[i]),
       font=2, font.lab=2,cex.lab=2,cex.main=3,cex.axis=1.5,ylim=c(0,200000000))
  title( ylab="FAO_Prod", line=2.5, cex.lab=2,font=2, font.lab=2)
  abline(0,1,lwd=2)
  abline(RegunoP[[i]],col="red",lwd=2)
  
  plot(DataBC1[[i]]$LYield,DataBC1[[i]]$FYield,xlab="", ylab="", main=paste0("Yield_",crop[i]), 
       font=2, font.lab=2,cex.lab=2,cex.main=3,cex.axis=1.5,ylim=c(0,10),lwd=2,xlim=c(0,10))
  abline(0,1,lwd=2)
  abline(RegunoY[[i]],col="red",lwd=2)
}

#1:1 regresion by decade coloring the first producers
Deca<-list()
AvgDec<-list()
for (j in 1:3){
  for (i in 1:5){
    temp1<-subset(DataBC1[[j]],year>1960+(i*10)-10 & year<=1960+i*10)
    Deca[[i]]<-aggregate(temp1,by=list(temp1$UN),FUN=mean)
  }
  AvgDec[[j]]<-do.call("rbind",Deca)
  AvgDec[[j]]<-subset(AvgDec[[j]],LYield!=0)
  AvgDec[[j]]<-AvgDec[[j]][,-4]
  AvgDec[[j]]$LProd<-AvgDec[[j]]$LProd/1000000
  AvgDec[[j]]$FProd<-AvgDec[[j]]$FProd/1000000
}
mycol<-rgb(255,0,0,max=255,alpha=75, names="red50")
RegAvY<-list()
RegAvP<-list()
X11(width=8,height=8)
par(mfrow=c(3,2))
for (i in 1:3){
  RegAvY[[i]]<-lm(FYield~LYield+0,AvgDec[[i]])
  newvar<-data.frame(LYield=seq(0,max(AvgDec[[i]]$FYield,na.rm=TRUE)))
  IC<-as.data.frame(predict.lm(RegAvY[[i]],newdata=newvar,interval='confidence'))
  IC$Lyield<-seq(0,max(AvgDec[[i]]$FYield,na.rm=TRUE))
  RegAvP[[i]]<-lm(FProd~LProd+0,AvgDec[[i]])
  newvar1<-data.frame(LProd=seq(0,max(AvgDec[[i]]$LProd,na.rm=TRUE)+7))
  IC1<-as.data.frame(predict.lm(RegAvP[[i]],newdata=newvar1,interval='confidence'))
  IC1$Lprod<-seq(0,max(AvgDec[[i]]$LProd,na.rm=TRUE)+7)
  plot(AvgDec[[i]]$LProd,AvgDec[[i]]$FProd,xlab="", ylab="", main=paste0("Production ",cropn[i]),
       font=2, font.lab=2,cex.lab=2,cex.main=2,cex.axis=1.5,cex=2,ylim=c(0,max(AvgDec[[i]]$LProd,na.rm=TRUE)),xlim=c(0,max(AvgDec[[i]]$LProd,na.rm=TRUE)-5))
  title( ylab="FAO (Mill. Tons)",xlab=if(i==3){"LPJ GUESS (Mill. Tons)"}, line=2.5, cex.lab=2,font=2, font.lab=2)
  polygon(c(IC1$Lprod,rev(IC1$Lprod)),c(IC1$upr,rev(IC1$lwr)),col=mycol,border=NA)
  abline(0,1,lwd=2)
  abline(RegAvP[[i]],col="red",lwd=2)
  top5<-subset(DataBC1[[i]],year==2009)[,c(2,8)];top5<-na.omit(top5[order(-top5$FProd),][c(1:5),])
  temp1<-subset(AvgDec[[i]],!UN %in% top5$UN)
  temp2<-AvgDec[[i]][AvgDec[[i]]$UN %in% top5$UN,]
  plot(temp1$LYield,temp1$FYield,xlab="", ylab="", main=paste0("Yield ",cropn[i]), 
       font=2, font.lab=2,cex.lab=2,cex.main=2,cex.axis=1.5,xlim=c(0,quantile(temp1$FYield,.99,na.rm=TRUE)),
       lwd=2,ylim=c(0,quantile(AvgDec[[i]]$FYield,.99,na.rm=TRUE)),cex=log10(temp1$FProd*20))
  points(temp2$LYield,temp2$FYield,xlab="", ylab="",lwd=2,cex=log10(temp2$FProd*20),pch=19,
         col=ifelse(temp2$UN==top5$UN[1],"red",
                    ifelse(temp2$UN==top5$UN[2],"blue",
                           ifelse(temp2$UN==top5$UN[3],"green",
                                  ifelse(temp2$UN==top5$UN[4],"gray",
                                         ifelse(temp2$UN==top5$UN[5],"orange","black"))))))
  title(ylab="FAO (t/ha)", xlab=if(i==3){"LPJ GUESS (t/ha)"}, line=2.5, cex.lab=2,font=2, font.lab=2)
  polygon(c(IC$Lyield,rev(IC$Lyield)),c(IC$upr,rev(IC$lwr)),col=mycol,border=NA)
  abline(0,1,lwd=2)
  abline(RegAvY[[i]],col="red",lwd=2)
}

#1:1 regresion global and by country last decade

Deca00<-list()
AvgDec00<-list()
for (j in 1:3){
    temp1<-subset(DataBC1[[j]],year>=2000)
    AvgDec00[[j]]<-aggregate(temp1,by=list(temp1$UN),FUN=mean)
    AvgDec00[[j]]<-subset(AvgDec00[[j]],LYield!=0)
    AvgDec00[[j]]<-AvgDec00[[j]][,-4]
    }

RegGY<-list()
RegAvY00<-list()
GLOBALY<-GLOBAL1[,c(3,5,7,9,11,13)]
nam1<-colnames(GLOBALY)
X11(width=8,height=8)
par(mfrow=c(3,2))
for (i in 1:3){
  RegGY[[i]]<-lm(eval(parse(text=nam1[i]))~eval(parse(text=nam1[i+3]))+0,GLOBALY)
  newvar1<-data.frame(seq(min(min(GLOBALY[,i]),min(GLOBALY[,i+3])),max(max(GLOBALY[,i]),max(GLOBALY[,i+3]))+1.5));colnames(newvar1)<-nam1[i+3]
  PR1<-as.data.frame(predict(RegGY[[i]],newdata=newvar1,interval='confidence'))
  PR1<-cbind(newvar1,PR1)
  limi1<-c(min(min(GLOBALY[,i]),min(GLOBALY[,i+3])),max(max(GLOBALY[,i]),max(GLOBALY[,i+3])))
  plot(GLOBALY[,i+3],GLOBALY[,i],xlab="", ylab="", main=paste0("Global ",cropn[i]),font=2, 
       font.lab=2,cex.lab=2,cex.main=2,cex.axis=1.5,xlim=limi1,ylim=limi1)
  title( ylab="FAO (t/ha)", xlab=if(i==3){"LPJ GUESS (t/ha)"}, line=2.5, cex.lab=2,font=2, font.lab=2)
  polygon(c(PR1[,1],rev(PR1[,1])),c(PR1$upr,rev(PR1$lwr)),col=mycol,border=NA)
  abline(0,1,lwd=2)
  abline(RegGY[[i]],col="red",lwd=2)
  RegAvY00[[i]]<-lm(FYield~LYield+0,AvgDec00[[i]])
  newvar00<-data.frame(LYield=seq(0,max(AvgDec00[[i]]$FYield,na.rm=TRUE)))
  IC<-as.data.frame(predict.lm(RegAvY00[[i]],newdata=newvar00,interval='confidence'))
  IC$Lyield<-seq(0,max(AvgDec00[[i]]$FYield,na.rm=TRUE))
  top5<-subset(DataBC1[[i]],year==2009)[,c(2,8)];top5<-na.omit(top5[order(-top5$FProd),][c(1:5),])
  temp1<-subset(AvgDec00[[i]],!UN %in% top5$UN)
  temp2<-AvgDec00[[i]][AvgDec00[[i]]$UN %in% top5$UN,]
  limi2<-c(0,max(quantile(AvgDec[[i]]$FYield,.99,na.rm=TRUE),quantile(temp1$FYield,.99,na.rm=TRUE)))
  plot(temp1$LYield,temp1$FYield,xlab="", ylab="", main=paste0("By Country ",cropn[i]), 
       font=2, font.lab=2,cex.lab=2,cex.main=2,cex.axis=1.5,xlim=limi2,
       lwd=2,ylim=limi2,cex=log10(temp1$FProd/50000))
  points(temp2$LYield,temp2$FYield,xlab="", ylab="",lwd=2,cex=log10(temp2$FProd/50000),pch=19,
         col=ifelse(temp2$UN==top5$UN[1],"firebrick3",
                    ifelse(temp2$UN==top5$UN[2],"blue",
                           ifelse(temp2$UN==top5$UN[3],"chartreuse4",
                                  ifelse(temp2$UN==top5$UN[4],"purple",
                                         ifelse(temp2$UN==top5$UN[5],"orange","black"))))))
  title(ylab="FAO (t/ha)", xlab=if(i==3){"LPJ GUESS (t/ha)"}, line=2.5, cex.lab=2,font=2, font.lab=2)
  polygon(c(IC$Lyield,rev(IC$Lyield)),c(IC$upr,rev(IC$lwr)),col=mycol,border=NA)
  abline(0,1,lwd=2)
  abline(RegAvY00[[i]],col="red",lwd=2)
}


