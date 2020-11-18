#source ("Z:/Scripts/Global_evaluation/Global_Evaluation/mergingFAOandRAy.R")
library(rworldmap)
library(forecast)
library(plyr)
library(dplyr)

#GLOBAL COMPARISON
LGlobal<-do.call("cbind",Global)[-c(2,3,4,8,9,10,11,15,16,17,18)]
names(LGlobal)<-c("year","LW_Prod","LW_Area","LW_Yield","LM_Prod","LM_Area","LM_Yield","LR_Prod","LR_Area","LR_Yield")
GLOBAL<-na.omit(join(FGlobal,LGlobal))
na<-c("Prod_","Area_","Yield_")

GLOBAL1<-GLOBAL[,c(-3,-6,-9,-12,-15,-18)]
GLOBAL1$yearn<-GLOBAL1$year-1960
GLOBALa<-GLOBAL1[,c(2:7,14)]
GLOBALb<-GLOBAL1[,c(8:13,14)]
colnames(GLOBALa)<-c("W_Prod","W_Yield","M_Prod","M_Yield","R_Prod","R_Yield","yearn");colnames(GLOBALb)<-names(GLOBALa)
GLOBAL2<-rbind(GLOBALa,GLOBALb)

#Dummy FAO is 1 LPJ is 0
GLOBAL2$Dummy<-c(rep(1,48),rep(0,48))

#GLOBAL
#Global Trend yield (regression) FAO-LPJ is the interaction parameter
Lin_yield<-list()
Lin_prod<-list()
for (i in 1:3){
  Lin_yield[[i]]<-lm(GLOBAL2[,2*i]~GLOBAL2$yearn*GLOBAL2$Dummy)
  Lin_prod[[i]]<-lm(GLOBAL2[,2*i-1]~(GLOBAL2$yearn*GLOBAL2$Dummy))
}
Parms<-function(x){
  c(Slope=(x$coefficients[4]),pv=coef(summary(x))[4, "Pr(>|t|)"])
}

GYparms<-lapply(Lin_yield,FUN = Parms)
GPparms<-lapply(Lin_prod,FUN = Parms)
var<-c("W_slodiff","W_pv","M_slodiff","M_pv","R_slodiff","R_pv")
RegG<-data.frame(var,yield=unlist(GYparms),Prod=unlist(GPparms))


#moving average detrend
detrend<-function(x){
  dt<-ma(x,order=7,centre=TRUE)
  dt_res<- x-dt
  dt_res<-subset(dt_res,!is.na(dt_res))
}
#GLOBAL CORR
Corr_Glob<-list()
Corr_GlobP<-list()
Corr_GlobY2<-list()
yielddt<-list()
X11(width=4,height=8)
par(mfrow=c(3,1))
for (i in 1:3){
  Fdty<-detrend(FGlobal[c(2:49),3*i+1])
  Fdtp<-detrend(FGlobal[c(2:49),3*i-1])
  Ldty<-detrend(LGlobal[,3*i+1])
  Ldtp<-detrend(LGlobal[,3*i-1])
  Corr_Glob[[i]]<-cor.test(Fdty,Ldty)
  Corr_GlobP[[i]]<-cor.test(Fdtp,Ldtp)
  Corr_GlobY2[[i]]<-cor.test(Fdty[c(19:42)],Ldty[c(19:42)])
  plot(Year=c(1962:2009),Fdty,col="red",type="l",main=cropn[i], font=2, font.lab=2, cex.lab=2,cex.main=2,cex.axis=1.5,ylab="",xlab="",lwd=2)
  title( ylab="Detrended Yield (t/ha)", line=2.5, cex.lab=2,font=2, font.lab=2)
  lines(Year=c(1962:2009),Ldty,col="blue",lwd=2) 
}

Ftest<-list()
for(i in 1:3){
  Ftest[[i]]<-var.test(detrend(FGlobal[c(2:49),3*i+1]),detrend(LGlobal[,3*i+1]),alternative = "two.sided")
}
names(Ftest)<-paste0(names(GLOBAL1[c(2:7)]),"_var")


#BY_COUNTRY
#Correlation
#Function tu remove the first two and last two years by country to match with m.a. data. 
cut<-function(x){
  cu<-x[c(-1,-2,-(length(x$year)),-(length(x$year)-1)),]
  return(data.frame(cu))
}
DataBC2<-list()
years_ma<-list()
for (i in 1:length(DataBC1)){
  DataBC2[[i]]<-na.omit(DataBC1[[i]])
  DataBC2[[i]]<-DataBC2[[i]][DataBC2[[i]]$UN %in% names(which(table(DataBC2[[i]]$UN)>7)), ]
  zeroes<-ddply(DataBC2[[i]], .(UN),summarise,sum=sum(LYield))
  DataBC2[[i]]<-DataBC2[[i]][DataBC2[[i]]$UN %in% zeroes[c(which(zeroes[,2]!=0)),1], ]
  DataBC2[[i]]<-DataBC2[[i]][c(-5,-7)]
  years_ma[[i]]<-DataBC2[[i]][c(1,2)]
  years_ma[[i]]<-ddply(years_ma[[i]], .(UN),cut)
}

dtcropbc<-list()
for(j in 1:3){
dtbc<-list()
for(i in 1:4){  
detrendbc<-function(x){
    dt<-ma(x[,i+3],order=5,centre=TRUE)
    dt_res<- x[,i+3]-dt
    dt_res<-subset(dt_res,!is.na(dt_res))
    return(data.frame(dt_res))
}
dtbc[[i]]<-ddply(DataBC2[[j]], .(UN),detrendbc)
}
dtcropbc[[j]]<-cbind(year=years_ma[[j]][,1],do.call("cbind",dtbc)[c(-3,-5,-7)])
colnames(dtcropbc[[j]])<-colnames(DataBC2[[j]][c(-3)])
}
names(dtcropbc)<-cropn

# Avera<-function(x){
#   avLP<-mean(x$LProd);avLY<-mean(x$LYield);avFP<-mean(x$FProd);avFY<-mean(x$FYield)
#   return(data.frame(avLP,avLY,avFP,avFY))
# }
# #change the name of the variable as needed in Average function
# DavgBC<-list()
# par(mfrow=c(3,2))
# for(i in 1:length(DataBC1)){
#   DavgBC[[i]]<-ddply(DataBC1[[i]], .(UN),Avera)
#   plot(DavgBC[[i]]$avLP,DavgBC[[i]]$avFP,xlab="", ylab="", main=paste0("Prod_",crop[i]),
#        font=2, font.lab=2,cex.lab=2,cex.main=3,cex.axis=1.5,ylim=c(0,200000000))
#   title( ylab="FAO_Prod", line=2.5, cex.lab=2,font=2, font.lab=2)
#   abline(0,1,lwd=2)
#   abline(RegunoP[[i]],col="red",lwd=2)
#   plot(DavgBC[[i]]$avLY,DavgBC[[i]]$avFY,xlab="", ylab="", main=paste0("Yield_",crop[i]), 
#        font=2, font.lab=2,cex.lab=2,cex.main=3,cex.axis=1.5,ylim=c(0,10),lwd=2,xlim=c(0,10))
#   abline(0,1,lwd=2)
#   abline(RegunoY[[i]],col="red",lwd=2)
# }

corrfun<-function(x){
  CORp=cor.test(x$LProd,x$FProd)
  CORy=cor.test(x$LYield,x$FYield)
  n1<-length(x$FYield)
  n2<-length(x$LYield)
  #CORy1=cor.test(x$LYield[-1],x$FYield[-n1])#one year swift back in LPJ (LPJ2001vsFAO2000)
  #CORy2=cor.test(x$LYield[-n2],x$FYield[-1])#one year swift back in LPJ (LPJ2000vsFAO2001)
  return(data.frame(CORp$estimate,CORp$p.value,CORy$estimate,CORy$p.value))
}
corrbc<-list()
corrbc1<-list()
corrbc2<-list()
corrmap<-list()
X11(width=4,height=9)
par(mfrow=c(3,1),omi=c(0.3,0,0,0),mai=c(0,0,0,0))
for(i in 1:3){
corrbc[[i]]<-ddply(dtcropbc[[i]], .(UN),corrfun)
corrbc1[[i]]<-corrbc[[i]]
corrbc1[[i]]$CORp.estimate<-ifelse(corrbc1[[i]]$CORp.p.value>0.05,corrbc1[[i]]$CORp.estimate==0,corrbc1[[i]]$CORp.estimate)
corrbc1[[i]]$CORy.estimate<-ifelse(corrbc1[[i]]$CORy.p.value>0.05,corrbc1[[i]]$CORy.estimate==0,corrbc1[[i]]$CORy.estimate)
#corrbc1[[i]]$CORy1.estimate<-ifelse(corrbc1[[i]]$CORy1.p.value>0.05,corrbc1[[i]]$CORy1.estimate==0,corrbc1[[i]]$CORy1.estimate)
#corrbc1[[i]]$CORy2.estimate<-ifelse(corrbc1[[i]]$CORy2.p.value>0.05,corrbc1[[i]]$CORy2.estimate==0,corrbc1[[i]]$CORy2.estimate)
#corrbc2[[i]]<-corrbc1[[i]]
#corrbc2[[i]]$CORyi<-pmax(corrbc1[[i]]$CORy.estimate,corrbc1[[i]]$CORy1.estimate,corrbc1[[i]]$CORy2.estimate)
#corrbc2[[i]]<-corrbc[[i]][-c(3:9)]
#write.csv(corrbc1[[i]],paste0('C:/Users/hac809/Documents/Pughtam-cropozone/Global_evaluation_outputs/corrBC',crop[[i]],'.csv'), row.names = FALSE)
corrmap[[i]]<-joinCountryData2Map(corrbc1[[i]],joinCode="UN", nameJoinColumn="UN")
#mapcorP<-mapCountryData(corrmap[[i]], nameColumnToPlot="CORp.estimate", numCats=30,missingCountryCol = gray(0.8),borderCol = "black", 
               #catMethod = c(-1,-0.8,-0.6,-0.4,-0.2,-0.00001,0.00001,0.2,0.4,0.6,0.8,1),
               #colourPalette = c("red4","red","hotpink1","plum1","pink","white","deepskyblue1","dodgerblue","steelblue3","blue","blue4"),
              # oceanCol="aquamarine",xlim=c(-180,180),ylim=c(57,90), mapTitle = "",addLegend=FALSE)
#if(i==3){do.call(addMapLegend,c(mapcorP,legendMar=0,legendLabels="all", legendWidth=0.8,digits=1))}
#title( main = paste0("Corr. Prod. ",cropn[i]), line=-6, cex.main=2,font.main=2)

mapcorY<-mapCountryData(corrmap[[i]], nameColumnToPlot="CORy.estimate", numCats=30,missingCountryCol = gray(0.8),
                        borderCol = "black",catMethod = c(-1,-0.8,-0.6,-0.4,-0.2,-0.00001,0.00001,0.2,0.4,0.6,0.8,1),
                colourPalette = c("red4","red","hotpink1","plum1","pink","white","deepskyblue1","dodgerblue","steelblue3","blue","blue4"),
               oceanCol="azure2",xlim=c(-180,180),ylim=c(57,90), mapTitle = "",addLegend=FALSE)
if(i==3){do.call(addMapLegend,c(mapcorY,legendMar=0,legendLabels="all", legendWidth=0.8,digits=1))}
title( main = paste0("Corr. Yield ",cropn[i]), line=-6, cex.main=2,font.main=2)
}

#line to check the number of countries and the non-significant, negatives or positives for Slopep and Slopey
length(na.omit(corrbc[[3]]$CORy.estimate))
dim(na.omit(subset(corrbc1[[2]],CORy.estimate==0)))
dim(na.omit(subset(corrbc1[[3]],CORy.estimate>0)))

#line to check the non-significant, positives and negative countries from 2010 top ten producers
count<-list()
Top1nscorr<-list()
for (i in 1:3){
  count[[i]]<-subset(corrbc1[[i]],CORy.estimate>0)
  Top1nscorr[[i]]<-Top10[[i]][Top10[[i]]$UN %in% count[[i]]$UN,2]
}

#line to check the areas of non-significant countries
Areans<-list()
Arean<-data.frame()
temp<-subset(FGlobal[-c(2,4,5,7,8,10)],year==2010)
for (i in 1:3){
  Areans[[i]]<-subset(FAO[[i]][FAO[[i]]$UN %in% nscount[[i]]$UN,],year==2010)
  Arean[i,1]<-sum(na.omit(Areans[[i]]$FArea))
  Arean[i,2]<-temp[1,i+1]
  Arean[i,3]<-100*Arean[i,1]/Arean[i,2]
}

#Regression
regfun<-function(x){
  BCP=lm(LProd~yearn*Dummy,data=x)
  BCY=lm(LYield~yearn*Dummy,data=x)
  return(data.frame(Slopep=BCP$coefficients[4],pvp=summary(BCP)$coefficients[4, 4],
                    Slopey=BCY$coefficients[4],pvy=summary(BCY)$coefficients[4, 4],
                    SlopeF=BCY$coefficients[2]+BCY$coefficients[4]))
}

DummyBC<-list()
regBC<-list()
regBC1<-list()
slopemap<-list()
X11(width=8,height=9)
par(mfrow=c(3,2),omi=c(0.3,0,0,0),mai=c(0,0,0,0))
for(i in 1:3){
  DummyBC[[i]]<-DataBC1[[i]]
  tem1<-DummyBC[[i]][c(1,2,8,9)]
  tem1$Dummy<-1
  tem2<-DummyBC[[i]][c(1,2,4,6)]
  tem2$Dummy<-0
  colnames(tem1)<-colnames(tem2)
  DummyBC[[i]]<-rbind(tem1,tem2)
  DummyBC[[i]]$yearn<-DummyBC[[i]]$year-1960
  regBC[[i]]<-ddply(DummyBC[[i]], .(UN),regfun)
  regBC1[[i]]<-regBC[[i]]
  regBC1[[i]]$Slopep<-ifelse(regBC1[[i]]$pvp>0.05,regBC1[[i]]$Slopep==0,regBC1[[i]]$Slopep)
  regBC1[[i]]$Slopey<-ifelse(regBC1[[i]]$pvy>0.05,regBC1[[i]]$Slopey==0,regBC1[[i]]$Slopey)
  #write.csv(regBC1[[i]],paste0('C:/Users/hac809/Documents/Pughtam-cropozone/Global_evaluation_outputs/slodifBC',crop[[i]],'.csv'), row.names = FALSE)
  slopemap[[i]]<-joinCountryData2Map(regBC1[[i]],joinCode="UN", nameJoinColumn="UN")
  slopemap[[i]]$Slopep<-ifelse(slopemap[[i]]$Slopep==0,0,slopemap[[i]]$Slopep/1000000)
  mapP<-mapCountryData(slopemap[[i]], nameColumnToPlot="Slopep", numCats=30,missingCountryCol = gray(0.8),
               catMethod = c(-4,-3,-2,-1,-0.00001,0,1,2,3), mapTitle = "",
               colourPalette = c("red4","red","hotpink1","plum1","white","steelblue1","steelblue3","blue"),
               borderCol = "black",oceanCol="azure2", xlim=c(-180,180),ylim=c(57,90),addLegend=FALSE)
  if(i==3){do.call(addMapLegend,c(mapP, legendMar=0,legendLabels="all", legendWidth=0.8,digits=0))}
  title( main =paste0("Diff. ",cropn[i]," (Mill.Tons/yr)"), line=-6, cex.main=2,font.main=2)
  mapY<-mapCountryData(slopemap[[i]], nameColumnToPlot="Slopey", numCats=30,missingCountryCol = gray(0.8),
               catMethod = c(-0.3,-0.2,-0.1,-0.00001,0,0.1,0.2,0.3,0.4,0.5,0.6), mapTitle = "",
               colourPalette = c("red4","red","hotpink1","white","powderblue","deepskyblue","dodgerblue","steelblue","blue","blue4"),
               borderCol = "black",oceanCol="azure2", xlim=c(-180,180),ylim=c(57,90),addLegend=FALSE)
  if(i==3){do.call(addMapLegend,c(mapY, legendMar=0,legendLabels="all", legendWidth=0.8))}
  title( main = paste0("Diff. ",cropn[i]," (t/ha*yr)"), line=-6, cex.main=2,font.main=2)
}

#map for GDP and FAO Slope
GDP<-read.csv("C:/Users/hac809/Documents/Pughtam-cropozone/Harvest Index/GDPpC.csv",h=T)
GDPbC10<-data.frame(Country=GDP[,1],Code=GDP[,2],GDP=GDP[,53])
X11(width=4,height=5)
par(mfrow=c(1,1),omi=c(0,0,0,0),mai=c(0,0,0,0))  
GDPmap<-joinCountryData2Map(GDPbC10,joinCode="ISO3", nameJoinColumn="Code")
mapP<-mapCountryData(GDPmap, nameColumnToPlot="GDP", numCats=30,missingCountryCol = gray(0.8),
                     catMethod = c(0,2000,5000,10000,30000,100000), mapTitle = "",
                     colourPalette = c("pink","red","purple","blue","darkblue"),
                     borderCol = "black",oceanCol="azure2", xlim=c(-180,180),ylim=c(57,90),addLegend=FALSE)
do.call(addMapLegend,c(mapP, legendMar=4,legendLabels="all", legendWidth=0.5,legendShrink=0.8,legendIntervals="page"))
title( main = "Gross Domestic Product (USD)", line=-6, cex.main=1.5,font.main=2)

regfunF<-function(x){
  BCYF=lm(FYield~yearn,data=x)
  return(data.frame(SlopeF=BCYF$coefficients[2],pvF=summary(BCYF)$coefficients[2,4]))
}

X11(width=4.1,height=9)
par(mfrow=c(3,1),omi=c(0.3,0,0,0),mai=c(0,0,0,0))  
regBCF<-list()
SlopeF1<-list()
for (i in 1:3){
regBCF[[i]]<-DataBC1[[i]]
regBCF[[i]]$yearn<-regBCF[[i]]$year-1960
regBCF[[i]]<-ddply(regBCF[[i]], .(UN),regfunF)
regBCF[[i]]$SlopeF<-ifelse(regBCF[[i]]$pvF>0.05,0,regBCF[[i]]$SlopeF)
SlopeF1[[i]]<-joinCountryData2Map(regBCF[[i]],joinCode="UN", nameJoinColumn="UN")
mapF<-mapCountryData(SlopeF1[[i]], nameColumnToPlot="SlopeF", numCats=30,missingCountryCol = gray(0.8),
                     catMethod = c(-0.1,-0.05,-0.001,0.001,0.05,0.1,0.15,0.2), mapTitle = "",
                     colourPalette = c("red4","red", "white","lightblue", "steelblue1","blue", "darkblue"),
                     borderCol = "black",oceanCol="azure2", xlim=c(-180,180),ylim=c(57,90),addLegend=FALSE)
if(i==3){do.call(addMapLegend,c(mapF, legendMar=0,legendLabels="all", legendWidth=0.8,digits=2))}
title( main = paste0("FAO Yield Slope ",cropn[i]," (t/ha*yr)"), line=-6, cex.main=2,font.main=2)
}



#line to check the number of countries and the non-significant, negatives or positives for Slopep and Slopey
length(na.omit(slopemap[[1]]$Slopey))
length(na.omit(subset(slopemap[[1]],Slopey==0)))

#line to check the non-significant, positives and negative countries from 2010 top ten producers
Top10<-list()
nscount<-list()
Top1ns<-list()
for (i in 1:3){
Top10[[i]]<-subset(FAO[[i]],year==2010)
Top10[[i]]<-Top10[[i]][order(-Top10[[i]]$FProd),]
Top10[[i]]<-Top10[[i]][c(1:10),]
nscount[[i]]<-subset(slopemap[[i]],Slopey==0)
Top1ns[[i]]<-Top10[[i]][Top10[[i]]$UN %in% nscount[[i]]$UN,]
}

#line to check the areas of non-significant countries
Areans<-list()
Arean<-data.frame()
temp<-subset(FGlobal[-c(2,4,5,7,8,10)],year==2010)
for (i in 1:3){
  Areans[[i]]<-subset(FAO[[i]][FAO[[i]]$UN %in% nscount[[i]]$UN,],year==2010)
  Arean[i,1]<-sum(na.omit(Areans[[i]]$FArea))
  Arean[i,2]<-temp[1,i+1]
  Arean[i,3]<-100*Arean[i,1]/Arean[i,2]
}

#1:1 regression
  RegunoY<-list()
  RegunoP<-list()
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
#cex=ifelse(avgMbc$Prod<2000000,1,sqrt(avgMbc$Prod)/1000),

############

  Avera<-function(x){
  avLP<-mean(x$LProd);avLY<-mean(x$LYield);avFP<-mean(x$FProd);avFY<-mean(x$FYield)
  return(data.frame(avLP,avLY,avFP,avFY))
}
#change the name of the variable as needed in Average function
  DavgBC<-list()
  par(mfrow=c(3,2))
  for(i in 1:length(DataBC1)){
  DavgBC[[i]]<-ddply(DataBC1[[i]], .(UN),Avera)
  plot(DavgBC[[i]]$avLP,DavgBC[[i]]$avFP,xlab="", ylab="", main=paste0("Prod_",crop[i]),
   font=2, font.lab=2,cex.lab=2,cex.main=3,cex.axis=1.5,ylim=c(0,200000000))
  title( ylab="FAO_Prod", line=2.5, cex.lab=2,font=2, font.lab=2)
  abline(0,1,lwd=2)
  abline(RegunoP[[i]],col="red",lwd=2)
  plot(DavgBC[[i]]$avLY,DavgBC[[i]]$avFY,xlab="", ylab="", main=paste0("Yield_",crop[i]), 
     font=2, font.lab=2,cex.lab=2,cex.main=3,cex.axis=1.5,ylim=c(0,10),lwd=2,xlim=c(0,10))
  abline(0,1,lwd=2)
  abline(RegunoY[[i]],col="red",lwd=2)
  }
  
  