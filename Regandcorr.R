#source ("Z:/Scripts/Global_evaluation/Global_Evaluation/mergingFAO.R")
library(rworldmap)
library(forecast)
library(plyr)
library(dplyr)

GLOBAL1<-GLOBAL[,c(-3,-6,-9,-12,-15,-18)]
GLOBAL1$yearn<-GLOBAL1$year-1960
GLOBALa<-GLOBAL1[,c(2:7,14)]
GLOBALb<-GLOBAL1[,c(8:13,14)]
colnames(GLOBALa)<-c("W_Prod","W_Yield","M_Prod","M_Yield","R_Prod","R_Yield","yearn");colnames(GLOBALb)<-names(GLOBALa)
GLOBAL2<-rbind(GLOBALa,GLOBALb)
#Dummy FAO is 1 LPJ is 0
GLOBAL2$Dummy<-c(rep(1,50),rep(0,50))

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

#Graph 1 (arreglar parametros abline llamando solo la nueva regresion)
par(mfrow=c(3,2))
for(i in 1:3){
plot(GLOBAL$year,GLOBAL[,i*3+1], type="l",col="red", xlab="", ylab="",
     main=crop[i], font=2, font.lab=2, ylim=c(1.0,6.0),cex.lab=2,cex.main=3,cex.axis=1.5,ylbias=0.5)
title( ylab="Yield (t/ha)", line=2.5, cex.lab=2,font=2, font.lab=2)
lines(GLOBAL$year,GLOBAL[,i*3+10],col="blue")
abline(Lin_yield[[i]]$coefficients[1]+Lin_yield[[i]]$coefficients[3]-(Lin_yield[[i]]$coefficients[2]+Lin_yield[[i]]$coefficients[4])*1960,Lin_yield[[i]]$coefficients[2]+Lin_yield[[i]]$coefficients[4],lwd=2)
abline(Lin_yield[[i]]$coefficients[1]-Lin_yield[[i]]$coefficients[2]*1960,Lin_yield[[i]]$coefficients[2],lwd=2, col="cornsilk4")

plot(GLOBAL$year,GLOBAL[,i*3-1]/1000000, type="l",col="red", xlab="", ylab="",
     main=crop[i], font=2, font.lab=2, ylim=c(100,1000),cex.lab=2,cex.main=3,cex.axis=1.5,ylbias=0.5)
title( ylab="Prod. (Mill. of Tons)", line=2.5, cex.lab=2,font=2, font.lab=2)
lines(GLOBAL$year,GLOBAL[,i*3+8]/1000000,col="blue")
abline((Lin_prod[[i]]$coefficients[1]+Lin_prod[[i]]$coefficients[3])/1000000-1960*(Lin_prod[[i]]$coefficients[2]+Lin_prod[[i]]$coefficients[4])/1000000,(Lin_prod[[i]]$coefficients[2]+Lin_prod[[i]]$coefficients[4])/1000000,lwd=2)
abline((Lin_prod[[i]]$coefficients[1]-1960*Lin_prod[[i]]$coefficients[2])/1000000,Lin_prod[[i]]$coefficients[2]/1000000,lwd=2, col="cornsilk4")
}
#moving average detrend
detrend<-function(x){
  dt<-ma(x,order=5,centre=TRUE)
  dt_res<- x-dt
  dt_res<-subset(dt_res,!is.na(dt_res))
}
DetreG<-list()
for(i in 1:12){
DetreG[[i]]<-detrend(GLOBAL1[[i+1]])
}
DetreG<-do.call("cbind",DetreG)
colnames(DetreG)<-paste0(names(GLOBAL1[c(-1,-14)]),"_ma")
Corr_Glob<-list()
for(i in 1:6){
Corr_Glob[[i]]<-cor.test(DetreG[,i],DetreG[,i+6])
}
names(Corr_Glob)<-paste0(names(GLOBAL1[c(2:7)]),"_corr")

#Plot 2
years<-c(1963:2008)
par(mfrow=c(3,2))
for(i in 1:3){
plot(years,DetreG[,i*2],type="l",xlab="", ylab="", main=crop[i],col="red",
     font=2, font.lab=2,cex.lab=2,cex.main=3,cex.axis=1.5, ylim=c(-0.45,0.25))
title( ylab="Yield (t/ha)", line=2.5, cex.lab=2,font=2, font.lab=2)
lines(years,DetreG[,i*2+6],col="blue")

plot(years,DetreG[,i*2-1]/1000000,type="l",xlab="Year", ylab="",main=crop[i],col="red",
     font=2, font.lab=2,cex.lab=2,cex.main=3,cex.axis=1.5)
title( ylab="Prod.(Mill.of Tons)", line=2.5, cex.lab=2,font=2, font.lab=2)
lines(years,DetreG[,i*2+5]/1000000,col="blue")
}

#BY_COUNTRY
#Correlation
#Function tu remove the first two and last two years by country to match with m.a. data. 
cut<-function(x){
  cu<-x[c(-1,-2,-(length(x$year)),-(length(x$year)-1)),]
  return(data.frame(cu))
}
DataBC1<-list()
years_ma<-list()
for (i in 1:length(DataBC)){
  DataBC1[[i]]<-na.omit(DataBC1[[i]])
  DataBC1[[i]]<-DataBC[[i]][DataBC[[i]]$UN %in% names(which(table(DataBC[[i]]$UN)>7)), ]
  zeroes<-ddply(DataBC1[[i]], .(UN),summarise,sum=sum(LYield))
  DataBC1[[i]]<-DataBC1[[i]][DataBC1[[i]]$UN %in% zeroes[c(which(zeroes[,2]!=0)),1], ]
  DataBC1[[i]]<-DataBC1[[i]][c(-5,-7)]
  years_ma[[i]]<-DataBC1[[i]][c(1,2)]
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
dtbc[[i]]<-ddply(DataBC1[[j]], .(UN),detrendbc)
}
dtcropbc[[j]]<-cbind(year=years_ma[[j]][,2],do.call("cbind",dtbc)[c(-3,-5,-7)])
colnames(dtcropbc[[j]])<-colnames(DataBC1[[j]][c(2,1,4:7)])
}
names(dtcropbc)<-crop

corrfun<-function(x){
  CORp=cor.test(x$LProd,x$FProd)
  CORy=cor.test(x$LYield,x$FYield)
    return(data.frame(CORp$estimate,CORp$p.value,CORy$estimate,CORy$p.value))
}
corrbc<-list()
corrbc1<-list()
corrmap<-list()
par(mfrow=c(3,2))
for(i in 1:3){
corrbc[[i]]<-ddply(dtcropbc[[i]], .(UN),corrfun)
corrbc1[[i]]<-corrbc[[i]]
corrbc1[[i]]$CORp.estimate<-ifelse(corrbc1[[i]]$CORp.p.value>0.05,corrbc1[[i]]$CORp.estimate==0,corrbc1[[i]]$CORp.estimate)
corrbc1[[i]]$CORy.estimate<-ifelse(corrbc1[[i]]$CORy.p.value>0.05,corrbc1[[i]]$CORy.estimate==0,corrbc1[[i]]$CORy.estimate)
corrmap[[i]]<-joinCountryData2Map(corrbc1[[i]],joinCode="UN", nameJoinColumn="UN")
mapCountryData(corrmap[[i]], nameColumnToPlot="CORp.estimate", numCats=30,missingCountryCol = gray(0.8),borderCol = "black", 
               catMethod = c(-1,-0.8,-0.6,-0.4,-0.2,-0.00001,0.00001,0.2,0.4,0.6,0.8,1),
               colourPalette = c("red4","red","pink4","pink","mistyrose3","white","lightblue3","steelblue1","steelblue3","blue","blue4"),
               oceanCol="aquamarine",mapRegion = "world", mapTitle = "")
title( main =paste0("Prod_corr_",crop[i]), line=-6, cex.main=2,font.main=2)
mapCountryData(corrmap[[i]], nameColumnToPlot="CORy.estimate",, numCats=30,missingCountryCol = gray(0.8),borderCol = "black",
               catMethod = c(-1,-0.8,-0.6,-0.4,-0.2,-0.00001,0.00001,0.2,0.4,0.6,0.8,1),
                colourPalette = c("red4","red","pink4","pink","mistyrose3","white","lightblue3","steelblue1","steelblue3","blue","blue4"),
               oceanCol="aquamarine",mapRegion = "world", mapTitle = "")
title( main = paste0("Yield_corr_",crop[i]), line=-6, cex.main=2,font.main=2)
}

#Regression
regfun<-function(x){
  BCP=lm(LProd~yearn*Dummy,data=x)
  BCY=lm(LYield~yearn*Dummy,data=x)
  return(data.frame(Slopep=BCP$coefficients[4],pvp=summary(BCP)$coefficients[4, 4],
                    Slopey=BCY$coefficients[4],pvy=summary(BCY)$coefficients[4, 4]))
}
DummyBC<-list()
regBC<-list()
regBC1<-list()
slopemap<-list()
par(mfrow=c(3,2))
for(i in 1:3){
  DummyBC[[i]]<-DataBC1[[i]]
  tem1<-DummyBC[[i]][c(1,2,6,7)]
  tem1$Dummy<-1
  tem2<-DummyBC[[i]][c(1,2,4,5)]
  tem2$Dummy<-0
  colnames(tem1)<-colnames(tem2)
  DummyBC[[i]]<-rbind(tem1,tem2)
  DummyBC[[i]]$yearn<-DummyBC[[i]]$year-1960
  regBC[[i]]<-ddply(DummyBC[[i]], .(UN),regfun)
  regBC1[[i]]<-regBC[[i]]
  regBC1[[i]]$Slopep<-ifelse(regBC1[[i]]$pvp>0.05,regBC1[[i]]$Slopep==0,regBC1[[i]]$Slopep)
  regBC1[[i]]$Slopey<-ifelse(regBC1[[i]]$pvy>0.05,regBC1[[i]]$Slopey==0,regBC1[[i]]$Slopey)
  slopemap[[i]]<-joinCountryData2Map(regBC1[[i]],joinCode="UN", nameJoinColumn="UN")
  mapCountryData(slopemap[[i]], nameColumnToPlot="Slopep", numCats=30,missingCountryCol = gray(0.8),
               catMethod = c(-0.3,-0.2,-0.1,-0.00001,0.00001,0.1,0.2,0.3,0.4,0.5,0.6), mapTitle = "",
               colourPalette = c("red4","red","pink1","Green","lightblue","lightblue3","steelblue1","steelblue3","blue","blue4"),
               borderCol = "black",oceanCol="aquamarine")
  title( main =paste0("Prod_slodiff_",crop[i]), line=-6, cex.main=2,font.main=2)
  mapCountryData(slopemap[[i]], nameColumnToPlot="Slopey", numCats=30,missingCountryCol = gray(0.8),
               catMethod = c(-0.3,-0.2,-0.1,-0.00001,0.00001,0.1,0.2,0.3,0.4,0.5,0.6), mapTitle = "",
               colourPalette = c("red4","red","pink1","white","lightblue","lightblue3","steelblue1","steelblue3","blue","blue4"),
               borderCol = "black",oceanCol="aquamarine")
  title( main = paste0("Yield_slodiff_",crop[i]), line=-6, cex.main=2,font.main=2)
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
  