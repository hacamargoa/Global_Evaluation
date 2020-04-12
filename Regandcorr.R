source ("Z:/Scripts/Global_evaluation/Global_Evaluation/mergingFAO.R")
library(rworldmap)

GLOBAL<-GLOBAL[,c(1:7,11,10,13,12,9,8)]
GLOBAL$yearn<-GLOBAL$year-1960
GLOBAL1<-GLOBAL[,c(1:7)]
GLOBAL2<-GLOBAL[,c(1,8:13)]
colnames(GLOBAL2)<-names(GLOBAL1)
GLOBAL3<-rbind(GLOBAL1,GLOBAL2)

#Dummy FAO is 1 LPJ is 0
GLOBAL3$Dummy<-c(rep(1,50),rep(0,50))
GLOBAL3$yearn<-GLOBAL3$year-1960
GLOBAL3<-GLOBAL3[,c(1,8,9,2:7)]

#GLOBAL
#Global Trend yield (regression) FAO-LPJ is the interaction parameter
Lin_MY<-lm(GLOBAL3$M_yield~GLOBAL3$yearn*GLOBAL3$Dummy)
summary(Lin_MY)
Lin_wY<-lm(GLOBAL3$W_yield~(GLOBAL3$yearn*GLOBAL3$Dummy))
summary(Lin_wY)
Lin_mp<-lm(GLOBAL3$M_prod~(GLOBAL3$yearn*GLOBAL3$Dummy))
summary(Lin_wp)
Lin_wp<-lm(GLOBAL3$W_prod~(GLOBAL3$yearn*GLOBAL3$Dummy))
summary(Lin_wp)

#compiling the slope difference and p values data
GlobLinregs<-list(Lin_MY,Lin_wY,Lin_mp,Lin_wp)

slope<-function(x){
  Slope=(x$coefficients[4])
}
p.value<-function(x){
  pv=coef(summary(x))[4, "Pr(>|t|)"]
}

Gslodiff<-lapply(GlobLinregs,FUN = slope)
Gpvalue<-lapply(GlobLinregs,FUN = p.value)
var<-c("Yield_Maize","Yield_Wheat","Maize_Prod","Wheat_Prod")
Reg<-data.frame(var,Slopediff=unlist(Gslodiff),p.value=unlist(Gpvalue))

#Graph 1 (arreglar parametros abline llamando solo la nueva regresion)
par(mfrow=c(2,2))
lin_y_MGFAO=lm(GLOBAL$M_yield~GLOBAL$year)
plot(GLOBAL$yearn+1960,GLOBAL$M_yield, type="l",col="red", xlab="", ylab="",
     main="Maize", font=2, font.lab=2, ylim=c(1.0,5.0),cex.lab=2,cex.main=3,cex.axis=1.5,ylbias=0.5)
title( ylab="Yield (t/ha)", line=2.5, cex.lab=2,font=2, font.lab=2)
lines(GLOBAL$yearn+1960,GLOBAL$MaizGlobalYield,col="blue")
abline(Lin_MY$coefficients[1]+Lin_MY$coefficients[3]-(Lin_MY$coefficients[2]+Lin_MY$coefficients[4])*1960,Lin_MY$coefficients[2]+Lin_MY$coefficients[4],lwd=2)
abline(Lin_MY$coefficients[1]-Lin_MY$coefficients[2]*1960,Lin_MY$coefficients[2],lwd=2, col="cornsilk4")


plot(GLOBAL$yearn+1960,GLOBAL$W_yield, type="l",col="red", xlab="", ylab="", 
     main="Wheat", font=2, font.lab=2, ylim=c(1.0,5.0),cex.lab=2,cex.main=3,cex.axis=1.5)
lines(GLOBAL$yearn+1960,GLOBAL$WheatGlobalYield,col="blue")
abline(Lin_wY$coefficients[1]+Lin_wY$coefficients[3]-(Lin_wY$coefficients[2]+Lin_wY$coefficients[4])*1960,Lin_wY$coefficients[2]+Lin_wY$coefficients[4],lwd=2)
abline(Lin_wY$coefficients[1]-Lin_wY$coefficients[2]*1960,Lin_wY$coefficients[2],lwd=2, col="cornsilk4")

plot(GLOBAL$yearn+1960,GLOBAL$M_prod/1000000, type="l",col="red", xlab="Year", ylab="", 
     font=2, font.lab=2,ylim=c(200,800),cex.lab=2,cex.main=3,cex.axis=1.5)
title( ylab="Prod. (Mill. of Tons)", line=2.5, cex.lab=2,font=2, font.lab=2)
lines(GLOBAL$yearn+1960,GLOBAL$MaizGlobalProd/1000000,col="blue")
abline((Lin_mp$coefficients[1]+Lin_mp$coefficients[3])/1000000-1960*(Lin_mp$coefficients[2]+Lin_mp$coefficients[4])/1000000,(Lin_mp$coefficients[2]+Lin_mp$coefficients[4])/1000000,lwd=2)
abline((Lin_mp$coefficients[1]-1960*Lin_mp$coefficients[2])/1000000,Lin_mp$coefficients[2]/1000000,lwd=2, col="cornsilk4")


plot(GLOBAL$yearn+1960,GLOBAL$W_prod/1000000, type="l",col="red", xlab="Year", ylab="", 
     font=2, font.lab=2,ylim=c(200,800),cex.lab=2,cex.main=3,cex.axis=1.5)
lines(GLOBAL$yearn+1960,GLOBAL$WheatGlobalProd/1000000,col="blue")
abline((Lin_wp$coefficients[1]+Lin_wp$coefficients[3])/1000000-1960*(Lin_wp$coefficients[2]+Lin_wp$coefficients[4])/1000000,(Lin_wp$coefficients[2]+Lin_wp$coefficients[4])/1000000,lwd=2)
abline((Lin_wp$coefficients[1]-1960*Lin_wp$coefficients[2])/1000000,Lin_wp$coefficients[2]/1000000,lwd=2, col="cornsilk4")

#moving average detrend
require(forecast)

detrend<-function(x){
  dt<-ma(x,order=5,centre=TRUE)
  dt_res<- x-dt
  dt_res<-subset(dt_res,dt_res!="NA")
}

VarGlob<-list(GLOBAL$M_yield,GLOBAL$MaizGlobalYield,GLOBAL$W_yield,GLOBAL$WheatGlobalYield,
              GLOBAL$M_prod,GLOBAL$MaizGlobalProd,GLOBAL$W_prod,GLOBAL$WheatGlobalProd)

Detrended<-lapply(VarGlob, FUN=detrend)
class(Detrended)
names(Detrended)<-c("FAOMYield","LPJMYield","FAOWYield","LPJWYield","FAOMProd","LPJMProd","FAOWProd","LPJWProd")

#Plot 2
years<-c(1963:2008)
par(mfrow=c(2,2))
plot(years,Detrended$FAOMYield,type="l",xlab="", ylab="", main="Maize",
     font=2, font.lab=2,cex.lab=2,cex.main=3,cex.axis=1.5, ylim=c(-0.45,0.25))
title( ylab="Yield (t/ha)", line=2.5, cex.lab=2,font=2, font.lab=2)
lines(years,Detrended$LPJMYield,col="red")

plot(years,Detrended$FAOWYield,type="l",xlab="", ylab="", main="Wheat",
     font=2, font.lab=2,cex.lab=2,cex.main=3,cex.axis=1.5,ylim=c(-0.45,0.25))
lines(years,Detrended$LPJWYield,col="red")

plot(years,Detrended$FAOMProd/1000000,type="l",xlab="Year", ylab="",
     font=2, font.lab=2,cex.lab=2,cex.main=3,cex.axis=1.5)
title( ylab="Prod.(Mill.of Tons)", line=2.5, cex.lab=2,font=2, font.lab=2)
lines(years,Detrended$LPJMProd/1000000,col="red")

plot(years,Detrended$FAOWProd/1000000,type="l",xlab="Year", ylab="",
     font=2, font.lab=2,cex.lab=2,cex.main=3,cex.axis=1.5)
lines(years,Detrended$LPJWProd/1000000,col="red")

#Correlation for de-trended 
yield_M_cor<-cor.test(FAO_M_Glob_res,LPJ_M_Glob_res)
yield_W_cor<-cor.test(FAO_W_Glob_res,LPJ_W_Glob_res)
prod_M_cor<-cor.test(FAO_M_GlobP_res,LPJ_M_GlobP_res)
prod_W_cor<-cor.test(FAO_W_GlobP_res,LPJ_W_GlobP_res)

#BY_COUNTRY
#Correlation
require(plyr)
require(dplyr)
library(dplyr)
#LPJ data is in the form Crop_prod/Yield FAO is in the form C_Prod/Yield
summary(newbc1)
newbcc<-newbc1[newbc1$UN %in% names(which(table(newbc1$UN)>3)), ]
zeroes<-ddply(newbcc, .(UN),summarise,sum=sum(Maiz_Yield))
newbcc<-newbcc[newbcc$UN %in% zeroes[c(which(zeroes[,2]!=0)),1], ]
Yearsm<-newbcc[c(1,2)]


summary(newbcw1)
newbccw<-newbcw1[newbcw1$UN %in% names(which(table(newbcw1$UN)>3)), ]
zeroes<-ddply(newbccw, .(UN),summarise,sum=sum(Wheat_Yield))
newbccw<-newbccw[newbccw$UN %in% zeroes[c(which(zeroes[,2]!=0)),1], ]
Yearsw<-newbccw[c(1,2)]

#fixing some NA and the yearsm
newbccw$W_Yield<-ifelse(newbccw$W_Prod==0 & newbccw$W_Area==0,0,newbccw$W_Yield)
#Function tu remove the first two and last two years to match with m.a. data. 
cut<-function(x){
  cu<-x[c(-1,-2,-(length(x$year)),-(length(x$year)-1)),]
  return(data.frame(cu))
}
yearsm<-ddply(Yearsm, .(UN),cut)
yearsw<-ddply(Yearsw, .(UN),cut)
detrendbc<-function(x){
  dt<-ma(x$W_Yield,order=5,centre=TRUE)
  dt_res<- x$W_Yield-dt
  dt_res<-subset(dt_res,dt_res!="NA")
  return(data.frame(dt_res))
}
detrendbcLPJ<-ddply(newbcc, .(UN),detrendbc)#change the name of the variable in the detrendbc function to Maiz_Yield
detrendbcFAO<-ddply(newbcc, .(UN),detrendbc)#change the name of the variable in the detrendbc function to M_Yield
detrendbcWLPJ<-ddply(newbccw, .(UN),detrendbc)#change the name of the variable in the detrendbc function to Wheat_Yield
detrendbcWFAO<-ddply(newbccw, .(UN),detrendbc)#change the name of the variable in the detrendbc function to W_Yield

detrendbcM<-cbind(yearsm,LPJ=detrendbcLPJ[,2],FAO=detrendbcFAO[,2])
detrendbcM<-detrendbcM[detrendbcM$UN %in% names(which(table(detrendbcM$UN)>3)), ]
detrendbcW<-cbind(yearsw,LPJ=detrendbcWLPJ[,2],FAO=detrendbcWFAO[,2])
detrendbcW<-detrendbcW[detrendbcW$UN %in% names(which(table(detrendbcW$UN)>3)), ]

corrfun<-function(x){
  COR=cor.test(x$LPJ,x$FAO)
    return(data.frame(COR$estimate,COR$p.value))
}

corrbcM<-ddply(detrendbcM, .(UN),corrfun)
corrbcM1<-corrbcM
corrbcM1$COR.estimate<-ifelse(corrbcM1$COR.p.value>0.05,corrbcM1$COR.estimate==0,corrbcM1$COR.estimate)
corrmapM<-joinCountryData2Map(corrbcM1,joinCode="UN", nameJoinColumn="UN")
corrbcW<-ddply(detrendbcW, .(UN),corrfun)
corrbcW1<-corrbcW
corrbcW1$COR.estimate<-ifelse(corrbcW1$COR.p.value>0.05,corrbcW1$COR.estimate==0,corrbcW1$COR.estimate)
corrmapW<-joinCountryData2Map(corrbcW1,joinCode="UN", nameJoinColumn="UN")

par(mfrow=c(1,2))
mapCountryData(corrmapM, nameColumnToPlot="COR.estimate", numCats=30,missingCountryCol = gray(0.8),borderCol = "black", 
               catMethod = c(-1,-0.8,-0.6,-0.4,-0.2,-0.00001,0.00001,0.2,0.4,0.6,0.8,1),
               colourPalette = c("red4","red","pink4","pink","mistyrose3","white","lightblue3","steelblue1","steelblue3","blue","blue4"),
               oceanCol="aquamarine",mapRegion = "world", mapTitle = "")
title( main = "Maize", line=-6, cex.main=2,font.main=2)
mapCountryData(corrmapW, nameColumnToPlot="COR.estimate", numCats=30,missingCountryCol = gray(0.8),borderCol = "black",
               catMethod = c(-1,-0.8,-0.6,-0.4,-0.2,-0.00001,0.00001,0.2,0.4,0.6,0.8,1),
                colourPalette = c("red4","red","pink4","pink","mistyrose3","white","lightblue3","steelblue1","steelblue3","blue","blue4"),
               oceanCol="aquamarine",mapRegion = "world", mapTitle = "")
title( main = "Wheat", line=-6, cex.main=2,font.main=2)
?mapCountryData

#Regression
newbc1<-newbc1[c(1,2,7,9,8,6,4,5,3)]
newbc1$yearn<-newbc1$year-1960
Maizbc1<-newbc1[,c(1,10,3,4,5)]#FAO data
Maizbc2<-newbc1[,c(1,10,7,8,9)]#LPJ data
Maizbc1$Dummy<-1
Maizbc2$Dummy<-0
colnames(Maizbc2)<-names(Maizbc1)
Maizbc3<-rbind(Maizbc1,Maizbc2)

newbcw1<-newbcw1[c(1,2,7,9,8,6,4,5,3)]
newbcw1$yearn<-newbcw1$year-1960
Wheatbc1<-newbcw1[,c(2,10,3,4,5)]#FAO data
Wheatbc2<-newbcw1[,c(2,10,7,8,9)]#LPJ data
#Dummy FAO is 1 LPJ is 0
Wheatbc1$Dummy<-1
Wheatbc2$Dummy<-0
colnames(Wheatbc2)<-names(Wheatbc1)
Wheatbc3<-rbind(Wheatbc1,Wheatbc2)
regfun<-function(x){
  L_BCMY=lm(M_Yield~yearn*Dummy,data=x)
  return(data.frame(Slope=L_BCMY$coefficients[4],pvi=summary(L_BCMY)$coefficients[4, 4]))
}
table(Wheat)
Maizbc3<-subset(Maizbc3,UN!=208)

regparsM<-ddply(Maizbc3, .(UN),regfun)#change the name of the variable to M_Yield in regfun function
regparsM1<-regparsM
regparsM1$Slope<-ifelse(regparsM1$pvi>0.05,regparsM1$Slope==0,regparsM1$Slope)
regparsW<-ddply(Wheatbc3, .(UN),regfun)#change the name of the variable to W_Yield in regfun function
regparsW1<-regparsW
regparsW1$Slope<-ifelse(regparsW1$pvi>0.05,regparsW1$Slope==0,regparsW1$Slope)

summary(regparsM1)

slopemapM<-joinCountryData2Map(regparsM1,joinCode="UN", nameJoinColumn="UN")
slopemapW<-joinCountryData2Map(regparsW1,joinCode="UN", nameJoinColumn="UN")

par(mfrow=c(1,2))

mapCountryData(slopemapM, nameColumnToPlot="Slope", numCats=30,
               catMethod = c(-0.3,-0.2,-0.1,-0.00001,0.00001,0.1,0.2,0.3,0.4,0.5,0.6), mapTitle = "",missingCountryCol = gray(0.8),
               colourPalette = c("red4","red","pink1","white","lightblue","lightblue3","steelblue1","steelblue3","blue","blue4"),
               borderCol = "black",oceanCol="aquamarine")
title( main = "Maize", line=-6, cex.main=2,font.main=2)

mapCountryData(slopemapW, nameColumnToPlot="Slope", numCats=30,missingCountryCol = gray(0.8), 
               catMethod = c(-0.3,-0.2,-0.1,-0.00001,0.00001,0.1,0.2,0.3,0.4,0.5,0.6), mapTitle = "",
               colourPalette = c("red4","red","pink1","white","lightblue","lightblue3","steelblue1","steelblue3","blue","blue4"),
               borderCol = "black",oceanCol="aquamarine")
title( main = "Wheat", line=-6, cex.main=2,font.main=2)

#1:1 regression
summary(newbc1)
maiz1_1line<-lm(newbc1$M_Yield~newbc1$Maiz_Yield+0)
wheat1_1line<-lm(newbcw1$W_Yield~newbcw1$Wheat_Yield+0)
summary(maiz1_1line)
summary(wheat1_1line)
Average<-function(x){
  avg<-mean(x$Maiz_Yield)
  return(data.frame(avg))
}
#change the name of the variable as needed in Average function
avbcFAOMY<-ddply(newbc1, .(UN),Average)
avbcLPJMY<-ddply(newbc1, .(UN),Average)
avbcFAOWY<-ddply(newbcw1, .(UN),Average)
avbcLPJWY<-ddply(newbcw1, .(UN),Average)
avbcFAOMP<-ddply(newbc1, .(UN),Average)
avbcFAOWP<-ddply(newbcw1, .(UN),Average)
avgMbc<-cbind(avbcFAOMY,LPJY=avbcLPJMY[,2],Prod=avbcFAOMP[,2])
avgWbc<-cbind(avbcFAOWY,LPJY=avbcLPJWY[,2],Prod=avbcFAOWP[,2])
maizavg1_1l<-lm(avgMbc$avg~avgMbc$LPJY+0)
wheatavg1_1l<-lm(avgWbc$avg~avgWbc$LPJY+0)
summary(maizavg1_1l)
summary(wheatavg1_1l)
par(mfrow=c(2,2))
plot(newbc1$Maiz_Yield,newbc1$M_Yield,xlab="", ylab="", main="Maize",
     font=2, font.lab=2,cex.lab=2,cex.main=3,cex.axis=1.5,ylim=c(0,15))
title( ylab="FAO Yield", line=2.5, cex.lab=2,font=2, font.lab=2)
abline(0,1,lwd=2)
abline(maiz1_1line,col="red",lwd=2)

plot(avgMbc$LPJY,avgMbc$avg,xlab="", ylab="", main="Maize", cex=ifelse(avgMbc$Prod<2000000,1,sqrt(avgMbc$Prod)/1000),
     font=2, font.lab=2,cex.lab=2,cex.main=3,cex.axis=1.5,ylim=c(0,10),lwd=2,xlim=c(0,10))
abline(0,1,lwd=2)
abline(maizavg1_1l,col="red",lwd=2)

plot(newbcw1$Wheat_Yield,newbcw1$W_Yield,xlab="LPJ Yield", ylab="", main="Wheat",
     font=2, font.lab=2,cex.lab=2,cex.main=3,cex.axis=1.5,xlim=c(0,10))
title( ylab="FAO Yield", line=2.5, cex.lab=2,font=2, font.lab=2)
abline(0,1,lwd=2)
abline(wheat1_1line,col="red",lwd=2)

plot(avgWbc$LPJY,avgWbc$avg,xlab="LPJ Yield", ylab="", main="Wheat",cex=ifelse(avgMbc$Prod<2000000,1,sqrt(avgMbc$Prod)/1000),
     font=2, font.lab=2,cex.lab=2,cex.main=3,cex.axis=1.5,ylim=c(0,10),lwd=2,xlim=c(0,10))
abline(0,1,lwd=2)
abline(wheatavg1_1l,col="red",lwd=2)
