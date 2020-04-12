source ("Z:/Scripts/Global_evaluation/Global_Evaluation/AreaMasking.R")
#BY COUNTRY COMPARISON
by_country<-subset(By_country,year>1960)
summary(by_country)

#Maiz comparison
lpjmaiz<-by_country[c(-4,-6)]
LPJmaiz<-lpjmaiz[-2]
LPJmaiz$Maiz_Yield<-ifelse(LPJmaiz$Maize_area==0,0,LPJmaiz$Maize_prod/LPJmaiz$Maize_area)
LPJmaiz<-subset(LPJmaiz,Maize_area != "NA")
summary(LPJmaiz)
table(LPJmaiz$UN,LPJmaiz$year)

#copy table of maize from FAOSTAT_Bycountry.xls in Firstpaper folder
FaoMaiz <- read.table(file = "clipboard", 
                      sep = "\t", header=TRUE)

summary(FaoMaiz)
dim(FaoMaiz)
newbc<-merge(LPJmaiz,FaoMaiz,by=c("UN","year"),all=TRUE)
newbc1<-subset(newbc,Maize_prod!="NA" & M_Area != "NA")
newbc1$Country<-ifelse(newbc1$UN==418,"Lao",ifelse(newbc1$UN==384,"Ivory Coast",ifelse(newbc1$UN==408,"North Korea",as.character(newbc1$Country))))
summary(newbc1)
dim(newbc1)
write.csv(newbc1,'Z:/Other simulations/First paper/Fao&LPJbc.csv', row.names = TRUE)
plot(newbc1$Maiz_Yield,newbc1$M_Yield,ylim=c(0,15))
plot(newbc1$Maize_prod,newbc1$M_Prod,xlim=c(0,300000000))
plot(newbc1$Maize_area,newbc1$M_Area)

#Wheat comparison
lpjwheat<-by_country[c(-5,-7)]
LPJwheat<-lpjwheat[-2]
LPJwheat$Wheat_Yield<-ifelse(LPJwheat$Wheat_area==0,0,LPJwheat$Wheat_prod/LPJwheat$Wheat_area)
LPJwheat<-subset(LPJwheat,Wheat_area != "NA")
summary(LPJwheat)
dim(LPJwheat)
head(lpjwheat)
table(lpjwheat$UN,lpjwheat$year)

#copy table of wheat from FAOSTAT_Bycountry.xls in Firstpaper folder
FaoWheat <- read.table(file = "clipboard", 
                      sep = "\t", header=TRUE)

summary(FaoWheat)
dim(FaoWheat)
newbcw<-merge(LPJwheat,FaoWheat,all=TRUE)
newbcw1<-subset(newbcw,Wheat_prod!="NA" & W_Area != "NA")
newbcw1$Country<-ifelse(newbcw1$UN==418,"Lao",ifelse(newbcw1$UN==384,"Ivory Coast",ifelse(newbcw1$UN==408,"North Korea",as.character(newbcw1$Country))))
library(stringr)
levels(newbcw1$Country)[str_detect(newbcw1$Country,"Democratic Peoples Republic of Korea")==TRUE]<-"Korea"
summary(newbcw1)
dim(newbcw1)
write.csv(newbcw1,'Z:/Other simulations/First paper/Fao&LPJbcw.csv', row.names = TRUE)
plot(newbcw1$Wheat_Yield,newbcw1$W_Yield,ylim=c(0,10))
plot(newbcw1$Wheat_prod,newbcw1$W_Prod,xlim=c(0,120000000))
plot(newbcw1$Wheat_area,newbcw1$W_Area)

#averaging all the countries
avbyc_w<-aggregate(newbcw1,by=UN,FUN=mean)
?aggregate
summary(avbyc_w)
plot(avbyc_w$Wheat_Yield,avbyc_w$W_Yield)
plot(avbyc_w$Wheat_prod,avbyc_w$W_Prod)
plot(avbyc_w$Wheat_area,avbyc_w$W_Area)

avbyc_m<-aggregate(.~newbc1$UN,newbc1, FUN=mean)
summary(avbyc_m)
plot(avbyc_m$Maiz_Yield,avbyc_m$M_Yield,size=avbyc_m$M_Prod)
plot(avbyc_m$Maize_prod,avbyc_m$M_Prod)
plot(avbyc_m$Maize_area,avbyc_m$M_Area)

#GLOBAL COMPARISON
#copy table of wheat from FAO+globaltotal.xls in Firstpaper folder
FaoGlobal<- read.table(file = "clipboard", 
                       sep = "\t", header=TRUE)
summary(FaoGlobal)
summary(Global)
colnames(Global)[1]<-"year"
Global1<-subset(Global,year>1960)
Global1$WheatGlobalYield<-Global1$WheatGlobalProd/Global1$WheatGlobalArea
Global1$MaizGlobalYield<-Global1$MaizGlobalProd/Global1$MaizGlobalArea
GLOBAL<-merge(FaoGlobal,Global1, all=TRUE)
summary(GLOBAL)
#1:1 plots
plot(GLOBAL$WheatGlobalYield,GLOBAL$W_yield,xlim=c(0.7,3))
plot(GLOBAL$WheatGlobalProd,GLOBAL$W_prod,xlim=c(150000000,700000000),ylim=c(150000000,700000000))
plot(GLOBAL$WheatGlobalArea,GLOBAL$W_area)
plot(GLOBAL$MaizGlobalYield,GLOBAL$M_yield)
plot(GLOBAL$MaizGlobalProd,GLOBAL$M_prod)
plot(GLOBAL$MaizGlobalArea,GLOBAL$M_area)

#Wheat plots
plot(GLOBAL$year,GLOBAL$W_prod, type="l",col="red")
lines(GLOBAL$year,GLOBAL$WheatGlobalProd, col="black")
plot(GLOBAL$year,GLOBAL$W_area, type="l",col="red")
lines(GLOBAL$year,GLOBAL$WheatGlobalArea, col="black")
plot(GLOBAL$year,GLOBAL$W_yield, type="l",col="red")
lines(GLOBAL$year,GLOBAL$WheatGlobalYield,col="black")
#Maize plots
plot(GLOBAL$year,GLOBAL$M_prod, type="l",col="red")
lines(GLOBAL$year,GLOBAL$MaizGlobalProd, col="black")
plot(GLOBAL$year,GLOBAL$M_area, type="l",col="red")
lines(GLOBAL$year,GLOBAL$MaizGlobalArea, col="black")


