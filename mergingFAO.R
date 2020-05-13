#source ("C:/Users/Hector/Documents/Pughtam-cropozone/Global_Evaluation/AreaMasking.R")
#BY COUNTRY COMPARISON

#copy table of maize from FAOSTAT_Bycountry.xls in Global_evaluation inputs
DataBC<-list()
AvgBC<-list()
for (i in 1:length(aggdf)){
  DataBC[[i]]<-merge(aggdf[[i]],FAO[[i]][-2],by=c("UN","year"))
  AvgBC[[i]]<-aggregate(DataBC[[i]],by=list(DataBC[[i]]$UN),FUN=mean)
  write.csv(DataBC2[[i]],paste0('C:/Users/Hector/Documents/Pughtam-cropozone/Global_evaluation_outputs/Fao&LPJ',crop[[i]],'.csv'), row.names = FALSE)
}
plot(DataBC[[3]]$LYield,DataBC[[3]]$FYield)
plot(DataBC[[3]]$LProd,DataBC[[3]]$FProd)
#plot(DataBC[[1]]$LArea,DataBC[[1]]$FArea)

plot(AvgBC[[3]]$LYield,AvgBC[[3]]$FYield)
plot(AvgBC[[3]]$LProd,AvgBC[[3]]$FProd)
#plot(AvgBC[[1]]$LArea,AvgBC[[1]]$FArea)

#GLOBAL COMPARISON
#copy table of wheat from FAO+globaltotal.xls in Firstpaper folder
FGlobal<- read.csv("C:/Users/Hector/Documents/Pughtam-cropozone/Global_evaluation_inputs/FAO/FAOGl.csv", h=T)
FGlobal<-subset(FGlobal,year<2011)
LGlobal<-do.call("cbind",Global)[c(-5,-9,-13,-14,-15,-16)]
names(LGlobal)<-c("year","LW_Prod","LW_Area","LW_Yield","LM_Prod","LM_Area","LM_Yield","LR_Prod","LR_Area","LR_Yield")
GLOBAL<-merge(FGlobal,LGlobal, all=TRUE)
na<-c("Prod_","Area_","Yield_")

#1:1 plots
for(j in c(2,5,8)){
  png(paste0("C:/Users/Hector/Documents/Pughtam-cropozone/Global_evaluation_outputs/",crop[(1+j)/3],"Global.png"))
  par(mfrow=c(2,2))
  v<-c(j,j+2)
  for(i in v){
  plot(GLOBAL[,9+i],GLOBAL[,i],ylab="FAO", xlab="LPJ", main=paste0(na[i-j+1],crop[(1+j)/3]))
  abline(0,1)
  plot(GLOBAL$year,GLOBAL[,9+i],xlab="Year", ylab="Ton", main=paste0(na[i-j+1],crop[(1+j)/3]), type="l",col="red")
  lines(GLOBAL$year,GLOBAL[,i],col="black")
  }
  dev.off()
  }
