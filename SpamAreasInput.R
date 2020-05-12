#creating a new raster to extend the grid of the areas
library(raster)
grd<-raster(nrows = 2160, ncols = 4320,crs="+proj=longlat +datum=WGS84")
g_df<-as.data.frame(grd,row.names=NULL,xy=TRUE)
head(g_df)
g_df<-g_df[c(-3)]
g_df$x<-round(g_df$x,digits = 4)
g_df$y<-round(g_df$y,digits = 4)
namel<-c("whea_i","whea_r","maiz_i","maiz_r","rice_i","rice_r","soyb_i","soyb_r")

#CALLING THE AREA FILE FROM SPAM2000, 2005 & 2010
a2000<-read.csv("C:/Users/Hector/Documents/Pughtam-cropozone/Global_evaluation_inputs/SPAM_areas/spam_h.csv",h=TRUE)
a2005<-read.csv("C:/Users/Hector/Documents/Pughtam-cropozone/Global_evaluation_inputs/SPAM_areas/spam2005harvestedA.csv", h=TRUE)
a2010<-read.csv("C:/Users/Hector/Documents/Pughtam-cropozone/Global_evaluation_inputs/SPAM_areas/spam2010HA.csv",h=TRUE)
area<-list(a2000,a2005,a2010)
SPAM<-list()
for(i in 1:length(area)){
SPAM[[i]]<-area[[i]][c("x","y",namel)]
SPAM[[i]]$x<-round(SPAM[[i]]$x,digits = 4)
SPAM[[i]]$y<-round(SPAM[[i]]$y,digits = 4)
SPAM[[i]]<-merge(g_df,SPAM[[i]],by=c("x","y"), all=TRUE)
}

test<-area[[1]]
test<-test[c(1,2,3)]
test$x<-round(test$x,digits = 2)
test$y<-round(test$y,digits = 2)
head(test)
test<-merge(g_df,test,by=c("x","y"), all=TRUE)
coordinates(test)<- ~ x+y
test<-SpatialPixelsDataFrame(test,tolerance=0.125,test@data)
testr<-raster(test)
xmin(testr)=-180;xmax(testr)=180;ymin(testr)=-90;ymax(testr)=90
testrag<-aggregate(testr, fact=6, FUN=sum, expand=TRUE)
testrag<-testrag(res=0.5,xmin=-180,xmax=180,ymin=-90,ymax=90)

Areas[[3]][[1]]
names(SPAM)<-c("Y2000","Y2005","Y2010")
Rast<-function (df,var){
  area_<-df[c("x","y",var)]
  coordinates(area_)<- ~ x+y
  area_<-SpatialPixelsDataFrame(area_,tolerance=0.00120048,area_@data)
  area_<-raster(area_)
  xmin(area_)=-180;xmax(area_)=180;ymin(area_)=-90;ymax(area_)=90
  area_<-aggregate(area_, fact=6, fun=sum)
}

for(i in 1:length(namel)){
Areas[[i]]<-lapply(SPAM,Rast,var=namel[i])
}
names(Areas)<-namel
#this script was tested to get the raster in the right order with te below code
# testA<-SPAM$Y2005[c("x","y","whea_i")]
# coordinates(testA)<- ~ x+y
# testA<-SpatialPixelsDataFrame(testA,tolerance=0.00120048,testA@data)
# testA<-raster(testA)
# testA<-aggregate(testA, fact=6, fun=sum)
# Areas[[1]][[2]]-testA

