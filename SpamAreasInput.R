#CALLING THE AREA FILE FROM SPAM2000
area<-read.csv("Z:/general_inputs/SPAM_areas/spam_h.csv",h=TRUE)
area2<-area[c("x","y","whea_i","whea_r","maiz_i","maiz_r")]


#creating a new raster to extend the grid of the areas
library(raster)
grd<-raster(nrows = 2160, ncols = 4320,crs="+proj=longlat +datum=WGS84")
g_df<-as.data.frame(grd,row.names=NULL,xy=TRUE)
head(g_df)
g_df<-g_df[c(-3)]
area2$x<-round(area2$x,digits = 4)
area2$y<-round(area2$y,digits = 4)
g_df$x<-round(g_df$x,digits = 4)
g_df$y<-round(g_df$y,digits = 4)
a<-merge(g_df,area2,by=c("x","y"), all=TRUE)


#creating a rasterizing function
a_coords<-cbind(a$x, a$y)
rasterize<-function(area){
areas<-SpatialPointsDataFrame(coords=a_coords,data.frame(area),proj4string = CRS("+proj=longlat +datum=WGS84"))
areas2<-SpatialPixelsDataFrame(areas,tolerance=0.00120048,areas@data) 
areas3<-as(areas2,"SpatialGridDataFrame")
Rstr<-raster(areas3)
}

wheat_i<-rasterize(a$whea_i)
wheat_r<-rasterize(a$whea_r)
maiz_i<-rasterize(a$maiz_i)
maiz_r<-rasterize(a$maiz_r)

#Aggregation to 0.5*0.5 degrees
wheat_i0.5<-aggregate(wheat_i, fact=6, fun=sum)
wheat_r0.5<-aggregate(wheat_r, fact=6, fun=sum)
maiz_i0.5<-aggregate(maiz_i, fact=6, fun=sum)
maiz_r0.5<-aggregate(maiz_r, fact=6, fun=sum)


#CALLING THE AREA FILE FROM SPAM2005
areas1<-read.csv("Z:/general_inputs/SPAM_areas/spam2005harvestedA.csv", h=TRUE)
areas1_2<-areas1[c("x","y","whea_i","whea_r","maiz_i","maiz_r")]

#creating a new raster to extend the grid of the areas
areas1_2$x<-round(areas1_2$x,digits = 4)
areas1_2$y<-round(areas1_2$y,digits = 4)
a_2<-merge(g_df,areas1_2,by=c("x","y"),all=TRUE)


#creating a rasterizing function
a_2_coords<-cbind(a_2$x, a_2$y)
rasterize<-function(area){
  areas<-SpatialPointsDataFrame(coords=a_2_coords,data.frame(area),proj4string = CRS("+proj=longlat +datum=WGS84"))
  areas2<-SpatialPixelsDataFrame(areas,tolerance=0.0192077,areas@data) 
  areas3<-as(areas2,"SpatialGridDataFrame")
  Rstr<-raster(areas3)
}

wheat_i05<-rasterize(a_2$whea_i)
wheat_r05<-rasterize(a_2$whea_r)
maiz_i05<-rasterize(a_2$maiz_i)
maiz_r05<-rasterize(a_2$maiz_r)

#Aggregation to 0.5*0.5 degrees
wheat_i05ag<-aggregate(wheat_i05, fact=6, fun=sum)
wheat_r05ag<-aggregate(wheat_r05, fact=6, fun=sum)
maiz_i05ag<-aggregate(maiz_i05, fact=6, fun=sum)
maiz_r05ag<-aggregate(maiz_r05, fact=6, fun=sum)

#CALLING THE AREA FILE FROM SPAM2010
areas2<-read.csv("Z:/general_inputs/SPAM_areas/spam2010HA.csv",h=TRUE)
areas2_3<-areas2[c("x","y","whea_i","whea_r","maiz_i","maiz_r")]

#creating a new raster to extend the grid of the areas
areas2_3$x<-round(areas2_3$x,digits = 4)
areas2_3$y<-round(areas2_3$y,digits = 4)
a_3<-merge(g_df,areas2_3,by=c("x","y"),all=TRUE)

#creating a rasterizing function
a_3_coords<-cbind(a_3$x, a_3$y)
rasterize<-function(coords,area){
  areas<-SpatialPointsDataFrame(coords=coords,data.frame(area),proj4string = CRS("+proj=longlat +datum=WGS84"))
  areas2<-SpatialPixelsDataFrame(areas,tolerance=0.0192077,areas@data) 
  areas3<-as(areas2,"SpatialGridDataFrame")
  Rstr<-raster(areas3)
}

wheat_i10<-rasterize(a_3_coords,a_3$whea_i)
wheat_r10<-rasterize(a_3_coords,a_3$whea_r)
maiz_i10<-rasterize(a_3_coords,a_3$maiz_i)
maiz_r10<-rasterize(a_3_coords,a_3$maiz_r)


#Aggregation to 0.5*0.5 degrees
wheat_i10ag<-aggregate(wheat_i10, fact=6, fun=sum)
wheat_r10ag<-aggregate(wheat_r10, fact=6, fun=sum)
maiz_i10ag<-aggregate(maiz_i10, fact=6, fun=sum)
maiz_r10ag<-aggregate(maiz_r10, fact=6, fun=sum)