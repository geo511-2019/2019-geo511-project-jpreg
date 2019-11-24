library(maptools)
library(sf)
library(sp)
library(raster)
library(rgdal)
library(methods)
library(ggplot2)
library(rasterVis)
library(tidyverse)
library(gdistance)
library(rgeos)
library(dplyr)
library(data.table)
library(foreach)
library(knitr)
library(kableExtra)

#This is the road shapefile from the historic map
roads<-read_sf("_data/1877_roads_talbot.shp")%>%
  st_transform(4326)


#This is the houses shapefile from the historic map and Census Data
houses<-read_sf("_data/1877_houses_talbot.shp")%>%
  st_transform(4326)
#converting to spatial
houses_sp <- as_Spatial(houses)


#These are the elevation data read in, from which the values for water will be created
elev_data<-"C:/Users/prego/Documents/Documents/folders/code R/n38_w076_1arc_v2.tif"
imported_elev_data=raster(elev_data)

elev_data2<-"C:/Users/prego/Documents/Documents/folders/code R/n38_w077_1arc_v2.tif"
imported_elev_data2=raster(elev_data2)

#this is to merge the two elev rasters: merge()
elev<-merge(imported_elev_data, imported_elev_data2)


#cropping the elevation data to the extent of the roads file

elev_crop<-crop(elev, roads)

#function for values of roads over water: bridges
bridges <- function(x) ifelse (x==13 | x==6 | x==7, 1, x)

water=(elev_crop<0.001)*6

#plot the elev data and roads to combine them visually
plot(water)
plot(roads, pch=16, col="red", add=TRUE)
plot(houses, col="royal blue", add=TRUE)


#Turning the Roads and Houses Shapefiles into Raster Format
roads_raster <- rasterize(roads, water, field=1, background=4)

#Assigning different values to the land cover types
Land_Cover_Guide = c(
  'water (10)'=10,
  'roads (1)' =1,
  'land (4)' =4)

lcd=data.frame(
  ID=Land_Cover_Guide,
  landcover=names(Land_Cover_Guide),
  col=c("cadetblue", "peachpuff2", gray(.5)),
  stringsAsFactors = F)

talbot_r <- water+roads_raster
talbot_r2 <- calc(talbot_r, fun=bridges)

talbot_rf=as.factor(talbot_r2)
levels(talbot_rf)=left_join(levels(talbot_rf)[[1]], lcd)
talbot_rf


#plot of the raster
roads_water_plot<-gplot(talbot_rf, maxpixels=1e6) +
  geom_raster(mapping=aes(fill=as.factor(value))) +
  scale_fill_manual(values=levels(talbot_rf)[[1]]$col,
                    labels=levels(talbot_rf)[[1]]$landcover,
                    name="Landcover Type")+
  coord_cartesian() +
  xlab("longitude") +
  ylab("latitude")
roads_water_plot

#filtering houses to create White population
#table and SpatialPointsDataFrame: houses_w, houses_w_sp 
houses_w<-houses%>%
  filter(race == "White")

houses_w_sp<-as_Spatial(houses_w)

#filtering houses to create Non-White and White population
#table and SpatialPointsDataFrame: houses_nw, houses_nw_sp
houses_nw<-houses%>%
  filter(race == c("Black", "Mulatto"))

houses_nw_sp<-as_Spatial(houses_nw)

#travel cost function
trCost <- transition(1/talbot_rf, transitionFunction = mean, directions=8, symm=F)
trCost <- geoCorrection(trCost, type="c")

##calculating distance measures
#distance measures to all points
dist <- costDistance(trCost, houses_sp, houses_sp[265,])
dist_df <-data.frame(dist)

#distance measures to white population
dist_w <- costDistance(trCost, houses_w_sp, houses_sp[265,])
dist_w_df <- data.frame(dist_w)

summary(dist_w)
summary_dist_w<-knitr::kable(summary(dist_w))
bp_dist_w <- ggplot(data=dist_w_df, aes(y=dist_w)) +
  geom_boxplot()+
  coord_cartesian(ylim = c(500, 18000), xlim = c(-1, 1)) +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+ 
  ylab(label = "Cost Distance")+
  ggtitle("White Population")
bp_dist_w



#distance measures to non-white population
dist_nw <- costDistance(trCost, houses_nw_sp, houses_sp[265,])
dist_nw_df <-data.frame(dist_nw)

summary(dist_nw)
summary_dist_nw <- knitr::kable(summary(dist_nw))
bp_dist_nw <- ggplot(data=dist_nw_df, aes(y=dist_nw))+
  geom_boxplot()+
  coord_cartesian(ylim = c(500, 18000), xlim = c(-1, 1)) +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+ 
  ylab(label = "Cost Distance")+
  ggtitle("non-White Population")
bp_dist_nw

#distance measures to random sample of white population
dist_w_dt<- data.table(dist_w)
randw1<-dist_w_dt[sample(.N, 70)]

summary(randw1)
summary_dist_randw1 <- knitr::kable(summary(randnw1)) 
bp_rand_w <- ggplot(data=randw1, aes(y=V1))+
  geom_boxplot()+
  coord_cartesian(ylim = c(500, 18000), xlim = c(-1, 1)) +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) + 
  ylab(label = "Cost Distance")+
  ggtitle("Random Sample of White Population (n=70)")
bp_rand_w


#distance measures to random sample of non-White population
dist_nw_dt <- data.table(dist_nw)
randnw1<-dist_nw_dt[sample(.N, 7)]

summary(randnw1)
summary_dist_randnw1<- knitr::kable(summary(randnw1))
bp_rand_nw <- ggplot(data=randnw1, aes(y=V1))+
  geom_boxplot()+
  coord_cartesian(ylim = c(500, 18000), xlim = c(-1, 1)) +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  ylab(label = "Cost Distance")+
  ggtitle("Random Sample of non-White Population (n=7)")
bp_rand_nw



#combining all of the samples to display together
data=bind_rows(
  data.frame(cost_distance=randnw1$V1, race="random non-white"),
  data.frame(cost_distance=randw1$V1, race="random white"),
  data.frame(cost_distance=dist_nw_dt$V1, race="non-white"),
  data.frame(cost_distance=dist_w_dt$V1, race="white")) %>%
  mutate(race = factor(race, levels=c("white", "non-white", "random white", "random non-white")))

boxplots_data<-data %>%
  group_by(race)%>%
  summarize(min=min(cost_distance), Q1=quantile(cost_distance, .25), median=median(cost_distance), 
            mean=mean(cost_distance), Q3=quantile(cost_distance, .75),
            max=max(cost_distance))

data_boxplots <- ggplot(data=data, mapping = aes(y=cost_distance, x=race)) +
                          geom_boxplot() +
                          xlab(label="Race") +
                          ylab(label="Cost Distance")+
                          ggtitle("Travel Costs for the White and non-White Populations")


#foreach loop over houses_sp with gdistance::shortestPath
houses_paths <- foreach(i=1:264, .combine = 'stack') %do%{
  shortestPath(trCost, houses_sp[i,], houses_sp[265,])%>%
  raster()}

#summing together the paths
cost_sum=(calc(houses_paths, sum, na.rm=T))

raster_plot <- gplot(cost_sum, maxpixels=1e6) +
  geom_raster(mapping=aes(fill=value)) +
  coord_cartesian() +
  scale_fill_gradientn(colors=c(low='white', high='forestgreen')) +
  xlab("longitude") +
  ylab("latitude") +
  ggtitle("Most Travelled Roads in Talbot County to Easton c.1880")




?gplot
?scale_fill_gradientn
?geom_boxplot
?kable
?foreach
?shortestPath
?costDistance
?gdistance()
?transition
?igraph
