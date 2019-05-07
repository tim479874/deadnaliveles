#########################
##### load packages #####
#########################

library(ggplot2)
library(ggspatial)
library(gtable)
library(mgcv)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(foreach)
library(sp)
library(rgdal)
library(rgeos)
library(dplyr)
library(tidyverse)

### set working directory
setwd("/ma_elephant/R/illustrations")

########### load segments of GEC ###########
segments_GEE<-rgdal::readOGR(dsn= "/ma_elephant",layer= "segments_GEE")


########### load acrifa shapefile ###########
africa <- ne_countries(scale = "medium", continent="africa",returnclass = "sf")

######### get africa centroids of countries ###########

africa_points<- st_centroid(africa)
africa_points <- cbind(africa, st_coordinates(st_centroid(africa$geometry)))
countries
#### extract those countries which are in the GEC survey
gec_points<-africa_points[ which ( africa_points$sov_a3 == "AGO" |
                                     africa_points$sov_a3 == "BWA" |
                                     africa_points$sov_a3 == "COD" |
                                     africa_points$sov_a3 == "ETH" |
                                     africa_points$sov_a3 == "KEN" |
                                     africa_points$sov_a3 == "TCD" |
                                     africa_points$sov_a3 == "BFA" |
                                     africa_points$sov_a3 == "NER" |
                                     africa_points$sov_a3 == "BEN" |
                                   africa_points$sov_a3 == "ZWE" ),]


#### transform GEC segments to sf #####
segments_GEE<-st_as_sf(segments_GEE)



############ convert dataframe to spatialpointsdataframe

pointsGEC1<-sp::merge(segments_GEE,data, by="ID", all.x=FALSE)

pointsGEC<-SpatialPointsDataFrame(pointsGEC, data=pointsGEC@data)

pointsGEC_sf<-st_as_sf(pointsGEC)

data[,"x"]<-sp::coordinates(pointsGEC)[,1]
data[,"y"]<-sp::coordinates(pointsGEC)[,2]



sites<-unique(pointsGEC$Site)

sites_hulls<-list()

for ( i in 1:length(unique(data$Site))){
  site_subset<-data[data$Site==sites[i],]
  ch <- chull(site_subset$x,site_subset$y)
  coords <- site_subset[c(ch, ch[1]),]
  sites_hulls[[i]] <- coords
}


hullAGO_Lui<-sites_hulls[[1]]
hullBWA_NOR<-sites_hulls[[2]]
hullCOD_GAR<-sites_hulls[[3]] 
hullCOD_VIR  <-sites_hulls[[4]]
hullETH_BAB <-sites_hulls[[5]]
hullETH_OMO <-sites_hulls[[6]]
hullKEN_LAI <-sites_hulls[[7]]
hullKEN_LAM  <-sites_hulls[[8]]
hullKEN_TSV <-sites_hulls[[9]]
hullTCD_CHA <-sites_hulls[[10]]
hullTCD_ZAK <-sites_hulls[[11]]
hullXWA_TBC <-sites_hulls[[12]]
hullZWE_MAT<-sites_hulls[[13]]
hullZWE_SEB <-sites_hulls[[14]]
hullZWE_SELV<-sites_hulls[[15]]
hullZWE_ZV  <-sites_hulls[[16]]


#### call plot of study area

studyarea<-  ggplot(data=africa)+
  xlab("Longitude")+
  ylab("Latitude")+
  geom_sf(colour="grey60",fill="grey80") +
  
  geom_polygon(data = hullAGO_Lui,  aes(x,y),fill="dodgerblue3",color="black")+
  geom_polygon(data = hullBWA_NOR,  aes(x,y),fill="yellow",color="black")+
  geom_polygon(data = hullCOD_GAR,  aes(x,y),fill="green",color="black")+
  geom_polygon(data = hullCOD_VIR,  aes(x,y),fill="dodgerblue3",color="black")+
  geom_polygon(data = hullETH_BAB,  aes(x,y),fill="dodgerblue3",color="black")+
  geom_polygon(data = hullETH_OMO,  aes(x,y),fill="dodgerblue3",color="black")+
  geom_polygon(data = hullKEN_LAI,  aes(x,y),fill="green",color="black")+
  geom_polygon(data = hullKEN_LAM,  aes(x,y),fill="dodgerblue3",color="black")+
  geom_polygon(data = hullKEN_TSV,  aes(x,y),fill="green",color="black")+
  geom_polygon(data = hullTCD_CHA,  aes(x,y),fill="dodgerblue3",color="black")+
  geom_polygon(data = hullTCD_ZAK,  aes(x,y),fill="green",color="black")+
  geom_polygon(data = hullXWA_TBC,  aes(x,y),fill="green",color="black")+
  geom_polygon(data = hullZWE_MAT,  aes(x,y),fill="dodgerblue3",color="black")+
  geom_polygon(data = hullZWE_SEB,  aes(x,y),fill="dodgerblue3",color="black")+
  geom_polygon(data = hullZWE_SELV,  aes(x,y),fill="dodgerblue3",color="black")+
  geom_polygon(data = hullZWE_ZV,  aes(x,y),fill="dodgerblue3",color="black")+
  
          geom_text(data= gec_points,aes(x=X+0.5, y=Y, label=name_sort),
          size=3.5 , color = "black", fontface = "bold", check_overlap = FALSE)+
  
          annotation_scale(location = "bl", width_hint = 0.25) +
           annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(0.25, "in"), pad_y = unit(0.25, "in"),
                         style = north_arrow_fancy_orienteering) +
           coord_sf(xlim = c(1, 50), ylim = c(15, -22)) +
  
theme(panel.background = element_rect(fill="white"),axis.text=element_text(size=15),
      axis.title=element_text(size=15),
      axis.text.x = element_text(colour = "black"),
      axis.text.y = element_text(colour = "black"))
  
 
### save plot
ggsave("studyarea.pdf", device = "pdf", dpi=400)
