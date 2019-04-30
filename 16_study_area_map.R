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


#### call plot of study area

studyarea<- ggplot(data=africa)+
  xlab("Longitude")+
  ylab("Latitude")+
  geom_sf(colour="grey30",fill="antiquewhite") +
  geom_sf(data=gec, color="red", fill="blue")+
  
  geom_text(data= gec_points,aes(x=X, y=Y, label=sovereignt),
            size=2.75 , color = "black", fontface = "bold", check_overlap = FALSE)+

annotation_scale(location = "bl", width_hint = 0.25) +
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(0.25, "in"), pad_y = unit(0.25, "in"),
                         style = north_arrow_fancy_orienteering) +
coord_sf(xlim = c(1, 50), ylim = c(15, -22)) 


studyarea



ggsave("studyarea.pdf", device = "pdf", dpi=300)
