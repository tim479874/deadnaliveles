library(sf)

setwd("~/ma_elephant/")

### Distance to raods ----
roaddist <- st_read(dsn="~/ma_elephant/gis/distance_to_roads",layer="distance")

DR<-roaddist[,c(1,13)]

st_geometry(DR)<-NULL

colnames(DR)<-c("ID","DR")


write.csv(DR, "~/ma_elephant/R/predictors/DR.csv",row.names=FALSE)
write.csv(DR, "~/ma_elephant/R/predictors_car/DR.csv",row.names=FALSE)

#### distance to water waterbodies (lakes, ponds) and river----

waterdist <- st_read(dsn="~/ma_elephant/gis/dist_water",layer="waterdist")

DW<-waterdist[,c(1,41)]

st_geometry(DW)<-NULL

colnames(DW)<-c("ID","DW")

write.csv(DW, "~/ma_elephant/R/predictors/DW.csv",row.names=FALSE)
write.csv(DW, "~/ma_elephant/R/predictors_car/DW.csv",row.names=FALSE)

#### distance to citys ----

citydist <- st_read(dsn="~/ma_elephant/gis/towns",layer="city_dist")

DC<-citydist[,c(1,122)]

st_geometry(DC)<-NULL

colnames(DC)<-c("ID","DC")

write.csv(DC, "~/ma_elephant/R/predictors/DC.csv",row.names=FALSE)
write.csv(DC, "~/ma_elephant/R/predictors_car/DC.csv",row.names=FALSE)

#### type of protected area ----

#read multipolygons from the gec segments and the protected areas
pa<-sf::st_read(dsn="/ma_elephant/gis/gec_pa/", layer="pa_africa")
segments_GEC<-sf::st_read(dsn="/ma_elephant/", layer="segments_GEE")

#intersect the multipolygon layers to see which gec segments is inside a protected area
overlap_poly<-sf:::st_intersection(pa,segments_GEC)
#reduce to only the ids of the polygons within the protected areas
poly_id<-overlap_poly[30]
#add the kategorial predictor protected area types ("pa")
poly_id[,"pa_type"]<-overlap_poly[10]
#reduce from sf to data.frame
dat_id<-as.data.frame(poly_id)
#remove geometry
dat_id<-dat_id[,-2]

#repace duplicates with mode

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

pred_pa<- aggregate(as.character(dat_id[,2]), by=list(dat_id$ID),FUN=Mode)

colnames(pred_pa)<-c("ID","pa_type")


#make empty data.frame

mode_pa<-data.frame(matrix(ncol = 2, nrow=21990))
colnames(mode_pa)<-c("ID","pa_type")



#merge the pa with the original segments
PA1<-merge(as.data.frame(segments_GEC, col.names=NULL),pred_pa, by="ID", all.x=TRUE)

#reduce to data.frame without geometry and time
pred_pa<-PA1[,c(-2,-3)]

#substitue NAs with None

levels<-levels(pred_pa$pa_type)
levels[length(levels) + 1] <- "None"

pred_pa$pa_type <- factor(pred_pa$pa_type, levels = levels)
pred_pa$pa_type[is.na(pred_pa$pa_type)]<-"None"


write.csv(pred_pa, "~/ma_elephant/R/predictors/PA.csv")
write.csv(pred_pa, "~/ma_elephant/R/predictors_car/PA.csv")