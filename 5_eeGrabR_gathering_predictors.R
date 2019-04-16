### set everything up ----

library(earthEngineGrabR)
library(rgdal)
library(geojsonio)
library(reticulate)
library(sp)
library(lubridate)

#set working directory
setwd("~/ma_elephant/R/predictors")

#ubuntu has trouble handling http2 - set http to 0
#Error in curl::curl_fetch_memory(url, handle = handle) : 
  #Error in the HTTP2 framing layer

httr::set_config(httr::config(http_version = 0))

#gathering predictors

segments_GEE<-readOGR(dsn= "~/Documents/Master/ma_elephant",layer= "segments_GEE")

#check for first and last detection
min(as.character((as.Date(as.POSIXct((segments_GEE$time*1e+9), tz = "UTC", origin = "1970-01-01")))))
#first one [1] "2014-02-26"
max(as.character((as.Date(as.POSIXct((segments_GEE$time*1e+9), tz = "UTC", origin = "1970-01-01")))))
#last one [1] "2015-11-12"

# predictors from 2014-01-01 till 2015-12-31

#gathering predictors

### Elevation  0 NAs----
#calculate the mean elevation for each segment at native 30m resolution

elev <- ee_grab(data = ee_data_image(datasetID = "USGS/SRTMGL1_003", 
                                     spatialReducer = "mean", 
                                     resolution = NULL, 
                                     bandSelection = "elevation"
),
targetArea = "~/ma_elephant/segments_GEE.shp")

elev<-elev[,c(1,2)]
st_geometry(elev)<-NULL
colnames(elev)<-c("ID","EL")


#check for NAs
anyNA(elev$EL)



write.csv(elev, "~/ma_elephant/R/predictors/EL.csv")
write.csv(elev, "~/ma_elephant/R/predictors_car/EL.csv")

rm(elev)


### NDVI vegetation index 0 NAs----

##NDVI 2014 #scale=250

ndvi14 <- ee_grab(data = ee_data_collection(datasetID = "VITO/PROBAV/C1/S1_TOC_333M",
                                            spatialReducer = "mean",
                                            temporalReducer = "mean", 
                                            timeStart = "2014-01-01",
                                            timeEnd = "2014-12-31", 
                                            resolution = NULL,
                                            bandSelection = "NDVI"
),
targetArea = "~/ma_elephant/segments_GEE.shp")



st_geometry(ndvi14)<-NULL
colnames(ndvi14)<-c("ID","NDVI","time")

anyNA(ndvi14$NDVI)
sum(is.na(ndvi14$NDVI))
which(is.na(ndvi14$NDVI))


##NVDI 2015


ndvi15 <- ee_grab(data = ee_data_collection(datasetID = "VITO/PROBAV/C1/S1_TOC_333M",
                                          spatialReducer = "mean",
                                          temporalReducer = "mean", 
                                          timeStart = "2015-01-01",
                                          timeEnd = "2015-12-31", 
                                          resolution = NULL,
                                          bandSelection = "NDVI"
),
targetArea = "~/ma_elephant/segments_GEE.shp")



st_geometry(ndvi15)<-NULL
colnames(ndvi15)<-c("ID","NDVI","time")

anyNA(ndvi15)
sum(is.na(ndvi15$NDVI))
which(is.na(ndvi15$NDVI))

#merge ndvi to the year it occured

NDVI<-data.frame(matrix(ncol = 3, nrow=48254))
colnames(NDVI)<-c("ID","NDVI","time")

NDVI[,c(1,3)]<-ndvi14[,c(1,3)]


NDVI$NDVI[as.numeric(format((as.Date(as.POSIXct((NDVI$time*1e+9), tz = "UTC", origin = "1970-01-01"))),'%Y')) == 2014] <- ndvi14$NDVI[as.numeric(format((as.Date(as.POSIXct((NDVI$time*1e+9), tz = "UTC", origin = "1970-01-01"))),'%Y')) == 2014]

NDVI$NDVI[as.numeric(format((as.Date(as.POSIXct((NDVI$time*1e+9), tz = "UTC", origin = "1970-01-01"))),'%Y')) == 2015] <- ndvi15$NDVI[as.numeric(format((as.Date(as.POSIXct((NDVI$time*1e+9), tz = "UTC", origin = "1970-01-01"))),'%Y')) == 2015]

summary(NDVI)

write.csv(NDVI, "~/ma_elephant/R/predictors/NDVI.csv")
write.csv(NDVI, "~/ma_elephant/R/predictors_car/NDVI.csv")


### TPA total annual precipitation 0 Nas----
##precipitation 2014


precip14 <- ee_grab(data = ee_data_collection(datasetID = "UCSB-CHG/CHIRPS/DAILY",
                                            spatialReducer = "mean",
                                            temporalReducer = "sum", 
                                            timeStart = "2014-01-01",
                                            timeEnd = "2014-12-31", 
                                            resolution = 300,
                                            bandSelection = "precipitation"
),
targetArea = "~/ma_elephant/segments_GEE.shp")

st_geometry(precip14)<-NULL


anyNA(precip14$precipitation_s.mean_t.sum_2014.01.01_to_2014.12.31)
sum(is.na(precip14$precipitation_s.mean_t.sum_2014.01.01_to_2014.12.31))
which(is.na(precip14$precipitation_s.mean_t.sum_2014.01.01_to_2014.12.31))



#filling NAs


precip14_2 <- ee_grab(data = ee_data_collection(datasetID = "TRMM/3B43V7",
                                              spatialReducer = "mean",
                                              temporalReducer = "mean", 
                                              timeStart = "2014-01-01",
                                              timeEnd = "2014-12-31", 
                                              resolution = 300,
                                              bandSelection = "precipitation"
),
targetArea = "~/ma_elephant/segments_GEE.shp")

st_geometry(precip14_2)<-NULL


anyNA(precip14_2$precipitation_s.mean_t.sum_2014.01.01_to_2014.12.31)
sum(is.na(precip14_2$precipitation_s.mean_t.sum_2014.01.01_to_2014.12.31))
which(is.na(precip14_2$precipitation_s.mean_t.sum_2014.01.01_to_2014.12.31))

precip14_2$precipitation_s.mean_t.mean_2014.01.01_to_2014.12.31<-precip14_2$precipitation_s.mean_t.mean_2014.01.01_to_2014.12.31*8760

summary(precip14_2)
summary(precip14)



precip14$precipitation_s.mean_t.sum_2014.01.01_to_2014.12.31[is.na(precip14$precipitation_s.mean_t.sum_2014.01.01_to_2014.12.31)]<-precip14_2$precipitation_s.mean_t.mean_2014.01.01_to_2014.12.31[is.na(precip14$precipitation_s.mean_t.sum_2014.01.01_to_2014.12.31)]



##precipitation 2015


precip15 <- ee_grab(data = ee_data_collection(datasetID = "UCSB-CHG/CHIRPS/DAILY",
                                         spatialReducer = "mean",
                                         temporalReducer = "sum", 
                                         timeStart = "2015-01-01",
                                         timeEnd = "2015-12-31", 
                                         resolution = 300,
                                         bandSelection = "precipitation"
),
targetArea = "~/ma_elephant/segments_GEE.shp")

st_geometry(precip15)<-NULL

anyNA(precip15$precipitation_s.mean_t.sum_2015.01.01_to_2015.12.31)
sum(is.na(precip15$precipitation_s.mean_t.sum_2015.01.01_to_2015.12.31))
which(is.na(precip15$precipitation_s.mean_t.sum_2015.01.01_to_2015.12.31))

#filling NAs


precip15_2 <- ee_grab(data = ee_data_collection(datasetID = "TRMM/3B43V7",
                                                spatialReducer = "mean",
                                                temporalReducer = "mean", 
                                                timeStart = "2015-01-01",
                                                timeEnd = "2015-12-31", 
                                                resolution = 300,
                                                bandSelection = "precipitation"
),
targetArea = "~/ma_elephant/segments_GEE.shp")

st_geometry(precip15_2)<-NULL


precip15_2$precipitation_s.mean_t.mean_2015.01.01_to_2015.12.31<-precip15_2$precipitation_s.mean_t.mean_2015.01.01_to_2015.12.31*8760


anyNA(precip15_2$precipitation_s.mean_t.mean_2015.01.01_to_2015.12.31)

precip15$precipitation_s.mean_t.sum_2015.01.01_to_2015.12.31[is.na(precip15$precipitation_s.mean_t.sum_2015.01.01_to_2015.12.31)]<-precip15_2$precipitation_s.mean_t.mean_2015.01.01_to_2015.12.31[is.na(precip15$precipitation_s.mean_t.sum_2015.01.01_to_2015.12.31)]

anyNA(precip15)


#merge precipitation to the year it occured

TPA<-precip14

colnames(TPA)<-c("ID","TPA","time")


TPA$TPA[as.numeric(format((as.Date(as.POSIXct((TPA$time*1e+9), tz = "UTC", origin = "1970-01-01"))),'%Y')) == 2014] <- precip14$precipitation_s.mean_t.sum_2014.01.01_to_2014.12.31[as.numeric(format((as.Date(as.POSIXct((TPA$time*1e+9), tz = "UTC", origin = "1970-01-01"))),'%Y')) == 2014]

TPA$TPA[as.numeric(format((as.Date(as.POSIXct((TPA$time*1e+9), tz = "UTC", origin = "1970-01-01"))),'%Y')) == 2015] <- precip15$precipitation_s.mean_t.sum_2015.01.01_to_2015.12.31[as.numeric(format((as.Date(as.POSIXct((TPA$time*1e+9), tz = "UTC", origin = "1970-01-01"))),'%Y')) == 2015]

summary(TPA)


write.csv(TPA, "~/ma_elephant/R/predictors/TPA.csv")
write.csv(TPA, "~/ma_elephant/R/predictors_car/TPA.csv")


###  Surface Air Temperature 0 Nas----


temp14 <- ee_grab(data = ee_data_collection(datasetID = "NCEP_RE/surface_temp",
                                          spatialReducer = "mean",
                                          temporalReducer = "mean", 
                                          timeStart = "2014-01-01",
                                          timeEnd = "2014-12-31", 
                                          resolution = 300,
                                          bandSelection = "air"
),
targetArea = "~/ma_elephant/segments_GEE.shp")

st_geometry(temp14)<-NULL

anyNA(temp14$air_s.mean_t.mean_2014.01.01_to_2014.12.31)
sum(is.na(temp14$air_s.mean_t.mean_2014.01.01_to_2014.12.31))
which(is.na(temp14$air_s.mean_t.mean_2014.01.01_to_2014.12.31))


temp15 <- ee_grab(data = ee_data_collection(datasetID = "NCEP_RE/surface_temp",
                                          spatialReducer = "mean",
                                          temporalReducer = "mean", 
                                          timeStart = "2015-01-01",
                                          timeEnd = "2015-12-31", 
                                          resolution = 300,
                                          bandSelection = "air"
),
targetArea = "~/ma_elephant/segments_GEE.shp")

st_geometry(temp15)<-NULL

anyNA(temp15$air_s.mean_t.mean_2015.01.01_to_2015.12.31)
sum(is.na(temp15$air_s.mean_t.mean_2015.01.01_to_2015.12.31))
which(is.na(temp15$air_s.mean_t.mean_2015.01.01_to_2015.12.31))


# merge observation to the year it occured

###################################################
######### NOTE ####################################
### naming a variable T should be avoided if ######
### you want to use T as shortcut for TRUE   ######
###################################################

T<-data.frame(matrix(ncol = 3, nrow=48254))

T[,c(1,3)]<-temp14[,c(1,3)]

colnames(T)<-c("ID","T","time")

ifelse(as.numeric(format((as.Date(as.POSIXct((segments_GEE$time*1e+9), tz = "UTC", origin = "1970-01-01"))),'%Y')) == 2014 ,T[,2]<-temp14[,2],T[,2]<-temp15[,2])



T$T[as.numeric(format((as.Date(as.POSIXct((T$time*1e+9), tz = "UTC", origin = "1970-01-01"))),'%Y')) == 2014] <- temp14$air_s.mean_t.mean_2014.01.01_to_2014.12.31[as.numeric(format((as.Date(as.POSIXct((T$time*1e+9), tz = "UTC", origin = "1970-01-01"))),'%Y')) == 2014]

T$T[as.numeric(format((as.Date(as.POSIXct((T$time*1e+9), tz = "UTC", origin = "1970-01-01"))),'%Y')) == 2015] <- temp15$air_s.mean_t.mean_2015.01.01_to_2015.12.31[as.numeric(format((as.Date(as.POSIXct((T$time*1e+9), tz = "UTC", origin = "1970-01-01"))),'%Y')) == 2015]



write.csv(T, "~/ma_elephant/R/predictors/T.csv",row.names=FALSE)
write.csv(T, "~/ma_elephant/R/predictors_car/T.csv",row.names=FALSE)


### Tree cover 0 NAs----


TC1 <- ee_grab(data = ee_data_collection(datasetID ="MODIS/051/MOD44B",
                                           spatialReducer = "mean",
                                           temporalReducer = "mean", 
                                           timeStart = "2014-01-01",
                                           timeEnd = "2015-12-31", 
                                           resolution = 300,
                                           bandSelection = "Percent_Tree_Cover"
),
targetArea = "~/ma_elephant/segments_GEE.shp")

st_geometry(TC1)<-NULL

anyNA(TC1$Percent_Tree_Cover_s.mean_t.mean_2014.01.01_to_2015.12.31)
sum(is.na(TC1$Percent_Tree_Cover_s.mean_t.mean_2014.01.01_to_2015.12.31))
which(is.na(TC1$Percent_Tree_Cover_s.mean_t.mean_2014.01.01_to_2015.12.31))


#2000 - 2010
TC2 <- ee_grab(data = ee_data_collection(datasetID ="GLCF/GLS_TCC",
                                          spatialReducer = "mean",
                                          temporalReducer = "mean", 
                                          timeStart = "2000-01-01",
                                          timeEnd = "2010-12-31", 
                                          resolution = 300,
                                          bandSelection = "tree_canopy_cover"
),
targetArea = "~/ma_elephant/segments_GEE.shp")


st_geometry(TC2)<-NULL

anyNA(TC2$tree_canopy_cover_s.mean_t.mean_2000.01.01_to_2010.12.31)
sum(is.na(TC2$tree_canopy_cover_s.mean_t.mean_2000.01.01_to_2010.12.31))
which(is.na(TC2$tree_canopy_cover_s.mean_t.mean_2000.01.01_to_2010.12.31))



TC2000 <- ee_grab(data = ee_data_image(datasetID = "UMD/hansen/global_forest_change_2017_v1_5", 
                                     spatialReducer = "mean", 
                                     resolution = 300, 
                                     bandSelection = "treecover2000"
),
targetArea = "~/ma_elephant/segments_GEE.shp")

st_geometry(TC2000)<-NULL

anyNA(TC2000$treecover2000_s.mean)


#fill NAs in MODIS with data from GLCF and hansen forest change

TC<-data.frame(matrix(ncol = 3, nrow=48254))

TC[,c(1,3)]<-TC1[,c(1,3)]


ifelse(is.na(TC1[,2]),TC[,2]<-TC2[,3],TC[,2]<-TC1[,2])

TC$X2[is.na(TC$X2)]<-TC2000[,3]

anyNA(TC$X2)

colnames(TC)<-c("ID","TC","time")


write.csv(TC, "~/ma_elephant/R/predictors/TC.csv")



### Herbaceous Vegetation ----

VC14 <- ee_grab(data = ee_data_collection(datasetID ="MODIS/051/MOD44B",
                                          spatialReducer = "mean",
                                          temporalReducer = "mean", 
                                          timeStart = "2014-01-01",
                                          timeEnd = "2014-12-31", 
                                          resolution = 300,
                                          bandSelection = "Percent_NonTree_Vegetation"
),
targetArea = "~/ma_elephant/segments_GEE.shp")

st_geometry(VC14)<-NULL

VC15 <- ee_grab(data = ee_data_collection(datasetID ="MODIS/051/MOD44B",
                                          spatialReducer = "mean",
                                          temporalReducer = "mean", 
                                          timeStart = "2015-01-01",
                                          timeEnd = "2015-12-31", 
                                          resolution = 300,
                                          bandSelection = "Percent_NonTree_Vegetation"
),
targetArea = "~/ma_elephant/segments_GEE.shp")

st_geometry(VC15)<-NULL

VC<-data.frame(matrix(ncol = 3, nrow=48254))

VC[,c(1,3)]<-VC14[,c(1,3)]


colnames(VC)<-c("ID","VC","time")

VC$VC[as.numeric(format((as.Date(as.POSIXct((VC$time*1e+9), tz = "UTC", origin = "1970-01-01"))),'%Y')) == 2014] <- VC14$Percent_NonTree_Vegetation_s.mean_t.mean_2014.01.01_to_2014.12.31[as.numeric(format((as.Date(as.POSIXct((VC$time*1e+9), tz = "UTC", origin = "1970-01-01"))),'%Y')) == 2014]

VC$VC[as.numeric(format((as.Date(as.POSIXct((VC$time*1e+9), tz = "UTC", origin = "1970-01-01"))),'%Y')) == 2015] <- VC15$Percent_NonTree_Vegetation_s.mean_t.mean_2015.01.01_to_2015.12.31[as.numeric(format((as.Date(as.POSIXct((VC$time*1e+9), tz = "UTC", origin = "1970-01-01"))),'%Y')) == 2015]




write.csv(VC, "~/ma_elephant/R/predictors/VC.csv")
write.csv(VC, "~/ma_elephant/R/predictors_car/VC.csv")

### Bare soil ----


BS14 <- ee_grab(data = ee_data_collection(datasetID ="MODIS/051/MOD44B",
                                          spatialReducer = "mean",
                                          temporalReducer = "mean", 
                                          timeStart = "2014-01-01",
                                          timeEnd = "2014-12-31", 
                                          resolution = 300,
                                          bandSelection = "Percent_NonVegetated"
),
targetArea = "~/ma_elephant/segments_GEE.shp")

st_geometry(BS14)<-NULL

BS15 <- ee_grab(data = ee_data_collection(datasetID ="MODIS/051/MOD44B",
                                          spatialReducer = "mean",
                                          temporalReducer = "mean", 
                                          timeStart = "2015-01-01",
                                          timeEnd = "2015-12-31", 
                                          resolution = 300,
                                          bandSelection = "Percent_NonVegetated"
),
targetArea = "~/ma_elephant/segments_GEE.shp")

st_geometry(BS15)<-NULL

BS<-data.frame(matrix(ncol = 3, nrow=48254))

BS[,c(1,3)]<-VC14[,c(1,3)]


colnames(BS)<-c("ID","BS","time")

BS$BS[as.numeric(format((as.Date(as.POSIXct((BS$time*1e+9), tz = "UTC", origin = "1970-01-01"))),'%Y')) == 2014] <- BS14$Percent_NonVegetated_s.mean_t.mean_2014.01.01_to_2014.12.31[as.numeric(format((as.Date(as.POSIXct((BS$time*1e+9), tz = "UTC", origin = "1970-01-01"))),'%Y')) == 2014]

BS$BS[as.numeric(format((as.Date(as.POSIXct((BS$time*1e+9), tz = "UTC", origin = "1970-01-01"))),'%Y')) == 2015] <- BS15$Percent_NonVegetated_s.mean_t.mean_2015.01.01_to_2015.12.31[as.numeric(format((as.Date(as.POSIXct((BS$time*1e+9), tz = "UTC", origin = "1970-01-01"))),'%Y')) == 2015]




write.csv(BS, "~/ma_elephant/R/predictors/BS.csv")
write.csv(BS, "~/ma_elephant/R/predictors_car/BS.csv")

### Landcover 0 NAs----

landcov14 <- ee_grab(data = ee_data_collection(datasetID ="MODIS/006/MCD12Q1",
                                            spatialReducer = "mode",
                                            temporalReducer = "mode", 
                                            timeStart = "2014-01-01",
                                            timeEnd = "2014-12-31", 
                                            resolution = 500,
                                            bandSelection = "LC_Type1"
),
targetArea = "~/ma_elephant/segments_GEE.shp")

st_geometry(landcov14)<-NULL

landcov15 <- ee_grab(data = ee_data_collection(datasetID ="MODIS/006/MCD12Q1",
                                               spatialReducer = "mode",
                                               temporalReducer = "mode", 
                                               timeStart = "2015-01-01",
                                               timeEnd = "2015-12-31", 
                                               resolution = 500,
                                               bandSelection = "LC_Type1"
),
targetArea = "~/ma_elephant/segments_GEE.shp")

st_geometry(landcov15)<-NULL

LC<-data.frame(matrix(ncol = 3, nrow=48254))

LC[,c(1,3)]<-landcov14[,c(1,3)]


colnames(LC)<-c("ID","LC","time")

LC$LC[as.numeric(format((as.Date(as.POSIXct((LC$time*1e+9), tz = "UTC", origin = "1970-01-01"))),'%Y')) == 2014] <- landcov14$LC_Type1_s.mode_t.mode_2014.01.01_to_2014.12.31[as.numeric(format((as.Date(as.POSIXct((LC$time*1e+9), tz = "UTC", origin = "1970-01-01"))),'%Y')) == 2014]

LC$LC[as.numeric(format((as.Date(as.POSIXct((LC$time*1e+9), tz = "UTC", origin = "1970-01-01"))),'%Y')) == 2015] <- landcov15$LC_Type1_s.mode_t.mode_2015.01.01_to_2015.12.31[as.numeric(format((as.Date(as.POSIXct((LC$time*1e+9), tz = "UTC", origin = "1970-01-01"))),'%Y')) == 2015]




write.csv(LC, "~/ma_elephant/R/predictors/LC.csv")
write.csv(LC, "~/ma_elephant/R/predictors_car/LC.csv")

### Population density 1x1km 0 NAs----

popdens <- ee_grab(data = ee_data_collection(datasetID ="CIESIN/GPWv4/population-density",
                                             spatialReducer = "mean",
                                             temporalReducer = "mean", 
                                             timeStart = "2015-01-01",
                                             timeEnd = "2015-12-31", 
                                             resolution = 1000,
                                             bandSelection = "population-density"
),
targetArea = "~/ma_elephant/segments_GEE.shp")

st_geometry(popdens)<-NULL

anyNA(popdens$population.density_s.mean_t.mean_2014.01.01_to_2015.12.31)
sum(is.na(popdens$population.density_s.mean_t.mean_2014.01.01_to_2015.12.31))
which(is.na(popdens$population.density_s.mean_t.mean_2014.01.01_to_2015.12.31))


popc <- ee_grab(data = ee_data_collection(datasetID ="JRC/GHSL/P2016/POP_GPW_GLOBE_V1",
                                          spatialReducer = "mean",
                                          temporalReducer = "mean", 
                                          timeStart = "2015-01-01",
                                          timeEnd = "2015-12-31", 
                                          resolution = 1000,
                                          bandSelection = "population_count"
),
targetArea = "~/ma_elephant/segments_GEE.shp")

anyNA(popc$population_count_s.mean_t.mean_2015.01.01_to_2015.12.31)
summary(popc)

st_geometry(popc)<-NULL


#fill NAs

PD<-data.frame(matrix(ncol = 3, nrow=48254))

PD[,c(1:3)]<-popdens[,c(1:3)]

PD$X2[is.na(PD$X2)]<-popc[,2]

colnames(PD)<-c("ID","PD","time")

summary(PD)



write.csv(PD, "~/ma_elephant/R/predictors/PD.csv")
write.csv(PD, "~/ma_elephant/R/predictors_car/PD.csv")


### Seasonality 4 months precipitation before obs----

start_day<-sort(unique(as.Date(as.POSIXct((segments_GEE$time*1e+9), tz = "UTC", origin = "1970-01-01")))-120)
end_day<-sort(unique(as.Date(as.POSIXct((segments_GEE$time*1e+9), tz = "UTC", origin = "1970-01-01"))))


#to get the structure of the dataframe get the first month and add this to a new dataframe for the combined observations

i=1

x<-ee_grab(data = ee_data_collection(datasetID = "UCSB-CHG/CHIRPS/DAILY",
                                     spatialReducer = "mean",
                                     temporalReducer = "sum", 
                                     timeStart = as.character(start_day[i]),
                                     timeEnd = as.character(end_day[i]), 
                                     resolution = 1000,
                                     bandSelection = "precipitation"
),
targetArea = "~/ma_elephant/segments_GEE.shp")


st_geometry(x)<-NULL


TP4M<-x[NA,]

#loop through all month for temporal observations

for (i in 106:length(start_day)){
  
  
  x<-ee_grab(data = ee_data_collection(datasetID = "UCSB-CHG/CHIRPS/DAILY",
                                       spatialReducer = "mean",
                                       temporalReducer = "sum", 
                                       timeStart = as.character(start_day[i]),
                                       timeEnd = as.character(end_day[i]), 
                                       resolution = 1000,
                                       bandSelection = "precipitation"
  ),
  targetArea = "~/ma_elephant/segments_GEE.shp")
  
  
  st_geometry(x)<-NULL
  
  x2<-ee_grab(data = ee_data_collection(datasetID = "UCSB-CHG/CHIRPS/PENTAD",
                                        spatialReducer = "mean",
                                        temporalReducer = "mean", 
                                        timeStart = as.character(start_day[i]),
                                        timeEnd = as.character(end_day[i]), 
                                        resolution = 1000,
                                        bandSelection = "precipitation"
  ),
  targetArea = "~/ma_elephant/segments_GEE.shp")
  
  st_geometry(x2)<-NULL
  
  x2[,2]<-x2[,2]*24
  
  x[,2][is.na(x[,2])]<-x2[,2][is.na(x[,2])]
  
  
  
  
  TP4M[as.Date(as.POSIXct((x$time*1e+9), tz = "UTC", origin = "1970-01-01")) == end_day[i],] <- x[as.Date(as.POSIXct((x$time*1e+9), tz = "UTC", origin = "1970-01-01")) == end_day[i],]
  
  
  
}



summary(TP4M)

colnames(TP4M)<-c("ID","TP4M","TIME")

write.csv(TP4M, "~/ma_elephant/R/predictors/TP4M.csv")
write.csv(TP4M, "~/ma_elephant/R/predictors_car/TP4M.csv")


### Daily Surface Air Temperature----
unique(as.Date(as.POSIXct((segments_GEE$time*1e+9), tz = "UTC", origin = "1970-01-01")))

start_day<-sort(unique(as.Date(as.POSIXct((segments_GEE$time*1e+9), tz = "UTC", origin = "1970-01-01"))))
end_day<-sort(unique(as.Date(as.POSIXct((segments_GEE$time*1e+9), tz = "UTC", origin = "1970-01-01")))+1)


#to get the structure of the dataframe get the first month and add this to a new dataframe for the combined observations

i=1

x<- ee_grab(data = ee_data_collection(datasetID = "NCEP_RE/surface_temp",
                                      spatialReducer = "mean",
                                      temporalReducer = "mean", 
                                      timeStart = as.character(start_day[i]),
                                      timeEnd = as.character(end_day[i]), 
                                      resolution = 300,
                                      bandSelection = "air"
),
targetArea = "~/ma_elephant/segments_GEE.shp")

st_geometry(x)<-NULL


TD<-x[NA,]

#loop through all days for temporal observations

for (i in 1:length(start_day)){
  
  
  x<- ee_grab(data = ee_data_collection(datasetID = "NCEP_RE/surface_temp",
                                                 spatialReducer = "mean",
                                                 temporalReducer = "mean", 
                                                 timeStart = as.character(start_day[i]),
                                                 timeEnd = as.character(end_day[i]), 
                                                 resolution = 300,
                                                 bandSelection = "air"
  ),
  targetArea = "~/ma_elephant/segments_GEE.shp")
  
  
  st_geometry(x)<-NULL
 
  
  
  
  TD[as.Date(as.POSIXct((x$time*1e+9), tz = "UTC", origin = "1970-01-01")) == start_day[i],] <- x[as.Date(as.POSIXct((x$time*1e+9), tz = "UTC", origin = "1970-01-01")) == start_day[i],]
  
  print(i)
  
}
summary(TD)

colnames(TD)<-c("ID","TD","TIME")

write.csv(TD, "~/ma_elephant/R/predictors/TD.csv")
write.csv(TD, "~/ma_elephant/R/predictors_car/TD.csv")