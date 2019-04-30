#########################
##### load packages #####
#########################
library(mgcv)
library(Hmisc)
library(sf)
library(rgdal)
library(SDraw)
library(sp)


### set working directory
setwd("/ma_elephant/R")

#### living elephants

datale<-read.table("/ma_elephant/R/predictors/yxtable.csv",
                   header=TRUE,sep=",")

datale$LC<-as.factor(datale$LC)

### omit NAs ###
datale<-na.omit(datale)
### add Binary Count data ###
datale[,"BIN"]<- ifelse(datale$COUNT==0,0,1)

numofdetectseles<-data.frame(Site=datale$Site, eles=datale$BIN)

table(numofdetectseles)





#read data table
datac<-read.table("/ma_elephant/R/predictors_car/yxtable_car.csv",
                  header=TRUE,sep=",")
datac$X<-NULL
datac$COUNT_c<-as.numeric(datac$COUNT_c)




numofdetectscarcs<-data.frame(Site=datac$Site, carcs=datac$COUNT_c)

table(numofdetectscarcs)



 #####lenght of transects ####

segments_GEE<-rgdal::readOGR(dsn= "/ma_elephant",layer= "segments_GEE")
sites<-data.frame(ID=datale$ID, Site=datale$Site)
segments_site<-sp::merge(segments_GEE, sites, by="ID", all.x=FALSE)
sites<-unique(segments_site$Site)
translength<-data.frame(Sites=unique(segments_site$Site), len=NA)

###### length of subunit 2500 -> sum of all transects should be number of datapoints * 2500 m

for (i in 1:length(unique(segments_site$Site))){
  len<-length(segments_site[segments_site$Site==sites[i],])*2500 # calculate the length of all transects
  translength[i,2]<-len/1000 #convert to km
}



#### get coordinates of the sites
cordssite<-data.frame(Sites=unique(segments_site$Site), x=NA,y=NA)
for (i in 1:length(unique(segments_site$Site))){
  
  cordssite[i,2]<-round(coordinates(gCentroid(segments_site[segments_site$Site==sites[i],],
                                              byid=FALSE))[,1],digits=0)
  cordssite[i,3]<-round(coordinates(gCentroid(segments_site[segments_site$Site==sites[i],],
                                              byid=FALSE))[,2],digits=0) 
}


