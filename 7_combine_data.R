library(rgdal)
library(zoo)
library(stringi)
library(pscl)
library(MASS)
library(corrplot)
library(readr)
library(sp)



setwd("~/Documents/Master/ma_elephant/")

httr::set_config(httr::config(http_version = 0))


rm(list = ls()[which(ls() != "segments")])
if(!"segments" %in% ls()) segments <- rgdal::readOGR("~/Documents/Master/ma_elephant/segments.shp")
summary(segments$SC)
segments$Site <- segments$SC 
segments$SC <- NULL

### load all csv files in the predictor folder
for(pred in list.files("~/Documents/Master/ma_elephant/R/predictors/")) assign(gsub(".csv", "", pred), read_csv(paste0("~/Documents/Master/ma_elephant/R/predictors/", pred)))



HT <- HT[,2:3] #remove column with row.names
COUNT <- COUNT[,2:3] #remove column with row.names
REPS <- REPS[,2:3] #remove column with row.names


#remove geometry and unnecessary columns
BS<-BS[,c(2:3)]
DC<-DC[,c(2:3)]
EL<-EL[,c(2:3)]
LC<-LC[,c(2:3)]
NDVI<-NDVI[,c(2:3)]
PA<-PA[,c(2:3)]
PD<-PD[,c(2:3)]
T<-T[,c(2:3)]
TPA<-TPA[,c(2:3)]
VC<-VC[,c(2:3)]
TP4M<-TP4M[,c(2:3)]
TC<-TC[,c(2:3)]
SL<-SL[,c(2:3)]
TD<-TD[,c(2:3)]
DR<-DR[,c(2:3)]
DW<-DW[,c(2:3)]

#colnames id -> ID

colnames(DC)<-c("ID","DC")
colnames(PD)<-c("ID","PD")
colnames(COUNT)<-c("ID","COUNT")
colnames(EL)<-c("ID","EL")
colnames(PA)<-c("ID","PA")
colnames(SL)<-c("ID","SL")
colnames(TC)<-c("ID","TC")
colnames(T)<-c("ID","T")
colnames(VC)<-c("ID","VC")
colnames(TD)<-c("ID","TD")

#combine all predictors + count data, do some unit conversions if necessary
preds <- gsub(".csv", "", list.files("~/ma_elephant/R/predictors/"))
View(preds)

preds.list <- mget(preds)
?mget
all.preds <- Reduce(function(d1, d2) merge(d1, d2, by = "ID", all.x = TRUE, all.y = FALSE), 
                    preds.list)

#reduce number of NAs (REPS, HT) and add Site and Country to the dataset
names(all.preds) <- c("ID", preds)
all.preds <- all.preds[match(as.character(segments$ID), as.character(all.preds$ID)), ]
all.preds$Country <- segments$CC
all.preds$Site <- segments$Site
all.preds$Transect <- unlist(lapply(strsplit(as.character(all.preds$ID), "p"), function(x) x[1]))
all.preds$REPS[is.na(all.preds$REPS)] <- 0

all.preds$HT <- as.character(all.preds$HT)
all.preds$HT <- ifelse(is.na(all.preds$HT), "none", all.preds$HT)
summary(all.preds)



table(data.frame(which(is.na(all.preds), arr.ind=TRUE)))




all.preds <- na.omit(all.preds)

#unit conversion, change names of predictors 
all.preds$T <- all.preds$T - 273.15 # scale to °C
all.preds$TD <- all.preds$TD - 273.15 # scale to °C
all.preds$NDVI <- all.preds$NDVI / 250 #scale to -0.1 to 0.6




summary(all.preds)
#save all predictors and the response
write.csv(all.preds, "~/ma_elephant/R/predictors/yxtable.csv")
View(all.preds)



### ### ### ### ### ### ### ### 
### extraction for carcasses ### 
### ### ### ### ### ### ### ### 

rm(list = ls()[which(ls() != "segments")])
if(!"segments" %in% ls()) segments <- rgdal::readOGR("~/ma_elephant/segments.shp")
summary(segments$SC)
segments$Site <- segments$SC 
segments$SC <- NULL


#add all predictors
for(pred in list.files("~/ma_elephant/R/predictors_car/")) assign(gsub(".csv", "", pred), read.csv(paste0("~/Documents/Master/ma_elephant/R/predictors_car/", pred), stringsAsFactors = F))
HT_c <- HT_c[,2:3] #remove column with row.names
COUNT_c <- COUNT_c[,2:3] #remove column with row.names
REPS_c <- REPS_c[,2:3] #remove column with row.names
#tif of protected areas does only contain 1 and NA. replace NAs with 0 in protected areas predictor
PA$max[is.na(PA$max)] <- 0

HS<-HS[,2:3]
colnames(HS)<-c("ID","HS")
BS<-BS[,2:3]
CA<-CA[,2:3]
EL<-EL[,2:3]
LC<-LC[,2:3]
NDVI<-NDVI[,2:3]
PA<-PA[,2:3]
PD<-PD[,2:3]
ST<-ST[,2:3]
T<-T[,2:3]
TC300<-TC300[,2:3]
TD<-TD[,2:3]
TP14<-TP14[,2:3]
TP4M<-TP4M[,2:3]
TPA<-TPA[,2:3]
VC<-VC[,2:3]


#combine all predictors + count data, do some unit conversions if necessary
preds <- gsub(".csv", "", list.files("~/ma_elephant/R/predictors_car/"))
preds.list <- mget(preds)
all.preds <- Reduce(function(d1, d2) merge(d1, d2, by = "ID", all.x = TRUE, all.y = FALSE), 
                    preds.list)

#reduce number of NAs (REPS, HT) and add Site and Country to the dataset
names(all.preds) <- c("ID", preds)
all.preds$PA <- as.factor(all.preds$PA)
all.preds <- all.preds[match(as.character(segments$ID), as.character(all.preds$ID)), ]
all.preds$Country <- segments$CC
all.preds$Site <- segments$Site
all.preds$Transect <- unlist(lapply(strsplit(as.character(all.preds$ID), "p"), function(x) x[1]))
all.preds$REPS_c[is.na(all.preds$REPS_c)] <- 0

all.preds$HT_c <- as.character(all.preds$HT_c)
all.preds$HT_c <- ifelse(is.na(all.preds$HT_c), "none", all.preds$HT_c)
summary(all.preds)
all.preds <- na.omit(all.preds)
?na.omit
#unit conversion, change names of predictors 
all.preds$T <- all.preds$T - 273.15 # scale to °C
all.preds$TD <- all.preds$TD - 273.15
all.preds$NDVI <- all.preds$NDVI / 250 # scale to 0 to 1

### remove all counts from herds, where car insert 1

all.preds$COUNT_c<-ifelse(all.preds$HT_c=="none",0,1)

View(all.preds$HT_c)
summary(all.preds)
#save all predictors and the response
write.csv(all.preds, "~/ma_elephant/R/predictors_car/yxtable_car.csv")