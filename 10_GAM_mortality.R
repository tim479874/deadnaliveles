#########################
##### load packages #####
#########################

library(foreach)
library(doMC)
library(mgcv)
library(Hmisc)
library(sf)
library(pROC)
library(sp)


##################################
##### set working directory ######
##################################

setwd("/ma_elephant/R")

############################
#### import data table #####
############################

data<-read.table("/ma_elephant/R/predictors_car/yxtable_car.csv",header=TRUE,sep=",")
data$X<-NULL
data$COUNT_c<-as.numeric(data$COUNT_c)
data$LC<-as.factor(data$LC)


#################################################
#### add predictor probability of elephants #####
#################################################


LE<-read.table("/ma_elephant/R/predictors_car/LE.csv", header=TRUE,sep=",")
data$LE<-LE$x


#########################
#### normalize data #####
#########################

hist(data$DC)
hist(data$DR)
hist(sqrt(data$DR))
data[,"SQRT_DR"]<- c(sqrt(data$DR))

hist(data$DW)
hist(sqrt(data$DW))
data[,"SQRT_DW"]<- c(sqrt(data$DW))

hist(data$EL) 

hist(data$BS)
hist(sqrt(data$BS)) 
data[,"SQRT_BS"]<- c(sqrt(data$BS))

hist(data$LE)
hist(sqrt(data$LE))
data[,"SQRT_LE"]<- c(sqrt(data$LE))

hist(data$HS) 

hist(data$TC)
hist(sqrt(data$TC))

data[,"SQRT_TC"]<- c(sqrt(data$TC))

hist(data$NDVI)

hist(as.numeric(data$LC))

hist(as.numeric(data$PA)) 

hist(data$PD) 
hist(sqrt(data$PD))
data[,"SQRT_PD"]<- c(sqrt(data$PD))

hist(data$SL) 
hist(sqrt(data$SL)) 
data[,"SQRT_SL"]<- c(sqrt(data$SL))


hist(data$T)

hist(data$TD) 

hist(data$TPA) 

hist(data$TP4M)

hist(data$VC)


############################################
##### delete transoformed predictors #######
############################################
data$BS<-NULL
data$PD<-NULL
data$SL<-NULL
data$DR<-NULL
data$DW<-NULL
data$TC<-NULL
data$LE<-NULL

###############################################
##### safe normalized data for plotting #######
###############################################

write.csv(data, "/ma_elephant/R/predictors_car/yxtable_norm2.csv", row.names=FALSE)
data<-read.table("/ma_elephant/R/predictors_car/yxtable_norm2.csv",header=TRUE,sep=",")


##############################
##### scale predictors #######
##############################
head(data)

data[,colnames(data)%in%c("CA","DC","EL","HS","LE","NDVI","T","TD",
                          "TP14","TP4M","TPA","VC","SQRT_DR","SQRT_DW",
                          "SQRT_BS","SQRT_LE","SQRT_TC",
                          "SQRT_PD","SQRT_SL")]<- scale(
                            data[,colnames(data)%in%c("CA","DC",
                                                      "EL","HS",
                                                      "LE","NDVI",
                                                      "T","TD",
                                                      "TP14","TP4M",
                                                      "TPA","VC",
                                                      "SQRT_DR","SQRT_DW",
                                                      "SQRT_BS","SQRT_LE",
                                                      "SQRT_TC","SQRT_PD",
                                                      "SQRT_SL")],center=TRUE, scale=TRUE)


#############################
##### save data table #######
#############################

write.csv(data, "/ma_elephant/R/predictors_car/yxtable_norm_scal2.csv", 
          row.names=FALSE)

data<-read.table("/ma_elephant/paper+lit/R/predictors_car/yxtable_norm_scal2.csv",
                 header=TRUE,sep=",")

######################################
##### subset to Botswana North #######
######################################

bwa_scaled<-data[data$Country=="BWA",]

### land cover as factor
bwa_scaled$LC<-as.factor(bwa_scaled$LC)

head(bwa_scaled)

##########################################
### add Binary presence pseudo absence data #####
##########################################
data[,"BIN"]<- ifelse(data$COUNT_c==0,0,1)

#####################################
#### check for multicollinearity ####
#####################################
plot(varclus(as.matrix(bwa_scaled[,c("DC","EL","NDVI","T","TD",
                                     "TP4M","TPA","VC","SQRT_BS",
                                     "SQRT_PD","SQRT_LE","SQRT_SL",
                                     "SQRT_DR","SQRT_TC300","SQRT_DW")])))
abline(h=0.5, col="red")

#### not sufficient -> check concurvity


######################
######## GAM #########
######################

#read GEC segments
segments_GEE<-rgdal::readOGR(dsn= "/ma_elephant",layer= "segments_GEE")
#merge with datatable
bwa<-sp::merge(segments_GEE, bwa_scaled, by="ID", all.x=FALSE)

### add coordinates for spatial model
bwa_scaled[,"x"]<-sp::coordinates(bwa)[,1]
bwa_scaled[,"y"]<-sp::coordinates(bwa)[,2]


#############################
##### FULL MODEL ############
#############################

### define model formula - penalized thin plate regression splines
gamformucomp<- COUNT_c ~s(DC,bs="ts") +s(EL, bs="ts")+ s(NDVI,bs="ts")+ 
  s(T,bs="ts")
  s(TPA,bs="ts") + s(VC,bs="ts")+ s(SQRT_BS,bs="ts")+
  s(SQRT_PD,bs="ts") + s(SQRT_LE,bs="ts")+s(SQRT_SL,bs="ts") +
  s(SQRT_DR,bs="ts") + s(SQRT_TC300,bs="ts") + 
  s(SQRT_DW, bs="ts") + 
  s(x,y) +  
  PA+LC 

### fit model
carc_mod <- gam(gamformucomp, data=bwa_scaled, family="binomial")

### check summary
summary(carc_mod)

### add fitted values to dataframe
bwa_scaled["fv"]<-carc_mod$fitted.values

##########################
##### DHARMa Plot ########
##########################

simulationOutput_carcs <- simulateResiduals(fittedModel = carc_mod, refit = TRUE, n=50)

plot(simulationOutput_carcs)


#####################################
######## Check for SAC ##############
#####################################

### calculate residual
bwa_scaled["res"]<-bwa_scaled$COUNT_c-carc_mod$fitted.values

### merge new dataset with spatial segments
BWA_cor<-sp::merge(segments_GEE, bwa_scaled, by="ID", all.x=FALSE)


### subset Botswana to two part because whole Botswana is tooooo big for my memory
BWA_1<-BWA_cor[c(1:6192),]
BWA_2<-BWA_cor[c(6193:12385),]

### calculate Morans I
corlog1<-ncf::correlog(x=sp::coordinates(BWA_1)[,1],y=sp::coordinates(BWA_1)[,2],
                       z=BWA_1$res ,latlon=TRUE,na.rm=TRUE, increment=10, resamp=20)
corlog2<-ncf::correlog(x=sp::coordinates(BWA_2)[,1],y=sp::coordinates(BWA_2)[,2],
                       z=BWA_2$res, latlon=TRUE,na.rm=TRUE, increment=10, resamp=20)


### call plot
par(col=alpha(colvec[1],0.7)) ### add some transparency
plot(corlog1, xlim=c(0,500),ylim=c(-1,1),col=colvec[1], main="")
par(new=TRUE, col= alpha(colvec[2], 0.7))
plot(corlog2, xlim=c(0,500),ylim=c(-1,1), xaxt="n" ,yaxt="n", xlab="", ylab="",main="")
box(col="black")



#################################
#### roc curve for full model ### 
#################################

roccurve_gam_comp <- roc(bwa_scaled$COUNT_c ~ carc_mod$fitted.values)
plot(roccurve_gam_comp, ylim=c(0,1))
text(0.2,0.2,"AUC:")
text(0.025,0.2,round(auc(bwa_scaled$COUNT_c,carc_mod$fitted.values),digits=2))

###############################################################################
######## Check for concurvity and examine residual and quantiles ##############
###############################################################################

### full concurvity between all smooths
concurvity(carc_mod,full=TRUE)
### selective concurvity for all smooths
concurvity(carc_mod,full=FALSE)

######################
####### GAM CV #######
######################

### points for spatial blocks
bwa2 <- SpatialPoints(bwa, proj4string = CRS(proj4string(bwa)))
### calculate spatial blocks
blocks<-blockCV::spatialBlock(speciesData = bwa2,   selection = "systematic", 
                              rows=4,cols=2,k=8)

### gam formula for cross validation
gamformu<- COUNT_c ~s(DC,bs="ts") + s(EL, bs="ts")+ s(NDVI,bs="ts")+ 
  s(T,bs="ts")+ 
  s(TPA,bs="ts") + s(VC,bs="ts")+ s(SQRT_BS,bs="ts")+
  s(SQRT_PD,bs="ts") + s(SQRT_LE,bs="ts")+s(SQRT_SL,bs="ts") +
  s(SQRT_DW,bs="ts")+s(SQRT_DR,bs="ts") + s(SQRT_TC300,bs="ts") +
  s(x,y) 



### assign foldID to vector fold.nr
fold.nr<-blocks$foldID

# check for NAs -> no idea where they come from...
anyNA(blocks$foldID)
which(is.na(blocks$foldID))

# assign fold numbers by hand...
fold.nr[7891]<-fold.nr[7892]
fold.nr[12215]<-fold.nr[12216]


### assingn fold.nr to data
bwa_scaled["fold.nr"]<-fold.nr

table(bwa_scaled$COUNT_c, bwa_scaled$fold.nr)

#####################################################
###        1    2    3    4    5    6    7    8 #####
### 0   1579  789 3271  824 2363 1329  357  900 #####
### 1    156  345  183   87  121   73    5    3 #####
#####################################################

### save predictions of cross validation to data
bwa_scaled[,"gam_preds"]<-rep(NA,length(bwa_scaled$COUNT_c))

#create matrix and name the cols
eval_mod_car_gam <- matrix(data=NA,nrow=10,ncol=1)
#vector to store AUC
v<-1
#set number of folds
k=length(unique(fold.nr))


eval_mod_car_gam<-foreach(i=1:k, .combine="rbind",.verbose=TRUE,.packages=c("mgcv"))%do%{
  
  carcgam<- gam(gamformu, data=bwa_scaled[fold.nr != i,], family="binomial")
  preds <- predict(carcgam, newdata=bwa_scaled[fold.nr == i,], type="response")
  v[1] <- auc(bwa_scaled$COUNT_c[fold.nr==i],preds)
  
  bwa_scaled[bwa_scaled$fold.nr==i,]$gam_preds<-preds
  
  return(v)
  
}

### add colnames to evaluation matrix
colnames(eval_mod_car_gam)<-c("AUC")

### save evaluation matrix
write.csv(eval_mod_car_gam, "/ma_elephant/R/predictors_car/xval_gam_blocked_8folds_auc.csv"
          , row.names=FALSE)

### calculate mean AUC
mean(eval_mod_car_gam[,1], na.rm=TRUE)

# mean AUC 0.7057387


#########################
### calibration curve ###
#########################


calibrate.plot(bwa_scaled$COUNT_c,bwa_scaled$gam_preds)



#####################
#### preds vs obs ### old
#####################

plot(bwa_scaled$gam_preds,bwa_scaled$COUNT_c,xlim=c(0,1),ylim=c(0,1), xlab="Predictions"
     , ylab="Observed")
abline(lm(bwa_scaled$gam_preds~bwa_scaled$COUNT_c))
text(0.95,0.25 ,"R² = 0.06")

summary(lm(bwa_scaled$gam_preds~bwa_scaled$COUNT_c))


#########################
#### roc curve for cv ### 
#########################

roccurve_gam <- roc(bwa_scaled$COUNT_c ~ bwa_scaled$gam_preds)
plot(roccurve_gam, ylim=c(0,1))
text(0.2,0.2,"AUC:")
text(0.025,0.2,round(auc(bwa_scaled$COUNT_c,bwa_scaled$gam_preds),digits=2))
