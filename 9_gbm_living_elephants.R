#########################
##### load packages #####
#########################
library(foreach)
library(doMC)
library(mgcv)
library(Hmisc)
library(rgdal)
library(sp)
library(spatialEco)
library(sf)
library(ggplot2)
library(ggspatial)
library(gbm)
library(gridExtra)
library(rnaturalearth)
library(scales)
library(pROC)

##################################
##### set working directory ######
##################################

setwd("/ma_elephant/R")

############################
#### import data table #####
############################

datale<-read.table("/ma_elephant/R/predictors/yxtable.csv",header=TRUE,sep=",")

datale$LC<-as.factor(datale$LC)

### omit NAs ###
datale<-na.omit(datale)

### add Binary presence absence data ###
datale[,"BIN"]<- ifelse(datale$COUNT==0,0,1)


hist(datale$BIN)

#################################
## estimate parameters for gbm ##
#################################

#### rule of thumb interaction depths floor(sqrt(ncol(data)))
floor(sqrt(ncol(datale)))
# 5


### number of trees 

gbmb<-gbm(BIN~BS+DR+DC+EL+LC+NDVI+PA+PD+SL+T+TC300+TD+TPA+TP4M+VC+DW+prev_intern,distribution="bernoulli",data=datale,n.trees=20000, interaction.depth = 5, n.cores=2,shrinkage = 0.01)


gbm.perf(gbmb ,plot.it = TRUE , method = "OOB")
# 2195



###################################
### blocked hold out  validation ##
###################################

### number of iterations/folds for xval loop ### 
k = as.numeric(length(unique(datale$Site)))

### set folds ### 
fold.nr<-as.numeric(datale$Site)

### check for distribution of zeros and ones in the folds ### 
table(datale$BIN, fold.nr)                   


###########################################################################################
############# Distribution of presence/absence ############################################
###########################################################################################
## fold.nr                                                                               ##
##      1    2    3    4    5    6    7    8    9   10   11   12   13   14   15   16     ##
## 0 3102 9374 3619  234  588  670 2235 5606 3288 1975 2061 3339 1509 1768 1189 1219     ##
## 1  171 3011   88    2    1    3  322   20  452   34   79  305  896  269  309  392     ##
###########################################################################################



### create matrix and name the cols ###
eval_mod_gbm <- matrix(data=NA,nrow=as.numeric(length(unique(datale$Site))),ncol=1)

### vector for AUC ###
v<-1

### list for predictions ###
all.preds<-list()

### call plot for preds vs obs ###
plot(NULL,NULL,xlab="Predicted",ylab="Observed", xlim=c(0,1),ylim=c(0,1))


### col verctor based on sites ###
colvec<-rainbow(length(unique(datale$Site)))

#run the cross validation
eval_mod_gbm<-foreach(i=1:k, .combine="rbind",.verbose=TRUE,.packages=c("mgcv","ranger"))%do%{
  
  ### fit model to train data
  gbmb<-gbm(BIN~BS+DR+DC+EL+LC+NDVI+PA+PD+SL+T+TC300+TD+TPA+TP4M+VC+DW+prev_intern,distribution="bernoulli",data=datale[fold.nr != i,],n.trees=2200, interaction.depth = 5, n.cores=2,shrinkage = 0.01)
  ### predict on test/hold out data
  preds<-predict(gbmb,newdata=datale[fold.nr==i,],type="response",n.trees=2200)
  # calculate AUC for hold out data
  v[1] <- auc(datale$BIN[fold.nr==i],preds)
  # store the predictions
  all.preds[[i]]<-preds
  # call points with different colour for each site
  points(preds,datale$BIN[fold.nr==i], col=alpha(colvec[i], 0.5))
  
  
  return(v)
  
}

# legend with sites for plot
legend("right", col=colvec, pch=1 , legen=c("Angola, Luengue-Luiana", "Botswana, Nord","Congo, Garamba","Congo, Virunga","Ethopia, Babila","Ethopia, Omo","Kenya, Laikipia","Kenya, Lamu","Kenya, Tsavo","Chad, Chad River","Chad, Zakouma","Burkina Faso, Niger, Benin, Pedjari Complex","Zimbabwe, Matabelleland","Zimbabwe, Sebungwe","Zimbabwe, South East Lowveld","Zimbabwe, Zambezi Valley"),cex=0.75)

# add colnames to evaluation matrix
colnames(eval_mod_gbm)<-c("AUC")


mean(eval_mod_gbm[,c(1)])

# mean AUC  0.6166217

write.csv(eval_mod_gbm, "/ma_elephant/R/illustrations/eval_mod_bin_gbm_AUC_GOF.csv")


#########################
### calibration curve ###
#########################

all_preds_cv_gbm <-unlist(all.preds)


### fit glm/gam with natural spline of preds vs obs and plot with 95% CI
calibrate.plot(datale$BIN,all_preds_cv_gbm)

#####################
#### preds vs obs ### old
#####################

all_preds_cv_gbm <-unlist(all.preds)

plot(all_preds_cv_gbm, datale$BIN,ylim=c(0,1), xlim=c(0,1), ylab="Observed", xlab="Predicted")
abline(lm(all_preds_cv_gbm~ datale$BIN))
summary(lm(all_preds_cv_gbm~ datale$BIN))


datale["gbm_preds_xval"]<-all_preds_cv_gbm
datale["gbm_resis_xval"]<-data$BIN-data$gbm_preds_xval

write.csv(data,"/ma_elephant/R/transport/yxtable_gbm.csv",row.names = FALSE)
datale<-read.table("/ma_elephant/R/transport/yxtable_gbm.csv",header=TRUE,sep=",")

################################
###### FULL GBM MODEL ##########
################################


gbmbfull<-gbm(BIN~BS+DR+DC+EL+LC+NDVI+PA+PD+SL+T+TC300+TD+TPA+TP4M+VC+DW+prev_intern,
          distribution="bernoulli",
          data=datale,
          n.trees=2200, interaction.depth = 5, shrinkage = 0.01,
          n.cores=2,bag.fraction=1,train.fraction=1)


## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## 
# GBM acts somewhat strange. modelobject$fitted.values are NOT the fitted values,   ##
# if parameters bag.fraction and train.fraction are not set to one !!!!             ##
# predicting on new data does not show this behavior                                ##
## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## 



################################################################
#### extract fitted values as predictor for carcass model ######
################################################################

fv<-plogis(gbmbfull$fit)


write.csv(fv, "/ma_elephant/R/predictors_car/LE.csv", col.names = NULL)

### add to dataframe for further inspection
datale["fv"]<-plogis(gbmbfull$fit)

############################################
#### estimate perfomance of full model #####
############################################

#################
### ROC curve ###
#################

roccurve_fv <- roc(datale$BIN ~ plogis(gbmbfull$fit))
plot(roccurve_fv)
text(0.2,0.2,"AUC:")
text(0.025,0.2,round(auc(datale$BIN,plogis(gbmbfull$fit)),digits=2))

###########
### AUC ###
###########

auc(datale$BIN,plogis(gbmbfull$fit))

# Area under the curve: 0.8589

###########################
### variable importance ###
###########################

gbmsum<-summary(gbmbfull)

gbmsum <- transform(gbmsum, variables = reorder(var, -rel.inf))

ggplot(gbmsum,aes(x=variables,y=rel.inf, fill=rel.inf))+
  ylab("Relative Influence")+
  xlab("Variables")+
  geom_bar(stat="identity")+
  scale_fill_gradient(low = "blue",high="darkred", breaks=c(0,35))


#####################################
###### conditional effect plots #####
#####################################

### call pdf device
pdf("conditional_eff_gbm.pdf")
par(mfrow=c(17,1))


### loop through all predictors
for (i in 1:17){
  
  mfpl<-plot(gbmbfull, i.var=i, lwd = 2, type="response")
  print(mfpl)
}
dev.off()

#### #### #### #### #### #### #### #### #### #### #### 
#### check full model for spatial autocorrelation ####
#### #### #### #### #### #### #### #### #### #### #### 

segments_GEE<-rgdal::readOGR(dsn= "/Master/ma_elephant",layer= "segments_GEE")


preddata<-datale

preddata[,"resi"]<-datale$BIN-plogis(gbmbfull$fit)

preddata<-merge(segments_GEE, preddata, all.x=TRUE, by="ID")


rgdal::writeOGR(preddata,dsn= "/Master/ma_elephant/R/",layer= "preddatagbm", driver="ESRI Shapefile")
preddata<-rgdal::readOGR(dsn="/Master/ma_elephant/R/",layer= "preddatagbm")


preddata<-spatialEco::sp.na.omit(preddata)



#########################
###### SAC for AGO ######
#########################

#subsett to country
AGO_cor<-subset(preddata, Country == "AGO")

#calculate Morans I
corlog_ago<-ncf::correlog(x=sp::coordinates(AGO_cor)[,1],y=sp::coordinates(AGO_cor)[,2],z=AGO_cor$resi, latlon=TRUE,na.rm=TRUE, increment=10, resamp=20)




#########################
###### SAC for BWA ######
#########################

BWA_cor<-subset(preddata, Country == "BWA")

BWA_cor_1<-BWA_cor[c(1:6192),]
BWA_cor_2<-BWA_cor[c(6193:12385),]


head(BWA_cor$resi)
corlog_bwa<-ncf::correlog(x=sp::coordinates(BWA_cor_1)[,1],y=sp::coordinates(BWA_cor_1)[,2],z=BWA_cor_1$resi, latlon=TRUE,na.rm=TRUE, increment=10, resamp=20)



corlog_bwa2<-ncf::correlog(x=sp::coordinates(BWA_cor_2)[,1],y=sp::coordinates(BWA_cor_2)[,2],z=BWA_cor_2$resi, latlon=TRUE,na.rm=TRUE, increment=10, resamp=20)


#########################
###### SAC for COD ######
#########################

unique(preddata$Country)

COD_cor<-subset(preddata, Country == "COD")

corlog_COD<-ncf::correlog(x=sp::coordinates(COD_cor)[,1],y=sp::coordinates(COD_cor)[,2],z=COD_cor$resi, latlon=TRUE,na.rm=TRUE, increment=10, resamp=20)


#########################
###### SAC for ETH ######
#########################

ETH_cor<-subset(preddata, Country == "ETH")

corlog_ETH<-ncf::correlog(x=sp::coordinates(ETH_cor)[,1],y=sp::coordinates(ETH_cor)[,2],z=ETH_cor$resi, latlon=TRUE,na.rm=TRUE, increment=10, resamp=20)


#########################
###### SAC for KEN ######
#########################

KEN_cor<-subset(preddata, Country == "KEN")


KEN_cor1<-KEN_cor[c(1:5961),]
KEN_cor2<-KEN_cor[c(5962:11923),]
corlog_KEN1<-ncf::correlog(x=sp::coordinates(KEN_cor1)[,1],y=sp::coordinates(KEN_cor1)[,2],z=KEN_cor1$resi, latlon=TRUE,na.rm=TRUE, increment=10, resamp=20)
corlog_KEN2<-ncf::correlog(x=sp::coordinates(KEN_cor2)[,1],y=sp::coordinates(KEN_cor2)[,2],z=KEN_cor2$resi, latlon=TRUE,na.rm=TRUE, increment=10, resamp=20)


#########################
###### SAC for TCD ######
#########################

TCD_cor<-subset(preddata, Country == "TCD")

corlog_TCD<-ncf::correlog(x=sp::coordinates(TCD_cor)[,1],y=sp::coordinates(TCD_cor)[,2],z=TCD_cor$resi, latlon=TRUE,na.rm=TRUE, increment=10, resamp=20)


#########################
###### SAC for XWA ######
#########################

XWA_cor<-subset(preddata, Country == "XWA")

corlog_XWA<-ncf::correlog(x=sp::coordinates(XWA_cor)[,1],y=sp::coordinates(XWA_cor)[,2],z=XWA_cor$resi, latlon=TRUE,na.rm=TRUE, increment=10, resamp=20)


#########################
###### SAC for  ZWE ######
#########################

ZWE_cor<-subset(preddata, Country == "ZWE")

ZWE_cor1<-ZWE_cor[c(1:3775),]
ZWE_cor2<-ZWE_cor[c(3776:7551),]
corlog_ZWE1<-ncf::correlog(x=sp::coordinates(ZWE_cor1)[,1],y=sp::coordinates(ZWE_cor1)[,2],z=ZWE_cor1$resi, latlon=TRUE,na.rm=TRUE, increment=10, resamp=20)
corlog_ZWE2<-ncf::correlog(x=sp::coordinates(ZWE_cor2)[,1],y=sp::coordinates(ZWE_cor2)[,2],z=ZWE_cor2$resi, latlon=TRUE,na.rm=TRUE, increment=10, resamp=20)

##### combine all correlograms in one plot

### colorvector
colvec<-rainbow(11)

tiff("gbm_SAC.tiff", units="in", width=12, height=7, res=300, pointsize = 12)
par(col=alpha(colvec[1],0.7)) ### add some transparency
plot(corlog_ago, xlim=c(0,900),ylim=c(-1,1),col=colvec[1], main="")
par(new=TRUE, col= alpha(colvec[2], 0.7))
plot(corlog_bwa, xlim=c(0,900),ylim=c(-1,1), xaxt="n" ,yaxt="n", xlab="", ylab="",main="")
par(new=TRUE, col=alpha(colvec[3], 0.7))
plot(corlog_bwa2, xlim=c(0,900),ylim=c(-1,1), xaxt="n" ,yaxt="n", xlab="", ylab="",main="")
par(new=TRUE,col=alpha(colvec[4], 0.7))
plot(corlog_COD, xlim=c(0,900),ylim=c(-1,1), xaxt="n" ,yaxt="n", xlab="", ylab="",main="")
par(new=TRUE,col=alpha(colvec[5], 0.7))
plot(corlog_ETH, xlim=c(0,900),ylim=c(-1,1), xaxt="n" ,yaxt="n",  xlab="", ylab="",main="")
par(new=TRUE,col=alpha(colvec[6], 0.7))
plot(corlog_KEN1, xlim=c(0,900),ylim=c(-1,1), xaxt="n" ,yaxt="n", xlab="", ylab="",main="")
par(new=TRUE,col=alpha(colvec[7], 0.7))
plot(corlog_KEN2, xlim=c(0,900),ylim=c(-1,1), xaxt="n" ,yaxt="n", xlab="", ylab="",main="")
par(new=TRUE,col=alpha(colvec[8], 0.7))
plot(corlog_TCD, xlim=c(0,900),ylim=c(-1,1), xaxt="n" ,yaxt="n",  xlab="", ylab="",main="")
par(new=TRUE, col=alpha(colvec[9], 0.7))
plot(corlog_XWA, xlim=c(0,900),ylim=c(-1,1), xaxt="n" ,yaxt="n", xlab="", ylab="",main="")
par(new=TRUE,col=alpha(colvec[10], 0.7))
plot(corlog_ZWE1, xlim=c(0,900),ylim=c(-1,1), xaxt="n" ,yaxt="n",  xlab="", ylab="",main="")
par(new=TRUE,col=alpha(colvec[11], 0.7))
plot(corlog_ZWE2, xlim=c(0,900),ylim=c(-1,1), xaxt="n" ,yaxt="n",  xlab="", ylab="",main="")
box(col="black")

dev.off()

############################################################
################ Map of residuals in space #################
############################################################


### vector with unique sites to loop through
sites<-unique(preddata$Site)

#### convert to sf obejct for ggplot

sfpreddt<-st_as_sf(preddata)


### call empty list to store the plots
p<-list()
for (i in 1:length(sites)){
  p[[i]] <- list() ### nest a new list in the called list
  p[[i]][[1]] <- ggplot() +  ### call plot and store in the nested list
    ggtitle(sites[i])+
    annotation_scale(location = "tr", width_hint = 0.25) +
    annotation_north_arrow(location = "tr", which_north = "true", 
                           pad_x = unit(0.1, "in"), pad_y = unit(0.25, "in"),
                           style = north_arrow_fancy_orienteering) +
    
    geom_sf(data = sfpreddt[sfpreddt$Site==sites[i],], aes(fill = resi ,color= resi)) + 
    scale_colour_gradient2(low = "red",mid="grey80",high = "blue", breaks=c(-1,0,1)) + 
    scale_fill_gradient2(low = "red",mid="grey80",high = "blue", breaks=c(-1,0,1)) +
    xlab("Longitude") + ylab("Latitude") 
  
  
  
  
}

### call pdf
pdf("/Master/ma_elephant/R/illustrations/comb.pdf", onefile = TRUE)
for (i in seq(length(p))) {
  do.call("grid.arrange", p[[i]])  ### loop through the lists and arrange each plot on a new pdf site
}
dev.off()



##########################################
### Visualize Living Elephants in BWA ####
##########################################

### subset data to Bwa
preddata_bots_le<-preddata[preddata$Country=="BWA",]

#set theme for ggplot
theme_set(theme_bw())

#get shape of botswana country
bots <- ne_countries(scale = "medium", country="Botswana",returnclass = "sf")

### plot for observations of Living elephants
bwa_plot_p_le<-SpatialPointsDataFrame(preddata_bots_le,preddata_bots_le@data)

### points need to be a data.table therefore convert spatialpointsdataframe to dataframe
points_bwa_le <- data.frame(bwa_plot_p_le)

### polygons need to be sf object therefore convert spatialpolygonsdataframe to sf
bwa_sf_le<-st_as_sf(preddata_bots_le)


#####################
##### call plot #####
#####################

ggplot(data = bots) +
  geom_sf(color = "black", fill = "white") +
  xlab("Longitude") + ylab("Latitude") +
  annotation_scale(location = "tr", width_hint = 0.5) +
  annotation_north_arrow(location = "tr", which_north = "true", 
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering) +
  geom_sf(data = bwa_sf_le, aes(fill = fv ,color=fv)) +
  scale_colour_gradient(low = "grey70",high = "blue", breaks=c(0,1),labels=c("0","1") ,limits=c(0,1),guide = "colourbar", aesthetics = c("fill","colour"),name="Fitted probabilty") + 
 geom_point(data = points_bwa_le[points_bwa_le$BIN==1,], aes(x = coords.x1, y = coords.x2), 
             colour="black",size = 0.25, shape = 4, alpha = 0.4) +
  coord_sf(xlim = c(21.9, 26.5), ylim = c(-17.75, -20.8)) 

ggsave("gbm_fv.pdf", device = "pdf", dpi=300)

save.image("/ma_elephant/gbm_living_elephants_SDM.RData")
