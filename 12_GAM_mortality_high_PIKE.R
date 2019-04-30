###################################################################
###################################################################
###########     Carcass Model with high PIKE values    ############
###################################################################
###################################################################
#GEC Code                                     MIKE SITE ID      has carcass observations
#COD_GAR  2014 PIKE 98 %                          GAR                  yes 19
#COD_VIR  2014 PIKE 100%                          VIR                   no
#ETH_BAB  2014 PIKE 100 %                         BBL                   no
#KEN_LAI  2014 PIKE 38%                           SBR                   yes 101
#KEN_TSV  2014 PIKE  50%                          TSV                   yes 228
#TCD_ZAK  2014 PIKE Missing 2015 75%              ZAK                   yes 69
#XWA_TBC  2015 PIKE 80 %                          PDJ                   yes 119


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
library(gbm)
library(scales)



##################################
##### set working directory ######
##################################

setwd("/ma_elephant/R")


######################
##### load data ######
######################

data<-read.table("/ma_elephant/R/predictors_car/yxtable_norm_scal2.csv",header=TRUE,
                 sep=",")



### subset data to sites with high PIKE
data_c<-data[data$Site %in% c("COD_GAR","KEN_LAI","KEN_TSV","TCD_ZAK","XWA_TBC"),]
data_c$LC<-as.factor(data_c$LC)

### add travel cost

TR<-read.table("/home/wurst/Documents/Master/ma_elephant/paper+lit/R/carcs/tr.csv",sep=",",
               header=TRUE)

colnames(TR)<-c("ID","TR")
hist(log(TR$TR))
TR$TR<-log(TR$TR)
colnames(TR)<-c("ID","LOG_TR")

data_c<-merge(data_c, TR, by="ID", all.x=TRUE)
data_c$LOG_TR<-scale(data_c$LOG_TR)


### add coordinates
temporal_data<-sp::merge(segments_GEE, data_c, by="ID", all.x=FALSE)

### coords

data_c[,"x"]<-sp::coordinates(temporal_data)[,1]
data_c[,"y"]<-sp::coordinates(temporal_data)[,2]


### ADD ESTIMATED PIKE AS PREDICTOR / comparable to prevalence
data_c["PIKE"]<-rep(NA,length(data_c$COUNT_c))


data_c$PIKE[data_c$Site=="COD_GAR"]<-mean(year2014[rownames(year2014) %in% "GAR",])
data_c$PIKE[data_c$Site=="KEN_LAI"]<-mean(year2014[rownames(year2014) %in% "SBR",])
data_c$PIKE[data_c$Site=="KEN_TSV"]<-mean(year2014[rownames(year2014) %in% "TSV",])
data_c$PIKE[data_c$Site=="TCD_ZAK"]<-mean(year2014[rownames(year2014) %in% "ZAK",])
data_c$PIKE[data_c$Site=="XWA_TBC"]<-mean(year2014[rownames(year2014) %in% "PDJ",])

data_c$PIKE<-scale(data_c$PIKE)


#####################################
#### check for multicollinearity ####
#####################################
plot(varclus(as.matrix(data_c[,c("DC","EL","NDVI","T","TPA","VC","SQRT_BS",
                                 "SQRT_PD","SQRT_LE","SQRT_SL","SQRT_DR",
                                 "SQRT_TC300","SQRT_DW","LOG_TR","PIKE")])))
abline(h=0.3, col="red")

### two pairs with high but not perfect collinearity -> keep to make models comparable

#############################
##### FULL MODEL ############
#############################


gamhipikefull<- COUNT_c ~s(DC,bs="ts") +s(EL, bs="ts")+ s(NDVI,bs="ts")+ 
  s(T,bs="ts")+ 
  s(TPA,bs="ts") + s(VC,bs="ts")+ s(SQRT_BS,bs="ts")+
  s(SQRT_PD,bs="ts") + s(SQRT_LE,bs="ts")+s(SQRT_SL,bs="ts") +
  s(SQRT_DR,bs="ts") + s(SQRT_TC300,bs="ts") + 
  s(SQRT_DW, bs="ts") +  s(LOG_TR, bs="ts" ) + s(PIKE,bs="ts", k=3)+ PA + LC

### fit model
carc_mod_hp <- gam(gamhipikefull, data=data_c, family="binomial")

### check summary
summary(carc_mod_hp)

plot(carc_mod_hp, ylim=c(-10,10))

### add fitted values to dataframe
data_c["fv"]<-carc_mod_hp$fitted.values

#####################################
######## Check for SAC ##############
#####################################

### calculate residual
data_c["res"]<-data_c$COUNT_c-carc_mod_hp$fitted.values

### merge new dataset with spatial segments
hp_cor<-sp::merge(segments_GEE, data_c, by="ID", all.x=FALSE)


### subset to the five sites
hp_cor_1<-hp_cor[hp_cor$Site=="COD_GAR",]
hp_cor_2<-hp_cor[hp_cor$Site=="KEN_LAI",]
hp_cor_3<-hp_cor[hp_cor$Site=="KEN_TSV",]
hp_cor_4<-hp_cor[hp_cor$Site=="TCD_ZAK",]
hp_cor_5<-hp_cor[hp_cor$Site=="XWA_TBC",]

### calculate Morans I
corlog1<-ncf::correlog(x=sp::coordinates(hp_cor_1)[,1],
                       y=sp::coordinates(hp_cor_1)[,2],z=hp_cor_1$res ,latlon=TRUE,na.rm=TRUE,
                       increment=10, resamp=20)

corlog2<-ncf::correlog(x=sp::coordinates(hp_cor_2)[,1],
                       y=sp::coordinates(hp_cor_2)[,2],z=hp_cor_2$res ,latlon=TRUE,na.rm=TRUE,
                       increment=10, resamp=20)

corlog3<-ncf::correlog(x=sp::coordinates(hp_cor_3)[,1],
                       y=sp::coordinates(hp_cor_3)[,2],z=hp_cor_3$res ,latlon=TRUE,na.rm=TRUE,
                       increment=10, resamp=20)

corlog4<-ncf::correlog(x=sp::coordinates(hp_cor_4)[,1],
                       y=sp::coordinates(hp_cor_4)[,2],z=hp_cor_4$res ,latlon=TRUE,na.rm=TRUE,
                       increment=10, resamp=20)

corlog5<-ncf::correlog(x=sp::coordinates(hp_cor_5)[,1],
                       y=sp::coordinates(hp_cor_5)[,2],z=hp_cor_5$res ,latlon=TRUE,na.rm=TRUE,
                       increment=10, resamp=20)

### call plot
tiff("hp_SAC.tiff", units="in", width=12, height=7, res=300, pointsize = 12)
par(col=alpha("red",0.7)) ### add some transparency
plot(corlog1, xlim=c(0,400),ylim=c(-1,1),col="red", main="")
par(new=TRUE, col= alpha("blue", 0.7))
plot(corlog2, xlim=c(0,400),ylim=c(-1,1), xaxt="n" ,yaxt="n", xlab="", ylab="",main="")
par(new=TRUE, col= alpha("yellow", 0.7))
plot(corlog3, xlim=c(0,400),ylim=c(-1,1), xaxt="n" ,yaxt="n", xlab="", ylab="",main="")
par(new=TRUE, col= alpha("darkblue", 0.7))
plot(corlog4, xlim=c(0,400),ylim=c(-1,1), xaxt="n" ,yaxt="n", xlab="", ylab="",main="")
par(new=TRUE, col= alpha("orange", 0.7))
plot(corlog5, xlim=c(0,400),ylim=c(-1,1), xaxt="n" ,yaxt="n", xlab="", ylab="",main="")
box(col="black")

dev.off()


#################################
#### roc curve for full model ### 
#################################

roccurve_gam_hp <- roc(data_c$COUNT_c ~ carc_mod_hp$fitted.values)
plot(roccurve_gam_hp, ylim=c(0,1))
text(0.2,0.2,"AUC:")
text(0.025,0.2,round(auc(data_c$COUNT_c,carc_mod_hp$fitted.values),digits=2))



###############################################################################
######## Check for concurvity and examine residual and quantiles ##############
###############################################################################

### full concurvity between all smooths
concurvity(carc_mod_hp,full=TRUE)
### selective concurvity for all smooths
concurvity(carc_mod_hp,full=FALSE)

### call qqplot and further diagnostic plots
par(mfrow=c(2,2))
gam.check(carc_mod_hp)



########################################
###  gam blocked hold out  validation ##
########################################

### number of iterations/folds for xval loop ### 
k = as.numeric(length(unique(data_c$Site)))


data_c$Site<-droplevels(data_c$Site)

### set folds ### 
fold.nr<-as.numeric(data_c$Site)

### check for distribution of zeros and ones in the folds ### 
table(data_c$COUNT_c, fold.nr) 

#################################
#####     fold.nr   #############
#################################
#      1    2    3    4    5
#0 3688 2456 3512 2071 3525
#1   19  101  228   69  119
#################################


### gam formula for cross validation 
gamhipike<- COUNT_c ~s(DC,bs="ts") +s(EL, bs="ts")+ s(NDVI,bs="ts")+ 
  s(T,bs="ts")+ 
  s(TPA,bs="ts") + s(VC,bs="ts")+ s(SQRT_BS,bs="ts")+
  s(SQRT_PD,bs="ts") + s(SQRT_LE,bs="ts")+s(SQRT_SL,bs="ts") +
  s(SQRT_DR,bs="ts") + s(SQRT_TC300,bs="ts") + 
  s(SQRT_DW, bs="ts") +  s(LOG_TR, bs="ts" ) + s(PIKE,bs="ts", k=3)



### save predictions of cross validation to data
data_c[,"gam_preds"]<-rep(NA,length(data_c$COUNT_c))

#create matrix and name the cols
eval_mod_car_gam_hp <- matrix(data=NA,nrow=5,ncol=1)
#vector to store AUC
v<-1


### blocked cross validation
eval_mod_car_gam_hp<-foreach(i=1:k, .combine="rbind",.verbose=TRUE,.packages=c("mgcv"))%do%{
  
  carcgam<- gam(gamhipike, data=data_c[fold.nr != i,], family="binomial")
  preds <- predict(carcgam, newdata=data_c[fold.nr == i,], type="response")
  v[1] <- auc(data_c$COUNT_c[fold.nr==i],preds)
  
  data_c[fold.nr==i,]$gam_preds<-preds
  
  return(v)
  
}

### add colnames to evaluation matrix
colnames(eval_mod_car_gam_hp)<-c("AUC")

### save evaluation matrix
write.csv(eval_mod_car_gam_hp, "/ma_elephant/R/predictors_car/xval_gam_blocked_8folds_auc_hp.csv",
          row.names=FALSE)

### calculate mean AUC
mean(eval_mod_car_gam_hp[,1], na.rm=TRUE)

# mean AUC  0.5937484


#########################
### calibration curve ###
#########################


calibrate.plot(data_c$COUNT_c,data_c$gam_preds)



#########################
#### roc curve for cv ### 
#########################

roccurve_gam <- roc(data_c$COUNT_c ~ data_c$gam_preds)
plot(roccurve_gam, ylim=c(0,1))
text(0.2,0.2,"AUC:")
text(0.025,0.2,round(auc(data_c$COUNT_c,data_c$gam_preds),digits=2))


##########################################
########   descriptive statistics  #######
##########################################

######## percentage of observations for mort u high PIKE

(table(data_c$COUNT_c, data_c$Site)[2,]/
(table(data_c$COUNT_c, data_c$Site)[2,]+table(data_c$COUNT_c, data_c$Site)[1,]))*100


# COD_GAR    KEN_LAI    KEN_LAM    KEN_TSV  TCD_ZAK    XWA_TBC   
#0.5125438 3.9499413  0.2843939  6.0962567 3.2242991  3.2656422



### mean percentage of carcass observations
sum(0.5125438 ,3.9499413 , 0.2843939 , 6.0962567, 3.2242991  ,3.2656422)/5
### 3.466615 %

### number of total datapoints
length(data_c$ID)
#15788
####### percentage natural mortality

(table(data$COUNT_c, data$Site)[2,2]/table(data$COUNT_c, data$Site)[1,2])*100
###   BWA
### 8.526113 %

length(data[data$Site=="BWA_NOR",]$ID)
####12385


save.image(file="/ma_elephant/R/gam_pike.RData")