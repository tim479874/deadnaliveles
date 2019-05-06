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
library(grid)
library(gridExtra)



#set working directory
setwd("/ma_elephant/paper+lit/R/carcs")

########### ########### ########### ########### ###########
########### Line Break for long axis labes #########
########### ########### ########### ########### ########### 

addline_format <- function(x,...){
  gsub('\\s','\n',x)
}


######################################d
### spatial dataframe for plotting ###
######################################

segments_GEE<-rgdal::readOGR(dsn= "/ma_elephant",layer= "segments_GEE")

count_hp_plot<-merge(segments_GEE, data_c, by="ID", all.x=FALSE)

count_hp_plot_p<-SpatialPointsDataFrame(count_hp_plot, count_hp_plot@data)

### points need to be a data.table therefore convert spatialpointsdataframe to dataframe
points_hp <- data.frame(count_hp_plot_p)

### polygons need to be sf object therefore convert spatialpolygonsdataframe to sf
count_hp_plot_sf<-st_as_sf(count_hp_plot)


###############################
########## COD_GAR plot ###########
#### fitted values from GAM ###
###############################

points_hp_cod_gar<-points_hp[points_hp$Site=="COD_GAR",]
count_hp_plot_sf_cod_gar<-count_hp_plot_sf[count_hp_plot_sf$Site=="COD_GAR",]
### call the plot

cod_gar_fv<-ggplot() +
  labs(title="Congo Garamba")+
  xlab("Longitude") + ylab("Latitude") +
  annotation_scale(location = "tr", width_hint = 0.25) +
  annotation_north_arrow(location = "tr", which_north = "true", 
                         pad_x = unit(0.25, "in"), pad_y = unit(0.25, "in"),
                         style = north_arrow_fancy_orienteering) +
  geom_sf(data = count_hp_plot_sf_cod_gar, aes(fill = fv ,color=fv)) +
  scale_colour_gradient(low = "grey70",high = "darkred", breaks=c(0,0.5,1),
                        labels=c("0","0.5","1") ,limits=c(0,1),guide = "colourbar", 
                        aesthetics = c("color","fill"),name=addline_format("Estimated probabilty")) + 
  geom_point(data = points_hp_cod_gar[points_hp_cod_gar$COUNT_c==1,],
             aes(x = coords.x1, y = coords.x2), size = 0.25, 
             shape = 4,color="black", alpha = 0.4) +
    theme(panel.background = element_rect(fill="white"),axis.text=element_text(size=14),
        axis.title=element_text(size=14),
        axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black"))



###############################
########## KEN_LAI plot ###########
#### fitted values from GAM ###
###############################

points_hp_ken_lai<-points_hp[points_hp$Site=="KEN_LAI",]
count_hp_plot_sf_ken_lai<-count_hp_plot_sf[count_hp_plot_sf$Site=="KEN_LAI",]
### call the plot

ken_lai_fv<-ggplot() +
  labs(title="Kenya Laikipia")+
  xlab("Longitude") + ylab("Latitude") +
  annotation_scale(location = "tr", width_hint = 0.25) +
  annotation_north_arrow(location = "tr", which_north = "true", 
                         pad_x = unit(0.25, "in"), pad_y = unit(0.25, "in"),
                         style = north_arrow_fancy_orienteering) +
  geom_sf(data = count_hp_plot_sf_ken_lai, aes(fill = fv ,color=fv)) +
  scale_colour_gradient(low = "grey70",high = "darkred", breaks=c(0,0.5,1),
                        labels=c("0","0.5","1") ,limits=c(0,1),guide = "colourbar",
                        aesthetics = c("color","fill"),name=addline_format("Estimated probabilty")) + 
  geom_point(data = points_hp_ken_lai[points_hp_ken_lai$COUNT_c==1,],
             aes(x = coords.x1, y = coords.x2), size = 0.25, 
             shape = 4,color="black", alpha = 0.4)  +
  theme(panel.background = element_rect(fill="white"),axis.text=element_text(size=14),
        axis.title=element_text(size=14),
        axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black"))

###############################
########## KEN_TSV plot ###########
#### fitted values from GAM ###
###############################

points_hp_ken_tsv<-points_hp[points_hp$Site=="KEN_TSV",]
count_hp_plot_sf_ken_tsv<-count_hp_plot_sf[count_hp_plot_sf$Site=="KEN_TSV",]
### call the plot

ken_tsv_fv<-ggplot() +
  labs(title="Kenya Tsavo")+
  xlab("Longitude") + ylab("Latitude") +
  annotation_scale(location = "tr", width_hint = 0.25) +
  annotation_north_arrow(location = "tr", which_north = "true", 
                         pad_x = unit(0.25, "in"), pad_y = unit(0.25, "in"),
                         style = north_arrow_fancy_orienteering) +
  geom_sf(data = count_hp_plot_sf_ken_tsv, aes(fill = fv ,color=fv)) +
  scale_colour_gradient(low = "grey70",high = "darkred", breaks=c(0,0.5,1),
                        labels=c("0","0.5","1") ,limits=c(0,1),guide = "colourbar", 
                        aesthetics = c("color","fill"),name=addline_format("Estimated probabilty")) + 
  geom_point(data = points_hp_ken_tsv[points_hp_ken_tsv$COUNT_c==1,],
             aes(x = coords.x1, y = coords.x2), size = 0.25, 
             shape = 4,color="black", alpha = 0.4)  +
  theme(panel.background = element_rect(fill="white"),axis.text=element_text(size=14),
        axis.title=element_text(size=14),
        axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black"))



###################################
########## TCD_ZAK plot ###########
#### fitted values from GAM #######
###################################

points_hp_tcd_zak<-points_hp[points_hp$Site=="TCD_ZAK",]
count_hp_plot_sf_tcd_zak<-count_hp_plot_sf[count_hp_plot_sf$Site=="TCD_ZAK",]
### call the plot

tcd_zak_fv<-ggplot() +
  labs(title="Chad Zakouma")+
  xlab("Longitude") + ylab("Latitude") +
  annotation_scale(location = "tr", width_hint = 0.25) +
  annotation_north_arrow(location = "tr", which_north = "true", 
                         pad_x = unit(0.25, "in"), pad_y = unit(0.25, "in"),
                         style = north_arrow_fancy_orienteering) +
  geom_sf(data = count_hp_plot_sf_tcd_zak, aes(fill = fv ,color=fv)) +
  scale_colour_gradient(low = "grey70",high = "darkred", breaks=c(0,0.5,1),
                        labels=c("0","0.5","1") ,limits=c(0,1),guide = "colourbar",
                        aesthetics = c("color","fill"),name=addline_format("Estimated probabilty")) + 
  geom_point(data = points_hp_tcd_zak[points_hp_tcd_zak$COUNT_c==1,], 
             aes(x = coords.x1, y = coords.x2), size = 0.25, 
             shape = 4,color="black", alpha = 0.4)  +
  theme(panel.background = element_rect(fill="white"),axis.text=element_text(size=14),
        axis.title=element_text(size=14),
        axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black"))



###################################
########## XWA_TBC plot ###########
#### fitted values from GAM #######
###################################

points_hp_xwa_tbc<-points_hp[points_hp$Site=="XWA_TBC",]
count_hp_plot_sf_xwa_tbc<-count_hp_plot_sf[count_hp_plot_sf$Site=="XWA_TBC",]
### call the plot

xwa_tbc_fv<-ggplot() +
  labs(title="Burkina Faso, Niger,Benin Pendjari-Complex")+
  xlab("Longitude") + ylab("Latitude") +
  annotation_scale(location = "tr", width_hint = 0.25) +
  annotation_north_arrow(location = "tr", which_north = "true", 
                         pad_x = unit(0.25, "in"), pad_y = unit(0.25, "in"),
                         style = north_arrow_fancy_orienteering) +
  geom_sf(data = count_hp_plot_sf_xwa_tbc, aes(fill = fv ,color=fv)) +
  scale_colour_gradient(low = "grey70",high = "darkred", breaks=c(0,0.5,1),
                        labels=c("0","0.5","1") ,limits=c(0,1),guide = "colourbar", 
                        aesthetics = c("color","fill"),name=addline_format("Estimated probabilty")) + 
  geom_point(data = points_hp_xwa_tbc[points_hp_xwa_tbc$COUNT_c==1,],
             aes(x = coords.x1, y = coords.x2), size = 0.25, 
             shape = 4,color="black", alpha = 0.4) +
  theme(panel.background = element_rect(fill="white"),axis.text=element_text(size=14),
        axis.title=element_text(size=14),
        axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black"))




######## combine maps ##########


grid.arrange(cod_gar_fv,ken_lai_fv,ncol=1)

grid.arrange( ken_tsv_fv,tcd_zak_fv, ncol=1)
             

             xwa_tbc_fv 


################################


###########################
### residuals in space ####
###########################

#cod_gar

cod_gar_res<-ggplot() +
  xlab("Longitude") + ylab("Latitude") +
  labs(title="Congo Garamba")+
  
  annotation_scale(location = "tr", width_hint = 0.25) +
  annotation_north_arrow(location = "tr", which_north = "true", 
                         pad_x = unit(0.25, "in"), pad_y = unit(0.25, "in"),
                         style = north_arrow_fancy_orienteering) +
  
  
  geom_sf(data = count_hp_plot_sf_cod_gar, aes(fill = res ,color= res)) +
  scale_colour_gradient2(low = "red",mid="grey70",high = "blue", 
                         breaks=c(-1,0,1),labels=c("-1","0","1") ,limits=c(-1,1),
                         guide = "colourbar", aesthetics = c("color","fill"),name="Residuals") +
  theme(panel.background = element_rect(fill="white"),axis.text=element_text(size=14),
        axis.title=element_text(size=14),
        axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) 
 
#ken_lai 
ken_lai_res<-ggplot() +
  xlab("Longitude") + ylab("Latitude") +
  labs(title="Kenya Laikipia")+
  
  annotation_scale(location = "tr", width_hint = 0.25) +
  annotation_north_arrow(location = "tr", which_north = "true", 
                         pad_x = unit(0.25, "in"), pad_y = unit(0.25, "in"),
                         style = north_arrow_fancy_orienteering) +
  
  
  geom_sf(data = count_hp_plot_sf_ken_lai, aes(fill = res ,color= res)) +
  scale_colour_gradient2(low = "red",mid="grey70",high = "blue", breaks=c(-1,0,1),
                         labels=c("-1","0","1") ,limits=c(-1,1),guide = "colourbar", 
                         aesthetics = c("color","fill"),name="Residuals")  +
  theme(panel.background = element_rect(fill="white"),axis.text=element_text(size=14),
        axis.title=element_text(size=14),
        axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black"))

#ken_tsv
ken_tsv_res<-ggplot() +
  xlab("Longitude") + ylab("Latitude") +
  labs(title="Kenya Tsavo")+
  
  annotation_scale(location = "tr", width_hint = 0.25) +
  annotation_north_arrow(location = "tr", which_north = "true", 
                         pad_x = unit(0.25, "in"), pad_y = unit(0.25, "in"),
                         style = north_arrow_fancy_orienteering) +
  
  
  geom_sf(data = count_hp_plot_sf_ken_tsv, aes(fill = res ,color= res)) +
  scale_colour_gradient2(low = "red",mid="grey70",high = "blue", breaks=c(-1,0,1),
                         labels=c("-1","0","1") ,limits=c(-1,1),guide = "colourbar", 
                         aesthetics = c("color","fill"),name="Residuals")  +
  theme(panel.background = element_rect(fill="white"),axis.text=element_text(size=14),
        axis.title=element_text(size=14),
        axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black"))


#tcd_zak
tcd_zak_res<-ggplot() +
  xlab("Longitude") + ylab("Latitude") +
  labs(title="Chad Zakouma")+
  
  annotation_scale(location = "tr", width_hint = 0.25) +
  annotation_north_arrow(location = "tr", which_north = "true", 
                         pad_x = unit(0.25, "in"), pad_y = unit(0.25, "in"),
                         style = north_arrow_fancy_orienteering) +
  
  
  geom_sf(data = count_hp_plot_sf_tcd_zak, aes(fill = res ,color= res)) +
  scale_colour_gradient2(low = "red",mid="grey70",high = "blue", breaks=c(-1,0,1),
                         labels=c("-1","0","1") ,limits=c(-1,1),guide = "colourbar", 
                         aesthetics = c("color","fill"),name="Residuals")  +
  theme(panel.background = element_rect(fill="white"),axis.text=element_text(size=14),
        axis.title=element_text(size=14),
        axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black"))


# xwa_tbc

xwa_tbc_res<-ggplot() +
  xlab("Longitude") + ylab("Latitude") +
  labs(title="Burkina Faso, Niger, Benin, Pendjari-Complex")+
  annotation_scale(location = "tr", width_hint = 0.25) +
  annotation_north_arrow(location = "tr", which_north = "true", 
                         pad_x = unit(0.25, "in"), pad_y = unit(0.25, "in"),
                         style = north_arrow_fancy_orienteering) +
  
  
  geom_sf(data = count_hp_plot_sf_xwa_tbc, aes(fill = res ,color= res)) +
  scale_colour_gradient2(low = "red",mid="grey70",high = "blue", breaks=c(-1,0,1),
                         labels=c("-1","0","1") ,limits=c(-1,1),guide = "colourbar", 
                         aesthetics = c("color","fill"),name="Residuals")  +
  theme(panel.background = element_rect(fill="white"),axis.text=element_text(size=14),
        axis.title=element_text(size=14),
        axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black"))



######## combine maps ##########

grid.arrange(cod_gar_res,ken_lai_res, ncol=1)
             
grid.arrange(    ken_tsv_res,       tcd_zak_res, ncol=1 )

xwa_tbc_res


#######################################
############ Effect plots #############
#######################################


##############################
##### get unscaled data ######
##############################

efdat<-read.table("/ma_elephant/paper+lit/R/predictors_car/yxtable_norm2.csv",header=TRUE,sep=",")

efdat$SQRT_DR<-(efdat$SQRT_DR)^2
efdat$SQRT_BS<-(efdat$SQRT_BS)^2
efdat$SQRT_LE<-(efdat$SQRT_LE)^2
efdat$SQRT_TC300<-(efdat$SQRT_TC300)^2
efdat$SQRT_PD<-(efdat$SQRT_PD)^2
efdat$SQRT_SL<-(efdat$SQRT_SL)^2
efdat$SQRT_DW<-(efdat$SQRT_DW)^2
efdat$LC<-as.factor(efdat$LC)
efdat<-merge(efdat, TR, by="ID")
efdat$LOG_TR<-exp(efdat$LOG_TR)
efdat_hp<-efdat[efdat$Site %in% c("COD_GAR","KEN_LAI","KEN_TSV","TCD_ZAK","XWA_TBC"),]

efdat_hp$PIKE[efdat_hp$Site=="COD_GAR"]<-mean(year2014[rownames(year2014) %in% "GAR",])
efdat_hp$PIKE[efdat_hp$Site=="KEN_LAI"]<-mean(year2014[rownames(year2014) %in% "SBR",])
efdat_hp$PIKE[efdat_hp$Site=="KEN_TSV"]<-mean(year2014[rownames(year2014) %in% "TSV",])
efdat_hp$PIKE[efdat_hp$Site=="TCD_ZAK"]<-mean(year2014[rownames(year2014) %in% "ZAK",])
efdat_hp$PIKE[efdat_hp$Site=="XWA_TBC"]<-mean(year2014[rownames(year2014) %in% "PDJ",])


######################## 
##### MODE FUN #########
########################

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}


###############################################################################################
######## Probabilty of living elephants (P) [LE] #####################
###############################################################################################

#create new data.frame with mean/median obs for predictions
nwdtleh<-data.frame(DC=mean(data_c$DC),
                   EL=mean(data_c$EL),
                   NDVI=mean(data_c$NDVI),
                   T=mean(data_c$T),
                   TPA=mean(data_c$TPA),
                   VC=mean(data_c$VC),
                   SQRT_BS=mean(data_c$VC),
                   SQRT_PD=mean(data_c$SQRT_PD),
                   SQRT_LE=seq(from= min(data_c$SQRT_LE), to=max(data_c$SQRT_LE), length.out = 200),
                   SQRT_SL=mean(data_c$SQRT_SL),
                   SQRT_DR=mean(data_c$SQRT_DR),
                   SQRT_TC300=mean(data_c$SQRT_TC300),
                   SQRT_DW=mean(data_c$SQRT_DW),
                   PA=getmode(data_c$PA),
                   LC=getmode(data_c$LC),
                   PIKE=mean(data_c$PIKE),
                   LOG_TR=mean(data_c$LOG_TR)
)

#predict on newdata
LE_fith<-predict(carc_mod_hp, newdata=nwdtleh,type="response", se.fit=TRUE)


#call dataframe to store fitted values, 0.05 CI and 0.95 CI
LEmh<-data.frame(fv=LE_fith$fit, lowCI=NA,upCI=NA,x=NA)


### 0.05 CI
LEmh$lowCI<-LE_fith$fit-(2*LE_fith$se.fit)

### set all values that are negativ to NA
LEmh[LEmh$lowCI<0,]$lowCI <- 0

### 0.95 CI
LEmh$upCI<-LE_fith$fit+(2*LE_fith$se.fit)

### ### set all values greater than one to NA

LEmh[LEmh$upCI>1,]$upCI <- 1


### get unscaled x axis
LEmh$x<-seq(from= min(efdat_hp$SQRT_LE), to=max(efdat_hp$SQRT_LE), length.out = 200)


#### marginal ggplot

LEploth<- ggplot(data=LEmh)+
  ylim(0, 1)+
  xlab("Fitted probability of living elephants (P)")+
  ylab("Estimated probability")+
  geom_ribbon(aes(x=LEmh$x, ymax=LEmh$upCI, ymin=LEmh$lowCI), fill="grey50", alpha=.5) +
  geom_line(aes(x=LEmh$x, y=LEmh$fv),col="grey10")+
  geom_line(aes(x=LEmh$x, y=LEmh$lowCI),linetype = 2, col="grey30")+
  geom_line(aes(x=LEmh$x, y=LEmh$upCI),linetype = 2, col="grey30")+
  geom_rug(data=efdat_hp, aes(x=efdat_hp$SQRT_LE), col="grey50", size=0.1, sides="b")+
  theme(axis.text=element_text(size=18,colour="black"),
        axis.title=element_text(size=16),
        panel.background = element_rect(fill = 'white', colour = 'white'))+
  theme(axis.text=element_text(size=18,colour="black"),
        axis.title=element_text(size=16),
        panel.background = element_rect(fill = 'white', colour = 'white'))


LEploth

#################################################################
####### Distance to nearest Settlement (km) [DC] #################
#################################################################

#create new data.frame with mean/median obs for predictions
nwdtdchp<-data.frame( DC=seq(from= min(data_c$DC), to=max(data_c$DC), length.out = 200),
                   EL=mean(data_c$EL),
                   NDVI=mean(data_c$NDVI),
                   T=mean(data_c$T),
                   TPA=mean(data_c$TPA),
                   VC=mean(data_c$VC),
                   SQRT_BS=mean(data_c$SQRT_BS),
                   SQRT_PD=mean(data_c$SQRT_PD),
                   SQRT_LE=mean(data_c$SQRT_LE),
                   SQRT_SL=mean(data_c$SQRT_SL),
                   SQRT_DR=mean(data_c$SQRT_DR),
                   SQRT_TC300=mean(data_c$SQRT_TC300),
                   SQRT_DW=mean(data_c$SQRT_DW),
                   PA=getmode(data_c$PA),
                   LC=getmode(data_c$LC),
                   PIKE=mean(data_c$PIKE),
                   LOG_TR=mean(data_c$LOG_TR)
)



#predict on newdata
DC_fit_hp<-predict(carc_mod_hp, newdata=nwdtdchp,type="response", se.fit=TRUE)


#call dataframe to store fitted values, 0.05 CI and 0.95 CI
DCmh<-data.frame(fv=DC_fit_hp$fit, lowCI=NA,upCI=NA, x=NA)



### 0.05 CI
DCmh$lowCI<-DC_fit_hp$fit-(2*DC_fit_hp$se.fit)

### set all values that are negativ to NA
DCmh[DCmh$lowCI<0,]$lowCI <- 0

### 0.95 CI
DCmh$upCI<-DC_fit_hp$fit+(2*DC_fit_hp$se.fit)

### ### set all values greater than one to NA

DCmh[DCmh$upCI>1,]$upCI <- 1


### get unscaled x axis
DCmh$x<-seq(from= min(efdat_hp$DC)/1000, to=max(efdat_hp$DC)/1000, length.out = 200)


#### effect ggplot
DCplothp<- ggplot(data=DCmh)+
  ylim(0, 1)+
  xlab("Distance to nearest settlement (km)")+
  ylab("Estimated probability")+
  geom_ribbon(aes(x=DCmh$x, ymax=DCmh$upCI, ymin=DCmh$lowCI), fill="grey50", alpha=.5) +
  geom_line(aes(x=DCmh$x, y=DCmh$fv),col="grey10")+
  geom_line(aes(x=DCmh$x, y=DCmh$lowCI),linetype = 2, col="grey30")+
  geom_line(aes(x=DCmh$x, y=DCmh$upCI),linetype = 2, col="grey30")+
  geom_rug(data=efdat_hp, aes(x=(efdat_hp$DC/1000)), col="grey50", size=0.1, sides="b")+
  theme(axis.text=element_text(size=18,colour="black"),
        axis.title=element_text(size=16),
        panel.background = element_rect(fill = 'white', colour = 'white'))

DCplothp



##############################################
####### Elevation [m a.s.l.] (EL) ############
##############################################

#create new data.frame with mean/median obs for predictions
nwdtelhp<-data.frame( DC=mean(data_c$DC),
                   EL=seq(from= min(data_c$EL), to=max(data_c$EL), length.out = 200),
                   NDVI=mean(data_c$NDVI),
                   T=mean(data_c$T),
                   TPA=mean(data_c$TPA),
                   VC=mean(data_c$VC),
                   SQRT_BS=mean(data_c$SQRT_BS),
                   SQRT_PD=mean(data_c$SQRT_PD),
                   SQRT_LE=mean(data_c$SQRT_LE),
                   SQRT_SL=mean(data_c$SQRT_SL),
                   SQRT_DR=mean(data_c$SQRT_DR),
                   SQRT_TC300=mean(data_c$SQRT_TC300),
                   SQRT_DW=mean(data_c$SQRT_DW),
                   PA=getmode(data_c$PA),
                   LC=getmode(data_c$LC),
                   PIKE=mean(data_c$PIKE),
                   LOG_TR=mean(data_c$LOG_TR)
)

#predict on newdata
EL_fit_hp<-predict(carc_mod_hp, newdata=nwdtelhp,type="response", se.fit=TRUE)


#call dataframe to store fitted values, 0.05 CI and 0.95 CI
ELmp<-data.frame(fv=EL_fit_hp$fit, lowCI=NA,upCI=NA, x=NA)



### 0.05 CI
ELmp$lowCI<-EL_fit_hp$fit-(2*EL_fit_hp$se.fit)

### set all values that are negativ to NA
ELmp[ELmp$lowCI<0,]$lowCI <- 0

### 0.95 CI
ELmp$upCI<-EL_fit_hp$fit+(2*EL_fit_hp$se.fit)

### ### set all values greater than one to NA

ELmp[ELmp$upCI>1,]$upCI <- 1


### get unscaled x axis
ELmp$x<-seq(from= min(efdat_hp$EL), to=max(efdat_hp$EL), length.out = 200)


#### effect ggplot
ELploth<- ggplot(data=ELmp)+
  ylim(0, 1)+
  xlab("Elevation (m a.s.l.)")+
  ylab("Estimated probability")+
  geom_ribbon(aes(x=ELmp$x, ymax=ELmp$upCI, ymin=ELmp$lowCI), fill="grey50", alpha=.5) +
  geom_line(aes(x=ELmp$x, y=ELmp$fv),col="grey10")+
  geom_line(aes(x=ELmp$x, y=ELmp$lowCI),linetype = 2, col="grey30")+
  geom_line(aes(x=ELmp$x, y=ELmp$upCI),linetype = 2, col="grey30")+
  geom_rug(data=efdat_hp, aes(x=efdat_hp$EL), col="grey30", size=0.1, sides="b")+
  theme(axis.text=element_text(size=18,colour="black"),
        axis.title=element_text(size=16),
        panel.background = element_rect(fill = 'white', colour = 'white'))

ELploth


hist(ELmp$fv)
##########################################################################################
######## Normalized Difference Vegetation Index (NDVI) ###################################
##########################################################################################

#create new data.frame with mean obs for predictions
nwdtndvih<-data.frame(DC=mean(data_c$DC),
                     EL=mean(data_c$EL),
                     NDVI=seq(from= min(data_c$NDVI), to=max(data_c$NDVI), length.out = 200),
                     T=mean(data_c$T),
                     TD=mean(data_c$TD),
                     TP4M=mean(data_c$TP4M),
                     TPA=mean(data_c$TPA),
                     VC=mean(data_c$VC),
                     SQRT_BS=mean(data_c$SQRT_BS),
                     SQRT_PD=mean(data_c$SQRT_PD),
                     SQRT_LE=mean(data_c$SQRT_LE),
                     SQRT_SL=mean(data_c$SQRT_SL),
                     SQRT_DR=mean(data_c$SQRT_DR),
                     SQRT_TC300=mean(data_c$SQRT_TC300),
                     SQRT_DW=mean(data_c$SQRT_DW),
                     PA=getmode(data_c$PA),
                     LC=getmode(data_c$LC),
                     PIKE=mean(data_c$PIKE),
                     LOG_TR=mean(data_c$LOG_TR)
)


#predict on newdata
NDVI_fith<-predict(carc_mod_hp, newdata=nwdtndvih,type="response", se.fit=TRUE)


#call dataframe to store fitted values, 0.05 CI and 0.95 CI
NDVImh<-data.frame(fv=NDVI_fith$fit, lowCI=NA,upCI=NA,x=NA)



### 0.05 CI
NDVImh$lowCI<-NDVI_fith$fit-(2*NDVI_fith$se.fit)

### set all values that are negativ to NA
NDVImh[NDVImh$lowCI<0,]$lowCI <- 0

### 0.95 CI
NDVImh$upCI<-NDVI_fith$fit+(2*NDVI_fith$se.fit)

### ### set all values greater than one to NA

NDVImh[NDVImh$upCI>1,]$upCI <- 1


### get unscaled x axis
NDVImh$x<-seq(from= min(efdat_hp$NDVI), to=max(efdat_hp$NDVI), length.out = 200)




#### effect ggplot
NDVIploth<- ggplot(data=NDVImh)+
  ylim(0, 1)+
  xlab("NDVI")+
  ylab("Estimated probability")+
  geom_ribbon(aes(x=NDVImh$x, ymax=NDVImh$upCI, ymin=NDVImh$lowCI), fill="grey50", alpha=.5) +
  geom_line(aes(x=NDVImh$x, y=NDVImh$fv),col="grey10")+
  geom_line(aes(x=NDVImh$x, y=NDVImh$lowCI),linetype = 2, col="grey30")+
  geom_line(aes(x=NDVImh$x, y=NDVImh$upCI),linetype = 2, col="grey30")+
  geom_rug(data=efdat_hp, aes(x=efdat_hp$NDVI), col="grey30", size=0.1, sides="b")+
  theme(axis.text=element_text(size=18,colour="black"),
        axis.title=element_text(size=16),
        panel.background = element_rect(fill = 'white', colour = 'white'))

NDVIploth


##########################################################################################
######## annual mean surface air temperature (°C) [T] ####################################
##########################################################################################

#create new data.frame with mean/median obs for  predictions
nwdtth<-data.frame( DC=mean(data_c$DC),
                  EL=mean(data_c$EL),
                  NDVI=mean(data_c$NDVI),
                  T=seq(from= min(data_c$T), to=max(data_c$T), length.out = 200),
                  TD=mean(data_c$TD),
                  TP4M=mean(data_c$TP4M),
                  TPA=mean(data_c$TPA),
                  VC=mean(data_c$VC),
                  SQRT_BS=mean(data_c$SQRT_BS),
                  SQRT_PD=mean(data_c$SQRT_PD),
                  SQRT_LE=mean(data_c$SQRT_LE),
                  SQRT_SL=mean(data_c$SQRT_SL),
                  SQRT_DR=mean(data_c$SQRT_DR),
                  SQRT_TC300=mean(data_c$SQRT_TC300),
                  SQRT_DW=mean(data_c$SQRT_DW),
                  PA=getmode(data_c$PA),
                  LC=getmode(data_c$LC),
                  PIKE=mean(data_c$PIKE),
                  LOG_TR=mean(data_c$LOG_TR)
                  
)

#predict on newdata
T_fith<-predict(carc_mod_hp, newdata=nwdtth,type="response", se.fit=TRUE)


#call dataframe to store fitted values, 0.05 CI and 0.95 CI
Tmh<-data.frame(fv=T_fith$fit, lowCI=NA,upCI=NA,x=NA)


summary(carc_mod_hp)
### 0.05 CI
Tmh$lowCI<-T_fith$fit-(2*T_fith$se.fit)

### set all values that are negativ to NA
Tmh[Tmh$lowCI<0,]$lowCI <- 0

### 0.95 CI
Tmh$upCI<-T_fith$fit+(2*T_fith$se.fit)

### ### set all values greater than one to NA

Tmh[Tmh$upCI>1,]$upCI <- 1


### get unscaled x axis
Tmh$x<-seq(from= min(efdat_hp$T), to=max(efdat_hp$T), length.out = 200)





#### effect ggplot
Tploth<- ggplot(data=Tmh)+
  ylim(0, 1)+
  xlab("Annual mean surface air temperature (°C) ")+
  ylab("Estimated probability")+
  geom_ribbon(aes(x=Tmh$x, ymax=Tmh$upCI, ymin=Tmh$lowCI), fill="grey50", alpha=.5) +
  geom_line(aes(x=Tmh$x, y=Tmh$fv),col="grey10")+
  geom_line(aes(x=Tmh$x, y=Tmh$lowCI),linetype = 2, col="grey30")+
  geom_line(aes(x=Tmh$x, y=Tmh$upCI),linetype = 2, col="grey30")+
  geom_rug(data=efdat_hp, aes(x=efdat_hp$T), col="grey50", size=0.1, sides="b")+
  theme(axis.text=element_text(size=18,colour="black"),
        axis.title=element_text(size=16),
        panel.background = element_rect(fill = 'white', colour = 'white'))

Tploth


####################################################################
######## Total annual precipitation (mm) [TPA] #####################
####################################################################

#create new data.frame with mean/median obs for predictions
nwdttpah<-data.frame( DC=mean(data_c$DC),
                    EL=mean(data_c$EL),
                    NDVI=mean(data_c$NDVI),
                    T=mean(data_c$T),
                    TD=mean(data_c$TD),
                    TP4M=mean(data_c$TP4M),
                    TPA=seq(from= min(data_c$TPA), to=max(data_c$TPA), length.out = 200),
                    VC=mean(data_c$VC),
                    SQRT_BS=mean(data_c$SQRT_BS),
                    SQRT_PD=mean(data_c$SQRT_PD),
                    SQRT_LE=mean(data_c$SQRT_LE),
                    SQRT_SL=mean(data_c$SQRT_SL),
                    SQRT_DR=mean(data_c$SQRT_DR),
                    SQRT_TC300=mean(data_c$SQRT_TC300),
                    SQRT_DW=mean(data_c$SQRT_DW),
                    PA=getmode(data_c$PA),
                    LC=getmode(data_c$LC),
                    PIKE=mean(data_c$PIKE),
                    LOG_TR=mean(data_c$LOG_TR)
)

#predict on newdata
TPA_fith<-predict(carc_mod_hp, newdata=nwdttpah,type="response", se.fit=TRUE)


#call dataframe to store fitted values, 0.05 CI and 0.95 CI
TPAmh<-data.frame(fv=TPA_fith$fit, lowCI=NA,upCI=NA, x=NA)



### 0.05 CI
TPAmh$lowCI<-TPA_fith$fit-(2*TPA_fith$se.fit)

### set all values that are negativ to NA
TPAmh[TPAmh$lowCI<0,]$lowCI <- 0

### 0.95 CI
TPAmh$upCI<-TPA_fith$fit+(2*TPA_fith$se.fit)

### ### set all values greater than one to NA

TPAmh[TPAmh$upCI>1,]$upCI <- 1


### get unscaled x axis
TPAmh$x<-seq(from= min(efdat_hp$TPA), to=max(efdat_hp$TPA), length.out = 200)



#### effect ggplot

TPAploth<- ggplot(data=TPAmh)+
  ylim(0, 1)+
  xlab("Total annual precipitation (mm)")+
  ylab("Estimated probability")+
  geom_ribbon(aes(x=TPAmh$x, ymax=TPAmh$upCI, ymin=TPAmh$lowCI), fill="grey50", alpha=.5) +
  geom_line(aes(x=TPAmh$x, y=TPAmh$fv),col="grey10")+
  geom_line(aes(x=TPAmh$x, y=TPAmh$lowCI),linetype = 2, col="grey30")+
  geom_line(aes(x=TPAmh$x, y=TPAmh$upCI),linetype = 2, col="grey30")+
  geom_rug(data=efdat_hp, aes(x=efdat_hp$TPA), col="grey50", size=0.1, sides="b")+
  theme(axis.text=element_text(size=18,colour="black"),
        axis.title=element_text(size=16),
        panel.background = element_rect(fill = 'white', colour = 'white'))

TPAploth


#############################################################################################
######## Percentage of ground covered by herbaceous vegetation [%] (VC) #####################
#############################################################################################

#create new data.frame with mean/median obs for predictions
nwdtvch<-data.frame( DC=mean(data_c$DC),
                   EL=mean(data_c$EL),
                   NDVI=mean(data_c$NDVI),
                   T=mean(data_c$T),
                   TPA=mean(data_c$TPA),
                   VC=seq(from= min(data_c$VC), to=max(data_c$VC), length.out = 200),
                   SQRT_BS=mean(data_c$SQRT_BS),
                   SQRT_PD=mean(data_c$SQRT_PD),
                   SQRT_LE=mean(data_c$SQRT_LE),
                   SQRT_SL=mean(data_c$SQRT_SL),
                   SQRT_DR=mean(data_c$SQRT_DR),
                   SQRT_TC300=mean(data_c$SQRT_TC300),
                   SQRT_DW=mean(data_c$SQRT_DW),
                   PA=getmode(data_c$PA),
                   LC=getmode(data_c$LC),
                   PIKE=mean(data_c$PIKE),
                   LOG_TR=mean(data_c$LOG_TR)
)

#predict on newdata
VC_fith<-predict(carc_mod_hp, newdata=nwdtvch,type="response", se.fit=TRUE)


#call dataframe to store fitted values, 0.05 CI and 0.95 CI
VCmh<-data.frame(fv=VC_fith$fit, lowCI=NA,upCI=NA,X=NA)



### 0.05 CI
VCmh$lowCI<-VC_fith$fit-(2*VC_fith$se.fit)

### set all values that are negativ to NA
VCmh[VCmh$lowCI<0,]$lowCI <- 0

### 0.95 CI
VCmh$upCI<-VC_fith$fit+(2*VC_fith$se.fit)

### ### set all values greater than one to NA

VCmh[VCmh$upCI>1,]$upCI <- 1


### get unscaled x axis
VCmh$x<-seq(from= min(efdat_hp$VC), to=max(efdat_hp$VC), length.out = 200)




#### effect ggplot

VCploth<- ggplot(data=VCmh)+
  ylim(0, 1)+
  xlab("Percentage of herbaceous vegetation")+
  ylab("Estimated probability")+
  geom_ribbon(aes(x=VCmh$x, ymax=VCmh$upCI, ymin=VCmh$lowCI), fill="grey50", alpha=.5) +
  geom_line(aes(x=VCmh$x, y=VCmh$fv),col="grey10")+
  geom_line(aes(x=VCmh$x, y=VCmh$lowCI),linetype = 2, col="grey30")+
  geom_line(aes(x=VCmh$x, y=VCmh$upCI),linetype = 2, col="grey30")+
  geom_rug(data=efdat_hp, aes(x=efdat_hp$VC), col="grey50", size=0.1, sides="b")+
  theme(axis.text=element_text(size=18,colour="black"),
        axis.title=element_text(size=16),
        panel.background = element_rect(fill = 'white', colour = 'white'))

VCploth

###############################################################################################
######## Percentage of ground covered by bare soil (%) [BS] ###################################
###############################################################################################

#create new data.frame with mean/median obs for  predictions
nwdtbsh<-data.frame(DC=mean(data_c$DC),
                   EL=mean(data_c$EL),
                   NDVI=mean(data_c$NDVI),
                   T=mean(data_c$T),
                   TPA=mean(data_c$TPA),
                   VC=mean(data_c$VC),
                   SQRT_BS=seq(from= min(data_c$SQRT_BS), to=max(data_c$SQRT_BS), length.out = 200),
                   SQRT_PD=mean(data_c$SQRT_PD),
                   SQRT_LE=mean(data_c$SQRT_LE),
                   SQRT_SL=mean(data_c$SQRT_SL),
                   SQRT_DR=mean(data_c$SQRT_DR),
                   SQRT_TC300=mean(data_c$SQRT_TC300),
                   SQRT_DW=mean(data_c$SQRT_DW),
                   PA=getmode(data_c$PA),
                   LC=getmode(data_c$LC),
                   PIKE=mean(data_c$PIKE),
                   LOG_TR=mean(data_c$LOG_TR)
)

#predict on newdata
BS_fith<-predict(carc_mod_hp, newdata=nwdtbsh,type="response", se.fit=TRUE)


#call dataframe to store fitted values, 0.05 CI and 0.95 CI
BSmh<-data.frame(fv=BS_fith$fit, lowCI=NA,upCI=NA)



### 0.05 CI
BSmh$lowCI<-BS_fith$fit-(2*BS_fith$se.fit)

### set all values that are negativ to NA
BSmh[BSmh$lowCI<0,]$lowCI <- 0

### 0.95 CI
BSmh$upCI<-BS_fith$fit+(2*BS_fith$se.fit)

### ### set all values greater than one to NA

BSmh[BSmh$upCI>1,]$upCI <- 1


### get unscaled x axis
BSmh$x<-seq(from= min(efdat_hp$SQRT_BS), to=max(efdat_hp$SQRT_BS), length.out = 200)


#### effect ggplot

BSploth<- ggplot(data=BSmh)+
  ylim(0, 1)+
  xlab("Percentage of bare soil")+
  ylab("Estimated probability")+
  geom_ribbon(aes(x=BSmh$x, ymax=BSmh$upCI, ymin=BSmh$lowCI), fill="grey50", alpha=.5) +
  geom_line(aes(x=BSmh$x, y=BSmh$fv),col="grey10")+
  geom_line(aes(x=BSmh$x, y=BSmh$lowCI),linetype = 2, col="grey30")+
  geom_line(aes(x=BSmh$x, y=BSmh$upCI),linetype = 2, col="grey30")+
  geom_rug(data=efdat_hp, aes(x=efdat_hp$SQRT_BS), col="grey50", size=0.1, sides="b")+
  theme(axis.text=element_text(size=18,colour="black"),
        axis.title=element_text(size=16),
        panel.background = element_rect(fill = 'white', colour = 'white'))

BSploth

############################################################################
######## Population density (inhabitants per km²) [PD] #####################
############################################################################

#create new data.frame with mean/median obs for predictions
nwdtpbh<-data.frame(DC=mean(data_c$DC),
                   EL=mean(data_c$EL),
                   NDVI=mean(data_c$NDVI),
                   T=mean(data_c$T),
                   TPA=mean(data_c$TPA),
                   VC=mean(data_c$VC),
                   SQRT_BS=mean(data_c$VC),
                   SQRT_PD=seq(from= min(data_c$SQRT_PD), to=max(data_c$SQRT_PD), length.out = 200),
                   SQRT_LE=mean(data_c$SQRT_LE),
                   SQRT_SL=mean(data_c$SQRT_SL),
                   SQRT_DR=mean(data_c$SQRT_DR),
                   SQRT_TC300=mean(data_c$SQRT_TC300),
                   SQRT_DW=mean(data_c$SQRT_DW),
                   PA=getmode(data_c$PA),
                   LC=getmode(data_c$LC),
                   PIKE=mean(data_c$PIKE),
                   LOG_TR=mean(data_c$LOG_TR)
)

#predict on newdata
PD_fith<-predict(carc_mod_hp, newdata=nwdtpbh,type="response", se.fit=TRUE)


#call dataframe to store fitted values, 0.05 CI and 0.95 CI
PDmh<-data.frame(fv=PD_fith$fit, lowCI=NA,upCI=NA,x=NA)



### 0.05 CI
PDmh$lowCI<-PD_fith$fit-(2*PD_fith$se.fit)

### set all values that are negativ to NA
PDmh[PDmh$lowCI<0,]$lowCI <- 0

### 0.95 CI
PDmh$upCI<-PD_fith$fit+(2*PD_fith$se.fit)

### ### set all values greater than one to NA

PDmh[PDmh$upCI>1,]$upCI <- 1


### get unscaled x axis
PDmh$x<-seq(from= min(efdat_hp$SQRT_PD), to=max(efdat_hp$SQRT_PD), length.out = 200)

#### effect ggplot

PDploth<- ggplot(data=PDmh)+
  ylim(0, 1)+
  xlab("Population density (Inhabitants per km²)")+
  ylab("Estimated probability")+
  geom_ribbon(aes(x=PDmh$x, ymax=PDmh$upCI, ymin=PDmh$lowCI), fill="grey50", alpha=.5) +
  geom_line(aes(x=PDmh$x, y=PDmh$fv),col="grey10")+
  geom_line(aes(x=PDmh$x, y=PDmh$lowCI),linetype = 2, col="grey30")+
  geom_line(aes(x=PDmh$x, y=PDmh$upCI),linetype = 2, col="grey30")+
  geom_rug(data=efdat_hp, aes(x=efdat_hp$SQRT_PD), col="grey50", size=0.1, sides="b")+
  theme(axis.text=element_text(size=18,colour="black"),
        axis.title=element_text(size=16),
        panel.background = element_rect(fill = 'white', colour = 'white'))

PDploth



#############################################
######## Slope (°) [SL] #####################
#############################################

#create new data.frame with mean/median obs for  predictions
nwdtslh<-data.frame( DC=mean(data_c$DC),
                   EL=mean(data_c$EL),
                   NDVI=mean(data_c$NDVI),
                   T=mean(data_c$T),
                   TPA=mean(data_c$TPA),
                   VC=mean(data_c$VC),
                   SQRT_BS=mean(data_c$VC),
                   SQRT_PD=mean(data_c$SQRT_PD),
                   SQRT_LE=mean(data_c$SQRT_LE),
                   SQRT_SL=seq(from= min(data_c$SQRT_SL), to=max(data_c$SQRT_SL), length.out = 200),
                   SQRT_DR=mean(data_c$SQRT_DR),
                   SQRT_TC300=mean(data_c$SQRT_TC300),
                   SQRT_DW=mean(data_c$SQRT_DW),
                   PA=getmode(data_c$PA),
                   LC=getmode(data_c$LC),
                   PIKE=mean(data_c$PIKE),
                   LOG_TR=mean(data_c$LOG_TR)
)

#predict on newdata
SL_fith<-predict(carc_mod_hp, newdata=nwdtslh,type="response", se.fit=TRUE)


#call dataframe to store fitted values, 0.05 CI and 0.95 CI
SLmh<-data.frame(fv=SL_fith$fit, lowCI=NA,upCI=NA,x=NA)



### 0.05 CI
SLmh$lowCI<-SL_fith$fit-(2*SL_fith$se.fit)

### set all values that are negativ to NA
SLmh[SLmh$lowCI<0,]$lowCI <- 0

### 0.95 CI
SLmh$upCI<-SL_fith$fit+(2*SL_fith$se.fit)

### ### set all values greater than one to NA

SLmh[SLmh$upCI>1,]$upCI <- 1


### get unscaled x axis
SLmh$x<-seq(from= min(efdat_hp$SQRT_SL), to=max(efdat_hp$SQRT_SL), length.out = 200)


#### effect ggplot ####

SLploth<- ggplot(data=SLmh)+
  ylim(0, 1)+
  xlab("Slope (°) ")+
  ylab("Estimated probability")+
  geom_ribbon(aes(x=SLmh$x, ymax=SLmh$upCI, ymin=SLmh$lowCI), fill="grey50", alpha=.5) +
  geom_line(aes(x=SLmh$x, y=SLmh$fv),col="grey10")+
  geom_line(aes(x=SLmh$x, y=SLmh$lowCI),linetype = 2, col="grey30")+
  geom_line(aes(x=SLmh$x, y=SLmh$upCI),linetype = 2, col="grey30")+
  geom_rug(data=efdat_hp, aes(x=efdat_hp$SQRT_SL), col="grey50", size=0.1, sides="b")+
  theme(axis.text=element_text(size=18,colour="black"),
        axis.title=element_text(size=16),
        panel.background = element_rect(fill = 'white', colour = 'white'))

SLploth

################################################################
######## Distance to nearest road [m] (DR) #####################
################################################################

#create new data.frame with mean/median obs for  predictions
nwdtdrh<-data.frame(DC=mean(data_c$DC),
                   EL=mean(data_c$EL),
                   NDVI=mean(data_c$NDVI),
                   T=mean(data_c$T),
                   TPA=mean(data_c$TPA),
                   VC=mean(data_c$VC),
                   SQRT_BS=mean(data_c$VC),
                   SQRT_PD=mean(data_c$SQRT_PD),
                   SQRT_LE=mean(data_c$SQRT_LE),
                   SQRT_SL=mean(data_c$SQRT_SL),
                   SQRT_DR=seq(from= min(data_c$SQRT_DR), to=max(data_c$SQRT_DR), length.out = 200),
                   SQRT_TC300=mean(data_c$SQRT_TC300),
                   SQRT_DW=mean(data_c$SQRT_DW),
                   PA=getmode(data_c$PA),
                   LC=getmode(data_c$LC),
                   PIKE=mean(data_c$PIKE),
                   LOG_TR=mean(data_c$LOG_TR)
)

#predict on newdata
DR_fith<-predict(carc_mod_hp, newdata=nwdtdrh,type="response", se.fit=TRUE)


#call dataframe to store fitted values, 0.05 CI and 0.95 CI
DRmh<-data.frame(fv=DR_fith$fit, lowCI=NA,upCI=NA,x=NA)



### 0.05 CI
DRmh$lowCI<-DR_fith$fit-(2*DR_fith$se.fit)

### set all values that are negativ to NA
DRmh[DRmh$lowCI<0,]$lowCI <- 0

### 0.95 CI
DRmh$upCI<-DR_fith$fit+(2*DR_fith$se.fit)

### ### set all values greater than one to NA

DRmh[DRmh$upCI>1,]$upCI <- 1


### get unscaled x axis
DRmh$x<-seq(from= min(efdat_hp$SQRT_DR)/1000, to=max(efdat_hp$SQRT_DR)/1000, length.out = 200)


#### effect ggplot ####

DRploth<- ggplot(data=DRmh)+
  ylim(0, 1)+
  xlab("Distance to nearest Road (km)")+
  ylab("Estimated probability")+
  geom_ribbon(aes(x=DRmh$x, ymax=DRmh$upCI, ymin=DRmh$lowCI), fill="grey50", alpha=.5) +
  geom_line(aes(x=DRmh$x, y=DRmh$fv),col="grey10")+
  geom_line(aes(x=DRmh$x, y=DRmh$lowCI),linetype = 2, col="grey30")+
  geom_line(aes(x=DRmh$x, y=DRmh$upCI),linetype = 2, col="grey30")+
  geom_rug(data=efdat_hp, aes((x=efdat_hp$SQRT_DR/1000)), col="grey50", size=0.1, sides="b")+
  theme(axis.text=element_text(size=18,colour="black"),
        axis.title=element_text(size=16),
        panel.background = element_rect(fill = 'white', colour = 'white'))

DRploth

################################################################
######## Treecover [%] (TC300) #####################
################################################################

#create new data.frame with mean/median obs for predictions
nwdttch<-data.frame(DC=mean(data_c$DC),
                   EL=mean(data_c$EL),
                   NDVI=mean(data_c$NDVI),
                   T=mean(data_c$T),
                   TPA=mean(data_c$TPA),
                   VC=mean(data_c$VC),
                   SQRT_BS=mean(data_c$VC),
                   SQRT_PD=mean(data_c$SQRT_PD),
                   SQRT_LE=mean(data_c$SQRT_LE),
                   SQRT_SL=mean(data_c$SQRT_SL),
                   SQRT_DR=mean(data_c$SQRT_DR),
                   SQRT_TC300=seq(from= min(data_c$SQRT_TC300), to=max(data_c$SQRT_TC300), length.out = 200),
                   SQRT_DW=mean(data_c$SQRT_DW),
                   PA=getmode(data_c$PA),
                   LC=getmode(data_c$LC),
                   PIKE=mean(data_c$PIKE),
                   LOG_TR=mean(data_c$LOG_TR)
)

#predict on newdata
TC_fith<-predict(carc_mod_hp, newdata=nwdttch,type="response", se.fit=TRUE)


#call dataframe to store fitted values, 0.05 CI and 0.95 CI
TCmh<-data.frame(fv=TC_fith$fit, lowCI=NA,upCI=NA,x=NA)



### 0.05 CI
TCmh$lowCI<-TC_fith$fit-(2*TC_fith$se.fit)

### set all values that are negativ to NA
TCmh[TCmh$lowCI<0,]$lowCI <- 0

### 0.95 CI
TCmh$upCI<-TC_fith$fit+(2*TC_fith$se.fit)

### ### set all values greater than one to NA

TCmh[TCmh$upCI>1,]$upCI <- 1


### get unscaled x axis
TCmh$x<-seq(from= min(efdat_hp$SQRT_TC300), to=max(efdat_hp$SQRT_TC300), length.out = 200)



#### effect ggplot ####

TCploth<- ggplot(data=TCmh)+
  ylim(0, 1)+
  xlab("Percentage of tree canopy")+
  ylab("Estimated probability")+
  geom_ribbon(aes(x=TCmh$x, ymax=TCmh$upCI, ymin=TCmh$lowCI), fill="grey50", alpha=.5) +
  geom_line(aes(x=TCmh$x, y=TCmh$fv),col="grey10")+
  geom_line(aes(x=TCmh$x, y=TCmh$lowCI),linetype = 2, col="grey30")+
  geom_line(aes(x=TCmh$x, y=TCmh$upCI),linetype = 2, col="grey30")+
  geom_rug(data=efdat_hp, aes(x=efdat_hp$SQRT_TC300), col="grey50", size=0.1, sides="b")+
  theme(axis.text=element_text(size=18,colour="black"),
        axis.title=element_text(size=16),
        panel.background = element_rect(fill = 'white', colour = 'white'))

TCploth




###############################################################################
######## Distance to nearest waterbody (m)  (DW) ####################
###############################################################################

nwdtdwh<-data.frame( DC=mean(data_c$DC),
                   EL=mean(data_c$EL),
                   NDVI=mean(data_c$NDVI),
                   T=mean(data_c$T),
                   TPA=mean(data_c$TPA),
                   VC=mean(data_c$VC),
                   SQRT_BS=mean(data_c$VC),
                   SQRT_PD=mean(data_c$SQRT_PD),
                   SQRT_LE=mean(data_c$SQRT_LE),
                   SQRT_SL=mean(data_c$SQRT_SL),
                   SQRT_DR=mean(data_c$SQRT_DR),
                   SQRT_TC300=mean(data_c$SQRT_TC300),
                   SQRT_DW=seq(from= min(data_c$SQRT_DW), to=max(data_c$SQRT_DW), length.out = 200),
                   PA=getmode(data_c$PA),
                   LC=getmode(data_c$LC),
                   PIKE=mean(data_c$PIKE),
                   LOG_TR=mean(data_c$LOG_TR)
)

#predict on newdata
DW_fith<-predict(carc_mod_hp, newdata=nwdtdwh,type="response", se.fit=TRUE)


#call dataframe to store fitted values, 0.05 CI and 0.95 CI
DWmh<-data.frame(fv=DW_fith$fit, lowCI=NA,upCI=NA,x=NA)



### 0.05 CI
DWmh$lowCI<-DW_fith$fit-(2*DW_fith$se.fit)

### set all values that are negativ to NA
DWmh[DWmh$lowCI<0,]$lowCI <- 0

### 0.95 CI
DWmh$upCI<-DW_fith$fit+(2*DW_fith$se.fit)

### ### set all values greater than one to NA

DWmh[DWmh$upCI>1,]$upCI <- 1


### get unscaled x axis
DWmh$x<-seq(from= min(efdat_hp$SQRT_DW), to=max(efdat_hp$SQRT_DW), length.out = 200)


#### effect ggplot ####

DWploth<- ggplot(data=DWmh)+
  ylim(0, 1)+
  xlab("Distance to nearest waterbody (km)")+
  ylab("Estimated probability")+
  geom_ribbon(aes(x=DWmh$x, ymax=DWmh$upCI, ymin=DWmh$lowCI), fill="grey50", alpha=.5) +
  geom_line(aes(x=DWmh$x, y=DWmh$fv),col="grey10")+
  geom_line(aes(x=DWmh$x, y=DWmh$lowCI),linetype = 2, col="grey30")+
  geom_line(aes(x=DWmh$x, y=DWmh$upCI),linetype = 2, col="grey30")+
  geom_rug(data=efdat_hp, aes(x=efdat_hp$SQRT_DW), col="grey50", size=0.1, sides="b")+
  theme(axis.text=element_text(size=18,colour="black"),
        axis.title=element_text(size=16),
        panel.background = element_rect(fill = 'white', colour = 'white'))

DWploth


##########################################################
######## Walk time to roads (h)  [TC] ####################
##########################################################

nwdtwth<-data.frame( DC=mean(data_c$DC),
                    EL=mean(data_c$EL),
                    NDVI=mean(data_c$NDVI),
                    T=mean(data_c$T),
                    TPA=mean(data_c$TPA),
                    VC=mean(data_c$VC),
                    SQRT_BS=mean(data_c$VC),
                    SQRT_PD=mean(data_c$SQRT_PD),
                    SQRT_LE=mean(data_c$SQRT_LE),
                    SQRT_SL=mean(data_c$SQRT_SL),
                    SQRT_DR=mean(data_c$SQRT_DR),
                    SQRT_TC300=mean(data_c$SQRT_TC300),
                    SQRT_DW=mean(data_c$SQRT_DW),
                    PA=getmode(data_c$PA),
                    LC=getmode(data_c$LC),
                    PIKE=mean(data_c$PIKE),
                    LOG_TR=seq(from= min(data_c$LOG_TR), to=max(data_c$LOG_TR), length.out = 200)
)

#predict on newdata
WT_fith<-predict(carc_mod_hp, newdata=nwdtwth,type="response", se.fit=TRUE)


#call dataframe to store fitted values, 0.05 CI and 0.95 CI
WTmh<-data.frame(fv=WT_fith$fit, lowCI=NA,upCI=NA,x=NA)



### 0.05 CI
WTmh$lowCI<-WT_fith$fit-(2*WT_fith$se.fit)

### set all values that are negativ to NA
WTmh[WTmh$lowCI<0,]$lowCI <- 0

### 0.95 CI
WTmh$upCI<-WT_fith$fit+(2*WT_fith$se.fit)

### ### set all values greater than one to NA

WTmh[WTmh$upCI>1,]$upCI <- 1


### get unscaled x axis
WTmh$x<-seq(from= min(efdat_hp$LOG_TR), to=max(efdat_hp$LOG_TR), length.out = 200)


#### effect ggplot ####

WTploth<- ggplot(data=WTmh)+
  ylim(0, 1)+
  xlab("Walk time to roads (h)")+
  ylab("Estimated probability")+
  geom_ribbon(aes(x=WTmh$x, ymax=WTmh$upCI, ymin=WTmh$lowCI), fill="grey50", alpha=.5) +
  geom_line(aes(x=WTmh$x, y=WTmh$fv),col="grey10")+
  geom_line(aes(x=WTmh$x, y=WTmh$lowCI),linetype = 2, col="grey30")+
  geom_line(aes(x=WTmh$x, y=WTmh$upCI),linetype = 2, col="grey30")+
  geom_rug(data=efdat_hp, aes(x=efdat_hp$LOG_TR), col="grey50", size=0.1, sides="b")+
  theme(axis.text=element_text(size=18,colour="black"),
        axis.title=element_text(size=16),
        panel.background = element_rect(fill = 'white', colour = 'white'))

WTploth


######################################
######## PIKE (%) ####################
######################################

nwdtpih<-data.frame(DC=mean(data_c$DC),
                    EL=mean(data_c$EL),
                    NDVI=mean(data_c$NDVI),
                    T=mean(data_c$T),
                    TPA=mean(data_c$TPA),
                    VC=mean(data_c$VC),
                    SQRT_BS=mean(data_c$VC),
                    SQRT_PD=mean(data_c$SQRT_PD),
                    SQRT_LE=mean(data_c$SQRT_LE),
                    SQRT_SL=mean(data_c$SQRT_SL),
                    SQRT_DR=mean(data_c$SQRT_DR),
                    SQRT_TC300=mean(data_c$SQRT_TC300),
                    SQRT_DW=mean(data_c$SQRT_DW),
                    PA=getmode(data_c$PA),
                    LC=getmode(data_c$LC),
                    PIKE=seq(from= min(data_c$PIKE), to=max(data_c$PIKE), length.out = 200),
                    LOG_TR=mean(data_c$LOG_TR)
)

#predict on newdata
PI_fith<-predict(carc_mod_hp, newdata=nwdtpih,type="response", se.fit=TRUE)


#call dataframe to store fitted values, 0.05 CI and 0.95 CI
PImh<-data.frame(fv=PI_fith$fit, lowCI=NA,upCI=NA,x=NA)



### 0.05 CI
PImh$lowCI<-PI_fith$fit-(2*PI_fith$se.fit)

### set all values that are negativ to NA
PImh[PImh$lowCI<0,]$lowCI <- 0

### 0.95 CI
PImh$upCI<-PI_fith$fit+(2*PI_fith$se.fit)

### ### set all values greater than one to NA

PImh[PImh$upCI>1,]$upCI <- 1


### get unscaled x axis
PImh$x<-seq(from= min(efdat_hp$PIKE), to=max(efdat_hp$PIKE), length.out = 200)


#### effect ggplot ####

PIploth<- ggplot(data=PImh)+
  ylim(0, 1)+
  xlab("Estimated PIKE (%)")+
  ylab("Estimated probability")+
  geom_ribbon(aes(x=PImh$x, ymax=PImh$upCI, ymin=PImh$lowCI), fill="grey50", alpha=.5) +
  geom_line(aes(x=PImh$x, y=PImh$fv),col="grey10")+
  geom_line(aes(x=PImh$x, y=PImh$lowCI),linetype = 2, col="grey30")+
  geom_line(aes(x=PImh$x, y=PImh$upCI),linetype = 2, col="grey30")+
  geom_rug(data=efdat_hp, aes(x=efdat_hp$PIKE), col="grey50", size=0.1, sides="b")+
  theme(axis.text=element_text(size=18,colour="black"),
        axis.title=element_text(size=16),
        panel.background = element_rect(fill = 'white', colour = 'white'))

PIploth



###############################################################################
######## Protected area type  (PA) ############################################
###############################################################################

#create new data.frame with mean/median obs for  predictions
nwdtpah<-data.frame(
                   DC=mean(data_c$DC),
                   EL=mean(data_c$EL),
                   NDVI=mean(data_c$NDVI),
                   T=mean(data_c$T),
                   TD=mean(data_c$TD),
                   TP4M=mean(data_c$TP4M),
                   TPA=mean(data_c$TPA),
                   VC=mean(data_c$VC),
                   SQRT_BS=mean(data_c$VC),
                   SQRT_PD=mean(data_c$SQRT_PD),
                   SQRT_LE=mean(data_c$SQRT_LE),
                   SQRT_SL=mean(data_c$SQRT_SL),
                   SQRT_DR=mean(data_c$SQRT_DR),
                   SQRT_TC300=mean(data_c$SQRT_TC300),
                   SQRT_DW=mean(data_c$SQRT_DW),
                   PA=unique(data_c$PA),
                   LC=getmode(data_c$LC),
                   PIKE=mean(data_c$PIKE),
                   LOG_TR=mean(data_c$LOG_TR)
)

#predict on newdata
PA_fith<-predict(carc_mod_hp, newdata=nwdtpah,type="response", se.fit=TRUE)


#call dataframe to store fitted values, 0.05 CI and 0.95 CI
PAmh<-data.frame(fv=PA_fith$fit, lowCI=NA,upCI=NA,x=NA)



### 0.05 CI
PAmh$lowCI<-PA_fith$fit-(2*PA_fith$se.fit)

### set all values that are negativ to NA
PAmh[PAmh$lowCI<0,]$lowCI <- 0

### 0.95 CI
PAmh$upCI<-PA_fith$fit+(2*PA_fith$se.fit)

### ### set all values greater than one to NA

PAmh[PAmh$upCI>1,]$upCI <- 1


### get unscaled x axis
PAmh$x<-unique(data_c$PA)

### get number of observations per factor

nums_of_pa<-as.numeric(table(data_c$PA))

#remove factors that have zero observations
nums_of_pa <- nums_of_pa[ nums_of_pa != 0 ]

nums_of_pa <- c("a","b","c","d","e")
#7604 1340 5495 1349

#### effect ggplot ####

PAploth <- ggplot() + 
  ylim(0, 1)+
  ylab("Estimated probability")+
  geom_errorbar(data=PAmh,aes(x=PAmh$x,ymin=PAmh$lowCI, ymax=PAmh$upCI), width=.1, color="grey50") +
  geom_point(data=PAmh, aes(x=PAmh$x, y=PAmh$fv), color="grey10")+
  scale_x_discrete(name="Protected area type",limits=c("II","IV","VI","None"),
                   labels=addline_format(c("National Park","Species Management Area",
                                           "Protected area with sustainable use of natural resources","None")))+
  annotate("text", x=PAmh$x ,y=1,label=c("1340","7604","5495","1349"))+
  theme(axis.text=element_text(size=18,colour="black"),
        axis.title=element_text(size=16),
        panel.background = element_rect(fill = 'white', colour = 'white'))



PAploth

###############################################################################
######## Land cover type  (LC) ############################################
###############################################################################

#create new data.frame with mean/median obs for predictions
nwdtlch<-data.frame(
  DC=mean(data_c$DC),
  EL=mean(data_c$EL),
  NDVI=mean(data_c$NDVI),
  T=mean(data_c$T),
  TD=mean(data_c$TD),
  TP4M=mean(data_c$TP4M),
  TPA=mean(data_c$TPA),
  VC=mean(data_c$VC),
  SQRT_BS=mean(data_c$VC),
  SQRT_PD=mean(data_c$SQRT_PD),
  SQRT_LE=mean(data_c$SQRT_LE),
  SQRT_SL=mean(data_c$SQRT_SL),
  SQRT_DR=mean(data_c$SQRT_DR),
  SQRT_TC300=mean(data_c$SQRT_TC300),
  SQRT_DW=mean(data_c$SQRT_DW),
  PA=getmode(data_c$PA),
  LC=unique(data_c$LC),
  PIKE=mean(data_c$PIKE),
  LOG_TR=mean(data_c$LOG_TR)
)

#predict on newdata
LC_fith<-predict(carc_mod_hp, newdata=nwdtlch,type="response", se.fit=TRUE)


#call dataframe to store fitted values, 0.05 CI and 0.95 CI
LCmh<-data.frame(fv=LC_fith$fit, lowCI=NA,upCI=NA,x=NA)



### 0.05 CI
LCmh$lowCI<-LC_fith$fit-(2*LC_fith$se.fit)

### set all values that are negativ to NA
LCmh[LCmh$lowCI<0,]$lowCI <- 0

### 0.95 CI
LCmh$upCI<-LC_fith$fit+(2*LC_fith$se.fit)

### ### set all values greater than one to NA

LCmh[LCmh$upCI>1,]$upCI <- 1


### get unscaled x axis

LCmh$x<-as.factor(unique(data_c$LC))


### get number of observations per factor

nums_of_lc<-as.numeric(table(data_c$LC))

#remove factors that have zero observations
nums_of_lc <- nums_of_lc[ nums_of_lc != 0 ]

###### LC type
#2 Evergreen Broadleaf
#4 Decidous Broadleaf
#5 Mixed Forest
#7 Open Shrublands
#8 Woody Savannas
#9 Savannas
#10 Grasslands
#12 Croplands
#14 Cropland Mosaics
#16 Bare Soil and Rocks
#17 Water Bodies

### effect ggplot

LCploth <- ggplot() + 
  ylim(0, 1)+
  ylab("Estimated probability")+
  geom_errorbar(data=LCmh,aes(x=LCmh$x,ymin=LCmh$lowCI, ymax=LCmh$upCI), width=.1, color="grey50") +
  geom_point(data=LCmh, aes(x=LCmh$x, y=LCmh$fv), color="grey10")+
  scale_x_discrete(name ="Land cover", labels=addline_format(c("Evergreen Broadleaf",
                                                               "Decidous Broadleaf",
                                                               "Mixed Forest",
                                                               "Open Shrublands",
                                                               "Woody Savannas",
                                                               "Savannas",
                                                               "Grasslands",
                                                               
                                                               "Croplands",
                                                               "Cropland Mosaics",
                                                               "Bare Soil and Rocks",
                                                               "Water Bodies")),
                   limits=c("2","4","5","7","8","9","10","12","14","16","17"))+
  annotate("text", x=LCmh$x ,y=1,label=c("54","3585","21","74","5","10882","94","1053","3","1","16"))+
  theme(axis.text=element_text(size=18,colour="black"),
        axis.title=element_text(size=16),
        panel.background = element_rect(fill = 'white', colour = 'white'))

LCploth





#####################################
###### combine effect plots  ######## 
##################################### 



grid.arrange(ELploth,  SLploth, 
               
             NDVIploth, TCploth)


             
grid.arrange( VCploth, Tploth ,
             TPAploth, DWploth)
             
grid.arrange(   BSploth,  DRploth,  PDploth, nrow=2)
        



grid.arrange(DCplothp,LEploth,WTploth,PIploth)
