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
setwd("~/Documents/Master/ma_elephant/R/illustrations")


########### ########### ########### ########### ###########
    ########### Line Break for long axis labes #########
########### ########### ########### ########### ########### 

addline_format <- function(x,...){
  gsub('\\s','\n',x)
}


#set theme for ggplot
theme_set(theme_bw())

#get shape of botswana country
bots <- ne_countries(scale = "medium", country="Botswana",returnclass = "sf")

######################################
### spatial dataframe for plotting ###
######################################

segments_GEE<-rgdal::readOGR(dsn= "/ma_elephant",layer= "segments_GEE")

bwa_plot<-merge(segments_GEE, bwa_scaled, by="ID", all.x=FALSE)

bwa_plot_p<-SpatialPointsDataFrame(bwa_plot, bwa_plot@data)

### points need to be a data.table therefore convert spatialpointsdataframe to dataframe
points_bwa <- data.frame(bwa_plot_p)

### polygons need to be sf object therefore convert spatialpolygonsdataframe to sf
bwa_sf<-st_as_sf(bwa_plot)

#################################
###### BWA North <-> Africa #####
#################################

bwa_north_africa<-  ggplot(data=africa)+
  xlab("Longitude")+
  ylab("Latitude")+
  geom_sf(colour="grey60",fill="grey80") +
  
  geom_polygon(data = hullBWA_NOR,  aes(x,y),fill="yellow",color="black")+
  geom_text(data= gec_points,aes(x=X+0.5, y=Y, label=name_sort),
            size=3 , color = "black", fontface = "bold", check_overlap = FALSE)+
  
  coord_sf(xlim = c(10, 40), ylim = c(-5, -25)) +
  
  theme(panel.background = element_rect(fill="white"),
        axis.line=element_blank(),axis.text.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text=element_text(size=13),
        axis.title=element_text(size=13),
        panel.border = element_rect(colour = "black", fill=NA, size=1))+
  theme(plot.margin=unit(c(0,0,0,0),"cm"))


###############################
########## BWA plot ###########
######## folds in space #######
###############################


gam_folds<-ggplot(data = bots) +
  geom_sf(color = "black", fill = "white") +
  xlab("Longitude") + ylab("Latitude") +
  annotation_scale(location = "bl", width_hint = 0.25) +
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(0.2, "in"), pad_y = unit(0.2, "in"),
                         style = north_arrow_fancy_orienteering) +
  geom_sf(data= bwa_sf, aes(fill=factor(fold.nr), colour=factor(fold.nr)))+
  scale_color_discrete(aesthetics = c("color","fill"),name="Fold number")+
  geom_point(data = points_bwa[points_bwa$COUNT_c==1,], aes(x = coords.x1, y = coords.x2, shape = "Elephants"),
             size = 0.35,alpha = 0.4,colour=I("black")) +
  scale_shape_manual(name="",labels = addline_format(c("Carcass observation")), values = 4)+
  coord_sf(xlim = c(21.9, 26.5), ylim = c(-17.75, -21)) +
  theme(plot.margin=unit(c(1.5,2,0.2,0.2),"cm"))+
  theme(panel.background = element_rect(fill="white"),axis.text=element_text(size=14),
        axis.title=element_text(size=14),
        axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black"),
        legend.position = c(1.1, 0.3),
        panel.border = element_rect(colour = "black", fill=NA, size=1))+
  guides(shape = guide_legend(override.aes = list(size = 2)))


#### combine 
grid.newpage()
vpb_ <- viewport(width = 1, height = 1, x = 0.5, y = 0.5)  # the larger map
vpa_ <- viewport(width = 0.28, height = 0.28, x = 0.85, y = 0.85)  # the inset in upper right
print(gam_folds, vp = vpb_)
print(bwa_north_africa, vp = vpa_)


###############################
########## BWA plot ###########
#### fitted values from GAM ###
###############################


### call the plot

bwa_fv<-ggplot(data = bots) +
  geom_sf(color = "black", fill = "white") +
  xlab("Longitude") + ylab("Latitude") +
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(0.2, "in"), pad_y = unit(0.2, "in"),
                         style = north_arrow_fancy_orienteering) +
  geom_sf(data = bwa_sf, aes(fill = fv ,color=fv)) +
  scale_colour_gradient(low = "grey70",high = "darkred",
                        breaks=c(0,0.5,1),labels=c("0","0.5","1") ,
                        limits=c(0,1),guide = "colourbar", 
                        aesthetics = c("color","fill"),
                        name=addline_format(c("Estimated probabilty of carcass occurrence"))) + 
  geom_point(data = points_bwa[points_bwa$COUNT_c==1,], 
             aes(x = coords.x1, y = coords.x2, shape = "Elephants"),
             size = 0.35,alpha = 0.4,colour=I("black")) +
  scale_shape_manual(name="",labels = addline_format(c("Carcass observation")), values = 4)+
  coord_sf(xlim = c(21.9, 26.5), ylim = c(-17.75, -21)) +
  theme(plot.margin=unit(c(1.5,2,0.2,0.2),"cm"))+
  theme(panel.background = element_rect(fill="white"),axis.text=element_text(size=14),
        axis.title=element_text(size=14),
        axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black"),
        legend.position = c(1.1, 0.3),
        panel.border = element_rect(colour = "black", fill=NA, size=1))+
  guides(shape = guide_legend(override.aes = list(size = 2)))
  
  


########################################
########## combine plots ###########
########################################


grid.newpage()
vpb_ <- viewport(width = 1, height = 1, x = 0.5, y = 0.5)  # the larger map
vpa_ <- viewport(width = 0.28, height = 0.28, x = 0.85, y = 0.85)  # the inset in upper right
print(bwa_fv, vp = vpb_)
print(bwa_north_africa, vp = vpa_)


###########################
### residuals in space ####
###########################

bwa_resi<-ggplot(data = bots) +
  geom_sf(color = "black", fill = "white") +
  xlab("Longitude") + ylab("Latitude") +
  
  annotation_scale(location = "bl", width_hint = 0.25) +
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(0.2, "in"), pad_y = unit(0.2, "in"),
                         style = north_arrow_fancy_orienteering) +
  
  
  geom_sf(data = bwa_sf, aes(fill = res ,color= res)) +
  scale_colour_gradient2(low = "red",mid="grey70",high = "blue",
                         breaks=c(-1,0,1),labels=c("-1","0","1") ,
                         limits=c(-1,1),guide = "colourbar", 
                         aesthetics = c("color","fill"),name="Residuals") +  
  coord_sf(xlim = c(21.9, 26.5), ylim = c(-17.75, -21)) +
theme(plot.margin=unit(c(1.5,2,0.2,0.2),"cm"))+
  theme(panel.background = element_rect(fill="white"),axis.text=element_text(size=14),
        axis.title=element_text(size=14),
        axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black"),
        legend.position = c(1.1, 0.3),
        panel.border = element_rect(colour = "black", fill=NA, size=1))


########################################
########## combine plots ###########
########################################


grid.newpage()
vpb_ <- viewport(width = 1, height = 1, x = 0.5, y = 0.5)  # the larger map
vpa_ <- viewport(width = 0.28, height = 0.28, x = 0.85, y = 0.85)  # the inset in upper right
print(bwa_resi, vp = vpb_)
print(bwa_north_africa, vp = vpa_)


#######################################
############ Effect plots #############
#######################################

###################################
### get family to backtransform ###
###################################

fam<-family(carc_mod)
ilink<-fam$linkinv



##############################
##### get unscaled data ######
##############################

efdat<-read.table("/ma_elephant/paper+lit/R/predictors_car/yxtable_norm2.csv",
                  header=TRUE,sep=",")

### backtransform data
efdat$SQRT_DR<-(efdat$SQRT_DR)^2
efdat$SQRT_BS<-(efdat$SQRT_BS)^2
efdat$SQRT_LE<-(efdat$SQRT_LE)^2
efdat$SQRT_TC300<-(efdat$SQRT_TC300)^2
efdat$SQRT_PD<-(efdat$SQRT_PD)^2
efdat$SQRT_SL<-(efdat$SQRT_SL)^2
efdat$SQRT_DW<-(efdat$SQRT_DW)^2
efdat$LC<-as.factor(efdat$LC)
#subset to botswana north
efdat_bwa<-efdat[data$Country=="BWA",]


######################## 
##### MODE FUN #########
########################

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

########################

#################################################################
####### Distance to nearest Settlement (km) [DC] #################
#################################################################

#create new data.frame with mean/median obs for predictions
nwdtdc<-data.frame(x=mean(bwa_scaled$x), y=mean(bwa_scaled$y), 
                   DC=seq(from= min(bwa_scaled$DC),
                          to=max(bwa_scaled$DC), length.out = 200),
                   EL=mean(bwa_scaled$EL),
                   NDVI=mean(bwa_scaled$NDVI),
                   T=mean(bwa_scaled$T),
                   TPA=mean(bwa_scaled$TPA),
                   VC=mean(bwa_scaled$VC),
                   SQRT_BS=mean(bwa_scaled$SQRT_BS),
                   SQRT_PD=mean(bwa_scaled$SQRT_PD),
                   SQRT_LE=mean(bwa_scaled$SQRT_LE),
                   SQRT_SL=mean(bwa_scaled$SQRT_SL),
                   SQRT_DR=mean(bwa_scaled$SQRT_DR),
                   SQRT_TC300=mean(bwa_scaled$SQRT_TC300),
                   SQRT_DW=mean(bwa_scaled$SQRT_DW),
                   PA=getmode(bwa_scaled$PA),
                   LC=getmode(bwa_scaled$LC)
)

#predict on newdata
DC_fit<-predict(carc_mod, newdata=nwdtdc, se.fit=TRUE)


#call dataframe to store fitted values, 0.05 CI and 0.95 CI
DCm<-data.frame(fv=ilink(DC_fit$fit), lowCI=NA,upCI=NA, x=NA)



### 0.05 CI
DCm$lowCI<-ilink(DC_fit$fit-(2*DC_fit$se.fit))

### 0.95 CI
DCm$upCI<-ilink(DC_fit$fit+(2*DC_fit$se.fit))



### get unscaled x axis
DCm$x<-seq(from= min(efdat_bwa$DC)/1000, to=max(efdat_bwa$DC)/1000, length.out = 200)


#### effect ggplot
DCplot<- ggplot(data=DCm)+
  ylim(0, 1)+
  xlab("Distance to nearest settlement (km)")+
  ylab("")+
  geom_ribbon(aes(x=DCm$x, ymax=DCm$upCI, ymin=DCm$lowCI), fill="grey50", alpha=.5) +
  geom_line(aes(x=DCm$x, y=DCm$fv),col="grey10")+
  geom_line(aes(x=DCm$x, y=DCm$lowCI),linetype = 2, col="grey30")+
  geom_line(aes(x=DCm$x, y=DCm$upCI),linetype = 2, col="grey30")+
  geom_rug(data=efdat_bwa, aes(x=(efdat_bwa$DC/1000)), col="grey50", size=0.1, sides="b")+
  theme(axis.text=element_text(size=18,colour="black"),
        axis.title=element_text(size=16),
        panel.background = element_rect(fill = 'white', colour = 'white'),
        panel.border = element_rect(colour = "black", fill=NA, size=1),
        aspect.ratio=1)

DCplot



##############################################
####### Elevation [m a.s.l.] (EL) ############
##############################################

#create new data.frame with mean/median obs for predictions
nwdtel<-data.frame(x=mean(bwa_scaled$x), y=mean(bwa_scaled$y), 
                   DC=mean(bwa_scaled$DC),
                   EL=seq(from= min(bwa_scaled$EL), 
                          to=max(bwa_scaled$EL), length.out = 200),
                   NDVI=mean(bwa_scaled$NDVI),
                   T=mean(bwa_scaled$T),
                   TPA=mean(bwa_scaled$TPA),
                   VC=mean(bwa_scaled$VC),
                   SQRT_BS=mean(bwa_scaled$SQRT_BS),
                   SQRT_PD=mean(bwa_scaled$SQRT_PD),
                   SQRT_LE=mean(bwa_scaled$SQRT_LE),
                   SQRT_SL=mean(bwa_scaled$SQRT_SL),
                   SQRT_DR=mean(bwa_scaled$SQRT_DR),
                   SQRT_TC300=mean(bwa_scaled$SQRT_TC300),
                   SQRT_DW=mean(bwa_scaled$SQRT_DW),
                   PA=getmode(bwa_scaled$PA),
                   LC=getmode(bwa_scaled$LC)
)

#predict on newdata
EL_fit<-predict(carc_mod, newdata=nwdtel, se.fit=TRUE)

#call dataframe to store fitted values, 0.05 CI and 0.95 CI
ELm<-data.frame(fv=ilink(EL_fit$fit), lowCI=NA,upCI=NA, x=NA)

### 0.05 CI
ELm$lowCI<-ilink(EL_fit$fit-(2*EL_fit$se.fit))

### 0.95 CI
ELm$upCI<-ilink(EL_fit$fit+(2*EL_fit$se.fit))


### get unscaled x axis
ELm$x<-seq(from= min(efdat_bwa$EL), to=max(efdat_bwa$EL), length.out = 200)

#### effect ggplot
ELplot<- ggplot(data=ELm)+
  ylim(0, 1)+
  xlab("Elevation (m a.s.l.)")+
  ylab("Estimated probability")+
  geom_ribbon(aes(x=ELm$x, ymax=ELm$upCI, ymin=ELm$lowCI), fill="grey50", alpha=.5) +
  geom_line(aes(x=ELm$x, y=ELm$fv),col="grey10")+
  geom_line(aes(x=ELm$x, y=ELm$lowCI),linetype = 2, col="grey30")+
  geom_line(aes(x=ELm$x, y=ELm$upCI),linetype = 2, col="grey30")+
  geom_rug(data=efdat_bwa, aes(x=efdat_bwa$EL), col="grey30", size=0.1, sides="b")+
  theme(axis.text=element_text(size=18,colour="black"),
        axis.title=element_text(size=16),
        panel.background = element_rect(fill = 'white', colour = 'white'),
        panel.border = element_rect(colour = "black", fill=NA, size=1),
        aspect.ratio=1)

ELplot

##########################################################################################
######## Normalized Difference Vegetation Index (NDVI) ###################################
##########################################################################################

#create new data.frame with mean obs for predictions
nwdtndvi<-data.frame(x=mean(bwa_scaled$x), y=mean(bwa_scaled$y), 
                     DC=mean(bwa_scaled$DC),
                     EL=mean(bwa_scaled$EL),
                     NDVI=seq(from= min(bwa_scaled$NDVI), 
                              to=max(bwa_scaled$NDVI), length.out = 200),
                     T=mean(bwa_scaled$T),
                     TPA=mean(bwa_scaled$TPA),
                     VC=mean(bwa_scaled$VC),
                     SQRT_BS=mean(bwa_scaled$SQRT_BS),
                     SQRT_PD=mean(bwa_scaled$SQRT_PD),
                     SQRT_LE=mean(bwa_scaled$SQRT_LE),
                     SQRT_SL=mean(bwa_scaled$SQRT_SL),
                     SQRT_DR=mean(bwa_scaled$SQRT_DR),
                     SQRT_TC300=mean(bwa_scaled$SQRT_TC300),
                     SQRT_DW=mean(bwa_scaled$SQRT_DW),
                     PA=getmode(bwa_scaled$PA),
                     LC=getmode(bwa_scaled$LC)
)

#predict on newdata
NDVI_fit<-predict(carc_mod, newdata=nwdtndvi, se.fit=TRUE)

#call dataframe to store fitted values, 0.05 CI and 0.95 CI
NDVIm<-data.frame(fv=ilink(NDVI_fit$fit), lowCI=NA,upCI=NA,x=NA)

### 0.05 CI
NDVIm$lowCI<-ilink(NDVI_fit$fit-(2*NDVI_fit$se.fit))


### 0.95 CI
NDVIm$upCI<-ilink(NDVI_fit$fit+(2*NDVI_fit$se.fit))

### get unscaled x axis
NDVIm$x<-seq(from= min(efdat_bwa$NDVI), to=max(efdat_bwa$NDVI), length.out = 200)

#### effect ggplot
NDVIplot<- ggplot(data=NDVIm)+
  ylim(0, 1)+
  xlab("NDVI")+
  ylab("")+
  geom_ribbon(aes(x=NDVIm$x, ymax=NDVIm$upCI, ymin=NDVIm$lowCI), fill="grey50", alpha=.5) +
  geom_line(aes(x=NDVIm$x, y=NDVIm$fv),col="grey10")+
  geom_line(aes(x=NDVIm$x, y=NDVIm$lowCI),linetype = 2, col="grey30")+
  geom_line(aes(x=NDVIm$x, y=NDVIm$upCI),linetype = 2, col="grey30")+
  geom_rug(data=efdat_bwa, aes(x=efdat_bwa$NDVI), col="grey30", size=0.1, sides="b")+
  theme(axis.text=element_text(size=18,colour="black"),
        axis.title=element_text(size=16),
        panel.background = element_rect(fill = 'white', colour = 'white'),
        panel.border = element_rect(colour = "black", fill=NA, size=1),
        aspect.ratio=1)

NDVIplot


##########################################################################################
######## annual mean surface air temperature (°C) [T] ####################################
##########################################################################################

#create new data.frame with mean/median obs for  predictions
nwdtt<-data.frame(x=mean(bwa_scaled$x), y=mean(bwa_scaled$y), 
                  DC=mean(bwa_scaled$DC),
                  EL=mean(bwa_scaled$EL),
                  NDVI=mean(bwa_scaled$NDVI),
                  T=seq(from= min(bwa_scaled$T), 
                        to=max(bwa_scaled$T), length.out = 200),
                  TPA=mean(bwa_scaled$TPA),
                  VC=mean(bwa_scaled$VC),
                  SQRT_BS=mean(bwa_scaled$SQRT_BS),
                  SQRT_PD=mean(bwa_scaled$SQRT_PD),
                  SQRT_LE=mean(bwa_scaled$SQRT_LE),
                  SQRT_SL=mean(bwa_scaled$SQRT_SL),
                  SQRT_DR=mean(bwa_scaled$SQRT_DR),
                  SQRT_TC300=mean(bwa_scaled$SQRT_TC300),
                  SQRT_DW=mean(bwa_scaled$SQRT_DW),
                  PA=getmode(bwa_scaled$PA),
                  LC=getmode(bwa_scaled$LC)
)

#predict on newdata
T_fit<-predict(carc_mod, newdata=nwdtt, se.fit=TRUE)

#call dataframe to store fitted values, 0.05 CI and 0.95 CI
Tm<-data.frame(fv=ilink(T_fit$fit), lowCI=NA,upCI=NA,x=NA)

### 0.05 CI
Tm$lowCI<-ilink(T_fit$fit-(2*T_fit$se.fit))


### 0.95 CI
Tm$upCI<-ilink(T_fit$fit+(2*T_fit$se.fit))

### get unscaled x axis
Tm$x<-seq(from= min(efdat_bwa$T), to=max(efdat_bwa$T), length.out = 200)

#### effect ggplot
Tplot<- ggplot(data=Tm)+
  ylim(0, 1)+
  xlab("Annual mean surface air temperature (°C) ")+
  ylab("")+
  geom_ribbon(aes(x=Tm$x, ymax=Tm$upCI, ymin=Tm$lowCI), fill="grey50", alpha=.5) +
  geom_line(aes(x=Tm$x, y=Tm$fv),col="grey10")+
  geom_line(aes(x=Tm$x, y=Tm$lowCI),linetype = 2, col="grey30")+
  geom_line(aes(x=Tm$x, y=Tm$upCI),linetype = 2, col="grey30")+
  geom_rug(data=efdat_bwa, aes(x=efdat_bwa$T), col="grey50", size=0.1, sides="b")+
  theme(axis.text=element_text(size=18,colour="black"),
        axis.title=element_text(size=16),
        panel.background = element_rect(fill = 'white', colour = 'white'),
        panel.border = element_rect(colour = "black", fill=NA, size=1),
        aspect.ratio=1)
Tplot


####################################################################
######## Total annual precipitation (mm) [TPA] #####################
####################################################################

#create new data.frame with mean/median obs for predictions
nwdttpa<-data.frame(x=mean(bwa_scaled$x), y=mean(bwa_scaled$y), 
                    DC=mean(bwa_scaled$DC),
                    EL=mean(bwa_scaled$EL),
                    NDVI=mean(bwa_scaled$NDVI),
                    T=mean(bwa_scaled$T),
                    TPA=seq(from= min(bwa_scaled$TPA), 
                            to=max(bwa_scaled$TPA), length.out = 200),
                    VC=mean(bwa_scaled$VC),
                    SQRT_BS=mean(bwa_scaled$SQRT_BS),
                    SQRT_PD=mean(bwa_scaled$SQRT_PD),
                    SQRT_LE=mean(bwa_scaled$SQRT_LE),
                    SQRT_SL=mean(bwa_scaled$SQRT_SL),
                    SQRT_DR=mean(bwa_scaled$SQRT_DR),
                    SQRT_TC300=mean(bwa_scaled$SQRT_TC300),
                    SQRT_DW=mean(bwa_scaled$SQRT_DW),
                    PA=getmode(bwa_scaled$PA),
                    LC=getmode(bwa_scaled$LC)
)

#predict on newdata
TPA_fit<-predict(carc_mod, newdata=nwdttpa, se.fit=TRUE)

#call dataframe to store fitted values, 0.05 CI and 0.95 CI
TPAm<-data.frame(fv=ilink(TPA_fit$fit), lowCI=NA,upCI=NA, x=NA)

### 0.05 CI
TPAm$lowCI<-ilink(TPA_fit$fit-(2*TPA_fit$se.fit))

### 0.95 CI
TPAm$upCI<-ilink(TPA_fit$fit+(2*TPA_fit$se.fit))


### get unscaled x axis
TPAm$x<-seq(from= min(efdat_bwa$TPA), to=max(efdat_bwa$TPA), length.out = 200)

#### effect ggplot

TPAplot<- ggplot(data=TPAm)+
  ylim(0, 1)+
  xlab("Total annual precipitation (mm)")+
  ylab("")+
  geom_ribbon(aes(x=TPAm$x, ymax=TPAm$upCI, ymin=TPAm$lowCI), fill="grey50", alpha=.5) +
  geom_line(aes(x=TPAm$x, y=TPAm$fv),col="grey10")+
  geom_line(aes(x=TPAm$x, y=TPAm$lowCI),linetype = 2, col="grey30")+
  geom_line(aes(x=TPAm$x, y=TPAm$upCI),linetype = 2, col="grey30")+
  geom_rug(data=efdat_bwa, aes(x=efdat_bwa$TPA), col="grey50", size=0.1, sides="b")+
  theme(axis.text=element_text(size=18,colour="black"),
        axis.title=element_text(size=16),
        panel.background = element_rect(fill = 'white', colour = 'white'),
        panel.border = element_rect(colour = "black", fill=NA, size=1),
        aspect.ratio=1)
TPAplot


###############################################################################################
######## Percentage of ground covered by herbaceous vegetation [%] (VC) #####################
###############################################################################################

#create new data.frame with mean/median obs for predictions
nwdtvc<-data.frame(x=mean(bwa_scaled$x), y=mean(bwa_scaled$y), 
                   DC=mean(bwa_scaled$DC),
                   EL=mean(bwa_scaled$EL),
                   NDVI=mean(bwa_scaled$NDVI),
                   T=mean(bwa_scaled$T),
                   TPA=mean(bwa_scaled$TPA),
                   VC=seq(from= min(bwa_scaled$VC), 
                          to=max(bwa_scaled$VC), length.out = 200),
                   SQRT_BS=mean(bwa_scaled$SQRT_BS),
                   SQRT_PD=mean(bwa_scaled$SQRT_PD),
                   SQRT_LE=mean(bwa_scaled$SQRT_LE),
                   SQRT_SL=mean(bwa_scaled$SQRT_SL),
                   SQRT_DR=mean(bwa_scaled$SQRT_DR),
                   SQRT_TC300=mean(bwa_scaled$SQRT_TC300),
                   SQRT_DW=mean(bwa_scaled$SQRT_DW),
                   PA=getmode(bwa_scaled$PA),
                   LC=getmode(bwa_scaled$LC)
)

#predict on newdata
VC_fit<-predict(carc_mod, newdata=nwdtvc, se.fit=TRUE)

#call dataframe to store fitted values, 0.05 CI and 0.95 CI
VCm<-data.frame(fv=ilink(VC_fit$fit), lowCI=NA,upCI=NA,X=NA)

### 0.05 CI
VCm$lowCI<-ilink(VC_fit$fit-(2*VC_fit$se.fit))

### 0.95 CI
VCm$upCI<-ilink(VC_fit$fit+(2*VC_fit$se.fit))



### get unscaled x axis
VCm$x<-seq(from= min(efdat_bwa$VC), to=max(efdat_bwa$VC), length.out = 200)

#### effect ggplot

VCplot<- ggplot(data=VCm)+
  ylim(0, 1)+
  xlim(0,99)+
  xlab("Percentage of herbaceous vegetation")+
  ylab("Estimated probability")+
  geom_ribbon(aes(x=VCm$x, ymax=VCm$upCI, ymin=VCm$lowCI), fill="grey50", alpha=.5) +
  geom_line(aes(x=VCm$x, y=VCm$fv),col="grey10")+
  geom_line(aes(x=VCm$x, y=VCm$lowCI),linetype = 2, col="grey30")+
  geom_line(aes(x=VCm$x, y=VCm$upCI),linetype = 2, col="grey30")+
  geom_rug(data=efdat_bwa, aes(x=efdat_bwa$VC), col="grey50", size=0.1, sides="b")+
  theme(axis.text=element_text(size=18,colour="black"),
        axis.title=element_text(size=16),
        panel.background = element_rect(fill = 'white', colour = 'white'),
        panel.border = element_rect(colour = "black", fill=NA, size=1),
        aspect.ratio=1)

VCplot

###############################################################################################
######## Percentage of ground covered by bare soil (%) [BS] ###################################
###############################################################################################

#create new data.frame with mean/median obs for  predictions
nwdtbs<-data.frame(x=mean(bwa_scaled$x), y=mean(bwa_scaled$y), 
                   DC=mean(bwa_scaled$DC),
                   EL=mean(bwa_scaled$EL),
                   NDVI=mean(bwa_scaled$NDVI),
                   T=mean(bwa_scaled$T),
                   TPA=mean(bwa_scaled$TPA),
                   VC=mean(bwa_scaled$VC),
                   SQRT_BS=seq(from= min(bwa_scaled$SQRT_BS),
                               to=max(bwa_scaled$SQRT_BS), length.out = 200),
                   SQRT_PD=mean(bwa_scaled$SQRT_PD),
                   SQRT_LE=mean(bwa_scaled$SQRT_LE),
                   SQRT_SL=mean(bwa_scaled$SQRT_SL),
                   SQRT_DR=mean(bwa_scaled$SQRT_DR),
                   SQRT_TC300=mean(bwa_scaled$SQRT_TC300),
                   SQRT_DW=mean(bwa_scaled$SQRT_DW),
                   PA=getmode(bwa_scaled$PA),
                   LC=getmode(bwa_scaled$LC)
)

#predict on newdata
BS_fit<-predict(carc_mod, newdata=nwdtbs, se.fit=TRUE)

#call dataframe to store fitted values, 0.05 CI and 0.95 CI
BSm<-data.frame(fv=ilink(BS_fit$fit), lowCI=NA,upCI=NA)

### 0.05 CI
BSm$lowCI<-ilink(BS_fit$fit-(2*BS_fit$se.fit))

### 0.95 CI
BSm$upCI<-ilink(BS_fit$fit+(2*BS_fit$se.fit))


### get unscaled x axis
BSm$x<-seq(from= min(efdat_bwa$SQRT_BS), to=max(efdat_bwa$SQRT_BS), length.out = 200)

#### effect ggplot

BSplot<- ggplot(data=BSm)+
  ylim(0, 1)+
  xlab("Percentage of bare soil")+
  ylab("")+
  geom_ribbon(aes(x=BSm$x, ymax=BSm$upCI, ymin=BSm$lowCI), fill="grey50", alpha=.5) +
  geom_line(aes(x=BSm$x, y=BSm$fv),col="grey10")+
  geom_line(aes(x=BSm$x, y=BSm$lowCI),linetype = 2, col="grey30")+
  geom_line(aes(x=BSm$x, y=BSm$upCI),linetype = 2, col="grey30")+
  geom_rug(data=efdat_bwa, aes(x=efdat_bwa$SQRT_BS), col="grey50", size=0.1, sides="b")+
  theme(axis.text=element_text(size=18,colour="black"),
        axis.title=element_text(size=16),
        panel.background = element_rect(fill = 'white', colour = 'white'),
        panel.border = element_rect(colour = "black", fill=NA, size=1),
        aspect.ratio=1)

BSplot


############################################################################
######## Population density (inhabitants per km²) [PD] #####################
############################################################################

#create new data.frame with mean/median obs for predictions
nwdtpb<-data.frame(x=mean(bwa_scaled$x), y=mean(bwa_scaled$y), 
                   DC=mean(bwa_scaled$DC),
                   EL=mean(bwa_scaled$EL),
                   NDVI=mean(bwa_scaled$NDVI),
                   T=mean(bwa_scaled$T),
                   TPA=mean(bwa_scaled$TPA),
                   VC=mean(bwa_scaled$VC),
                   SQRT_BS=mean(bwa_scaled$VC),
                   SQRT_PD=seq(from= min(bwa_scaled$SQRT_PD), 
                               to=max(bwa_scaled$SQRT_PD), length.out = 200),
                   SQRT_LE=mean(bwa_scaled$SQRT_LE),
                   SQRT_SL=mean(bwa_scaled$SQRT_SL),
                   SQRT_DR=mean(bwa_scaled$SQRT_DR),
                   SQRT_TC300=mean(bwa_scaled$SQRT_TC300),
                   SQRT_DW=mean(bwa_scaled$SQRT_DW),
                   PA=getmode(bwa_scaled$PA),
                   LC=getmode(bwa_scaled$LC)
)

#predict on newdata
PD_fit<-predict(carc_mod, newdata=nwdtpb, se.fit=TRUE)

#call dataframe to store fitted values, 0.05 CI and 0.95 CI
PDm<-data.frame(fv=ilink(PD_fit$fit), lowCI=NA,upCI=NA,x=NA)

### 0.05 CI
PDm$lowCI<-ilink(PD_fit$fit-(2*PD_fit$se.fit))

### 0.95 CI
PDm$upCI<-ilink(PD_fit$fit+(2*PD_fit$se.fit))


### get unscaled x axis
PDm$x<-seq(from= min(efdat_bwa$SQRT_PD), to=max(efdat_bwa$SQRT_PD), length.out = 200)

#### effect ggplot

PDplot<- ggplot(data=PDm)+
  ylim(0, 1)+
  xlab("Population density (Inhabitants per km²)")+
  ylab("")+
  geom_ribbon(aes(x=PDm$x, ymax=PDm$upCI, ymin=PDm$lowCI), fill="grey50", alpha=.5) +
  geom_line(aes(x=PDm$x, y=PDm$fv),col="grey10")+
  geom_line(aes(x=PDm$x, y=PDm$lowCI),linetype = 2, col="grey30")+
  geom_line(aes(x=PDm$x, y=PDm$upCI),linetype = 2, col="grey30")+
  geom_rug(data=efdat_bwa, aes(x=efdat_bwa$SQRT_PD), col="grey50", size=0.1, sides="b")+
  theme(axis.text=element_text(size=18,colour="black"),
        axis.title=element_text(size=16),
        panel.background = element_rect(fill = 'white', colour = 'white'),
        panel.border = element_rect(colour = "black", fill=NA, size=1),
        aspect.ratio=1)

PDplot

###############################################################################################
######## Probabilty of living elephants (P) [LE] #####################
###############################################################################################

#create new data.frame with mean/median obs for predictions
nwdtle<-data.frame(x=mean(bwa_scaled$x), y=mean(bwa_scaled$y), 
                   DC=mean(bwa_scaled$DC),
                   EL=mean(bwa_scaled$EL),
                   NDVI=mean(bwa_scaled$NDVI),
                   T=mean(bwa_scaled$T),
                   TPA=mean(bwa_scaled$TPA),
                   VC=mean(bwa_scaled$VC),
                   SQRT_BS=mean(bwa_scaled$VC),
                   SQRT_PD=mean(bwa_scaled$SQRT_PD),
                   SQRT_LE=seq(from= min(bwa_scaled$SQRT_LE),
                               to=max(bwa_scaled$SQRT_LE), length.out = 200),
                   SQRT_SL=mean(bwa_scaled$SQRT_SL),
                   SQRT_DR=mean(bwa_scaled$SQRT_DR),
                   SQRT_TC300=mean(bwa_scaled$SQRT_TC300),
                   SQRT_DW=mean(bwa_scaled$SQRT_DW),
                   PA=getmode(bwa_scaled$PA),
                   LC=getmode(bwa_scaled$LC)
)

#predict on newdata
LE_fit<-predict(carc_mod, newdata=nwdtle, se.fit=TRUE)

#call dataframe to store fitted values, 0.05 CI and 0.95 CI
LEm<-data.frame(fv=ilink(LE_fit$fit), lowCI=NA,upCI=NA,x=NA)

### 0.05 CI
LEm$lowCI<-ilink(LE_fit$fit-(2*LE_fit$se.fit))


### 0.95 CI
LEm$upCI<-ilink(LE_fit$fit+(2*LE_fit$se.fit))


### get unscaled x axis
LEm$x<-seq(from= min(efdat_bwa$SQRT_LE), to=max(efdat_bwa$SQRT_LE), length.out = 200)

#### conditional ggplot
LEplot<- ggplot(data=LEm)+
  ylim(0, 1)+
  xlab("Estimated probability of living elephants (P)")+
  ylab("Estimated probability")+
  geom_ribbon(aes(x=LEm$x, ymax=LEm$upCI, ymin=LEm$lowCI), fill="grey50", alpha=.5) +
  geom_line(aes(x=LEm$x, y=LEm$fv),col="grey10")+
  geom_line(aes(x=LEm$x, y=LEm$lowCI),linetype = 2, col="grey30")+
  geom_line(aes(x=LEm$x, y=LEm$upCI),linetype = 2, col="grey30")+
  geom_rug(data=efdat_bwa, aes(x=efdat_bwa$SQRT_LE), col="grey50", size=0.1, sides="b")+
  theme(axis.text=element_text(size=18,colour="black"),
        axis.title=element_text(size=16),
        panel.background = element_rect(fill = 'white', colour = 'white'),
        panel.border = element_rect(colour = "black", fill=NA, size=1),
        aspect.ratio=1)

LEplot

#############################################
######## Slope (°) [SL] #####################
#############################################

#create new data.frame with mean/median obs for  predictions
nwdtsl<-data.frame(x=mean(bwa_scaled$x), y=mean(bwa_scaled$y), 
                   DC=mean(bwa_scaled$DC),
                   EL=mean(bwa_scaled$EL),
                   NDVI=mean(bwa_scaled$NDVI),
                   T=mean(bwa_scaled$T),
                   TPA=mean(bwa_scaled$TPA),
                   VC=mean(bwa_scaled$VC),
                   SQRT_BS=mean(bwa_scaled$VC),
                   SQRT_PD=mean(bwa_scaled$SQRT_PD),
                   SQRT_LE=mean(bwa_scaled$SQRT_LE),
                   SQRT_SL=seq(from= min(bwa_scaled$SQRT_SL),
                               to=max(bwa_scaled$SQRT_SL), length.out = 200),
                   SQRT_DR=mean(bwa_scaled$SQRT_DR),
                   SQRT_TC300=mean(bwa_scaled$SQRT_TC300),
                   SQRT_DW=mean(bwa_scaled$SQRT_DW),
                   PA=getmode(bwa_scaled$PA),
                   LC=getmode(bwa_scaled$LC)
)

#predict on newdata
SL_fit<-predict(carc_mod, newdata=nwdtsl, se.fit=TRUE)

#call dataframe to store fitted values, 0.05 CI and 0.95 CI
SLm<-data.frame(fv=ilink(SL_fit$fit), lowCI=NA,upCI=NA,x=NA)

### 0.05 CI
SLm$lowCI<-ilink(SL_fit$fit-(2*SL_fit$se.fit))


### 0.95 CI
SLm$upCI<-ilink(SL_fit$fit+(2*SL_fit$se.fit))



### get unscaled x axis
SLm$x<-seq(from= min(efdat_bwa$SQRT_SL), to=max(efdat_bwa$SQRT_SL), length.out = 200)


#### effect ggplot ####

SLplot<- ggplot(data=SLm)+
  ylim(0, 1)+
  xlab("Slope (°) ")+
  ylab("")+
  geom_ribbon(aes(x=SLm$x, ymax=SLm$upCI, ymin=SLm$lowCI), fill="grey50", alpha=.5) +
  geom_line(aes(x=SLm$x, y=SLm$fv),col="grey10")+
  geom_line(aes(x=SLm$x, y=SLm$lowCI),linetype = 2, col="grey30")+
  geom_line(aes(x=SLm$x, y=SLm$upCI),linetype = 2, col="grey30")+
  geom_rug(data=efdat_bwa, aes(x=efdat_bwa$SQRT_SL), col="grey50", size=0.1, sides="b")+
  theme(axis.text=element_text(size=18,colour="black"),
        axis.title=element_text(size=16),
        panel.background = element_rect(fill = 'white', colour = 'white'),
        panel.border = element_rect(colour = "black", fill=NA, size=1),
        aspect.ratio=1)

SLplot

################################################################
######## Distance to nearest road [m] (DR) #####################
################################################################

#create new data.frame with mean/median obs for  predictions
nwdtdr<-data.frame(x=mean(bwa_scaled$x), y=mean(bwa_scaled$y), 
                   DC=mean(bwa_scaled$DC),
                   EL=mean(bwa_scaled$EL),
                   NDVI=mean(bwa_scaled$NDVI),
                   T=mean(bwa_scaled$T),
                   TPA=mean(bwa_scaled$TPA),
                   VC=mean(bwa_scaled$VC),
                   SQRT_BS=mean(bwa_scaled$VC),
                   SQRT_PD=mean(bwa_scaled$SQRT_PD),
                   SQRT_LE=mean(bwa_scaled$SQRT_LE),
                   SQRT_SL=mean(bwa_scaled$SQRT_SL),
                   SQRT_DR=seq(from= min(bwa_scaled$SQRT_DR), to=max(bwa_scaled$SQRT_DR), length.out = 200),
                   SQRT_TC300=mean(bwa_scaled$SQRT_TC300),
                   SQRT_DW=mean(bwa_scaled$SQRT_DW),
                   PA=getmode(bwa_scaled$PA),
                   LC=getmode(bwa_scaled$LC)
)

#predict on newdata
DR_fit<-predict(carc_mod, newdata=nwdtdr, se.fit=TRUE)

#call dataframe to store fitted values, 0.05 CI and 0.95 CI
DRm<-data.frame(fv=ilink(DR_fit$fit), lowCI=NA,upCI=NA,x=NA)

### 0.05 CI
DRm$lowCI<-ilink(DR_fit$fit-(2*DR_fit$se.fit))

### 0.95 CI
DRm$upCI<-ilink(DR_fit$fit+(2*DR_fit$se.fit))

### get unscaled x axis
DRm$x<-seq(from= min(efdat_bwa$SQRT_DR)/1000, to=max(efdat_bwa$SQRT_DR)/1000, length.out = 200)


#### effect ggplot ####

DRplot<- ggplot(data=DRm)+
  ylim(0, 1)+
  xlab("Distance to nearest Road (km)")+
  ylab("")+
  geom_ribbon(aes(x=DRm$x, ymax=DRm$upCI, ymin=DRm$lowCI), fill="grey50", alpha=.5) +
  geom_line(aes(x=DRm$x, y=DRm$fv),col="grey10")+
  geom_line(aes(x=DRm$x, y=DRm$lowCI),linetype = 2, col="grey30")+
  geom_line(aes(x=DRm$x, y=DRm$upCI),linetype = 2, col="grey30")+
  geom_rug(data=efdat_bwa, aes((x=efdat_bwa$SQRT_DR/1000)), col="grey50", size=0.1, sides="b")+
  theme(axis.text=element_text(size=18,colour="black"),
        axis.title=element_text(size=16),
        panel.background = element_rect(fill = 'white', colour = 'white'),
        panel.border = element_rect(colour = "black", fill=NA, size=1),
        aspect.ratio=1)

DRplot

################################################################
######## Treecover [%] (TC300) #####################
################################################################

#create new data.frame with mean/median obs for predictions
nwdttc<-data.frame(x=mean(bwa_scaled$x), y=mean(bwa_scaled$y), 
                   DC=mean(bwa_scaled$DC),
                   EL=mean(bwa_scaled$EL),
                   NDVI=mean(bwa_scaled$NDVI),
                   T=mean(bwa_scaled$T),
                   TPA=mean(bwa_scaled$TPA),
                   VC=mean(bwa_scaled$VC),
                   SQRT_BS=mean(bwa_scaled$VC),
                   SQRT_PD=mean(bwa_scaled$SQRT_PD),
                   SQRT_LE=mean(bwa_scaled$SQRT_LE),
                   SQRT_SL=mean(bwa_scaled$SQRT_SL),
                   SQRT_DR=mean(bwa_scaled$SQRT_DR),
                   SQRT_TC300=seq(from= min(bwa_scaled$SQRT_TC300),
                                  to=max(bwa_scaled$SQRT_TC300), length.out = 200),
                   SQRT_DW=mean(bwa_scaled$SQRT_DW),
                   PA=getmode(bwa_scaled$PA),
                   LC=getmode(bwa_scaled$LC)
)

#predict on newdata
TC_fit<-predict(carc_mod, newdata=nwdttc, se.fit=TRUE)

#call dataframe to store fitted values, 0.05 CI and 0.95 CI
TCm<-data.frame(fv=ilink(TC_fit$fit), lowCI=NA,upCI=NA,x=NA)

### 0.05 CI
TCm$lowCI<-ilink(TC_fit$fit-(2*TC_fit$se.fit))


### 0.95 CI
TCm$upCI<-ilink(TC_fit$fit+(2*TC_fit$se.fit))



### get unscaled x axis
TCm$x<-seq(from= min(efdat_bwa$SQRT_TC300), to=max(efdat_bwa$SQRT_TC300), length.out = 200)



#### effect ggplot ####

TCplot<- ggplot(data=TCm)+
  ylim(0, 1)+
  xlab("Percentage of tree canopy")+
  ylab("")+
  geom_ribbon(aes(x=TCm$x, ymax=TCm$upCI, ymin=TCm$lowCI), fill="grey50", alpha=.5) +
  geom_line(aes(x=TCm$x, y=TCm$fv),col="grey10")+
  geom_line(aes(x=TCm$x, y=TCm$lowCI),linetype = 2, col="grey30")+
  geom_line(aes(x=TCm$x, y=TCm$upCI),linetype = 2, col="grey30")+
  geom_rug(data=efdat_bwa, aes(x=efdat_bwa$SQRT_TC300), col="grey50", size=0.1, sides="b")+
  theme(axis.text=element_text(size=18,colour="black"),
        axis.title=element_text(size=16),
        panel.background = element_rect(fill = 'white', colour = 'white'),
        panel.border = element_rect(colour = "black", fill=NA, size=1),
        aspect.ratio=1)

TCplot

###############################################################################
######## Distance to nearest waterbody (m)  (DW) ####################
###############################################################################

nwdtdw<-data.frame(x=mean(bwa_scaled$x), y=mean(bwa_scaled$y), 
                   DC=mean(bwa_scaled$DC),
                   EL=mean(bwa_scaled$EL),
                   NDVI=mean(bwa_scaled$NDVI),
                   T=mean(bwa_scaled$T),
                   TPA=mean(bwa_scaled$TPA),
                   VC=mean(bwa_scaled$VC),
                   SQRT_BS=mean(bwa_scaled$VC),
                   SQRT_PD=mean(bwa_scaled$SQRT_PD),
                   SQRT_LE=mean(bwa_scaled$SQRT_LE),
                   SQRT_SL=mean(bwa_scaled$SQRT_SL),
                   SQRT_DR=mean(bwa_scaled$SQRT_DR),
                   SQRT_TC300=mean(bwa_scaled$SQRT_TC300),
                   SQRT_DW=seq(from=min(bwa_scaled$SQRT_DW),
                               to=max(bwa_scaled$SQRT_DW),length.out = 200),
                   PA=getmode(bwa_scaled$PA),
                   LC=getmode(bwa_scaled$LC)
)

#predict on newdata
DW_fit<-predict(carc_mod, newdata=nwdtdw, se.fit=TRUE)

#call dataframe to store fitted values, 0.05 CI and 0.95 CI
DWm<-data.frame(fv=ilink(DW_fit$fit), lowCI=NA,upCI=NA,x=NA)

### 0.05 CI
DWm$lowCI<-ilink(DW_fit$fit-(2*DW_fit$se.fit))


### 0.95 CI
DWm$upCI<-ilink(DW_fit$fit+(2*DW_fit$se.fit))

### get unscaled x axis
DWm$x<-seq(from= min(efdat_bwa$SQRT_DW), to=max(efdat_bwa$SQRT_DW), length.out = 200)


#### effect ggplot ####

DWplot<- ggplot(data=DWm)+
  ylim(0, 1)+
  xlab("Distance to nearest waterbody (km)")+
  ylab("Estimated probability")+
  geom_ribbon(aes(x=DWm$x, ymax=DWm$upCI, ymin=DWm$lowCI), fill="grey50", alpha=.5) +
  geom_line(aes(x=DWm$x, y=DWm$fv),col="grey10")+
  geom_line(aes(x=DWm$x, y=DWm$lowCI),linetype = 2, col="grey30")+
  geom_line(aes(x=DWm$x, y=DWm$upCI),linetype = 2, col="grey30")+
  geom_rug(data=efdat_bwa, aes(x=efdat_bwa$SQRT_DW), col="grey50", size=0.1, sides="b")+
  theme(axis.text=element_text(size=18,colour="black"),
        axis.title=element_text(size=16),
        panel.background = element_rect(fill = 'white', colour = 'white'),
        panel.border = element_rect(colour = "black", fill=NA, size=1),
        aspect.ratio=1)

DWplot



###############################################################################
######## Protected area type  (PA) ############################################
###############################################################################

#create new data.frame with mean/median obs for  predictions
nwdtpa<-data.frame(x=mean(bwa_scaled$x), y=mean(bwa_scaled$y), 
                   DC=mean(bwa_scaled$DC),
                   EL=mean(bwa_scaled$EL),
                   NDVI=mean(bwa_scaled$NDVI),
                   T=mean(bwa_scaled$T),
                   TPA=mean(bwa_scaled$TPA),
                   VC=mean(bwa_scaled$VC),
                   SQRT_BS=mean(bwa_scaled$VC),
                   SQRT_PD=mean(bwa_scaled$SQRT_PD),
                   SQRT_LE=mean(bwa_scaled$SQRT_LE),
                   SQRT_SL=mean(bwa_scaled$SQRT_SL),
                   SQRT_DR=mean(bwa_scaled$SQRT_DR),
                   SQRT_TC300=mean(bwa_scaled$SQRT_TC300),
                   SQRT_DW=mean(bwa_scaled$SQRT_DW),
                   PA=unique(bwa_scaled$PA),
                   LC=getmode(bwa_scaled$LC)
)

#predict on newdata
PA_fit<-predict(carc_mod, newdata=nwdtpa, se.fit=TRUE)

#call dataframe to store fitted values, 0.05 CI and 0.95 CI
PAm<-data.frame(fv=ilink(PA_fit$fit), lowCI=NA,upCI=NA,x=NA)

### 0.05 CI
PAm$lowCI<-ilink(PA_fit$fit-(2*PA_fit$se.fit))



### 0.95 CI
PAm$upCI<-ilink(PA_fit$fit+(2*PA_fit$se.fit))

### get unscaled x axis
PAm$x<-unique(efdat_bwa$PA)

### get number of observations per factor
nums_of_pa<-as.numeric(table(efdat_bwa$PA))

#remove factors that have zero observations
nums_of_pa <- nums_of_pa[ nums_of_pa != 0 ]

nums_of_pa <- c(3160,514,2,2,8707)

#### add numbers by hand -> annotate

#### effect ggplot ####
PAplot <- ggplot() + 
  ylim(0, 1)+
  ylab("Estimated probability")+
  geom_errorbar(data=PAm,aes(x=PAm$x,ymin=PAm$lowCI, ymax=PAm$upCI), width=.1, color="grey50") +
  geom_point(data=PAm, aes(x=PAm$x, y=PAm$fv), color="grey10")+
  scale_x_discrete(name="Protected area type",limits=c("Ib","II","IV","VI","None"),
                   labels=addline_format(c("Wilderness Area","National Park","Species Management Area",
                                           "Protected area with sustainable use of natural resources","None")))+
  annotate("text", x=PAm$x ,y=1,label=c("8707","3160","2","514","2"))+
  theme(axis.text=element_text(size=10,colour="black"),
        axis.title=element_text(size=12),
        panel.background = element_rect(fill = 'white', colour = 'white'),
        panel.border = element_rect(colour = "black", fill=NA, size=1),
        aspect.ratio = 1)



PAplot

###############################################################################
######## Land cover type  (LC) ############################################
###############################################################################

#create new data.frame with mean/median obs for predictions
nwdtlc<-data.frame(x=mean(bwa_scaled$x), y=mean(bwa_scaled$y), 
                   DC=mean(bwa_scaled$DC),
                   EL=mean(bwa_scaled$EL),
                   NDVI=mean(bwa_scaled$NDVI),
                   T=mean(bwa_scaled$T),
                   TPA=mean(bwa_scaled$TPA),
                   VC=mean(bwa_scaled$VC),
                   SQRT_BS=mean(bwa_scaled$VC),
                   SQRT_PD=mean(bwa_scaled$SQRT_PD),
                   SQRT_LE=mean(bwa_scaled$SQRT_LE),
                   SQRT_SL=mean(bwa_scaled$SQRT_SL),
                   SQRT_DR=mean(bwa_scaled$SQRT_DR),
                   SQRT_TC300=mean(bwa_scaled$SQRT_TC300),
                   SQRT_DW=mean(bwa_scaled$SQRT_DW),
                   PA=getmode(bwa_scaled$PA),
                   LC=unique(bwa_scaled$LC)
)

#predict on newdata
LC_fit<-predict(carc_mod, newdata=nwdtlc, se.fit=TRUE)

#call dataframe to store fitted values, 0.05 CI and 0.95 CI
LCm<-data.frame(fv=ilink(LC_fit$fit), lowCI=NA,upCI=NA,x=NA)

### 0.05 CI
LCm$lowCI<-ilink(LC_fit$fit-(2*LC_fit$se.fit))

### 0.95 CI
LCm$upCI<-ilink(LC_fit$fit+(2*LC_fit$se.fit))


### get unscaled x axis
LCm$x<-as.factor(unique(efdat_bwa$LC))

### get number of observations per factor
nums_of_lc<-as.numeric(table(efdat_bwa$LC))

#remove factors that have zero observations
nums_of_lc <- nums_of_lc[ nums_of_lc != 0 ]

#### add numbers by hand  -> annotate

###### LC type

#6 Closed Shrublands
#7 Woody Savannas
#9 Savannas
#10 Grasslands
#11 Permanent Wetlands
#16 Bare Soil and Rocks
#17 Water Bodies

### effect ggplot

LCplot <- ggplot() + 
  ylim(0, 1)+
  ylab("Estimated probability")+
  geom_errorbar(data=LCm,aes(x=LCm$x,ymin=LCm$lowCI, ymax=LCm$upCI), width=.1, color="grey50") +
  geom_point(data=LCm, aes(x=LCm$x, y=LCm$fv), color="grey10")+
  scale_x_discrete(name ="Land cover",
                   labels=addline_format(c("Closed Shrublands","Woody Savannas","Savannas",
                                            "Grasslands","Permanent Wetlands","Bare Soil and Rocks",
                                            "Water Bodies")),
                   limits=c("6","7","9","10","11","16","17"))+
  annotate("text", x=LCm$x ,y=1,label=c("8735","560","175","2253","623","16","23"))+
  theme(axis.text=element_text(size=10,colour="black"),
        axis.title=element_text(size=12),
        panel.background = element_rect(fill = 'white', colour = 'white'),
        panel.border = element_rect(colour = "black", fill=NA, size=1),
        aspect.ratio = 1)

LCplot





#####################################################
###### 4 patches for continous effect plots  ########
#####################################################


grid.arrange(ELplot,SLplot, NDVIplot ,
             TCplot)



grid.arrange(VCplot , BSplot, 
             Tplot,TPAplot)
           


grid.arrange( DWplot , DCplot,DRplot, 
              PDplot)



grid.arrange( LEplot)



PAplot



LCplot
