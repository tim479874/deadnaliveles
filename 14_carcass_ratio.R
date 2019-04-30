#########################
##### load packages #####
#########################


library(dplyr)
library(corrplot)


#### load temporal carcass observations from the GEC
#### 2014 and 2015 
carcr<- read.table(file="/ma_elephant/R/HT_c_sum_time.csv", sep=",", header=TRUE)
carcr$X<-NULL

#########################################################
#### import data table for number of living eles    #####
#########################################################

datale<-read.table("/ma_elephant/R/predictors/yxtable.csv",
                   header=TRUE,sep=",")


eles<- aggregate(datale$COUNT,
                     by = list(datale$Country),
                     FUN = sum)



### split dataset based on year and do some sorting and naming
carc<-split(carcr, carcr$Year)

carc2014<-carc$`2014`
carc2015<-carc$`2015`


carc2014<-carc2014[,c(1,2,3,4,5,7)]
carc2015<-carc2015[,c(1,2,3,4,5,7)]

colnames(carc2014)<-c("ID","c2014","c2013","c2012","c2011","Country")
colnames(carc2015)<-c("ID","c2015","c2014","c2013","c2012","Country")


carctemp<-data.frame(matrix(ncol=7, nrow=8))
carctemp$X1<-c("AGO","BWA", "COD", "ETH", "KEN" ,"TCD", "XWA" ,"ZWE")
colnames(carctemp)<-c("Country","c2015","c2014","c2013","c2012","c2011","eles")
carctemp$eles<-eles$x

### calculate number of carcasses per country in 2015
car2015 <- aggregate(carc2015$c2015,
                  by = list(carc2015$Country),
                  FUN = sum)

carctemp[carctemp$Country %in% car2015$Group.1,]$c2015<-car2015$x

### calculate number of carcasses per country in 2014
car20141<- aggregate(carc2014$c2014,
                     by = list(carc2014$Country),
                     FUN = sum)

car20142<- aggregate(carc2015$c2014,
                    by = list(carc2015$Country),
                    FUN = sum)


carces2014<-rbind(car20141,car20142)



carscomb2014<-dplyr::group_by(carces2014,Group.1)%>% dplyr::summarise_all(sum)

carctemp$c2014<-carscomb2014$x


### calculate number of carcasses per country in 2013
car20131<- aggregate(carc2014$c2013,
                     by = list(carc2014$Country),
                     FUN = sum)

car20132<- aggregate(carc2015$c2013,
                     by = list(carc2015$Country),
                     FUN = sum)



carces2013<-rbind(car20131,car20132)



carscomb2013<-dplyr::group_by(carces2013,Group.1)%>% dplyr::summarise_all(sum)

carctemp$c2013<-carscomb2013$x

### calculate number of carcasses per country in 2012

car20121<- aggregate(carc2014$c2012,
                     by = list(carc2014$Country),
                     FUN = sum)

car20122<- aggregate(carc2015$c2012,
                     by = list(carc2015$Country),
                     FUN = sum)



carces2012<-rbind(car20121,car20122)



carscomb2012<-dplyr::group_by(carces2012,Group.1)%>% dplyr::summarise_all(sum)

carctemp$c2012<-carscomb2012$x

### calculate number of carcasses per country in 2011

car20111<- aggregate(carc2014$c2011,
                     by = list(carc2014$Country),
                     FUN = sum)

carctemp[carctemp$Country %in% car20111$Group.1,]$c2011<-car20111$x


#### save output

write.csv(carctemp, file = "/ma_elephant/R/temporalcarcs.csv", row.names=FALSE)


####################

### temporal carcass ratios


tcr<-data.frame(matrix(ncol=6, nrow=8))
tcr$X1<-c("AGO","BWA", "COD", "ETH", "KEN" ,"TCD", "XWA" ,"ZWE")
colnames(tcr)<-c("Country","tcr2015","tcr2014","tcr2013","tcr2012","tcr2011")

i=1
#calculate ratios for year and country

for (i in 1:8) {
  tcr$tcr2015[i]<-carctemp$c2015[i]/sum(carctemp$c2015[i],carctemp$eles[i])
  tcr$tcr2014[i]<-carctemp$c2014[i]/sum(carctemp$c2014[i],carctemp$eles[i])
  tcr$tcr2013[i]<-carctemp$c2013[i]/sum(carctemp$c2013[i],carctemp$eles[i])
  tcr$tcr2012[i]<-carctemp$c2012[i]/sum(carctemp$c2012[i],carctemp$eles[i])
  tcr$tcr2011[i]<-carctemp$c2011[i]/sum(carctemp$c2011[i],carctemp$eles[i])
}

####### save output


write.csv(tcr, file = "/ma_elephant/R/temporalcarcratios.csv", row.names=FALSE)

sort(unique(covars2015$siteid))
unique(covars2015$ccode)

#### MIKE Sites which lay inside GEC survey data #######

# AGO -> ZBZ        Angola Zambezi closest MIKE site
# BWA -> CHO        Botswana
# COD -> GAR VIR    Kongo
# ETH -> BBL        Ethopia
# KEN -> TSV SBR    Kenia
# TCD -> ZAK        Tschad
# XWA -> PDJ        W-Arly-Pendjari
# ZWE -> CHE        Simbabwe


#################################
##### load PIKE predictions #####
#################################

all_ps.full<-readRDS("/ma_elephant/R/PIKE_preds.rds")

dim(all_ps.full)

#######################
###   53 Site       ###
###   16 Years      ###
###   3000 draws    ###
#######################




#### extract PIKE estimations for corresponding years and MIKE sites

sites<-c("ZBZ" ,
         "CHO" ,
         "GAR",  "VIR" ,
         "BBL" ,
         "TSV", "SBR" ,
         "ZAK" ,
         "PDJ",
         "CHE" )



year2011<-all_ps.full[,10,]
year2012<-all_ps.full[,11,]
year2013<-all_ps.full[,12,]
year2014<-all_ps.full[,13,]
year2015<-all_ps.full[,14,]

year2011sites<-year2011[rownames(year2011) %in% sites,]
year2012sites<-year2012[rownames(year2012) %in% sites,]
year2013sites<-year2013[rownames(year2013) %in% sites,]
year2014sites<-year2014[rownames(year2014) %in% sites,]
year2015sites<-year2015[rownames(year2013) %in% sites,]


####### get the estimated PIKE values for the year and site
### 2015
AGO2015PIKE<-year2015[rownames(year2015)%in% "ZBZ",]
BWA2015PIKE<-year2015[rownames(year2015) %in% "CHO",]
COD2015PIKE<-(year2015[rownames(year2015) %in% "GAR",]+ year2015[rownames(year2015) %in% "VIR",] ) /2
ETH2015PIKE<-year2015[rownames(year2015) %in% "BBL",]
KEN2015PIKE<-(year2015[rownames(year2015) %in% "TSV",] + year2015[rownames(year2015) %in% "SBR",] ) /2
TCV2015PIKE<-year2015[rownames(year2015) %in% "ZAK",]
XWA2015PIKE<-year2015[rownames(year2015) %in% "PDJ",]
ZWE2015PIKE<-year2015[rownames(year2015) %in% "CHE",]


PIKE2015<-data.frame(AGO2015PIKE,BWA2015PIKE,COD2015PIKE,
                     ETH2015PIKE,KEN2015PIKE,TCV2015PIKE,XWA2015PIKE,ZWE2015PIKE)

###2014
AGO2014PIKE<-year2014[rownames(year2014)%in% "ZBZ",]
BWA2014PIKE<-year2014[rownames(year2014) %in% "CHO",]
COD2014PIKE<-(year2014[rownames(year2014) %in% "GAR",]+ year2014[rownames(year2014) %in% "VIR",] ) /2
ETH2014PIKE<-year2014[rownames(year2014) %in% "BBL",]
KEN2014PIKE<-(year2014[rownames(year2014) %in% "TSV",] + year2014[rownames(year2014) %in% "SBR",] ) /2
TCV2014PIKE<-year2014[rownames(year2014) %in% "ZAK",]
XWA2014PIKE<-year2014[rownames(year2014) %in% "PDJ",]
ZWE2014PIKE<-year2014[rownames(year2014) %in% "CHE",]


PIKE2014<-data.frame(AGO2014PIKE,BWA2014PIKE,COD2014PIKE,
                     ETH2014PIKE,KEN2014PIKE,TCV2014PIKE,XWA2014PIKE,ZWE2014PIKE)


###2013
AGO2013PIKE<-year2013[rownames(year2013)%in% "ZBZ",]
BWA2013PIKE<-year2013[rownames(year2013) %in% "CHO",]
COD2013PIKE<-(year2013[rownames(year2013) %in% "GAR",]+ year2013[rownames(year2013) %in% "VIR",] ) /2
ETH2013PIKE<-year2013[rownames(year2013) %in% "BBL",]
KEN2013PIKE<-(year2013[rownames(year2013) %in% "TSV",] + year2013[rownames(year2013) %in% "SBR",] ) /2
TCV2013PIKE<-year2013[rownames(year2013) %in% "ZAK",]
XWA2013PIKE<-year2013[rownames(year2013) %in% "PDJ",]
ZWE2013PIKE<-year2013[rownames(year2013) %in% "CHE",]


PIKE2013<-data.frame(AGO2013PIKE,BWA2013PIKE,COD2013PIKE,
                     ETH2013PIKE,KEN2013PIKE,TCV2013PIKE,XWA2013PIKE,ZWE2013PIKE)


###2012
AGO2012PIKE<-year2012[rownames(year2012)%in% "ZBZ",]
BWA2012PIKE<-year2012[rownames(year2012) %in% "CHO",]
COD2012PIKE<-(year2012[rownames(year2012) %in% "GAR",]+ year2012[rownames(year2012) %in% "VIR",] ) /2
ETH2012PIKE<-year2012[rownames(year2012) %in% "BBL",]
KEN2012PIKE<-(year2012[rownames(year2012) %in% "TSV",] + year2012[rownames(year2012) %in% "SBR",] ) /2
TCV2012PIKE<-year2012[rownames(year2012) %in% "ZAK",]
XWA2012PIKE<-year2012[rownames(year2012) %in% "PDJ",]
ZWE2012PIKE<-year2012[rownames(year2012) %in% "CHE",]


PIKE2012<-data.frame(AGO2012PIKE,BWA2012PIKE,COD2012PIKE,
                     ETH2012PIKE,KEN2012PIKE,TCV2012PIKE,XWA2012PIKE,ZWE2012PIKE)



###2011
AGO2011PIKE<-year2011[rownames(year2011)%in% "ZBZ",]
BWA2011PIKE<-year2011[rownames(year2011) %in% "CHO",]
COD2011PIKE<-(year2011[rownames(year2011) %in% "GAR",]+ year2011[rownames(year2011) %in% "VIR",] ) /2
ETH2011PIKE<-year2011[rownames(year2011) %in% "BBL",]
KEN2011PIKE<-(year2011[rownames(year2011) %in% "TSV",] + year2011[rownames(year2011) %in% "SBR",] ) /2
TCV2011PIKE<-year2011[rownames(year2011) %in% "ZAK",]
XWA2011PIKE<-year2011[rownames(year2011) %in% "PDJ",]
ZWE2011PIKE<-year2011[rownames(year2011) %in% "CHE",]


PIKE2011<-data.frame(AGO2011PIKE,BWA2011PIKE,COD2011PIKE,
                     ETH2011PIKE,KEN2011PIKE,TCV2011PIKE,XWA2011PIKE,ZWE2011PIKE)

tcrf<-tcr

#################################################
###########    spearman  rank         ###########
#################################################

#### correlation loop 2015


core2015<-matrix(ncol=3000,nrow=5)
cors2015<-c(1:3000)

for (i in 1:5){
  
  cr_year<-as.numeric(tcrf[,i+1])
  
  for(k in 1:3000){
    
    pikes2015<-as.numeric(PIKE2015[k,])
    
    cors2015[k]<-cor(pikes2015, cr_year, method="spearman", use="complete.obs")
    
    
  }
  
  core2015[i,]<-cors2015
}


mean(core2015[1,])
mean(core2015[2,])
mean(core2015[3,])
mean(core2015[4,])
mean(core2015[5,])


#### correlation loop 2014

core2014<-matrix(ncol=3000,nrow=5)
cors2014<-c(1:3000)


for (i in 1:5){
  
  cr_year<-as.numeric(tcrf[,i+1])
  
  for(k in 1:3000){
    
    pikes2014<-as.numeric(PIKE2014[k,])
    
    cors2014[k]<-cor(pikes2014,cr_year, method="spearman",  use="complete.obs")
    
    
  }
  
  core2014[i,]<-cors2014
}

core2014

mean(core2014[1,])
mean(core2014[2,])
mean(core2014[3,])
mean(core2014[4,])
mean(core2014[5,])


#### correlation loop 2013

core2013<-matrix(ncol=3000,nrow=5)
cors2013<-c(1:3000)


for (i in 1:5){
  
  cr_year<-as.numeric(tcrf[,i+1])
  
  for(k in 1:3000){
    
    pikes2013<-as.numeric(PIKE2013[k,])
    
    cors2013[k]<-cor(pikes2013,cr_year, method="spearman", use="complete.obs")
    
    
  }
  
  core2013[i,]<-cors2013
}

core2013

mean(core2013[1,])
mean(core2013[2,])
mean(core2013[3,])
mean(core2013[4,])
mean(core2013[5,])

#### correlation loop 2012

core2012<-matrix(ncol=3000,nrow=5)
cors2012<-c(1:3000)


for (i in 1:5){
  
  cr_year<-as.numeric(tcrf[,i+1])
  
  for(k in 1:3000){
    
    pikes2012<-as.numeric(PIKE2012[k,])
    
    cors2012[k]<-cor(pikes2012,cr_year, method="spearman",use="complete.obs")
    
    
  }
  
  core2012[i,]<-cors2012
}

core2012

mean(core2012[1,])
mean(core2012[2,])
mean(core2012[3,])
mean(core2012[4,])
mean(core2012[5,])

####### Correlation for PIKE 2011

core2011<-matrix(ncol=3000,nrow=5)
cors2011<-c(1:3000)


for (i in 1:5){
  
  cr_year<-as.numeric(tcrf[,i+1])
  
  for(k in 1:3000){
    
    pikes2011<-as.numeric(PIKE2011[k,])
    
    cors2011[k]<-cor(pikes2011,cr_year, method="spearman",use="complete.obs")
    
    
  }
  
  core2011[i,]<-cors2011
}

core2011

mean(core2011[1,])
mean(core2011[2,])
mean(core2011[3,])
mean(core2011[4,])
mean(core2011[5,])




### function for 95% CI
upper <- function(x) mean(x) + qnorm(0.975)*(sd(x)/sqrt(length(x)))
lower <- function(x) mean(x) - qnorm(0.975)*(sd(x)/sqrt(length(x)))

#### data frame to store correlation coefficients
mean_correlation<-data.frame(matrix(nrow=5,ncol=5))
colnames(mean_correlation)<-c("Carcass ratio GEC 2015",  
                         "Carcass ratio GEC 2014",
                         "Carcass ratio GEC 2013",
                         "Carcass ratio GEC 2012",
                         "Carcass ratio GEC 2011")

rownames(mean_correlation)<-c("Pike Estimations 2015","Pike Estimations 2014",
                         "Pike Estimations 2013","Pike Estimations 2012",
                         "Pike Estimations 2011")

### fill correlation data frame

for (i in 1:5 ){
    mean_correlation[1,i]<-mean(core2015[i,])
    mean_correlation[2,i]<-mean(core2014[i,])
    mean_correlation[3,i]<-mean(core2013[i,])
    mean_correlation[4,i]<-mean(core2012[i,])
    mean_correlation[5,i]<-mean(core2011[i,])
}
  


####### 0.025CI
#### data frame to store correlation coefficients
low_correlation<-data.frame(matrix(nrow=5,ncol=5))
  
colnames(low_correlation)<-c("cr2015_0.025CI" ,
                             "cr2014_0.025CI" ,
                             "cr2013_0.025CI" ,
                            "cr2012_0.025CI" ,
                            "cr2011_0.025CI")

rownames(low_correlation)<-c("Pike Estimations 2015","Pike Estimations 2014",
                              "Pike Estimations 2013","Pike Estimations 2012",
                              "Pike Estimations 2011")

for (i in 1:5 ){
  low_correlation[1,i]<-lower(core2015[i,])
  low_correlation[2,i]<-lower(core2014[i,])
  low_correlation[3,i]<-lower(core2013[i,])
  low_correlation[4,i]<-lower(core2012[i,])
  low_correlation[5,i]<-lower(core2011[i,])
  }



####0.975CI
#### data frame to store correlation coefficients
up_correlation<-data.frame(matrix(nrow=5,ncol=5))

colnames(up_correlation)<-c("cr2015_0.975CI",
                            "cr2014_0.975CI",
                            "cr2013_0.975CI",
                           "cr2012_0.975CI",
                            "cr2011_0.975CI")

rownames(up_correlation)<-c("Pike Estimations 2015","Pike Estimations 2014",
                             "Pike Estimations 2013","Pike Estimations 2012",
                             "Pike Estimations 2011")


for (i in 1:5){
  up_correlation[1,i]<-upper(core2015[i,])
  up_correlation[2,i]<-upper(core2014[i,]) 
  up_correlation[3,i]<-upper(core2013[i,])
  up_correlation[4,i]<-upper(core2012[i,])
  up_correlation[5,i]<-upper(core2011[i,])
}


#### 

correlation<-data.frame(mean_correlation,low_correlation,up_correlation)




write.csv(correlation, file="/ma_elephant/R/correlation_matrix.csv")




##############################################################
##############################################################
###                                                        ###
###                   Correlation Plot                     ###
###                                                        ###
##############################################################
##############################################################


corrplot(as.matrix(mean_correlation),
         addCoef.col = "black",number.cex=0.75,
         tl.cex=0.75, bg="grey60",outline=TRUE, 
         plotCI="square", addgrid.col="grey20",
         lowCI.mat=as.matrix(low_correlation),
         uppCI.mat=as.matrix(up_correlation),
         tl.col="black",tl.srt=45)