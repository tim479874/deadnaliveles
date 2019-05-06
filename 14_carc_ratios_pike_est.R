#########################
##### load packages #####
#########################


library(dplyr)
library(corrplot)



#########################################################
#### import data table for number of living eles    #####
#########################################################

datale<-read.table("/ma_elephant/R/predictors/yxtable.csv",
                   header=TRUE,sep=",")

### get number of counted elephants per country
eles<- aggregate(datale$COUNT,
                 by = list(datale$Country),
                 FUN = sum)




###### this are the numbers of carcasses per segments/datapoint
#### 2014 and 2015 
carcr<- read.table(file="/ma_elephant/R/HT_c_sum_time.csv", sep=",", header=TRUE)
carcr$X<-NULL


carcx<-split(carcr, carcr$Year)

carc2014<-carc$`2014`
carc2015<-carc$`2015`


carc2014<-carc2014[,c(1,2,3,4,5,7)]
carc2015<-carc2015[,c(1,2,3,4,5,7)]


colnames(carc2014)<-c("ID","EC1","EC2","EC3","EC4","Country")
colnames(carc2015)<-c("ID","EC1","EC2","EC3","EC4","Country")



carctemp<-data.frame(matrix(ncol=6, nrow=9))
carctemp$X1<-c("AGO","BWA", "COD", "ETH", "KENI" ,"KENII","TCD", "XWA" ,"ZWE")
colnames(carctemp)<-c("Country","EC1","EC2","EC3","EC4","eles")


### KEN 2 times because gec took place in 2014 and 2015
for (i in 1:4){
carctemp$eles[i]<-countryratios$eles[i]
}

carctemp$eles[5]<-countryratios$eles[5]
carctemp$eles[6]<-countryratios$eles[5]

for (i in 6:8){
  carctemp$eles[i+1]<-countryratios$eles[i]
}
levels(carc2014$Country)

carc2014$Country<-as.character(carc2014$Country)
carc2015$Country<-as.character(carc2015$Country)
#### rename factor level
levels(carc2014$Country) <-c("AGO","BWA","COD","ETH","KEN","KENI","KENII", "TCD","XWA","ZWE")
levels(carc2015$Country) <-c("AGO","BWA","COD","ETH","KEN","KENI","KENII", "TCD","XWA","ZWE")

carc2014$Country[carc2014$Country=="KEN"]<-c("KENI")
carc2015$Country[carc2015$Country=="KEN"]<-c("KENII")

sum(carc2015$EC1)
sum(carc2014$EC1)

### calculate number of carcasses per country  EC1/2015
ec12015 <- aggregate(carc2015$EC1,
                     by = list(carc2015$Country),
                     FUN = sum)

carctemp[carctemp$Country %in% ec12015$Group.1,]$EC1<-ec12015$x


### calculate number of carcasses per country  EC1/ 2014
ec12014<- aggregate(carc2014$EC1,
                     by = list(carc2014$Country),
                     FUN = sum)

carctemp[carctemp$Country %in% ec12014$Group.1,]$EC1<-ec12014$x



### calculate number of carcasses per country  EC2/2015
ec22015 <- aggregate(carc2015$EC2,
                     by = list(carc2015$Country),
                     FUN = sum)

carctemp[carctemp$Country %in% ec22015$Group.1,]$EC2<-ec22015$x



### calculate number of carcasses per country  EC2/ 2014
ec22014<- aggregate(carc2014$EC2,
                    by = list(carc2014$Country),
                    FUN = sum)

carctemp[carctemp$Country %in% ec22014$Group.1,]$EC2<-ec22014$x




### calculate number of carcasses per country  EC3/2015
ec32015 <- aggregate(carc2015$EC3,
                     by = list(carc2015$Country),
                     FUN = sum)

carctemp[carctemp$Country %in% ec32015$Group.1,]$EC3<-ec32015$x



### calculate number of carcasses per country  EC3/ 2014
ec32014<- aggregate(carc2014$EC3,
                    by = list(carc2014$Country),
                    FUN = sum)

carctemp[carctemp$Country %in% ec32014$Group.1,]$EC3<-ec32014$x


### calculate number of carcasses per country  EC4/2015
ec42015 <- aggregate(carc2015$EC4,
                     by = list(carc2015$Country),
                     FUN = sum)

carctemp[carctemp$Country %in% ec42015$Group.1,]$EC4<-ec42015$x



### calculate number of carcasses per country  EC4/ 2014
ec42014<- aggregate(carc2014$EC4,
                    by = list(carc2014$Country),
                    FUN = sum)

carctemp[carctemp$Country %in% ec42014$Group.1,]$EC4<-ec42014$x



###############################
### temporal carcass ratios ###
###############################



tcr<-data.frame(matrix(ncol=5, nrow=9))
tcr$X1<-c("AGO","BWA", "COD", "ETH", "KENI" ,"KENII","TCD", "XWA" ,"ZWE")
colnames(tcr)<-c("Country","EC1","EC2","EC3","EC4")


for (i in 1:9) {
  tcr$EC1[i]<-carctemp$EC1[i]/sum(carctemp$EC1[i],carctemp$eles[i])
  tcr$EC2[i]<-carctemp$EC2[i]/sum(carctemp$EC2[i],carctemp$eles[i])
  tcr$EC3[i]<-carctemp$EC3[i]/sum(carctemp$EC3[i],carctemp$eles[i])
  tcr$EC4[i]<-carctemp$EC4[i]/sum(carctemp$EC4[i],carctemp$eles[i])
  
  
}



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


#### MIKE Sites which lay inside GEC survey data #######

# AGO -> ZBZ Angola Zambezi closest MIKE site
# BWA -> CHO Botswana
# COD -> GAR VIR Kongo
# ETH -> BBL Tansania
# KEN -> TSV SBR Kenia
# TCD -> ZAK Tschad
# XWA -> PDJ W-Arly-Pendjari
# ZWE -> CHE Simbabwe

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


###2014
ZBZ2014PIKE<-year2014[rownames(year2014)%in% "ZBZ",]
BWA2014PIKE<-year2014[rownames(year2014) %in% "CHO",]
COD2014PIKE<-(year2014[rownames(year2014) %in% "GAR",]+ year2014[rownames(year2014) %in% "VIR",] ) /2
ETH2014PIKE<-year2014[rownames(year2014) %in% "BBL",]
KEN2014PIKE<-(year2014[rownames(year2014) %in% "TSV",] + year2014[rownames(year2014) %in% "SBR",] ) /2
TCV2014PIKE<-year2014[rownames(year2014) %in% "ZAK",]
XWA2014PIKE<-year2014[rownames(year2014) %in% "PDJ",]
ZWE2014PIKE<-year2014[rownames(year2014) %in% "CHE",]

###2013
ZBZ2013PIKE<-year2013[rownames(year2013)%in% "ZBZ",]
BWA2013PIKE<-year2013[rownames(year2013) %in% "CHO",]
COD2013PIKE<-(year2013[rownames(year2013) %in% "GAR",]+ year2013[rownames(year2013) %in% "VIR",] ) /2
ETH2013PIKE<-year2013[rownames(year2013) %in% "BBL",]
KEN2013PIKE<-(year2013[rownames(year2013) %in% "TSV",] + year2013[rownames(year2013) %in% "SBR",] ) /2
TCV2013PIKE<-year2013[rownames(year2013) %in% "ZAK",]
XWA2013PIKE<-year2013[rownames(year2013) %in% "PDJ",]
ZWE2013PIKE<-year2013[rownames(year2013) %in% "CHE",]

###2012
ZBZ2012PIKE<-year2012[rownames(year2012)%in% "ZBZ",]
BWA2012PIKE<-year2012[rownames(year2012) %in% "CHO",]
COD2012PIKE<-(year2012[rownames(year2012) %in% "GAR",]+ year2012[rownames(year2012) %in% "VIR",] ) /2
ETH2012PIKE<-year2012[rownames(year2012) %in% "BBL",]
KEN2012PIKE<-(year2012[rownames(year2012) %in% "TSV",] + year2012[rownames(year2012) %in% "SBR",] ) /2
TCV2012PIKE<-year2012[rownames(year2012) %in% "ZAK",]
XWA2012PIKE<-year2012[rownames(year2012) %in% "PDJ",]
ZWE2012PIKE<-year2012[rownames(year2012) %in% "CHE",]

###2011
AGO2011PIKE<-year2011[rownames(year2011)%in% "ZBZ",]
BWA2011PIKE<-year2011[rownames(year2011) %in% "CHO",]
COD2011PIKE<-(year2011[rownames(year2011) %in% "GAR",]+ year2011[rownames(year2011) %in% "VIR",] ) /2
ETH2011PIKE<-year2011[rownames(year2011) %in% "BBL",]
KEN2011PIKE<-(year2011[rownames(year2011) %in% "TSV",] + year2011[rownames(year2011) %in% "SBR",] ) /2
TCV2011PIKE<-year2011[rownames(year2011) %in% "ZAK",]
XWA2011PIKE<-year2011[rownames(year2011) %in% "PDJ",]
ZWE2011PIKE<-year2011[rownames(year2011) %in% "CHE",]




EC1pike<-data.frame(AGO2015PIKE,BWA2014PIKE,COD2014PIKE,ETH2014PIKE,KEN2014PIKE,KEN2015PIKE,TCV2014PIKE,XWA2015PIKE,ZWE2014PIKE)
EC2pike<-data.frame(AGO2014PIKE,BWA2013PIKE,COD2013PIKE,ETH2013PIKE,KEN2013PIKE,KEN2014PIKE,TCV2013PIKE,XWA2014PIKE,ZWE2013PIKE)
EC3pike<-data.frame(AGO2013PIKE,BWA2012PIKE,COD2012PIKE,ETH2012PIKE,KEN2012PIKE,KEN2013PIKE,TCV2012PIKE,XWA2013PIKE,ZWE2012PIKE)
EC4pike<-data.frame(AGO2012PIKE,BWA2011PIKE,COD2011PIKE,ETH2011PIKE,KEN2011PIKE,KEN2012PIKE,TCV2011PIKE,XWA2012PIKE,ZWE2011PIKE)

tcrf

tcrec<-tcr


  #################################################
###########    spearman  rank   cor      ###########
  #################################################

##########################################
### EC1 : Pike current year correlation ##
##########################################

EC1_cor<-matrix(ncol=3000,nrow=5)
ec1cors<-c(1:3000)

for (i in 1:4){
  
  cr_year<-as.numeric(tcrec[,i+1])
  
  for(k in 1:3000){
    
    ec1pikes<-as.numeric(EC1pike[k,])
    
    ec1cors[k]<-cor(ec1pikes, cr_year, method="spearman", use="pairwise.complete.obs")
    
    
  }
  
  EC1_cor[i,]<-ec1cors
}



mean(EC1_cor[1,])
mean(EC1_cor[2,])
mean(EC1_cor[3,])
mean(EC1_cor[4,])



#################################
### EC2 : Pike y-1 correlation ##
#################################


EC2_cor<-matrix(ncol=3000,nrow=5)
ec2cors<-c(1:3000)

for (i in 1:4){
  
  cr_year<-as.numeric(tcrec[,i+1])
  
  for(k in 1:3000){
    
    ec2pikes<-as.numeric(EC2pike[k,])
    
    ec2cors[k]<-cor(ec2pikes, cr_year, method="spearman", use="pairwise.complete.obs")
    
    
  }
  
  EC2_cor[i,]<-ec2cors
}



mean(EC2_cor[1,])
mean(EC2_cor[2,])
mean(EC2_cor[3,])
mean(EC2_cor[4,])



#################################
### EC3 : Pike y-2 correlation ##
#################################


EC3_cor<-matrix(ncol=3000,nrow=5)
ec3cors<-c(1:3000)

for (i in 1:4){
  
  cr_year<-as.numeric(tcrec[,i+1])
  
  for(k in 1:3000){
    
    ec3pikes<-as.numeric(EC3pike[k,])
    
    ec3cors[k]<-cor(ec3pikes, cr_year, method="spearman", use="pairwise.complete.obs")
    
    
  }
  
  EC3_cor[i,]<-ec3cors
}



mean(EC3_cor[1,])
mean(EC3_cor[2,])
mean(EC3_cor[3,])
mean(EC3_cor[4,])



#################################
### EC4 : Pike y-3 correlation ##
#################################


EC4_cor<-matrix(ncol=3000,nrow=5)
ec4cors<-c(1:3000)

for (i in 1:4){
  
  cr_year<-as.numeric(tcrec[,i+1])
  
  for(k in 1:3000){
    
    ec4pikes<-as.numeric(EC4pike[k,])
    
    ec4cors[k]<-cor(ec4pikes, cr_year, method="spearman", use="pairwise.complete.obs")
    
    
  }
  
  EC4_cor[i,]<-ec4cors
}



mean(EC4_cor[1,])
mean(EC4_cor[2,])
mean(EC4_cor[3,])
mean(EC4_cor[4,])


### function for 95% CI
upper <- function(x) mean(x) + qnorm(0.975)*(sd(x)/sqrt(length(x)))
lower <- function(x) mean(x) - qnorm(0.975)*(sd(x)/sqrt(length(x)))

#### data frame to store correlation coefficients
mean_correlation<-data.frame(matrix(nrow=4,ncol=4))
colnames(mean_correlation)<-c("Carcass ratio GEC EC1",  
                              "Carcass ratio GEC EC2",
                              "Carcass ratio GEC EC3",
                              "Carcass ratio GEC EC4")

rownames(mean_correlation)<-c("PIKE Current Year","PIKE Year -1",
                              "PIKE Year -2","PIKE Year -3")

### fill correlation data frame

for (i in 1:4 ){
  mean_correlation[1,i]<-mean(EC1_cor[i,])
  mean_correlation[2,i]<-mean(EC2_cor[i,])
  mean_correlation[3,i]<-mean(EC3_cor[i,])
  mean_correlation[4,i]<-mean(EC4_cor[i,])
 
}



####### 0.025CI
#### data frame to store correlation coefficients
low_correlation<-data.frame(matrix(nrow=4,ncol=4))

colnames(low_correlation)<-c("EC1_0.025CI" ,
                             "EC2_0.025CI" ,
                             "EC3_0.025CI" ,
                             "EC4_0.025CI" )

rownames(low_correlation)<-c("Pike Current Year","Pike Year-1",
                             "Pike Year-2","Pike Year-3")

for (i in 1:4 ){
  low_correlation[1,i]<-lower(EC1_cor[i,])
  low_correlation[2,i]<-lower(EC2_cor[i,])
  low_correlation[3,i]<-lower(EC3_cor[i,])
  low_correlation[4,i]<-lower(EC4_cor[i,])
}



####0.975CI
#### data frame to store correlation coefficients
up_correlation<-data.frame(matrix(nrow=4,ncol=4))

colnames(up_correlation)<-c("EC1_0.975CI",
                            "EC2_0.975CI",
                            "EC3_0.975CI",
                            "EC4_0.975CI")

rownames(up_correlation)<-c("Pike Current Year","Pike Year-1",
                            "Pike Year-2","Pike Year-3")


for (i in 1:5){
  up_correlation[1,i]<-upper(EC1_cor[i,])
  up_correlation[2,i]<-upper(EC2_cor[i,]) 
  up_correlation[3,i]<-upper(EC3_cor[i,])
  up_correlation[4,i]<-upper(EC4_cor[i,])
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
