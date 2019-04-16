library(data.table)

#set wd
setwd("~/ma_elephant/R")

datale<-read.table("~/ma_elephant/R/predictors/yxtable.csv",header=TRUE,sep=",")

#adding prevalence per site from data 

sapply(datale, class)

### add binary

datale[,"BIN"]<- ifelse(datale$COUNT==0,0,1)

#sum of detections per Site
prev2<-as.data.frame(xtabs(BIN ~ Site, datale))

#sum of Zeros/nondetection per Site
prev3<-as.data.frame(xtabs(BIN==0 ~ Site, datale))

#merge detections and zeros
prev4<-merge(prev2, prev3, by="Site")

#add internal prevalence per Site
prev4[,"prev_intern"]<-prev4$Freq.x/prev4$Freq.y

#reduce data.frame only to internal prevalence

prev5<-prev4[,c(1,4)]

#add internal prevalence per Site to dataframe

datale<-merge(datale, prev5, by="Site", all.x = TRUE)

        