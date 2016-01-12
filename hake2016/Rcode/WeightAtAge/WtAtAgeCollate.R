#collate some of the age-weight data

setwd("C:/NOAA2016/Hake/Data")

#acoustic
#Shimada
datUS <- read.csv("AcousticSurvey/BioData/csvFiles/2015_biodata_specimen.csv")
datUS$Sex2 <- "U"; datUS$Sex2[datUS$Sex==1] <- "M"; datUS$Sex2[datUS$Sex==2] <- "F"
haul <- read.csv("AcousticSurvey/BioData/csvFiles/2015_biodata_haul.csv")
datUS <- merge(datUS,haul[,c("Haul","EQ_Date_Time","EQ_Latitude","EQ_Longitude")],by="Haul",all.x=T)
datUS <- data.frame(Source="Acoustic U.S.", Weight_kg=datUS$Weight, 
	                 Sex=datUS$Sex2, Age_yrs=datUS$Age,
	                 Length_cm=datUS$Length, 
	                 Month=as.numeric(unlist(lapply(strsplit(as.character(datUS$EQ_Date_Time),"/"),function(x){x[1]}))),
	                 Year = 2015)


datCAN <- read.csv("AcousticSurvey/BioData/csvFiles/2015_biodata_specimen_CAN.csv")
datCAN$Sex2 <- "U"; datCAN$Sex2[datCAN$Sex==1] <- "M"; datCAN$Sex2[datCAN$Sex==2] <- "F"
haul <- read.csv("AcousticSurvey/BioData/csvFiles/2015_biodata_haul_CAN.csv")
datCAN <- merge(datCAN,haul[,c("Haul","EQ_Date_Time","EQ_Latitude","EQ_Longitude")],by="Haul",all.x=T)
datCAN <- data.frame(Source="Acoustic Canada", Weight_kg=datCAN$Weight, 
	                 Sex=datCAN$Sex2, Age_yrs=datCAN$Age,
	                 Length_cm=datCAN$Length, 
	                 Month=as.numeric(unlist(lapply(strsplit(as.character(datCAN$EQ_Date_Time),"/"),function(x){x[1]}))),
	                 Year = 2015)

dat <- rbind(datUS,datCAN)
dat <- dat[!(is.na(dat$Age_yrs) | is.na(dat$Weight_kg)),] 

tapply(dat$Weight_kg,list(dat$Age_yrs),mean)

#US at sea fishery
load("extractedData/atsea.ages.Rdat") #atsea.ages
tmp <- atsea.ages[!is.na(atsea.ages$AGE)&!is.na(atsea.ages$WEIGHT)&atsea.ages$YEAR==2015,]
tapply(tmp$WEIGHT,list(tmp$AGE),mean)
tmp <- data.frame(Source="ATSEA", Weight_kg=tmp$WEIGHT, Sex=tmp$SEX, Age_yrs=tmp$AGE, Length_cm=tmp$LENGTH, 
	              Month=as.numeric(substring(tmp$HAUL_OFFLOAD_DATE,6,7)), Year=tmp$YEAR)

dat <- rbind(dat,tmp)

#US Shore-based fishery
source("Rcode/functions/Functions.R")
source("Rcode/functions/workupPacFinTablesBDS.R")
load("extractedData/bds.age.Rdat")
load("extractedData/bds.allsp.cluster.Rdat")
load("extractedData/bds.fish.Rdat")
load("extractedData/bds.sp.cluster.Rdat")

bds.fish.worked <- workupPacFinTablesBDS(bds_fish=bds.fish,age_temp=bds.age,sp_cluster=bds.sp.cluster,all_cluster=bds.allsp.cluster)
bds.fish.worked$SEX <- factor(bds.fish.worked$SEX)

tmp <- bds.fish.worked[!is.na(bds.fish.worked$FISH_AGE_YEARS_FINAL)&!is.na(bds.fish.worked$FISH_WEIGHT)&bds.fish.worked$SAMPLE_YEAR==2015,]
tapply(tmp$FISH_WEIGHT*0.453592,list(tmp$FISH_AGE_YEARS_FINAL),mean)
tmp <- data.frame(Source="SHORE", Weight_kg=tmp$FISH_WEIGHT*0.453592, Sex=tmp$SEX, Age_yrs=tmp$FISH_AGE_YEARS_FINAL, Length_cm=tmp$FISH_LENGTH/10, 
	              Month=tmp$SAMPLE_MONTH, Year=tmp$SAMPLE_YEAR)

dat <- rbind(dat,tmp)

write.csv(dat,file="LengthWeightAge/2015data.csv",row.names=F)
