### code for figures in 2015 hake stock assessment
### modified from Figures.R by Allan Hicks created for 2014 hake assessment

stop("\n  This file should not be sourced!")

# paths on Ian's computers (other folks can add their own statements)
if (system("hostname", intern=TRUE) %in% c("NWCDW01724920","NWCLW01724829","ian-THINK") ){
  hakedir <- "C:/SS/hake/Hake_2015/"
  rDir    <- "c:/GitHub/hakeAssess/hake2015/Rcode/"
  figDir  <- file.path(hakedir,"WriteUp/Figures999")
  SSdir   <- file.path(hakedir, "Models")
  doMaps  <- FALSE
}

#source("C:/NOAA2014/Hake/WriteUp/Rcode/WriteUpFunctions.R")
source(file.path(rDir, "WriteUpFunctions.R"))

#devtools::install_github("nwfsc-assess/nwfscSurvey")
#devtools::install_github("r4ss/r4ss")
library(nwfscSurvey)
library(date)
library(r4ss)

# load more packages and get mapping data only if map will be updated
if(doMaps){
  data(westCoastLL)
  data(WCstatesInlandPBS)
  devtools::install_github("nwfsc-assess/nwfscMapping")
  library(nwfscMapping)
  library(PBSmapping)

  source("C:/Mapping/WestCoastMapping.R")
  LMEoffshore <- importShapefile("C:/Mapping/Shapefiles/LME66_Offshore/LME66_Offshore.shp",readDBF=T)
  LME <- importShapefile("C:/Mapping/Shapefiles/LME66/LME66.shp",readDBF=T)
  province <- importShapefile("C:/Mapping/Shapefiles/province/province.shp",readDBF=T)
  alberta <- attributes(province)$PolyData[attributes(province)$PolyData$NAME == "Alberta","PID"]
  CCLME <- attributes(LME)$PolyData[attributes(LME)$PolyData$LME_NAME == "California Current","PID"]
  GOALME <- attributes(LME)$PolyData[attributes(LME)$PolyData$LME_NAME == "Gulf of Alaska","PID"]
  data(nepacLL)
}

# important stuff to load for any plotting
base <- SS_output(dir=file.path(SSdir,"2015hake_basePreSRG"))
#mcmc <- SSgetMCMC(dir=file.path(SSdir,"2015hake_basePreSRG_mcmc12e6"),writecsv=FALSE)
mcmc <- SSgetMCMC(dir=file.path(SSdir,"2015hake_basePreSRG_mcmc12e6"),writecsv=FALSE,
                  burnin=401,thin=2)
base$mcmc <- data.frame(mcmc$model1)
# data file for use in a few places
dat <- SS_readdat(file.path(base$inputs$dir, "2015Hake_data.ss"))
endYr <- 2015
lastCatchYr <- endYr-1



spb2015 <- base$mcmc[,grep("SPB",names(base$mcmc))]/2e6
# add more years to list each time (or generalize in the future)
spb2015 <- spb2015[,!names(spb2015) %in% c("SPB_Virgin",
                                           paste0("SPB_",endYr+1:20))]
slower <- apply(spb2015,2,quantile,prob=0.025)   #hard-wired probability
smed   <- apply(spb2015,2,quantile,prob=0.5)   #hard-wired probability
supper <- apply(spb2015,2,quantile,prob=0.975)   #hard-wired probability

depl2015 <- t(apply(spb2015,1,function(x){x/x[1]}))[,-1]
dlower <- apply(depl2015,2,quantile,prob=0.025)   #hard-wired probability
dmed   <- apply(depl2015,2,quantile,prob=0.5)   #hard-wired probability
dupper <- apply(depl2015,2,quantile,prob=0.975)   #hard-wired probability

recr2015 <- base$mcmc[,grep("Recr_",names(base$mcmc))]/1e6
recr2015 <- recr2015[,-grep("Fore",names(recr2015))]
yrs <- unlist(lapply(strsplit(names(recr2015),"_"),function(x){x[2]}))
recr2015 <- recr2015[,yrs%in%c("Virgin",1966:2015)]
yrs <- as.numeric(unlist(lapply(strsplit(names(recr2015),"_"),function(x){x[2]})))
rmed   <- apply(recr2015,2,quantile,prob=0.5)   #hard-wired probability
rmean   <- apply(recr2015,2,mean)   #hard-wired probability
rlower <- apply(recr2015,2,quantile,prob=0.025)   #hard-wired probability
rupper <- apply(recr2015,2,quantile,prob=0.975)   #hard-wired probability


if(doMaps){
  ##########################################################################
  ## Map of area
  portLats <- read.csv("Data/Rcode/portLats.csv")
  theCities <- c("Newport","Westport","Astoria","Eureka","Charleston (Coos Bay)")
  theCities <- portLats[portLats$Name%in%theCities,]
  doPNG <- T
  doTIFF <- F
  ht <- 10; wd<- 6.5
  if(doPNG) {png(filename = paste(figDir,"overviewMap.png",sep="\\"), width = wd, height = ht,units="in",res=300, pointsize = 11)}
  if(doTIFF) {tiff(filename = paste(figDir,"overviewMap.tiff",sep="\\"), width = wd, height = ht,units="in",res=300, pointsize = 11)}
  if(!doPNG & !doTIFF) {windows(height=ht,width=wd)}
  par(mfrow=c(1,1),mar=c(4,4,0,0)+0.1,las=1)
  plotMap(westCoastLL, tck = c(-0.02), xlim=c(-140,-113.0), ylim=c(29.9,59.1),col=gray(0.8))
  addLines(WCstatesInlandPBS)
  map('state',add=T,region=c("Idaho","Montana","Nevada","Arizona"))
  addLines(province,polyProps=data.frame(PID=alberta))
  addLines(province,polyProps=data.frame(PID=12,SID=c(123)))
  addLines(LMEoffshore,polyProps=data.frame(PID=76)) #foudn this by x <- addLines(LMEoffshore) then looking at x
  addLines(LMEoffshore,polyProps=data.frame(PID=169))
  points(-theCities$Lon,theCities$Lat,pch=16,col=gray(0.4))
  text(-theCities$Lon,theCities$Lat,theCities$Name,pos=4,cex=0.7,col=gray(0.4))

  text(-123,56,"BC",cex=1.2)
  text(-120,47.1,"WA",cex=1.1)
  text(-120,44.3,"OR",cex=1.2)
  text(-118.8,35.9,"CA",cex=1.2)
  text(-122.8,51,"Strait of\nGeorgia",adj=0,cex=0.7)
  arrows(-122.8,51,-123.55,49.67,length=0.05)
  text(-120.7,48.4,"Puget\nSound",adj=0,cex=0.7)
  arrows(-120.7,48.4,-122.1,47.7,length=0.05)
  text(-133.2,52,"Haida\nGwaii",adj=1,cex=0.7)
  arrows(-133.2,52,-132.2,52.5,length=0.05)
  text(-117.8,30.5,"Baja\nCalifornia",adj=1,cex=0.7)
  arrows(-117.8,30.5,-115.5,30.5,length=0.05)
  text(-131.2,58,"SE\nAlaska",adj=0,cex=0.7)
  arrows(-131.2,58,-132.3,57.5,length=0.05)
  text(-128.3,49.9,"Vancouver\nIsland",adj=1,cex=0.7)
  arrows(-128.3,49.9,-127.4,49.8,length=0.05)
  text(-129.5,51.6,"Queen\nCharlotte\nSound",adj=0.5,cex=0.6)
  text(-133.5,54.5,"Dixon Entrance",adj=1,cex=0.7)
  arrows(-133.5,54.45,-132.5,54.45,length=0.05)
  text(-126.2,48.5,"Strait of\nJuan de Fuca",adj=1,cex=0.7)
  arrows(-126.2,48.5,-124.7,48.5,length=0.05)

  text(-128,39,"California Current LME",cex=1.4,srt=285)
  text(-138,56.8,"Gulf Of Alaska\nLME",cex=0.9,srt=300)
  if(doPNG | doTIFF) {dev.off()}

}

#######################################
#Catch plot

###### Allan will have updated figure for 2015

## catches <- read.csv("Data/Catches/2014_Hake_Catches.csv")
## catches[is.na(catches)] <- 0
## #catch <- catches[,c(6,3,7,4,2,5,8)]
## catch <- catches[,c(5,3,6,4,7,4,2,8,9)]
## catch <- catches[,c("US_foreign","CAN_forgn","US_JV","CAN_JV","CAN_domes","atSea_US_MS","atSea_US_CP","US_shore")]

## doPNG <- T
## ht <- 4; wd<- 7
## if(doPNG) {png("WriteUp/Figures/catches.png",height=ht,width=wd,pointsize=10,units="in",res=300)}
## if(!doPNG) {windows(height=ht,width=wd)}
## par(las=1,mar=c(4, 4, 4, 2) + 0.1,cex.axis=0.9)
## cols<- c(rgb(70,130,180,maxColorValue = 255),rgb(128,0,0,maxColorValue = 255),rgb(218,165,32,maxColorValue = 255),rgb(107,142,35,maxColorValue = 255),rgb(0,0,205,maxColorValue = 255),rgb(72,61,139,maxColorValue = 255),rgb(200,200,220,maxColorValue = 255),rgb(0.5,0.5,0.5))
## tmp <- barplot(t(as.matrix(catch))/1000,beside=F,names=catches[,1],
##         col=cols,xlab="Year",ylab="",cex.lab=1,xaxs="i",mgp=c(2.2,1,0))
## axis(1,at=tmp,label=F,line=-0.12)
## grid(NA,NULL,lty=1,lwd = 1)
## mtext("Catch (thousand mt)",side=2,line=2.8,las=0,cex=1.3)
## barplot(t(as.matrix(catch))/1000,beside=F,names=catches[,1],
##         legend.text=c("U.S. Foreign","Canadian Foreign","U.S. Joint-Venture","Canadian Joint-Venture","Canadian Domestic","U.S. MS","U.S. CP","U.S. Shore-based"),
##         col=cols,
##         args.legend=list(x=57.5,y=430,bg="white",horiz=F,xpd=NA,cex=0.83,ncol=4),xlab="Year",ylab="",cex.lab=1,xaxs="i",add=T,mgp=c(2.2,1,0))
## if(doPNG){dev.off()}
## #run barplot a second time to overwrite grid lines



#################################
# Acoustic survey estimates
# Acoustic survey
# Biomass estimate
#
# NOTE: not updated for 2015 document because there was no new survey
#
tmp <- readLines("Models/2014hake_21_TVselex1991start/2014Hake_data.ss")
ind <- grep("Number of index observations",tmp)
numObs <- as.numeric(strsplit(tmp[ind],"#")[[1]][1])
ind <- grep("Acoustic survey",tmp)[1]+1
ind <- ind:(ind+numObs-1)
ests <- strsplit(gsub("\t"," ",tmp[ind])," +")
ests <- t(as.data.frame(lapply(ests,function(x){as.numeric(x[1:5])})))
dimnames(ests) <- list(NULL,c("Year","seas","index","obs","SElog"))
ests <- as.data.frame(ests)
tmpSE <- ests[ests$Year==2009,"SElog"]
ests[ests$Year==2009,"SElog"] <- 0.0682 #se without squid inflation
ests$lo <- exp(log(ests$obs)-1.96*ests$SElog)
ests$hi <- exp(log(ests$obs)+1.96*ests$SElog)
ests$value <- ests$obs

ests2 <- ests
ests2$SElog <- NA
ests2[ests2$Year==2009,"SElog"] <- tmpSE #se without squid inflation
ests2$lo <- exp(log(ests2$obs)-1.96*ests2$SElog)
ests2$hi <- exp(log(ests2$obs)+1.96*ests2$SElog)
ests2$value <- ests2$obs

doPNG <- T
ht<-5;wd=8
if(doPNG) {png("WriteUp/Figures/acousticBio.png",height=ht,width=wd,pointsize=10,units="in",res=300)}
if(!doPNG) {windows(height=ht,width=wd)}
par(las=1,mar=c(5, 4, 1, 1) + 0.1,cex.axis=0.9)
plotBars.fn(ests2$Year,ests2,scalar=1e6,ylim=c(0,3),pch=20,xlab="Year",ylab="Biomass Index Estimate (million t)",cex=1.5,las=1,gap=0.05,xaxt="n",ciLwd=3,ciCol=rgb(0,0,1,0.6))
plotBars.fn(ests$Year,ests,scalar=1e6,ylim=c(0,3),pch=20,add=T,cex=1.5,las=1,gap=0.05,xaxt="n",ciLwd=3,ciCol=gray(0.2))
axis(1,at=ests$Year,cex.axis=0.8)
if(doPNG) {dev.off()}

#################################
# Acoustic survey fits
# Acoustic survey
# Biomass estimate

doPNG <- T
ht<-4;wd=6.5
if(doPNG) {png(file.path(figDir,"acousticBioFit.png"),height=ht,width=wd,pointsize=10,units="in",res=300)}
if(!doPNG) {windows(height=ht,width=wd)}
par(las=1,mar=c(5, 4, 1, 1) + 0.1,cex.axis=0.9)
plot(0, type='n', xlim=c(1994,2014), xaxs='i', ylim=c(0,5.5e6), yaxs='i', axes=FALSE,
     xlab="Year",ylab="Biomass index (million t)")
cpue <- dat$CPUE[dat$CPUE$index > 0,]
segments(x0 = cpue$year,
         y0=qlnorm(.025,meanlog=log(cpue$ob),sdlog=cpue$se_log),
         y1=qlnorm(.975,meanlog=log(cpue$ob),sdlog=cpue$se_log),
         lwd=3, lend=1)
SSplotIndices(base, subplot=2, add=TRUE, col3=rgb(1,0,0,.7))
#plotBars.fn(ests2$Year,ests2,scalar=1e6,ylim=c(0,3),pch=20,xlab="Year",ylab="Biomass Index Estimate (million mt)",cex=1.5,las=1,gap=0.05,xaxt="n",ciLwd=3,ciCol=rgb(0,0,1,0.6))
#plotBars.fn(ests$Year,ests,scalar=1e6,ylim=c(0,3),pch=20,add=T,cex=1.5,las=1,gap=0.05,xaxt="n",ciLwd=3,ciCol=gray(0.2))
axis(1, at=base$cpue$Yr[base$cpue$Use==1], cex.axis=0.8, tcl=-0.6)
axis(1, at=1990:2020, lab=rep("",length(1990:2020)), cex.axis=0.8, tcl=-0.3)
box()
axis(2, at=(0:5)*1e6, lab=0:5, las=1) 
if(doPNG) {dev.off()}

###############################################################################################################

doPNG <- TRUE

ht <- 3.75; wd<- 6.5
if(doPNG) {png(file.path(figDir, "biomass.png"),
               height=ht,width=wd,pointsize=10,units="in",res=300)}
if(!doPNG) {windows(width=wd,height=ht)}
yrs <- 1964:endYr
par(mfrow=c(1,1),las=1,mar=c(3.5,3.5,1,1))
plot(yrs[-c(1,2)],c(smed[2:length(smed)]),type="l",lwd=3,ylim=c(0,max(supper)+0.1),
     xlab="Year",ylab="Female Spawning Biomass (million t)",
     xlim=range(yrs),cex.axis=0.9,cex.lab=1,mgp=c(2.3,1,0),yaxs="i")
axis(1,at=endYr, cex.axis=0.9)
axis(1,at=yrs[1], labels="Unfished\nequilibrium", cex.axis=0.9, mgp=c(3,1.5,0))
points(yrs[1],smed[1],pch=16)
arrows(yrs[1],slower[1],yrs[1],supper[1],angle=90,code=3,length=0.06)
addpoly(yrs[-c(1,2)],slower[-1],supper[-1],"black")
if(doPNG) {dev.off()}

ht <- 3.75; wd<- 6.5
if(doPNG) {png(file.path(figDir, "biomassColor.png"),
               height=ht,width=wd,pointsize=10,units="in",res=300)}
if(!doPNG) {windows(width=wd,height=ht)}
yrs <- 1964:endYr
par(mfrow=c(1,1),las=1,mar=c(3.5,3.5,1,1))
plot(yrs[-c(1,2)],c(smed[2:length(smed)]),type="l",lwd=3,ylim=c(0,max(supper)+0.1),xlab="Year",ylab="Female Spawning Biomass (million t)",xlim=range(yrs),cex.axis=0.9,cex.lab=1,mgp=c(2.3,1,0),yaxs="i")
points(yrs[1],smed[1],pch=16)
arrows(yrs[1],slower[1],yrs[1],supper[1],angle=90,code=3,length=0.06,col=rgb(0,0,1,0.5))
axis(1,at=endYr, cex.axis=0.9)
axis(1,at=yrs[1], labels="Unfished\nequilibrium", cex.axis=0.9, mgp=c(3,1.5,0))
addpoly(yrs[-c(1,2)],slower[-1],supper[-1],"blue")
if(doPNG) {dev.off()}



ht <- 3.75; wd<- 6.5
if(doPNG) {png(file.path(figDir, "relativeSpawningBio.png"),
               height=ht,width=wd,pointsize=10,units="in",res=300)}
if(!doPNG) {windows(width=wd,height=ht)}
par(mfrow=c(1,1),las=1,mar=c(3.5,3.5,1,1))
yrs <- 1966:endYr
plot(yrs,dmed,type="l",lwd=3,ylim=c(0,1.1*max(dupper)),xlab="Year",
     ylab=expression(paste("Relative spawning biomass",~~~(italic(B[t])/italic(B)[0]))),
     xlim=range(yrs),cex.axis=0.9,
     cex.lab=1,mgp=c(2.3,1,0),yaxs="i")
addpoly(yrs,dlower,dupper,"black")
abline(h=c(0.1,0.4,1),lty=2,col=gray(0.5))
axis(2,at=c(0.1,0.4),cex.axis=0.8)
axis(1,at=endYr,cex.axis=0.9)
mtext("Year",side=1,cex=1.1,outer=T,line=1.4)
if(doPNG) {dev.off()}

ht <- 3.75; wd<- 6.5
if(doPNG) {png(file.path(figDir,"relativeSpawningBioColor.png"),
               height=ht,width=wd,pointsize=10,units="in",res=300)}
if(!doPNG) {windows(width=wd,height=ht)}
par(mfrow=c(1,1),las=1,mar=c(3.5,3.5,1,1))
yrs <- 1966:endYr
plot(yrs,dmed,type="l",lwd=3,ylim=c(0,1.1*max(dupper)),xlab="Year",
     ylab=expression(paste("Relative spawning biomass",~~~(italic(B[t])/italic(B)[0]))),
     xlim=range(yrs),cex.axis=0.9,
     cex.lab=1,mgp=c(2.3,1,0),yaxs="i")
addpoly(yrs,dlower,dupper,"blue")
abline(h=c(0.1,0.4,1),lty=2,col=gray(0.5))
axis(2,at=c(0.1,0.4),cex.axis=0.8)
axis(1,at=endYr,cex.axis=0.9)
mtext("Year",side=1,cex=1.1,outer=T,line=1.4)
if(doPNG) {dev.off()}


#Recruitment

y <- data.frame(value=rmed[-1],lo=rlower[-1],hi=rupper[-1])
#y <- data.frame(value=c(rmed[1],NA,rmed[-1]),lo=c(rlower[1],NA,rlower[-1]),hi=c(rupper[1],NA,rupper[-1]))

ht <- 3.25; wd<- 6.5
yrs <- 1964:endYr
if(doPNG) {png(file.path(figDir,"recruitment.png"),height=ht,width=wd,pointsize=10,units="in",res=300)}
if(!doPNG) {windows(width=wd,height=ht)}
par(mfrow=c(1,1),las=1,mar=c(3.5,3.5,1,1),oma=c(0,0,0,0))
plotBars.fn(yrs[-c(1,2)],y,scalar=1,ylim=c(0,35),pch=20,xlab="Year",ylab="Age 0 recruits (billions)",cex=0.8,las=1,gap=0,xaxt="n",ciLwd=1,ciCol=rgb(0,0,1,0.5),mgp=c(2.3,1,0),xlim=range(yrs))
plotBars.fn(yrs[1],data.frame(value=rmed[1],lo=rlower[1],hi=rupper[1]),scalar=1,pch=4,cex=0.8,las=1,gap=0,ciLwd=1,ciCol=rgb(0,0,1,0.5),add=T)
legend("topleft","Unfished equilibrium recruitment",pch=4,bty="n")
axis(1,at=seq(1965,2015,5))
abline(h=0,col=rgb(0,0,0,0.5))
if(doPNG){dev.off()}

ht <- 3.25; wd<- 6.5
yrs <- 1964:endYr
if(doPNG) {png(file.path(figDir,"recruitmentWithMean.png"),height=ht,width=wd,pointsize=10,units="in",res=300)}
if(!doPNG) {windows(width=wd,height=ht)}
par(mfrow=c(1,1),las=1,mar=c(3.5,3.5,1,1),oma=c(0,0,0,0))
plotBars.fn(yrs[-c(1,2)],y,scalar=1,ylim=c(0,35),pch=20,
            xlab="Year",ylab="Age 0 recruits (billions)",cex=0.8,las=1,gap=0,
            xaxt="n",ciLwd=1,ciCol=rgb(0,0,1,0.5),mgp=c(2.3,1,0),
            xlim=range(yrs[-c(1,2)]))
points(yrs[-c(1,2)],rmean[-1],pch=4,cex=0.8)
#R0
abline(h=rmed[1],lty=2,col=rgb(0,0,0,0.5))
polygon(c(0,0,max(yrs+10),max(yrs)+10),c(rlower[1],rupper[1],rupper[1],rlower[1]),col=rgb(0,0,0,0.1),lty=3)
#plotBars.fn(yrs[1]-3,data.frame(value=rmed[1],lo=rlower[1],hi=rupper[1]),scalar=1,pch=4,cex=0.8,las=1,gap=0,ciLwd=1,ciCol=rgb(0,0,1,0.5),add=T)
#legend("topleft","Unfished equilibrium recruitment",pch=4,bty="n")
axis(1,at=seq(1965,2015,5))
axis(2,at=rmed[1],label=expression(italic(R)[0]),cex.axis=0.7,mgp=c(1,0.3,0),tcl=-0.2)
abline(h=0,col=rgb(0,0,0,0.5))
#arrows(2010,30.9,2010,31,length=0.05,col=rgb(0,0,1,0.5))
if(doPNG){dev.off()}

#Fishing Intensity
spr2015 <- base$mcmc[,grep("SPRratio_",names(base$mcmc))]
yrs <- unlist(lapply(strsplit(names(spr2015),"_"),function(x){x[2]}))
spr2015 <- spr2015[,yrs%in%c(1966:lastCatchYr)]
yrs <- as.numeric(unlist(lapply(strsplit(names(spr2015),"_"),function(x){x[2]})))
pmed   <- apply(spr2015,2,quantile,prob=0.5)   #hard-wired probability
plower <- apply(spr2015,2,quantile,prob=0.025)   #hard-wired probability
pupper <- apply(spr2015,2,quantile,prob=0.975)   #hard-wired probability

y <- data.frame(value=pmed,lo=plower,hi=pupper)

ht <- 3.25; wd<- 6.5
yrs <- 1966:lastCatchYr
if(doPNG) {png(file.path(figDir,"SPRratio.png"),height=ht,width=wd,pointsize=10,units="in",res=300)}
if(!doPNG) {windows(width=wd,height=ht)}
par(mfrow=c(1,1),las=1,mar=c(3.5,3.5,1,1),oma=c(0,0,0,0))
plotBars.fn(yrs,y,scalar=1,ylim=c(0,1.3),pch=20,
            #xlab="Year",ylab="Fishing intensity (1-SPR)/(1-SPR_40%)",
            xlab="Year",
            ylab=expression(paste("Fishing intensity",~~(1-italic(SPR))/(1-italic(SPR)['40%']))),
            cex=0.8,las=1,gap=0.02,xaxt="n",ciLwd=1,ciCol=rgb(0,0,1,0.5),
            mgp=c(2.3,1,0),xlim=range(yrs),yaxs="i")
axis(1,at=c(seq(1970,endYr-1,5), endYr-1))
axis(1,at=1966:lastCatchYr, lab=rep("",length(1966:lastCatchYr)), tcl=-0.3)
#axis(1,at=seq(1965,lastCatchYr,2))
abline(h=1,col=rgb(0,0,0,0.4))
text(1969.9,1.05,"Management Target",cex=0.8,col=rgb(0,0,0,0.4))
if(doPNG){dev.off()}


#Expoitation rate
f2014 <- base$mcmc[,grep("F_",names(base$mcmc))]
yrs <- unlist(lapply(strsplit(names(f2014),"_"),function(x){x[2]}))
f2014 <- f2014[,yrs%in%c(1966:lastCatchYr)]
yrs <- as.numeric(unlist(lapply(strsplit(names(f2014),"_"),function(x){x[2]})))
fmed   <- apply(f2014,2,quantile,prob=0.5)   #hard-wired probability
flower <- apply(f2014,2,quantile,prob=0.025)   #hard-wired probability
fupper <- apply(f2014,2,quantile,prob=0.975)   #hard-wired probability

y <- data.frame(value=fmed,lo=flower,hi=fupper)

ht <- 3.25; wd<- 6.5
yrs <- 1966:lastCatchYr
if(doPNG) {png(file.path(figDir,"ExpRate.png"),height=ht,width=wd,pointsize=10,units="in",res=300)}
if(!doPNG) {windows(width=wd,height=ht)}
par(mfrow=c(1,1),las=1,mar=c(3.5,3.5,1,1),oma=c(0,0,0,0))
plotBars.fn(yrs,y,scalar=1,ylim=c(0,0.4),pch=20,xlab="Year",ylab="Exploitation fraction",cex=0.8,las=1,gap=0.005,xaxt="n",ciLwd=1,ciCol=rgb(0,0,1,0.5),mgp=c(2.3,1,0),xlim=range(yrs),yaxs="i")
#axis(1,at=seq(1965,lastCatchYr,2))
axis(1,at=c(seq(1970,endYr-1,5), endYr-1))
axis(1,at=1966:lastCatchYr, lab=rep("",length(1966:lastCatchYr)), tcl=-0.3)
if(doPNG){dev.off()}


#phase plot (needs values from phase plots above)
yrs <- unlist(lapply(strsplit(names(smed),"_"),function(x){x[2]}))
sb40 <- smed["SPB_Initial"]*0.4
sb0 <- smed["SPB_Initial"]
sb <- smed[yrs%in%c(1966:lastCatchYr)]/sb0
sb.hi <- supper[yrs%in% lastCatchYr]/sb0
sb.lo <- slower[yrs%in% lastCatchYr]/sb0
yrs <- unlist(lapply(strsplit(names(pmed),"_"),function(x){x[2]}))
spr <- pmed[yrs%in%c(1966:lastCatchYr)]
spr.hi <- pupper[yrs%in% lastCatchYr]
spr.lo <- plower[yrs%in% lastCatchYr]


phase.fn <- function(){
  par(mfrow=c(1,1),las=1,mar=c(3.6,3.6,1,1),oma=c(0,0,0,0))
  plot(sb,spr,type="n",pch=20,xlim=c(0,1.3),ylim=c(0,1.3),
       #xlab="Spawning depletion (SB/SB0)",
       xlab=expression(paste("Relative spawning biomass",~~~(italic(B[t])/italic(B)[0]))),
       #ylab="Relative fishing intensity (1-SPR)/(1-SPR_40%)",
       ylab=expression(paste("Relative fishing intensity",~~(1-italic(SPR))/(1-italic(SPR)['40%']))),
       xaxs="i",yaxs="i",mgp=c(2.4,1,0))
  colvec <- rev(rich.colors.short(n=length(sb))[-1])
  arrows(sb[-length(sb)],spr[-length(spr)],sb[-1],spr[-1],length=0.09,
         #col=rgb(0,0,0,0.4))
         col=colvec)
  points(sb,spr,type="p",pch=20)
  points(sb[length(sb)],spr[length(spr)],pch=16,col=1,cex=1.2)
  points(sb[1],spr[1],pch=16,col=1,cex=1.2)
  text(sb[1],spr[1]-0.025,"1966",cex=0.6,pos=2,offset=0.15)
  segments(sb[length(sb)],spr.lo,sb[length(sb)],spr.hi,col=rgb(0,0,0,0.5))
  segments(sb.lo,spr[length(spr)],sb.hi,spr[length(spr)],col=rgb(0,0,0,0.5))
  text(sb[length(sb)],spr[length(spr)]+0.045,lastCatchYr,pos=4,cex=0.6)
  #abline(h=1,v=1,lty=2,col=rgb(0,0,0,0.4))
  abline(h=1,v=c(0.1,0.4),lty=2,col=rgb(0,0,0,0.4))
}
ht <- 6.5; wd<- 6.5
if(doPNG) {png(file.path(figDir,"phasePlot_tall.png"),height=ht,width=wd,pointsize=10,units="in",res=300)}
if(!doPNG) {windows(width=wd,height=ht)}
phase.fn()
if(doPNG){dev.off()}
ht <- 4; wd<- 6.5
if(doPNG) {png(file.path(figDir,"phasePlot_short.png"),height=ht,width=wd,pointsize=10,units="in",res=300)}
if(!doPNG) {windows(width=wd,height=ht)}
phase.fn()
if(doPNG){dev.off()}


#### NOTE from Ian to himself (1/29/2015 5pm): skipping forecast stuff for now
###############3
## Forecasts    
models <- paste(SSdir,"2014hake_21_decisionTableRuns",c("9_2014hake_21_default","7_2014hake_21_stableCatch","5_2014hake_21_375K","1_2014hake_21_0"),sep="/")
tmp <- SSgetMCMC(dir=models,writecsv=F)
doPNG <- T
mymodels <- list(base,base,base,base); modelnames <- c("Default: 872,424 t","Stable Catch: 727,000 t","375,000 t","No Fishing")
mysummary <- SSsummarize(mymodels)
mysummary$mcmc <- tmp
ht <- 3.25; wd<- 6.5
if(doPNG) {png(file.path(figDir,"forecasts.png"),height=ht,width=wd,pointsize=10,units="in",res=300)}
if(!doPNG) {windows(width=wd,height=ht)} 
par(mar=c(4.5,4,1,1))
SSplotComparisons(mysummary, legendlabels=modelnames,endyr=2016,densitynames=c("Bratio_2013"),new=F,minbthresh=0,subplots=4,plot=T,mcmc=rep(T,4),xlim=c(2005,2016),legendloc="topleft",
        labels=c("Year","Spawning biomass (t)","Spawning depletion","Age-0 recruits (1,000s)","Recruitment deviations","Index",
                 "Log index","SPR ratio","Density","",""),btarg=-0.4,staggerpoints= 1990, spacepoints=200)
abline(h=c(0.1,0.4),lty=2,col="grey")
axis(2,at=c(0.1,0.4),las=1,cex.axis=0.8)
if(doPNG){dev.off()}





#draw a figure of metrics
metrics <- read.csv("WriteUp/Tables/metricsTableRisk2015.csv")
names(metrics)[1:2] <- c("Model","Catch")
metrics$Catch <- metrics$Catch/1e3
metrics <- metrics[,-1]
metrics <- metrics[metrics$Catch<245 | metrics$Catch>255,]
catches <- metrics$Catch

doPNG <- T
ht <- 3.75; wd<- 6.5
if(doPNG) {png(file.path(figDir,"metrics2015.png"),height=ht,width=wd,pointsize=10,units="in",res=300)}
if(!doPNG) {windows(width=wd,height=ht)}
par(mfrow=c(1,1),las=1,mar=c(3.6,3.6,1,1),oma=c(0,0,0,0),mgp=c(2.5,1,0))
plot(metrics$Catch,metrics[,2],ylim=c(0,1),xaxt="n",ylab="Probability",xlab="Catch in 2014 ('000 t)",type="b",lty=2,pch=16)
lines(metrics$Catch,metrics[,3],col="blue",type="b",lty=2,pch=17)
lines(metrics$Catch,metrics[,4],col="green",type="b",lty=2,pch=17)
lines(metrics$Catch,metrics[,5],col="orange",type="b",lty=2,pch=17)
lines(metrics$Catch,metrics[,6],col="red",type="b",lty=2,pch=15)
lines(metrics$Catch,metrics[,7],col="tan",type="b",lty=2,pch=18)
abline(h=0.5,lty=2,lwd=1,col="grey")
legend("topleft",c("P(B2015 < B2014): Stock declines in 2015","P(2014 Fishing Intensity > Target of 40%)","P(C2015 < C2014): F40% catch declines in 2015","P(B2015 < B40%)","P(B2015 < B25%)","P(B2015 < B10%)"),col=c("black","red","tan","blue","green","orange"),lty=1,lwd=2,pch=c(16,15,18,17,17,17),cex=0.7)
axis(1,at=round(metrics$Catch,0),cex.axis=0.9,las=2)
if(doPNG){dev.off()}

metrics <- read.csv("WriteUp/Tables/metricsTableRisk2016.csv")
names(metrics)[1:2] <- c("Model","Catch")
metrics <- metrics[order(metrics$Catch),]
metrics$Catch <- metrics$Catch/1e3
metrics <- metrics[,-1]
metrics <- metrics[metrics$Catch<245 | metrics$Catch>255,]
catches <- metrics$Catch
doPNG <- T
ht <- 3.75; wd<- 6.5
if(doPNG) {png(file.path(figDir,"metrics2016.png"),height=ht,width=wd,pointsize=10,units="in",res=300)}
if(!doPNG) {windows(width=wd,height=ht)}
par(mfrow=c(1,1),las=1,mar=c(3.6,3.6,1,1),oma=c(0,0,0,0),mgp=c(2.5,1,0))
plot(metrics$Catch,metrics[,2],ylim=c(0,1),xaxt="n",ylab="Probability",xlab="Catch in 2015 ('000 t)",type="b",lty=2,pch=16)
lines(metrics$Catch,metrics[,3],col="blue",type="b",lty=2,pch=17)
lines(metrics$Catch,metrics[,4],col="green",type="b",lty=2,pch=17)
lines(metrics$Catch,metrics[,5],col="orange",type="b",lty=2,pch=17)
lines(metrics$Catch,metrics[,6],col="red",type="b",lty=2,pch=15)
lines(metrics$Catch,metrics[,7],col="tan",type="b",lty=2,pch=18)
abline(h=0.5,lty=2,lwd=1,col="grey")
legend("topleft",c("P(B2016 < B2015): Stock declines in 2016","P(2015 Fishing Intensity > Target of 40%)","P(C2016 < C2015): F40% catch declines in 2016","P(B2016 < B40%)","P(B2016 < B25%)","P(B2016 < B10%)"),col=c("black","red","tan","blue","green","orange"),lty=1,lwd=2,pch=c(16,15,18,17,17,17),cex=0.7)
axis(1,at=round(metrics$Catch,0),cex.axis=0.9,las=2)
if(doPNG){dev.off()}



#### NOTE from Ian to himself (1/30/2015 9am): starting up again here


#############################################################
#############################################################
#############################################################
#
#Data plot
doPNG <- T
#hake2014.06 <- SS_output(dir=paste(SSdir,"2014hake_06_final2013Data",sep="/"),covar=F,verbose=F)
ht <- 4; wd<- 6.5
if(doPNG) {png(file.path(figDir,"data.png"),height=ht,width=wd,pointsize=10,units="in",res=300)}
if(!doPNG) {windows(width=wd,height=ht)}
par(mfrow=c(1,1),las=1,mar=c(3.6,3.6,1,1),oma=c(0,0,0,0))
SS_plots(base,uncertainty=F,minbthresh=-100,plot=24,new=F,png=FALSE,
         fleetnames=c("Fishery","Survey"))
if(doPNG){dev.off()}


############### Ian says: Allan will need to do this one
#Percent Seasonal Catch
doPNG <- T
ht <- 4; wd<- 6.5
if(doPNG) {png(file.path(figDir,"percSeasCatch.png"),height=ht,width=wd,pointsize=10,units="in",res=300)}
if(!doPNG) {windows(width=wd,height=ht)}
catchdat <- read.csv("Data/Catches/percentSeasonalCatch.csv")
plot(catchdat$Year,100*catchdat$Spring,type="b",pch=20,col="green2",ylim=c(0,70),las=1,xlab="Year",ylab="Percent of total catch",xaxt="n")
lines(catchdat$Year,100*catchdat$Summer,type="b",pch=17,col="blue")
lines(catchdat$Year,100*catchdat$Fall,type="b",pch=15,col="red")
legend("topright",c("Apr-Jun","Jul-Sep","Oct-Dec"),pch=c(20,17,15),col=c("green2","blue","red"),lty=1)
axis(1,at=seq(1991,2013,2),cex.axis=0.9)
if(doPNG){dev.off()}





##################################################################
## Combined age comps

## # code from 2014, modified for 2015
## tmp <- readLines(file.path(base$inputs$dir, "2015Hake_data.ss"))
## ind <- grep("Aggregate marginal fishery age comps",tmp)+2
## numObs <- 40   #as.numeric(strsplit(tmp[ind],"=")[[1]][2])
## ind <- ind:(ind+numObs-1)
## ages <- strsplit(gsub("\t"," ",tmp[ind])," +")
## ages <- t(as.data.frame(lapply(ages,function(x){as.numeric(x)})))
## dimnames(ages) <- list(NULL,c("Year","seas","fleet","gender","partition","ageError","L1","L2","nTrips",paste("a",1:15,sep="")))
## commComps <- as.data.frame(ages)
##
## ind <- grep("Number of index observations",tmp)
## numObs <- 10
## ind <- grep("Acoustic survey",tmp)[2]+1
## ind <- ind:(ind+numObs-1)
## ages <- strsplit(gsub("\t"," ",tmp[ind])," +")
## ages <- t(as.data.frame(lapply(ages,function(x){as.numeric(x)})))
## dimnames(ages) <- list(NULL,c("Year","seas","fleet","gender","partition","ageError","L1","L2","nTrips",paste("a",1:15,sep="")))
## acComps <- as.data.frame(ages)

# alternative approach in 2015
dat <- SS_readdat(file.path(base$inputs$dir, "2015Hake_data.ss"))
commComps <- dat$agecomp[dat$agecomp$FltSvy==1,]
acComps <- dat$agecomp[dat$agecomp$FltSvy==2,]

maxProp <- 0.77  #make sure that this is bigger than any actual proportion
max(commComps[,-(1:9)])
#[1] 0.701
max(acComps[,-(1:9)])
#[1] 0.762

doPNG <- T
ht<-8;wd=8
xlim <- c(1975,2014)
inches <- 0.12
fg <- gray(level=0.1, alpha=0.5)
bg <- gray(level=0.5, alpha=0.5)
if(doPNG) {png(file.path(figDir, "bothAgeComps.png"),height=ht,width=wd,pointsize=10,units="in",res=300)}
if(!doPNG) {windows(height=ht,width=wd)}
par(mfrow=c(2,1),las=1,mar=c(0, 4, 0, 4) + 0.1,cex.axis=0.9,oma=c(3,0,2,0))
x <- data.frame(expand.grid(acComps$Yr,1:15),prop=unlist(acComps[,paste("a",1:15,sep="")]))
names(x) <- c("Yr","Age","prop")
symbols(c(x[,1],-1),c(x[,2],-1),circles=sqrt(c(x[,3],maxProp)),inches=inches,ylim=c(1,15),xlim=xlim,xlab="",ylab="Acoustic Ages",xaxt="n",
        fg=fg, bg=bg)
axis(4)
symbols(.2+c(1990,1994,1998,2002,-1),c(16.2,16.2,16.2,16.2,-1),circles=sqrt(c(0.01,0.1,0.2,0.4,maxProp)),inches=inches,add=T,xpd=NA,
        fg=fg, bg=bg)
text(c(1990,1994,1998,2002)+1.1,c(16.2,16.2,16.2,16.2),c("0.01","0.1","0.2","0.4"),xpd=NA,cex=0.8)
x <- data.frame(expand.grid(commComps$Yr,1:15),prop=unlist(commComps[,paste("a",1:15,sep="")]))
names(x) <- c("Yr","Age","prop")
symbols(c(x[,1],-1),c(x[,2],-1),circles=sqrt(c(x[,3],maxProp)),inches=inches,ylim=c(1,15),xlim=xlim,xlab="Year",ylab="Commercial Ages",xaxt="n",
        fg=fg, bg=bg)
axis(4)
axis(1,at=seq(1975,2020,5))
if(doPNG) dev.off()


doPNG <- T
ht<-4.5;wd=7
if(doPNG) {png(file.path(figDir, "AcAgeComps_superImpose.png"),height=ht,width=wd,pointsize=10,units="in",res=300)}
if(!doPNG) {windows(height=ht,width=wd)}
par(mfrow=c(1,1),las=1,mar=c(0, 4, 0, 1) + 0.1,cex.axis=0.9,oma=c(3,0,2,0))
tmp <- commComps[commComps$Yr %in% acComps$Yr,]
x <- data.frame(expand.grid(acComps$Yr,1:15),prop=unlist(acComps[,paste("a",1:15,sep="")]))
names(x) <- c("Yr","Age","prop")
symbols(c(x[,1],-1),c(x[,2],-1),circles=sqrt(c(x[,3],maxProp)),inches=0.14,ylim=c(1,17),xlim=c(1995,2013),xlab="",ylab="Age",xaxt="n",fg=rgb(0,0,1,0.5),lwd=2,bg=rgb(0,0,1,0.5),yaxt="n")
axis(1,at=c(1995,1998,2001,2003,2005,2007,2009,2011,2012,2013))
axis(2,at=1:15,labels=c(1:14,"15+"))
symbols(c(1990.2,1994.2,1998.2,-1)+5,c(16.4,16.4,16.4,-1),circles=sqrt(c(0.1,0.2,0.4,maxProp)),inches=inches,add=T,xpd=NA)
text(c(1990,1994,1998)+1.1+5,c(16.4,16.4,16.4),c("0.1","0.2","0.4"),xpd=NA,cex=0.8)
legend("topright",c("Acoustic"),pch=16,col=c("blue"),bty="n")
abline(h=15.5)
if(doPNG) dev.off()

doPNG <- T
ht<-4.5;wd=7
if(doPNG) {png(file.path(figDir, "bothAgeComps_superImpose.png"),height=ht,width=wd,pointsize=10,units="in",res=300)}
if(!doPNG) {windows(height=ht,width=wd)}
par(mfrow=c(1,1),las=1,mar=c(0, 4, 0, 1) + 0.1,cex.axis=0.9,oma=c(3,0,2,0))
tmp <- commComps[commComps$Yr %in% acComps$Yr,]
x <- data.frame(expand.grid(acComps$Yr,1:15),prop=unlist(acComps[,paste("a",1:15,sep="")]))
names(x) <- c("Yr","Age","prop")
symbols(c(x[,1],-1),c(x[,2],-1),circles=sqrt(c(x[,3],maxProp)),inches=0.14,ylim=c(1,17),xlim=c(1995,2013),xlab="",ylab="Age",xaxt="n",fg=rgb(0,0,1,0.5),lwd=2,bg=rgb(0,0,1,0.5),yaxt="n")
axis(1,at=c(1995,1998,2001,2003,2005,2007,2009,2011,2012,2013))
axis(2,at=1:15,labels=c(1:14,"15+"))
symbols(c(1990.2,1994.2,1998.2,-1)+5,c(16.4,16.4,16.4,-1),circles=sqrt(c(0.1,0.2,0.4,maxProp)),inches=inches,add=T,xpd=NA)
text(c(1990,1994,1998)+1.1+5,c(16.4,16.4,16.4),c("0.1","0.2","0.4"),xpd=NA,cex=0.8)
legend("topright",c("Acoustic","Fishery"),pch=16,col=c("blue","red"),bty="n")
abline(h=15.5)
x <- data.frame(expand.grid(tmp$Yr,1:15),prop=unlist(tmp[,paste("a",1:15,sep="")]))
names(x) <- c("Yr","Age","prop")
symbols(c(x[,1],-1),c(x[,2],-1),circles=sqrt(c(x[,3],maxProp)),inches=0.14,ylim=c(1,15),xlim=range(x$Yr),xlab="Year",ylab="Commercial Ages",xaxt="n",fg=rgb(1,0,0,0.5),lwd=2,bg=rgb(1,0,0,0.5),add=T)
if(doPNG) dev.off()


doPNG <- T
ht<-4.5;wd=7
if(doPNG) {png(file.path(figDir, "bothAgeComps_superImpose_shift.png"),height=ht,width=wd,pointsize=10,units="in",res=300)}
if(!doPNG) {windows(height=ht,width=wd)}
par(mfrow=c(1,1),las=1,mar=c(0, 4, 0, 1) + 0.1,cex.axis=0.9,oma=c(3,0,2,0))
tmp <- commComps[commComps$Yr %in% acComps$Yr,]
x <- data.frame(expand.grid(acComps$Yr,1:15),prop=unlist(acComps[,paste("a",1:15,sep="")]))
names(x) <- c("Yr","Age","prop")
symbols(c(x[,1],-1),c(x[,2],-1),circles=sqrt(c(x[,3],maxProp)),inches=0.14,ylim=c(1,17),xlim=c(1995,2013),xlab="",ylab="Age",xaxt="n",fg=rgb(0,0,1,0.5),lwd=2,bg=rgb(0,0,1,0.5),yaxt="n")
axis(1,at=c(1995,1998,2001,2003,2005,2007,2009,2011,2012,2013))
axis(2,at=1:15,labels=c(1:14,"15+"))
symbols(c(1990.2,1994.2,1998.2,-1)+5,c(16.4,16.4,16.4,-1),circles=sqrt(c(0.1,0.2,0.4,maxProp)),inches=inches,add=T,xpd=NA)
text(c(1990,1994,1998)+1.1+5,c(16.4,16.4,16.4),c("0.1","0.2","0.4"),xpd=NA,cex=0.8)
legend("topright",c("Acoustic","Fishery"),pch=16,col=c("blue","red"),bty="n")
abline(h=15.5)
x <- data.frame(expand.grid(tmp$Yr,1:15),prop=unlist(tmp[,paste("a",1:15,sep="")]))
names(x) <- c("Yr","Age","prop")
symbols(c(0.2+x[,1],-1),c(x[,2],-1),circles=sqrt(c(x[,3],maxProp)),inches=0.14,ylim=c(1,15),xlim=range(x$Yr),xlab="Year",ylab="Commercial Ages",xaxt="n",fg=rgb(1,0,0,0.5),lwd=2,bg=rgb(1,0,0,0.5),add=T)
if(doPNG) dev.off()


##################################################################
## Fit to age comps
doPNG <- T
ht <- 6; wd<- 6.5
if(doPNG) {png(file.path(figDir,"ageCompFitsFishery.png"),height=ht,width=wd,pointsize=10,units="in",res=300)}
if(!doPNG) {windows(width=wd,height=ht)}
SSplotComps(base,kind="AGE",subplot=1,fleets=1,fleetnames=c("Fishery","Survey"),
            printmkt=FALSE,maxrows=7,maxcols=6,axis1=seq(1,15,2),axis2=seq(0,.7,.2))
if(doPNG){dev.off()}
ht <- 2.7; wd<- 6.5
if(doPNG) {png(file.path(figDir,"ageCompFitsSurvey.png"),height=ht,width=wd,pointsize=10,units="in",res=300)}
if(!doPNG) {windows(width=wd,height=ht)}
SSplotComps(base,kind="AGE",subplot=1,fleets=2,fleetnames=c("Fishery","Survey"),
            printmkt=FALSE,maxrows=2,maxcols=6,axis1=seq(1,15,2),axis2=seq(0,.7,.2))
if(doPNG){dev.off()}

##################################################################
## Pearson residuals
doPNG <- T
ht <- 7; wd<- 6.5
if(doPNG) {png(file.path(figDir,"ageCompPearsons.png"),height=ht,width=wd,pointsize=10,units="in",res=300)}
if(!doPNG) {windows(width=wd,height=ht)}
SSplotComps(base,kind="AGE",subplot=24,printmkt=FALSE,printsex=FALSE,fleetnames=c("Fishery","Survey"))
if(doPNG){dev.off()}

# data only
doPNG <- T
ht <- 7; wd<- 6.5
if(doPNG) {png(file.path(figDir,"ageCompDataOnly.png"),height=ht,width=wd,pointsize=10,units="in",res=300)}
if(!doPNG) {windows(width=wd,height=ht)}
SSplotComps(base,kind="AGE",subplot=24,datonly=TRUE,
            printmkt=FALSE,printsex=FALSE,fleetnames=c("Fishery","Survey"),
            cexZ1=3)
if(doPNG){dev.off()}


####################################################################################

#################################################
#Acoustic Age-1 index
#
# Ian: SKIPPING THIS IN 2015
#
doPNG <- T
ht <- 4; wd<- 6.5
if(doPNG) {png(file.path(figDir,"age1Index.png"),height=ht,width=wd,pointsize=10,units="in",res=300)}
if(!doPNG) {windows(width=wd,height=ht)}
par(mar=c(3, 4, 1, 1) + 0.1)
x <- read.csv("Data/AcousticSurvey/Age1Index.csv")
plot(x$Year,x$Age1_Index/1e9,pch=16,col="blue",cex=1.5,log="y",ylim=range(c(x$Age1_Index),na.rm=T)*c(0.65,1.5)/1e9,lwd=2,xaxt="n",xlab="Year",ylab="Acoustic Age-1 Index",las=1,yaxt="n")
axis(2,at=c(0.01,0.05,0.10,0.25,1.00,2.00,5.00,10.00),las=1)
axis(1,at=x$Year[!is.na(x$Age1_Index)])
legend("bottomleft",c("Acoustic survey age-1 index"),col=c("blue"),pch=c(16),lty=NA,lwd=2,bty="n")
if(doPNG) dev.off()


doPNG <- T
ht <- 4; wd<- 6.5
if(doPNG) {png(file.path(figDir,"age1IndexWithRecr.png"),height=ht,width=wd,pointsize=10,units="in",res=300)}
if(!doPNG) {windows(width=wd,height=ht)}
par(mar=c(3, 4, 1, 1) + 0.1)
x <- read.csv("Data/AcousticSurvey/Age1Index.csv")
logAge1 <- log(x$Age.1[!is.na(x$Age1_Index)])
logIndex <- log(x$Age1_Index)
mn <- mean(logAge1)
index <- mn*logIndex/mean(logIndex[!is.na(x$Age1_Index)])
plot(x$Year,x$Age.1/1e6,pch=4,type="b",log="y",ylim=range(c(x$Age.1,exp(index)),na.rm=T)*c(1,1)/1e6,lwd=2,xaxt="n",xlab="Year",ylab="Age-1 Recruitment (billions)",las=1,col=gray(0.7),cex=0.8)
points(x$Year,exp(index)/1e6,pch=16,col="blue",cex=1.5)
points(x$Year[!is.na(x$Age1_Index)],x$Age.1[!is.na(x$Age1_Index)]/1e6,pch=4,col="black",cex=1,lwd=2)
#mn <- mean(x$Age.1[!is.na(x$Age1_Index)])
#index <- exp(mn*x$Age1_Index/mean(x$Age1_Index),na.rm=T)
#plot(x$Year,x$Age.1/1e6,pch=4,type="b",log="y",ylim=range(c(x$Age.1,index),na.rm=T)/1e6,lwd=2,xaxt="n",xlab="Year",ylab="Age-1 Recruitment (billions)",las=1)
#points(x$Year,index/1e6,pch=16,col="blue",cex=1.5)
axis(1,at=x$Year[!is.na(x$Age1_Index)])
legend("bottomleft",c("Estimated age-1 recruitment","Scaled acoustic survey age-1 index"),col=c("black","blue"),pch=c(4,16),lty=NA,lwd=2,bty="n")
if(doPNG) dev.off()









#########
# MCMC diagnostics 
## source("WriteUp/Rcode/mcmc.out.R")
## source("WriteUp/Rcode/mcmc.nuisance.R")

# Ian: SKIPPING THIS FOR NOW, PERHAPS ALLAN WILL DO FOR 2015

## doPNG <- T
require(r4ss)
hakeMCMC <- SSgetMCMC(dir=file.path(SSdir,"2015hake_basePreSRG_mcmc12e6"),writecsv=TRUE,
            keystrings = c("NatM", "R0", "steep", "Q_extraSD"),
                      nuisancestrings = c("Objective_function", "SPB_", "InitAge", "RecrDev"))
ht <- 4; wd<- 4.5
if(doPNG) {png(file.path(figDir,"mcmcM.png"),height=ht,width=wd,pointsize=10,units="in",res=300)}
if(!doPNG) {windows(width=wd,height=ht)}
par(mar=c(5,3.5,0,0.5),oma=c(0,2.5,0.2,0))
mcmc.out(file.path(SSdir,"2015hake_basePreSRG_mcmc12e6"),run="",numparams=1,closeall=F,new=F)
mtext("M (natural mortality)",side=2,outer=T,line=1.5,cex=1.1)

hakeMCMC <- SSgetMCMC(dir=file.path(SSdir,"2015hake_basePreSRG_mcmc12e6"),writecsv=T,
            keystrings = c("R0", "steep", "Q_extraSD"),nuisancestrings = c("Objective_function", "SPB_", "InitAge", "RecrDev"))
if(doPNG){dev.off()}
if(doPNG) {png(file.path(figDir,"mcmcR0.png"),height=ht,width=wd,pointsize=10,units="in",res=300)}
if(!doPNG) {windows(width=wd,height=ht)}
par(mar=c(5,3.5,0,0.5),oma=c(0,2.5,0.2,0))
mcmc.out(paste(SSdir,"2014hake_21_TVselex1991start_MCMC/",sep="/"),run="",numparams=1,closeall=F,new=F)
mtext("ln(R0) (initial recruitment)",side=2,outer=T,line=1.5,cex=1.1)
if(doPNG){dev.off()}

hakeMCMC <- SSgetMCMC(dir=paste(SSdir,"2014hake_21_TVselex1991start_MCMC",sep="/"),writecsv=T,
            keystrings = c("steep", "Q_extraSD"),nuisancestrings = c("Objective_function", "SPB_", "InitAge", "RecrDev"))
if(doPNG){dev.off()}
if(doPNG) {png(file.path(figDir,"mcmcSteep.png"),height=ht,width=wd,pointsize=10,units="in",res=300)}
if(!doPNG) {windows(width=wd,height=ht)}
par(mar=c(5,3.5,0,0.5),oma=c(0,2.5,0.2,0))
mcmc.out(paste(SSdir,"2014hake_21_TVselex1991start_MCMC/",sep="/"),run="",numparams=1,closeall=F,new=F)
mtext("h (steepness)",side=2,outer=T,line=1.5,cex=1.1)
if(doPNG){dev.off()}

hakeMCMC <- SSgetMCMC(dir=paste(SSdir,"2014hake_21_TVselex1991start_MCMC",sep="/"),writecsv=T,
            keystrings = c("Q_extraSD"),nuisancestrings = c("Objective_function", "SPB_", "InitAge", "RecrDev"))
if(doPNG){dev.off()}
if(doPNG) {png(file.path(figDir,"mcmcQextra.png"),height=ht,width=wd,pointsize=10,units="in",res=300)}
if(!doPNG) {windows(width=wd,height=ht)}
par(mar=c(5,3.5,0,0.5),oma=c(0,2.5,0.2,0))
mcmc.out(paste(SSdir,"2014hake_21_TVselex1991start_MCMC/",sep="/"),run="",numparams=1,closeall=F,new=F)
mtext("Extra SD in survey",side=2,outer=T,line=1.5,cex=1.1)
if(doPNG){dev.off()}


hakeMCMC <- SSgetMCMC(dir=paste(SSdir,"2014hake_21_TVselex1991start_MCMC",sep="/"),writecsv=T,
            keystrings = c("NatM","R0", "steep", "Q_extraSD"),nuisancestrings = c("SPB_","Bratio_","Recr_"))
#mcmc.out(paste(SSdir,"2013hake_19_mcmc/",sep="/"),run="",numparams=4)


if(doPNG) {png(file.path(figDir,"mcmcDiagnostics.png"),height=ht,width=wd,pointsize=10,units="in",res=300)}
if(!doPNG) {windows(width=wd,height=ht)}
par(mar=c(5,4,0,0.5),oma=c(0,0,0.5,0.5))
mcmc.nuisance(paste(SSdir,"2014hake_21_TVselex1991start_MCMC/",sep="/"),run="",labelstrings=c("NatM","R0", "steep", "Q_extraSD","SPB_","Bratio_","RecrDev_"),bothfiles=T)
if(doPNG){dev.off()}

#scatterplot of key params and derived quants
ht <- 6.5; wd<- 6.5
if(doPNG) {png(file.path(figDir,"mcmcPairs.png"),height=ht,width=wd,pointsize=10,units="in",res=300)}
if(!doPNG) {windows(width=wd,height=ht)}
pairs(data.frame(base$mcmc$Object,base$mcmc$NatM,base$mcmc$SR_LN,base$mcmc$SR_BH_steep,base$mcmc$Q,base$mcmc$Recr_2010,base$mcmc$Recr_2011,base$mcmc$Bratio_2014,base$mcmc$ForeCatch_2014),
    labels=c("Obj_fun","M","ln(R0)","h","surv_SD","Recr 2010","Recr 2011","Depl 2014","2014 Catch"),pch=".",cex.labels=1.2,xaxt="n",yaxt="n",las=1,gap=0.5,oma=c(0,0,0,0))
if(doPNG){dev.off()}


### Ian says: modifying the selectivity uncertainty plotting code below
### while listening to GMT meeting proved totally hopeless

# adding a year is complicated when your inputs have a format like this:
head(tmpHi)
##      X1990    X1991    X1992    X1993    X1994    X1995    X1996    X1997
## 1 2014.000 2013.000 2012.000 2011.000 2010.000 2009.000 2008.000 2007.000
## 2 2014.008 2013.013 2012.011 2011.011 2010.010 2009.009 2008.012 2007.011
## 3 2014.135 2013.237 2012.185 2011.195 2010.171 2009.173 2008.200 2007.198


#################################################
# Selex
#################################################
# Fishery selectivity
yrs <- 1991:2013
selex <- list()
selex[["1990"]] <- matrix(NA,nrow=nrow(base$mcmc),ncol=16)
for(i in 1:nrow(base$mcmc)) {
    ind <- grep("AgeSel_1P_[1-9]_Fishery",names(base$mcmc))[1:5]
    selex[["1990"]][i,] <- randWalkSelex.fn(unlist(c(-1000,0,base$mcmc[i,ind],0,0,0,0,0,0,0,0,0)))
}

for(i in yrs) {
    selex[[as.character(i)]] <- selexYear.fn(base$mcmc,i)
}

selexMed21 <- selexMed <- as.data.frame(lapply(selex,function(x){apply(x,2,median)}))
selexUpp21 <- selexUpp <- as.data.frame(lapply(selex,function(x){apply(x,2,quantile,prob=0.975)}))
selexLow21 <- selexLow <- as.data.frame(lapply(selex,function(x){apply(x,2,quantile,prob=0.025)}))

doPNG <- T
ht <- 9; wd<-5 
if(doPNG) {png(file.path(figDir,"TVselexAll.png"),height=ht,width=wd,pointsize=10,units="in",res=300)}
if(!doPNG) {windows(width=wd,height=ht)}
ind <- 1:9
tmp <- t(selexMed[ind,])
mountains(tmp,xvec=ind-1,yvec=as.numeric(substring(row.names(tmp),2)),rev=T,cex.axis=0.8)
if(doPNG) {dev.off()}


yrs <- as.numeric(names(selex))
tmp <- matrix(rep(rev(yrs),each=nrow(selexMed)),nrow=nrow(selexMed),ncol=length(yrs))
tmpHi <- selexUpp+tmp
tmpLo <- selexLow+tmp
tmp <- selexMed+tmp



ind <- 2:9
plot((0:15)[ind],tmp[ind,1],type="b",ylim=range(yrs),yaxt="n",pch=20,xlab="Age",ylab="Year")
#segments((0:15)[ind],tmpHi[ind,1],(0:15)[ind],tmpLo[ind,1])
for(i in 2:ncol(tmp)) {
    lines((0:15)[ind],tmp[ind,i],type="b",pch=20)
    #segments((0:15)[ind],tmpHi[ind,i],(0:15)[ind],tmpLo[ind,i])
}
axis(2,at=yrs+0.5,label=rev(yrs),las=2,cex.axis=0.5)
abline(h=c(yrs,max(yrs)+1),col=rgb(0,0,0,0.2))



doPNG <- T
ht <- 10; wd<-8
if(doPNG) {png(file.path(figDir,"TVselexAllUncertainty.png"),height=ht,width=wd,pointsize=10,units="in",res=300)}
if(!doPNG) {windows(width=wd,height=ht)}
par(mfrow=c(1,2))
ind <- 2:9
n <- 23
plot((0:15)[ind],tmp[ind,ncol(tmp)-n],type="b",ylim=c(yrs[1]+n-11,yrs[n]+2),yaxt="n",pch=20,xlab="",ylab="Year")
segments((0:15)[ind],tmpHi[ind,ncol(tmp)-n],(0:15)[ind],tmpLo[ind,ncol(tmp)-n])
polygon(c((0:15)[ind],rev((0:15)[ind])),c(tmpHi[ind,ncol(tmp)-n],rev(tmpLo[ind,ncol(tmp)-n])),col=rgb(0,0,1,0.2),lty=3)
for(i in (n-1):12) {
    lines((0:15)[ind],tmp[ind,ncol(tmp)-i],type="b",pch=20)
    segments((0:15)[ind],tmpHi[ind,ncol(tmp)-i],(0:15)[ind],tmpLo[ind,ncol(tmp)-i])
    polygon(c((0:15)[ind],rev((0:15)[ind])),c(tmpHi[ind,ncol(tmp)-i],rev(tmpLo[ind,ncol(tmp)-i])),col=rgb(0,0,1,0.2),lty=3)
}
axis(2,at=yrs+0.5,label=rev(yrs),las=2,cex.axis=0.7)
abline(h=c(yrs,max(yrs)+1),col=rgb(0,0,0,0.2))

n <- 11
plot((0:15)[ind],tmp[ind,ncol(tmp)-n],type="b",ylim=c(yrs[1],yrs[n]+2),yaxt="n",pch=20,xlab="",ylab="")
segments((0:15)[ind],tmpHi[ind,ncol(tmp)-n],(0:15)[ind],tmpLo[ind,ncol(tmp)-n])
polygon(c((0:15)[ind],rev((0:15)[ind])),c(tmpHi[ind,ncol(tmp)-n],rev(tmpLo[ind,ncol(tmp)-n])),col=rgb(0,0,1,0.2),lty=3)
for(i in (n-1):0) {
    lines((0:15)[ind],tmp[ind,ncol(tmp)-i],type="b",pch=20)
    segments((0:15)[ind],tmpHi[ind,ncol(tmp)-i],(0:15)[ind],tmpLo[ind,ncol(tmp)-i])
    polygon(c((0:15)[ind],rev((0:15)[ind])),c(tmpHi[ind,ncol(tmp)-i],rev(tmpLo[ind,ncol(tmp)-i])),col=rgb(0,0,1,0.2),lty=3)
}
axis(2,at=yrs+0.5,label=rev(yrs),las=2,cex.axis=0.7)
abline(h=c(yrs,max(yrs)+1),col=rgb(0,0,0,0.2))
mtext("Age",outer=T,side=1,line=-2)
if(doPNG) {dev.off()}


doPNG <- T
ht <- 6; wd<-10
if(doPNG) {png(file.path(figDir,"TVselexLinesUncertainty.png"),height=ht,width=wd,pointsize=10,units="in",res=300)}
if(!doPNG) {windows(width=wd,height=ht)}
par(mfrow=c(1,1),mar=c(4,4,1,1))
cols <- rev(rich.colors.short(6))
polyCols <- rev(rich.colors.short(6,alpha=0.1))
plot(1990:2014,c(selexMed21[2,],NA),col=cols[1],ylim=c(0,1),type="l",lwd=3,xlab="Year",ylab="",xaxt="n",las=1)
for(i in 1:6) {
    polygon(c(1990:2013,rev(1990:2013)),c(selexLow21[i+1,],rev(selexUpp21[i+1,])),col=polyCols[i],lty=3,border=cols[i])
}
for(i in 1:6) {
    lines(1990:2013,c(selexMed21[i+1,]),col=cols[i],lwd=3)
}
axis(1,at=seq(1990,2013,3))
mtext("Proportion Selected",side=2,line=3)
text(rep(2013,6),selexMed21[2:7,ncol(selexMed21)],paste("Age",1:6),pos=4)
if(doPNG) {dev.off()}


#####################################################################
##MLE fit to index

# fit to index covered in "Acoustic survey fits" above

## mymodels <- list(base)
## mysummary <- SSsummarize(mymodels)
## modelnames <- c("Base")
## ht <- 3.5; wd<- 6.5
## if(doPNG) {png(file.path(figDir,"fitIndex.png"),height=ht,width=wd,pointsize=10,units="in",res=300)}
## if(!doPNG) {windows(width=wd,height=ht)}
## par(mfrow=c(1,1),las=1,mar=c(3.6,4,1,1),oma=c(0,0,0,0),mgp=c(2.7,1,0),cex.axis=0.7)
## SSplotComparisons(mysummary,legendlabels=modelnames,endyr=2012,new=F,minbthresh=0,subplots=11,indexUncertainty=T,legend=F,col=c("red"),shadecol=rgb(0,0,0,alpha=0.1),btarg=-0.4)
## if(doPNG){dev.off()}


# Acoustic selectivity
doPNG <- TRUE
selex <- base$mcmc[,grep("Selex_std_2_Fem_A_",names(base$mcmc))]
selexMed <- apply(selex,2,median)
selexUpp <- apply(selex,2,quantile,prob=0.975)
selexLow <- apply(selex,2,quantile,prob=0.025)
doPNG <- T
ht <- 3.25; wd<-6.5
if(doPNG) {png(file.path(figDir,"acousticSelex.png"),height=ht,width=wd,pointsize=10,units="in",res=300)}
if(!doPNG) {windows(width=wd,height=ht)}
par(mar=c(4,4,1,1)+0.1)
plot((1:9),selexMed[1:9],type="b",ylim=c(0,1),pch=20,xlab="Age",ylab="Selectivity",xaxt="n",las=1)
for(i in 1:nrow(selex)) {
    lines((1:9),selex[i,1:9],col=rgb(0,0,0,0.1))
}
segments((1:9),selexUpp[1:9],(1:9),selexLow[1:9],col=rgb(0.1,0.1,1,0.8),lwd=3)
points((1:9),selexMed[1:9],pch=16,cex=1.2,col=rgb(0.1,0.1,1,0.5))
axis(1,at=1:9)
if(doPNG) {dev.off()}

tmp <- t(apply(selex,1,function(x){diff(as.numeric(x))}))
xx <- apply(tmp<0,1,any)
xxx <- apply(tmp<0,2,sum)

# 2013 Commercial selectivity
yrs <- 2013
selex <- list()
selex[["1990"]] <- matrix(NA,nrow=nrow(base$mcmc),ncol=16)
for(i in 1:nrow(base$mcmc)) {
    ind <- grep("AgeSel_1P_[1-9]_Fishery",names(base$mcmc))[1:5]
    selex[["1990"]][i,] <- randWalkSelex.fn(unlist(c(-1000,0,base$mcmc[i,ind],0,0,0,0,0,0,0,0,0)))
}
for(i in yrs) {
    selex[[as.character(i)]] <- selexYear.fn(base$mcmc,i)
}
selexMed <- as.data.frame(lapply(selex,function(x){apply(x,2,median)}))
selexUpp <- as.data.frame(lapply(selex,function(x){apply(x,2,quantile,prob=0.975)}))
selexLow <- as.data.frame(lapply(selex,function(x){apply(x,2,quantile,prob=0.025)}))

doPNG <- T
ht <- 3.25; wd<-6.5
if(doPNG) {png(file.path(figDir,"commSelex2013.png"),height=ht,width=wd,pointsize=10,units="in",res=300)}
if(!doPNG) {windows(width=wd,height=ht)}
par(mar=c(4,4,1,1)+0.1)
plot((1:9),selexMed[2:10,1],type="n",ylim=c(0,1),pch=20,xlab="Age",ylab="Selectivity",xaxt="n",las=1)
for(i in 1:nrow(selex[["2013"]])) {
    lines((1:9),selex[["2013"]][i,2:10],col=rgb(0,0,0,0.1))
}
points((1:9),selexMed[2:10,"X1990"],pch=16,cex=1.2,col=rgb(0,0,0,0.5))
segments((1:9),selexUpp[2:10,"X2013"],(1:9),selexLow[2:10,"X2013"],col=rgb(1,0,0,0.8))
points((1:9),selexMed[2:10,"X2013"],pch=16,cex=1.2,col=rgb(1,0,0,0.5))
points((1:9),selexMed[2:10,"X1990"],pch=16,cex=1.2,col=rgb(1,1,1,0.5))
axis(1,at=1:9)
if(doPNG) {dev.off()}



#####################################
##
ht <- 5; wd<- 5
if(doPNG) {png(file.path(figDir,"plotPars.png"),height=ht,width=wd,pointsize=10,units="in",res=300)}
if(!doPNG) {windows(width=wd,height=ht)}
par(mfrow=c(2,2),mar=c(3,3,1,1))
## SSplotPars(paste(SSdir,c("2015hake_basePreSRG_mcmc12e6"),sep="/"),
##            strings=c("SR_BH","SR_LN","NatM","Q_extraSD"),
##            newheaders=c("Steepness","LN(R0)","Natural mortality","Survey extra SD"),
##            nrows=2,ncols=2,new=F)
SSplotPars(paste(SSdir,c("2015hake_basePreSRG_plotPars"),sep="/"),
           strings=c("SR_BH","SR_LN","NatM","Q_extraSD"),
           newheaders=c("Steepness","LN(R0)","Natural mortality","Survey extra SD"),
           nrows=2,ncols=2,new=F,thin=2,burn=401)
if(doPNG){dev.off()}

####################################
#MLE vs MCMC
mymodels <- list(base,base)
models <- SSsummarize(mymodels)
models$mcmc <- vector(mode="list",length=length(mymodels))  #create the mcmc list of model dataframes
models$mcmc <- list(base$mcmc,base$mcmc)
modelnames <- c("MLE","MCMC")
ht <- 3.75; wd<- 6.5
if(doPNG) {png(file.path(figDir,"MLEvsMCMC_depl.png"),height=ht,width=wd,pointsize=10,units="in",res=300)}
if(!doPNG) {windows(width=wd,height=ht)}
par(mfrow=c(1,1),las=1,mar=c(3.6,3.6,1,1),oma=c(0,0,0,0),mgp=c(2.5,1,0))
SSplotComparisons(models,legendlabels=modelnames,endyr=2014,new=F,minbthresh=0,subplots=4,legend=T,col=c("red","black"),shadecol=rgb(c(1,0),c(0,0),c(0,0),alpha=0.1),btarg=-0.4,mcmc=c(F,T),legendloc="topleft")
abline(h=c(0.1,0.4),lty=2)
axis(2,at=c(0.1,0.4),cex.axis=0.8)
if(doPNG){dev.off()}

ht <- 3.75; wd<- 6.5
if(doPNG) {png(file.path(figDir,"MLEvsMCMC_spb.png"),height=ht,width=wd,pointsize=10,units="in",res=300)}
if(!doPNG) {windows(width=wd,height=ht)}
par(mfrow=c(1,1),las=1,mar=c(3.6,3.6,1,1),oma=c(0,0,0,0),mgp=c(2.5,1,0))
SSplotComparisons(models,legendlabels=modelnames,endyr=2014,new=F,minbthresh=0,subplots=2,legend=T,col=c("red","black"),shadecol=rgb(c(1,0),c(0,0),c(0,0),alpha=0.1),btarg=-0.4,mcmc=c(F,T),legendloc="topleft")
if(doPNG){dev.off()}

ht <- 3.75; wd<- 6.5
if(doPNG) {png(file.path(figDir,"MLEvsMCMC_recr.png"),height=ht,width=wd,pointsize=10,units="in",res=300)}
if(!doPNG) {windows(width=wd,height=ht)}
par(mfrow=c(1,1),las=1,mar=c(3.6,3.6,1,1),oma=c(0,0,0,0),mgp=c(2.5,1,0))
SSplotComparisons(models,legendlabels=modelnames,endyr=2014,new=F,minbthresh=0,subplots=8,legend=T,col=c("red","black"),shadecol=rgb(c(1,0),c(0,0),c(0,0),alpha=0.1),btarg=-0.4,mcmc=c(F,T),legendloc="topleft")
if(doPNG){dev.off()}



############################################
### FITS
#To get aggregated fit to age
SS_plots(base,uncertainty=T,minbthresh=-100,plot=17,png=T)
#fishery comps
SS_plots(base,uncertainty=T,minbthresh=-100,plot=17,png=T,pwidth=6.5,pheight=8,rows=7,cols=6,maxrows=7,maxcols=6,showsampsize=F,showeffN=F)
#SS_plots(base,uncertainty=T,minbthresh=-100,plot=17,png=T,pwidth=25,pheight=5,rows=2,cols=19,maxrows=2,maxcols=19,showsampsize=F,showeffN=F)
#survey comps
SS_plots(base,uncertainty=T,minbthresh=-100,plot=17,png=T,pwidth=6.5,pheight=5,rows=2,cols=5,maxrows=2,maxcols=5,showsampsize=F,showeffN=F)
#SS_plots(base,uncertainty=T,minbthresh=-100,plot=17,png=T,pwidth=10,pheight=2.2,rows=1,cols=9,maxrows=1,maxcols=9,showsampsize=F,showeffN=F)
#comp resids
SS_plots(base,uncertainty=T,minbthresh=-100,plot=17,png=T,pwidth=6.5,pheight=8)


#numbers at age
#SS_plots(base,uncertainty=T,minbthresh=-100,plot=12,png=T,pwidth=6.5,pheight=7)
SSplotNumbers(base,subplot=1,plot=FALSE,print=TRUE,pwidth=6.5,pheight=6,plotdir=figDir)



medCatch <- median(base$mcmc$ForeCatch_2014)/1000
ht <- 3; wd<- 6.5
if(doPNG) {png(file.path(figDir,"catch2014.png"),height=ht,width=wd,pointsize=10,units="in",res=300)}
if(!doPNG) {windows(width=wd,height=ht)}
par(mfrow=c(1,1),las=1,mar=c(3.6,1.6,1,1),oma=c(0,0,0,0),mgp=c(2.5,1,0))
origy <- y <- density(base$mcmc$ForeCatch_2014/1000,from=min(base$mcmc$ForeCatch_2014/1000))
plot(y,yaxt="n",ylab="Density",xlab="Projected 2014 catch based on the default harvest policy ('000 t)",type="l",lty=1,pch=16,xlim=c(0,2500),xaxs="i",yaxs="i",ylim=c(0,max(y$y)*1.02),lwd=3,main="",xaxt="n")
axis(1,at=seq(0,2600,200))
mtext("Density",side=2,line=0.5,las=0)
yy <- y
yy$x <- c(min(y$x),y$x,max(y$x))
yy$y <- c(-1,y$y,-1)
polygon(yy$x,yy$y,col=gray(0.9),lwd=3)
ind <- y$x>=quantile(base$mcmc$ForeCatch_2014/1000,0.025) & y$x<=quantile(base$mcmc$ForeCatch_2014/1000,0.975)
y$x <- y$x[ind]
y$y <- y$y[ind]
yy$x <- c(min(y$x),y$x,max(y$x))
yy$y <- c(-1,y$y,-1)
polygon(yy$x,yy$y,col=rgb(0,0,1,0.3),lty=0)
lines(origy,lwd=3)
tmpy <- y$y[min(abs(y$x-medCatch))==abs(y$x-medCatch)]
lines(c(medCatch,medCatch),c(0,tmpy),lwd=2)
text(medCatch,mean(c(0,tmpy)),paste("Median",round(medCatch,3),sep=" = "),srt=90,adj=c(0.5,-0.5))
box()
if(doPNG){dev.off()}



################################################################################
#Sensitivities
#################################################


hake25 <- SS_output(dir=file.path(SSdir,"2014hake_25_noTVfrom21"),covar=T)
mcmc <- SSgetMCMC(dir=file.path(SSdir,"2014hake_25_noTVfrom21_MCMC"),writecsv=F)
hake25$mcmc <- data.frame(mcmc$model1)
yrs <- 1991:2013
selex <- list()
selex[["1990"]] <- matrix(NA,nrow=nrow(hake25$mcmc),ncol=16)
for(i in 1:nrow(base$mcmc)) {
    ind <- grep("AgeSel_1P_[1-9]_Fishery",names(base$mcmc))[1:5]
    selex[["1990"]][i,] <- randWalkSelex.fn(unlist(c(-1000,0,base$mcmc[i,ind],0,0,0,0,0,0,0,0,0)))
}

selexMed25 <- selexMed <- as.data.frame(lapply(selex,function(x){apply(x,2,median)}))
selexUpp25 <- selexUpp <- as.data.frame(lapply(selex,function(x){apply(x,2,quantile,prob=0.975)}))
selexLow25 <- selexLow <- as.data.frame(lapply(selex,function(x){apply(x,2,quantile,prob=0.025)}))

# Selex
#################################################
# Fishery selectivity
hake31 <- NULL#SS_output(dir=file.path(SSdir,"2014hake_31_maxSelex10_MCMC"),covar=T)
mcmc <- SSgetMCMC(dir=file.path(SSdir,"2014hake_31_maxSelex10_MCMC"),writecsv=F)
hake31$mcmc <- data.frame(mcmc$model1)

yrs <- 1991:2013
selex <- list()
selex[["1990"]] <- matrix(NA,nrow=nrow(hake31$mcmc),ncol=16)
for(i in 1:nrow(hake31$mcmc)) {
    ind <- grep("AgeSel_1P_[0-9]+_Fishery",names(hake31$mcmc))[1:9]
    selex[["1990"]][i,] <- randWalkSelex.fn(unlist(c(-1000,0,hake31$mcmc[i,ind],0,0,0,0,0)))
}

for(i in yrs) {
    selex[[as.character(i)]] <- selexYear10.fn(hake31$mcmc,i)
}

selexMed31 <- selexMed <- as.data.frame(lapply(selex,function(x){apply(x,2,median)}))
selexUpp31 <- selexUpp <- as.data.frame(lapply(selex,function(x){apply(x,2,quantile,prob=0.975)}))
selexLow31 <- selexLow <- as.data.frame(lapply(selex,function(x){apply(x,2,quantile,prob=0.025)}))

yrs <- as.numeric(names(selex))
tmp <- matrix(rep(rev(yrs),each=nrow(selexMed)),nrow=nrow(selexMed),ncol=length(yrs))
tmpHi <- selexUpp+tmp
tmpLo <- selexLow+tmp
tmp <- selexMed+tmp


doPNG <- T
ht <- 10; wd<-8
if(doPNG) {png(file.path(figDir,"TVselex31AllUncertainty.png"),height=ht,width=wd,pointsize=10,units="in",res=300)}
if(!doPNG) {windows(width=wd,height=ht)}
par(mfrow=c(1,2))
ind <- 2:12
n <- 23
plot((0:15)[ind],tmp[ind,ncol(tmp)-n],type="b",ylim=c(yrs[1]+n-11,yrs[n]+2),yaxt="n",pch=20,xlab="",ylab="Selectivity")
segments((0:15)[ind],tmpHi[ind,ncol(tmp)-n],(0:15)[ind],tmpLo[ind,ncol(tmp)-n])
polygon(c((0:15)[ind],rev((0:15)[ind])),c(tmpHi[ind,ncol(tmp)-n],rev(tmpLo[ind,ncol(tmp)-n])),col=rgb(0,0,1,0.2),lty=3)
for(i in (n-1):12) {
    lines((0:15)[ind],tmp[ind,ncol(tmp)-i],type="b",pch=20)
    segments((0:15)[ind],tmpHi[ind,ncol(tmp)-i],(0:15)[ind],tmpLo[ind,ncol(tmp)-i])
    polygon(c((0:15)[ind],rev((0:15)[ind])),c(tmpHi[ind,ncol(tmp)-i],rev(tmpLo[ind,ncol(tmp)-i])),col=rgb(0,0,1,0.2),lty=3)
}
axis(2,at=yrs+0.5,label=rev(yrs),las=2,cex.axis=0.7)
abline(h=c(yrs,max(yrs)+1),col=rgb(0,0,0,0.2))

n <- 11
plot((0:15)[ind],tmp[ind,ncol(tmp)-n],type="b",ylim=c(yrs[1],yrs[n]+2),yaxt="n",pch=20,xlab="",ylab="")
segments((0:15)[ind],tmpHi[ind,ncol(tmp)-n],(0:15)[ind],tmpLo[ind,ncol(tmp)-n])
polygon(c((0:15)[ind],rev((0:15)[ind])),c(tmpHi[ind,ncol(tmp)-n],rev(tmpLo[ind,ncol(tmp)-n])),col=rgb(0,0,1,0.2),lty=3)
for(i in (n-1):0) {
    lines((0:15)[ind],tmp[ind,ncol(tmp)-i],type="b",pch=20)
    segments((0:15)[ind],tmpHi[ind,ncol(tmp)-i],(0:15)[ind],tmpLo[ind,ncol(tmp)-i])
    polygon(c((0:15)[ind],rev((0:15)[ind])),c(tmpHi[ind,ncol(tmp)-i],rev(tmpLo[ind,ncol(tmp)-i])),col=rgb(0,0,1,0.2),lty=3)
}
axis(2,at=yrs+0.5,label=rev(yrs),las=2,cex.axis=0.7)
abline(h=c(yrs,max(yrs)+1),col=rgb(0,0,0,0.2))
mtext("Age",outer=T,side=1,line=-2)
if(doPNG) {dev.off()}

# 2013 Commercial selectivity
doPNG <- T
ht <- 3.25; wd<-6.5
if(doPNG) {png(file.path(figDir,"Sensitivities/commSelex2013_hake2014_31.png"),height=ht,width=wd,pointsize=10,units="in",res=300)}
if(!doPNG) {windows(width=wd,height=ht)}
par(mar=c(4,4,1,1)+0.1)
plot((1:12),selexMed[1:12,1],type="n",ylim=c(0,1),pch=20,xlab="Age",ylab="Selectivity",xaxt="n",las=1)
for(i in 1:nrow(selex[["2013"]])) {
    lines((1:12),selex[["2013"]][i,1:12],col=rgb(0,0,0,0.1))
}
points((1:12),selexMed[1:12,"X1990"],pch=16,cex=1.2,col=rgb(0,0,0,0.5))
segments((1:12),selexUpp[1:12,"X2013"],(1:12),selexLow[1:12,"X2013"],col=rgb(1,0,0,0.8))
points((1:12),selexMed[1:12,"X2013"],pch=16,cex=1.2,col=rgb(1,0,0,0.5))
points((1:12),selexMed[1:12,"X1990"],pch=16,cex=1.2,col=rgb(1,1,1,0.5))
axis(1,at=1:12)
if(doPNG) {dev.off()}


# Acoustic selectivity
selex <- hake31$mcmc[,grep("Selex_std_2_Fem_A_",names(hake31$mcmc))]
selexMed <- apply(selex,2,median)
selexUpp <- apply(selex,2,quantile,prob=0.975)
selexLow <- apply(selex,2,quantile,prob=0.025)
vals <- 1:12
doPNG <- T
ht <- 3.25; wd<-6.5
if(doPNG) {png(file.path(figDir,"acousticSelex31.png"),height=ht,width=wd,pointsize=10,units="in",res=300)}
if(!doPNG) {windows(width=wd,height=ht)}
par(mar=c(4,4,1,1)+0.1)
plot((vals),selexMed[vals],type="b",ylim=c(0,1),pch=20,xlab="Age",ylab="Selectivity",xaxt="n",las=1)
for(i in 1:nrow(selex)) {
    lines((vals),selex[i,vals],col=rgb(0,0,0,0.1))
}
segments(vals,selexUpp[vals],vals,selexLow[vals],col=rgb(0,0,1,0.8))
points(vals,selexMed[vals],pch=16,cex=1.2,col=rgb(0,0,1,0.5))
axis(1,at=vals)
if(doPNG) {dev.off()}



hake25 <- SS_output(dir=file.path(SSdir,"2014hake_25_noTVfrom21"),covar=T)
mcmc <- SSgetMCMC(dir=file.path(SSdir,"2014hake_25_noTVfrom21_MCMC"),writecsv=F)
hake25$mcmc <- data.frame(mcmc$model1)
hake31 <- SS_output(dir=file.path(SSdir,"2014hake_31_maxSelex10"),covar=T)
mcmc <- SSgetMCMC(dir=file.path(SSdir,"2014hake_31_maxSelex10_MCMC"),writecsv=F)
hake31$mcmc <- data.frame(mcmc$model1)
mymodels <- list(base,hake31,hake25)
modelnames <- c("Base","Selex age-10","No TV selex")
mysummary <- SSsummarize(mymodels)
mysummary$mcmc <- list(base$mcmc,hake31$mcmc,hake25$mcmc)
ht <- 3.25; wd<- 6.5
if(doPNG) {png(file.path(figDir,"sens.25.31_depl.png"),height=ht,width=wd,pointsize=10,units="in",res=300)}
if(!doPNG) {windows(width=wd,height=ht)}
par(mfrow=c(1,1),las=1,mar=c(3.6,3.6,1,1),oma=c(0,0,0,0),mgp=c(2.5,1,0))
SSplotComparisons(mysummary,legendlabels=modelnames,endyr=2014,new=F,minbthresh=0,subplots=4,legend=T,col=c("black","blue","red"),shadecol=rgb(c(0,0,1),c(0,0,0),c(0,1,0),alpha=0.1),btarg=-0.4,mcmc=c(T,T,T),legendloc="topleft")
abline(h=c(0.1,0.4),lty=2)
axis(2,at=c(0.1,0.4),cex.axis=0.8)
if(doPNG){dev.off()}

ht <- 3.25; wd<- 6.5
if(doPNG) {png(file.path(figDir,"sens.25.31_spb.png"),height=ht,width=wd,pointsize=10,units="in",res=300)}
if(!doPNG) {windows(width=wd,height=ht)}
par(mfrow=c(1,1),las=1,mar=c(3.6,3.6,1,1),oma=c(0,0,0,0),mgp=c(2.5,1,0))
SSplotComparisons(mysummary,legendlabels=modelnames,endyr=2014,new=F,minbthresh=0,subplots=2,legend=T,col=c("black","blue","red"),shadecol=rgb(c(0,0,1),c(0,0,0),c(0,1,0),alpha=0.1),btarg=-0.4,mcmc=c(T,T,T),legendloc="topleft")
if(doPNG){dev.off()}

ht <- 3.25; wd<- 6.5
if(doPNG) {png(file.path(figDir,"sens.25.31_recr.png"),height=ht,width=wd,pointsize=10,units="in",res=300)}
if(!doPNG) {windows(width=wd,height=ht)}
par(mfrow=c(1,1),las=1,mar=c(3.6,3.6,1,1),oma=c(0,0,0,0),mgp=c(2.5,1,0))
SSplotComparisons(mysummary,legendlabels=modelnames,endyr=2014,new=F,minbthresh=0,subplots=8,legend=T,col=c("black","blue","red"),shadecol=rgb(c(0,0,1),c(0,0,0),c(0,1,0),alpha=0.1),btarg=-0.4,mcmc=c(T,T,T),legendloc="topleft")
if(doPNG){dev.off()}



#############################################################
#Retrospective plot 
retro <- read.csv("Models/HakeRetro.csv",as.is=T)
retro <- retro[retro$Value=="SB million t",]
retro <- retro[!(retro$Model=="TINSS STAR update" | retro$Model=="TINSS Post-STAR" | retro$Model=="Base lowCI" | retro$Model=="Base highCI" ),]
retro[retro$Model=="TINSS SSC Final","Model"] <- "TINSS"
yearInd <- grep("X",names(retro))
years <- as.numeric(substring(names(retro[yearInd]),2))
spb2014 <- base$mcmc[,grep("SPB",names(base$mcmc))][,-c(1,2)]
retro[nrow(retro),paste("X",1966:2014,sep="")] <- apply(spb2014,2,median)[paste("SPB_",1966:2014,sep="")]/2e6


#cols <- c(rgb(0.1,0.1,0.44),rgb(1,0.1,0.6),rgb(1,0.8,0),rgb(0,1,1),rgb(0.5,0,0.5),rgb(0.5,0,0),rgb(0.18,0.55,0.34),rgb(0,0,0.8),rgb(0,0.8,0.8),rgb(0.25,0.88,0.82,0.7),rgb(0.5,1,0.8,0.7),
#          rgb(1,0.84,0,0.7,0.7),rgb(0,0.75,1,0.7),rgb(1,0,1,0.7),rgb(0.73,0.33,0.83,0.7),rgb(0.85,0.65,0.13,0.7),rgb(0.27,0.51,0.71),rgb(0.13,0.70,0.67),rgb(0.6,0.8,0.2),rgb(1,0.27,0),
#          rgb(0,0,0),rgb(0,0,0),rgb(1,0,0),rgb(0.12,0.56,1))
#lwds <- c(rep(1,21),rep(2,3))
#pchs <- c(18,15,17,4,8,20,3,NA,7,18,15,17,4,8,20,3,NA,9,18,4,NA,4,8,1)

yrs <- sort(unique(retro$Year))
#cols <- rep(gray(c(0.4,0.5,0.6,0.7)),6)
cols=c(rgb(0.1,0.1,0.44),rgb(1,0.1,0.6),rgb(1,0.8,0),rgb(0,1,1),rgb(0.5,0,0.5),rgb(0.18,0.55,0.34),rgb(0,0,0.8),rgb(0,0.8,0.8),
       rgb(0.25,0.88,0.82,0.7),rgb(0.5,1,0.8,0.7),rgb(1,0.84,0,0.7,0.7),rgb(0,0.75,1,0.7),rgb(1,0,1,0.7),rgb(0.5,0.5,0.2,1),
       rgb(0.85,0.65,0.13,0.7),rgb(0.27,0.51,0.71),rgb(0.13,0.70,0.67),rgb(1,0,1),rgb(1,0,0))
              
lwds <- c(rep(1,25),3)
pchs <- rep(c(18,15,17,4,20,3),4) #repeat it more than necessary
legCol <- legPch <- rep(NA,nrow(retro))

ht <- 3.75; wd<- 6.5
if(doPNG) {png("WriteUp/Figures/retro.png"),height=ht,width=wd,pointsize=10,units="in",res=300,family="serif")}
if(!doPNG) {windows(width=wd,height=ht)}
par(mar=c(3,3,1,6)+0.1, mgp=c(2, 1, 0))
years <- as.numeric(substring(names(retro[yearInd]),2))
plot(range(years),range(retro[,yearInd],na.rm=T),type="n",xlab="Year",ylab="Spawning Biomass (million t)",las=1,xlim=c(min(years),max(years)),cex.axis=0.9,cex.lab=1)
for(i in 1:nrow(retro)) {
    legCol[i] <- cols[yrs==retro$Year[i]]
    if(sum(retro$Year==retro$Year[i])>1) {  #put a symbol on it to differentiate years
        legPch[i] <- pchs[i]
    }
    lines(years,retro[i,yearInd],col=legCol[i],lwd=lwds[i],pch=legPch[i],type="o",cex=0.7)
}
legCol[i] <- rgb(0,0,0,0.8)
lwds[i] <- 3
lines(years,retro[i,yearInd],col=legCol[i],lwd=lwds[i])
addpoly(1966:2014,slower[-c(1)],supper[-c(1)],"black")

legend(2016,6.5,paste(retro$Year,retro$Model),col=legCol,lwd=lwds+1,pch=legPch,cex=0.7,bty="n",xpd=NA)
if(doPNG) {dev.off()}




################################################
#Retros on 21
retro01 <- SS_output(dir=file.path(SSdir,"2014hake_21_retros/2014hake_21_retro01"),covar=F)
retro02 <- SS_output(dir=file.path(SSdir,"2014hake_21_retros/2014hake_21_retro02"),covar=F)
retro03 <- SS_output(dir=file.path(SSdir,"2014hake_21_retros/2014hake_21_retro03"),covar=F)
retro04 <- SS_output(dir=file.path(SSdir,"2014hake_21_retros/2014hake_21_retro04"),covar=F)
retro05 <- SS_output(dir=file.path(SSdir,"2014hake_21_retros/2014hake_21_retro05"),covar=F)
retro06 <- SS_output(dir=file.path(SSdir,"2014hake_21_retros/2014hake_21_retro06"),covar=F)
retro07 <- SS_output(dir=file.path(SSdir,"2014hake_21_retros/2014hake_21_retro07"),covar=F)
retro08 <- SS_output(dir=file.path(SSdir,"2014hake_21_retros/2014hake_21_retro08"),covar=F)
retro09 <- SS_output(dir=file.path(SSdir,"2014hake_21_retros/2014hake_21_retro09"),covar=F)
retro10 <- SS_output(dir=file.path(SSdir,"2014hake_21_retros/2014hake_21_retro10"),covar=F)
retro11 <- SS_output(dir=file.path(SSdir,"2014hake_21_retros/2014hake_21_retro11"),covar=F)
retro12 <- SS_output(dir=file.path(SSdir,"2014hake_21_retros/2014hake_21_retro12"),covar=F)
retro13 <- SS_output(dir=file.path(SSdir,"2014hake_21_retros/2014hake_21_retro13"),covar=F)

retroDir <- file.path(SSdir,"2014hake_21_retros")
mcmc <- SSgetMCMC(dir=rev(file.path(retroDir,dir(retroDir))),writecsv=F)

mcmc[[length(mcmc)+1]] <- base$mcmc
mymodels <- list(retro13,retro12,retro11,retro10,retro09,retro08,retro07,retro06,retro05,retro04,retro03,retro02,retro01,base)
modelnames <- c("Retro -13","Retro -12","Retro -11","Retro -10","Retro -9","Retro -8","Retro -7","Retro -6","Retro -5","Retro -4","Retro -3","Retro -2","Retro -1","base 2014")
mysummary <- SSsummarize(mymodels)
names(mcmc) <- modelnames
mysummary$mcmc <- mcmc
pdf(file.path(figDir,"Retros/21_retros.pdf")
SSplotComparisons(mysummary, legendlabels=modelnames,endyr=2014-(13:0),new=F,minbthresh = 0,
                densitynames = c("SPB_Virgin","R0","NatM_p_1_Fem_GP_1","SR_BH_steep","Recr_2008","Recr_2009","Recr_2010","Recr_2011"),mcmc=rep(T,14))
#plot and compare selex
columns <- 1:9
# setup colors, points, and line types
if(length(mymodels) > 3) col <- rc(length(mymodels)+1,alpha=0.6)[-1]
if(length(mymodels) < 3) col <- rc(length(mymodels),alpha=0.6)
if(length(mymodels) == 3) col <- c(rgb(0,0,1,0.6),rgb(1,0,0,0.6),rgb(0,205/255,0,0.6))
tmp <- mysummary$agesel
#commSelex <- tmp[tmp$factor=="Asel" & tmp$year==2013 & tmp$fleet==1,as.character(0:20)]
acSelex <- tmp[tmp$factor=="Asel" & tmp$year==2013 & tmp$fleet==2,as.character(0:20)]
par(mfrow=c(1,1))
#plot(as.numeric(names(commSelex))[columns],commSelex[1,columns],type="b",col=col[1],xlab="Age",ylab="Selectivity",lwd=3,pch=20,main="Commercial")
#for(i in 2:nrow(commSelex)) {
#    lines(as.numeric(names(commSelex))[columns],commSelex[i,columns],col=col[i],lwd=3,type="b",pch=20)
#}
plot(as.numeric(names(acSelex))[columns],acSelex[1,columns],type="b",col=col[1],xlab="Age",ylab="Selectivity",lwd=3,pch=20,main="Survey")
for(i in 2:nrow(acSelex)) {
    lines(as.numeric(names(acSelex))[columns],acSelex[i,columns],col=col[i],lwd=3,type="b",pch=20)
}
legend("bottomright",modelnames,col=col,pch=20,lty=1)
dev.off()

ht <- 3.75; wd<- 6.5
if(doPNG) {png("WriteUp/Figures/squidPlot_21.png"),height=ht,width=wd,pointsize=10,units="in",res=300,family="serif")}
if(!doPNG) {windows(width=wd,height=ht)}
par(mar=c(4,4,1,1)+0.1)
SSplotRetroDevs(mysummary,endyrvec=2014-(14:1),cohorts=1999:2013,mcmcVec=rep(T,14),labels=c("","Recruitment deviation","Age of cohort"),main="")
#SSplotRetroRecruits(mysummary,endyrvec=2014-(13:0),cohorts=1999:2010,devs=F,mcmcVec=rep(T,14))
if(doPNG) {dev.off()}


#boxplot(mysummary$mcmc[["Retro -1"]]$Recr_2010,mysummary$mcmc[["Retro -2"]]$Recr_2010)
#boxplot(mysummary$mcmc[["Retro -1"]]$Late_RecrDev_2010,mysummary$mcmc[["Retro -2"]]$Late_RecrDev_2010)


retroDir <- file.path(SSdir,"2014hake_21_retros")
mcmc <- SSgetMCMC(dir=c(file.path(SSdir,"2014hake_21_TVselex1991start_MCMC"),file.path(retroDir,c("2014hake_21_retro01","2014hake_21_retro02","2014hake_21_retro03","2014hake_21_retro04","2014hake_21_retro05"))),writecsv=F)
mymodels <- list(base,retro01,retro02,retro03,retro04,retro05)
modelnames <- c("base 2014","Retro -1","Retro -2","Retro -3","Retro -4","Retro -5")
mysummary <- SSsummarize(mymodels)
names(mcmc) <- modelnames
mysummary$mcmc <- mcmc

doPNG <- T
ht <- 3.25; wd<- 6.5
if(doPNG) {png(file.path(figDir,"Retros/retro21_depl.png"),height=ht,width=wd,pointsize=10,units="in",res=300)}
if(!doPNG) {windows(width=wd,height=ht)}
par(mfrow=c(1,1),las=1,mar=c(3.6,3.6,1,1),oma=c(0,0,0,0),mgp=c(2.5,1,0))
SSplotComparisons(mysummary,legendlabels=modelnames,endyr=2014:2009,new=F,minbthresh=0,subplots=4,legend=T,btarg=-0.4,mcmc=c(T,T,T),legendloc="topleft")
axis(1,seq(1975,2015,10))
if(doPNG) {dev.off()}

doPNG <- T
ht <- 3.25; wd<- 6.5
if(doPNG) {png(file.path(figDir,"Retros/retro21_recr.png"),height=ht,width=wd,pointsize=10,units="in",res=300)}
if(!doPNG) {windows(width=wd,height=ht)}
par(mfrow=c(1,1),las=1,mar=c(3.6,3.6,1,1),oma=c(0,0,0,0),mgp=c(2.5,1,0))
SSplotComparisons(mysummary,legendlabels=modelnames,endyr=2014:2009,new=F,minbthresh=0,subplots=8,legend=T,btarg=-0.4,mcmc=c(T,T,T),legendloc="topleft")
axis(1,seq(1975,2015,10))
if(doPNG) {dev.off()}


SSplotComparisons(mysummary, legendlabels=modelnames,endyr=2014-(13:0),new=F,minbthresh = 0,
                densitynames = c("SPB_Virgin","R0","NatM_p_1_Fem_GP_1","SR_BH_steep","Recr_2008","Recr_2009","Recr_2010","Recr_2011"),mcmc=rep(T,14))
#plot and compare selex
columns <- 1:9
# setup colors, points, and line types
if(length(mymodels) > 3) col <- rc(length(mymodels)+1,alpha=0.6)[-1]
if(length(mymodels) < 3) col <- rc(length(mymodels),alpha=0.6)
if(length(mymodels) == 3) col <- c(rgb(0,0,1,0.6),rgb(1,0,0,0.6),rgb(0,205/255,0,0.6))
tmp <- mysummary$agesel
#commSelex <- tmp[tmp$factor=="Asel" & tmp$year==2013 & tmp$fleet==1,as.character(0:20)]
acSelex <- tmp[tmp$factor=="Asel" & tmp$year==2013 & tmp$fleet==2,as.character(0:20)]
par(mfrow=c(1,1))
#plot(as.numeric(names(commSelex))[columns],commSelex[1,columns],type="b",col=col[1],xlab="Age",ylab="Selectivity",lwd=3,pch=20,main="Commercial")
#for(i in 2:nrow(commSelex)) {
#    lines(as.numeric(names(commSelex))[columns],commSelex[i,columns],col=col[i],lwd=3,type="b",pch=20)
#}
plot(as.numeric(names(acSelex))[columns],acSelex[1,columns],type="b",col=col[1],xlab="Age",ylab="Selectivity",lwd=3,pch=20,main="Survey")
for(i in 2:nrow(acSelex)) {
    lines(as.numeric(names(acSelex))[columns],acSelex[i,columns],col=col[i],lwd=3,type="b",pch=20)
}
legend("bottomright",modelnames,col=col,pch=20,lty=1)
dev.off()



################################################
#Retros on 25
retro01 <- SS_output(dir=file.path(SSdir,"2014hake_25_retros/2014hake_25_retro01"),covar=F)
retro02 <- SS_output(dir=file.path(SSdir,"2014hake_25_retros/2014hake_25_retro02"),covar=F)
retro03 <- SS_output(dir=file.path(SSdir,"2014hake_25_retros/2014hake_25_retro03"),covar=F)
retro04 <- SS_output(dir=file.path(SSdir,"2014hake_25_retros/2014hake_25_retro04"),covar=F)
retro05 <- SS_output(dir=file.path(SSdir,"2014hake_25_retros/2014hake_25_retro05"),covar=F)
retro06 <- SS_output(dir=file.path(SSdir,"2014hake_25_retros/2014hake_25_retro06"),covar=F)
retro07 <- SS_output(dir=file.path(SSdir,"2014hake_25_retros/2014hake_25_retro07"),covar=F)
retro08 <- SS_output(dir=file.path(SSdir,"2014hake_25_retros/2014hake_25_retro08"),covar=F)
retro09 <- SS_output(dir=file.path(SSdir,"2014hake_25_retros/2014hake_25_retro09"),covar=F)
retro10 <- SS_output(dir=file.path(SSdir,"2014hake_25_retros/2014hake_25_retro10"),covar=F)
retro11 <- SS_output(dir=file.path(SSdir,"2014hake_25_retros/2014hake_25_retro11"),covar=F)
retro12 <- SS_output(dir=file.path(SSdir,"2014hake_25_retros/2014hake_25_retro12"),covar=F)
retro13 <- SS_output(dir=file.path(SSdir,"2014hake_25_retros/2014hake_25_retro13"),covar=F)

retroDir <- file.path(SSdir,"2014hake_25_retros")
mcmc <- SSgetMCMC(dir=rev(file.path(retroDir,dir(retroDir))),writecsv=F)
#READ in proper base
mcmc[[length(mcmc)+1]] <- base$mcmc
mymodels <- list(retro13,retro12,retro11,retro10,retro09,retro08,retro07,retro06,retro05,retro04,retro03,retro02,retro01,base)
modelnames <- c("Retro -13","Retro -12","Retro -11","Retro -10","Retro -9","Retro -8","Retro -7","Retro -6","Retro -5","Retro -4","Retro -3","Retro -2","Retro -1","base 2014")
mysummary25 <- SSsummarize(mymodels)
names(mcmc) <- modelnames
mysummary25$mcmc <- mcmc

mysummary <- mysummary25
pdf(file.path(figDir,"Retros/25_retros.pdf")
SSplotComparisons(mysummary, legendlabels=modelnames,endyr=2014-(13:0),new=F,minbthresh = 0,
                densitynames = c("SPB_Virgin","R0","NatM_p_1_Fem_GP_1","SR_BH_steep","Recr_2008","Recr_2009","Recr_2010","Recr_2011"),mcmc=rep(T,14))
#plot and compare selex
columns <- 1:9
# setup colors, points, and line types
if(length(mymodels) > 3) col <- rc(length(mymodels)+1,alpha=0.6)[-1]
if(length(mymodels) < 3) col <- rc(length(mymodels),alpha=0.6)
if(length(mymodels) == 3) col <- c(rgb(0,0,1,0.6),rgb(1,0,0,0.6),rgb(0,205/255,0,0.6))
tmp <- mysummary$agesel
#commSelex <- tmp[tmp$factor=="Asel" & tmp$year==2013 & tmp$fleet==1,as.character(0:20)]
acSelex <- tmp[tmp$factor=="Asel" & tmp$year==2013 & tmp$fleet==2,as.character(0:20)]
par(mfrow=c(1,1))
#plot(as.numeric(names(commSelex))[columns],commSelex[1,columns],type="b",col=col[1],xlab="Age",ylab="Selectivity",lwd=3,pch=20,main="Commercial")
#for(i in 2:nrow(commSelex)) {
#    lines(as.numeric(names(commSelex))[columns],commSelex[i,columns],col=col[i],lwd=3,type="b",pch=20)
#}
plot(as.numeric(names(acSelex))[columns],acSelex[1,columns],type="b",col=col[1],xlab="Age",ylab="Selectivity",lwd=3,pch=20,main="Survey")
for(i in 2:nrow(acSelex)) {
    lines(as.numeric(names(acSelex))[columns],acSelex[i,columns],col=col[i],lwd=3,type="b",pch=20)
}
legend("bottomright",modelnames,col=col,pch=20,lty=1)
dev.off()

ht <- 3.75; wd<- 6.5
if(doPNG) {png("WriteUp/Figures/Retros/squidPlot_25.png"),height=ht,width=wd,pointsize=10,units="in",res=300,family="serif")}
if(!doPNG) {windows(width=wd,height=ht)}
par(mar=c(4,4,1,1)+0.1)
SSplotRetroDevs(mysummary25,endyrvec=2014-(14:1),cohorts=1999:2013,mcmcVec=rep(T,14),labels=c("","Recruitment deviation","Age of cohort"),main="")
#SSplotRetroRecruits(mysummary,endyrvec=2014-(13:0),cohorts=1999:2010,devs=F,mcmcVec=rep(T,14))
if(doPNG) {dev.off()}





mcmc <- mysummary25$mcmc
mcmc <- mysummary$mcmc
n <- length(mcmc)
recdevs <- NULL
  for(imodel in (1:n)){
    tmp <- unique(c(grep("_RecrDev_",names(mcmc[[imodel]])),
                    #grep("_InitAge_",names(mcmc[[imodel]])),
                    grep("ForeRecr_",names(mcmc[[imodel]]))))
    if(length(tmp) > 0) { #there are some mcmc values to use
      mcmc.tmp <- mcmc[[imodel]][,tmp] # subset of columns from MCMC for this model 
      mcmclabs <- names(mcmc.tmp)
      med   <- apply(mcmc.tmp,2,quantile,prob=0.5)   #hard-wired probability
      recdevs <- cbind(recdevs,med)
    }
  }
recdevs <- cbind(as.numeric(unlist(lapply(strsplit(rownames(recdevs),"_"),function(x){x[length(x)]}))),recdevs)
colnames(recdevs)[1] <- "Yr"

yrs <- (2014-n):2013
cohorts <- 1999:2012
tmp <- (min(yrs)-max(cohorts)):(max(yrs)-min(cohorts))
ageMat <- matrix(NA,nrow=length(cohorts),ncol=length(tmp),dimnames=list(cohorts,tmp))

for(coh in cohorts) {
    ages <- yrs-coh
    ageMat[as.character(coh),as.character(ages)] <- recdevs[recdevs[,"Yr"]==coh,(1:n)+1]
}



tv <- apply(ageMat,2,sd,na.rm=T)
noTV <- apply(ageMat,2,sd,na.rm=T)
plot(0:13,tv[as.character(0:13)],type="l",col="black")
lines(0:13,noTV[as.character(0:13)],type="l",col="red")







#
################################################
#Retros on 21: five year forecasts
retro01 <- SS_output(dir=file.path(SSdir,"2014hake_21_retros/2014hake_21_retro01"),covar=F)
retro02 <- SS_output(dir=file.path(SSdir,"2014hake_21_retros/2014hake_21_retro02"),covar=F)
retro03 <- SS_output(dir=file.path(SSdir,"2014hake_21_retros/2014hake_21_retro03"),covar=F)
retro04 <- SS_output(dir=file.path(SSdir,"2014hake_21_retros/2014hake_21_retro04"),covar=F)
retro05 <- SS_output(dir=file.path(SSdir,"2014hake_21_retros/2014hake_21_retro05"),covar=F)
retro06 <- SS_output(dir=file.path(SSdir,"2014hake_21_retros/2014hake_21_retro06"),covar=F)
retro07 <- SS_output(dir=file.path(SSdir,"2014hake_21_retros/2014hake_21_retro07"),covar=F)
retro08 <- SS_output(dir=file.path(SSdir,"2014hake_21_retros/2014hake_21_retro08"),covar=F)
retro09 <- SS_output(dir=file.path(SSdir,"2014hake_21_retros/2014hake_21_retro09"),covar=F)
retro10 <- SS_output(dir=file.path(SSdir,"2014hake_21_retros/2014hake_21_retro10"),covar=F)
retro11 <- SS_output(dir=file.path(SSdir,"2014hake_21_retros/2014hake_21_retro11"),covar=F)
retro12 <- SS_output(dir=file.path(SSdir,"2014hake_21_retros/2014hake_21_retro12"),covar=F)
retro13 <- SS_output(dir=file.path(SSdir,"2014hake_21_retros/2014hake_21_retro13"),covar=F)

retroDir <- file.path(SSdir,"2014hake_21_retros")
mcmc <- SSgetMCMC(dir=rev(file.path(retroDir,dir(retroDir))),writecsv=F)
modelnames <- c("Retro -13","Retro -12","Retro -11","Retro -10","Retro -9","Retro -8","Retro -7","Retro -6","Retro -5","Retro -4","Retro -3","Retro -2","Retro -1","base 2014")
names(mcmc) <- modelnames
mcmc[[length(mcmc)+1]] <- base$mcmc

mymodels <- list(retro13,retro12,retro11,retro10,retro09,retro08,retro07,retro06,retro05,retro04,retro03,retro02,retro01,base)
allModNames <- modelnames <- c("Retro -13","Retro -12","Retro -11","Retro -10","Retro -9","Retro -8","Retro -7","Retro -6","Retro -5","Retro -4","Retro -3","Retro -2","Retro -1","base 2014")
mysummary <- SSsummarize(mymodels)
mysummary$mcmc <- mcmc

mymodels <- list(retro13,base)
modelnames <- c("Retro -13","base 2014")
mysummary <- SSsummarize(mymodels)
mysummary$mcmc <- NULL
j <- 0
for(i in c(1,14)) {
    j <- j+1
    mysummary$mcmc[[j]] <- mcmc[[i]]
}

doPNG <- F
ht <- 3.25; wd<- 6.5
if(doPNG) {png(file.path(figDir,"Retros/retro21_depl.png"),height=ht,width=wd,pointsize=10,units="in",res=300)}
if(!doPNG) {windows(width=wd,height=ht)}
par(mfrow=c(1,1),las=1,mar=c(3.6,3.6,1,1),oma=c(0,0,0,0),mgp=c(2.5,1,0))
SSplotComparisons(mysummary,legendlabels=modelnames,endyr=c(2005,2014),new=F,minbthresh=0,subplots=4,legend=T,btarg=-0.4,mcmc=c(T,T),legendloc="topleft")
axis(1,seq(1975,2015,10))
if(doPNG) {dev.off()}

mymodels <- list(retro05,base)
modelnames <- c("Retro -5","base 2014")
mysummary <- SSsummarize(mymodels)
mysummary$mcmc <- NULL
j <- 0
for(i in c(which(allModNames=="Retro -5"),14)) {
    j <- j+1
    mysummary$mcmc[[j]] <- mcmc[[i]]
}
doPNG <- T
ht <- 3.25; wd<- 6.5
if(doPNG) {png(file.path(figDir,"Retros/retro05_depl_5yrFore_1.png"),height=ht,width=wd,pointsize=10,units="in",res=300)}
if(!doPNG) {windows(width=wd,height=ht)}
par(mfrow=c(1,1),las=1,mar=c(3.6,3.6,1,1),oma=c(0,0,0,0),mgp=c(2.5,1,0))
SSplotComparisons(mysummary,legendlabels=modelnames,models=1,endyr=c(2009),new=F,minbthresh=0,subplots=4,legend=T,btarg=-0.4,mcmc=c(T),legendloc="topleft",xlim=c(1966,2014))
abline(v=2014-5)
abline(h=c(0.1,0.4),lty=2)
axis(1,seq(1975,2015,10))
axis(2,at=c(0.1,0.4),cex.axis=0.7)
if(doPNG) {dev.off()}
doPNG <- T
ht <- 3.25; wd<- 6.5
if(doPNG) {png(file.path(figDir,"Retros/retro05_depl_5yrFore_2.png"),height=ht,width=wd,pointsize=10,units="in",res=300)}
if(!doPNG) {windows(width=wd,height=ht)}
par(mfrow=c(1,1),las=1,mar=c(3.6,3.6,1,1),oma=c(0,0,0,0),mgp=c(2.5,1,0))
SSplotComparisons(mysummary,legendlabels=modelnames,models=1,endyr=c(2014),new=F,minbthresh=0,subplots=4,legend=T,btarg=-0.4,mcmc=c(T),legendloc="topleft",xlim=c(1966,2014))
abline(v=2014-5)
abline(h=c(0.1,0.4),lty=2)
axis(1,seq(1975,2015,10))
axis(2,at=c(0.1,0.4),cex.axis=0.7)
if(doPNG) {dev.off()}

doPNG <- T
ht <- 3.25; wd<- 6.5
if(doPNG) {png(file.path(figDir,"Retros/retro05_depl_5yrFore.png"),height=ht,width=wd,pointsize=10,units="in",res=300)}
if(!doPNG) {windows(width=wd,height=ht)}
par(mfrow=c(1,1),las=1,mar=c(3.6,3.6,1,1),oma=c(0,0,0,0),mgp=c(2.5,1,0))
SSplotComparisons(mysummary,legendlabels=modelnames,endyr=c(2014,2014),new=F,minbthresh=0,subplots=4,legend=T,btarg=-0.4,mcmc=c(T,T),legendloc="topleft",xlim=c(1966,2014))
abline(v=2014-5)
abline(h=c(0.1,0.4),lty=2)
axis(1,seq(1975,2015,10))
axis(2,at=c(0.1,0.4),cex.axis=0.7)
if(doPNG) {dev.off()}
if(doPNG) {png(file.path(figDir,"Retros/retro05_catch_5yrFore.png"),height=ht,width=wd,pointsize=10,units="in",res=300)}
if(!doPNG) {windows(width=wd,height=ht)}
par(mfrow=c(1,1),las=1,mar=c(3.6,3.6,1,1),oma=c(0,0,0,0),mgp=c(2.5,1,0))
SSplotComparisons(mysummary,legendlabels=modelnames,endyr=c(2014,2014),new=F,minbthresh=0,subplot=14,legend=T,btarg=-0.4,mcmc=c(T,T),legendloc="topright",
                    densitynames=c("ForeCatch_2014"))
if(doPNG) {dev.off()}

mymodels <- list(retro10,base)
modelnames <- c("Retro -10","base 2014")
mysummary <- SSsummarize(mymodels)
mysummary$mcmc <- NULL
j <- 0
for(i in c(which(allModNames=="Retro -10"),14)) {
    j <- j+1
    mysummary$mcmc[[j]] <- mcmc[[i]]
}
doPNG <- T
ht <- 3.25; wd<- 6.5
if(doPNG) {png(file.path(figDir,"Retros/retro10_depl_10yrFore.png"),height=ht,width=wd,pointsize=10,units="in",res=300)}
if(!doPNG) {windows(width=wd,height=ht)}
par(mfrow=c(1,1),las=1,mar=c(3.6,3.6,1,1),oma=c(0,0,0,0),mgp=c(2.5,1,0))
SSplotComparisons(mysummary,legendlabels=modelnames,endyr=c(2014,2014),new=F,minbthresh=0,subplots=4,legend=T,btarg=-0.4,mcmc=c(T,T),legendloc="topleft")
abline(v=2014-10)
abline(h=c(0.1,0.4),lty=2,col=rgb(0,0,0,0.5))
axis(1,seq(1975,2015,10))
axis(2,at=c(0.1,0.4),cex.axis=0.7)
if(doPNG) {dev.off()}
if(doPNG) {png(file.path(figDir,"Retros/retro10_catch_10yrFore.png"),height=ht,width=wd,pointsize=10,units="in",res=300)}
if(!doPNG) {windows(width=wd,height=ht)}
par(mfrow=c(1,1),las=1,mar=c(3.6,3.6,1,1),oma=c(0,0,0,0),mgp=c(2.5,1,0))
SSplotComparisons(mysummary,legendlabels=modelnames,endyr=c(2014,2014),new=F,minbthresh=0,subplot=14,legend=T,btarg=-0.4,mcmc=c(T,T),legendloc="topright",
                    densitynames=c("ForeCatch_2014"))
if(doPNG) {dev.off()}








run1.2 <- SS_output(dir=file.path(SSdir,"2014hake_SRG1.1_surveyProfile/prof5"),covar=F)
mymodels <- list(run1.2,base)
modelnames <- c("Survey 1.8million t","base 2014")
mysummary <- SSsummarize(mymodels)
mysummary$mcmc <- 
doPNG <- F
ht <- 3.25; wd<- 6.5
if(doPNG) {png(file.path(figDir,"Sensitivities/run1.2_foreCatch.png"),height=ht,width=wd,pointsize=10,units="in",res=300)}
if(!doPNG) {windows(width=wd,height=ht)}
par(mfrow=c(1,1),las=1,mar=c(3.6,3,1,1),oma=c(0,0,0,0),mgp=c(2.5,1,0))
SSplotComparisons(mysummary,legendlabels=modelnames,endyr=c(2014,2014),new=F,minbthresh=0,subplot=14,legend=T,btarg=-0.4,mcmc=c(T,T),legendloc="topright",
                    densitynames=c("ForeCatch_2014"),densityxlabs="Default harvest catch in 2014 (thousands t)",
                    labels=c("Year","Spawning biomass (t)","Spawning depletion","Age-0 recruits (1,000s)","Recruitment deviations","Index",
                             "Log index","SPR ratio","","Management target","Minimum stock size threshold"))
text(median(base$mcmc$ForeCatch_2014),mean(par()$usr[3:4]),paste("Median",round(median(base$mcmc$ForeCatch_2014)/1000,3),sep=" = "),srt=90,adj=c(0.5,-0.5))
if(doPNG) {dev.off()}

doPNG <- T
ht <- 3.25; wd<- 6.5
if(doPNG) {png(file.path(figDir,"Sensitivities/run1.2_depl.png"),height=ht,width=wd,pointsize=10,units="in",res=300)}
if(!doPNG) {windows(width=wd,height=ht)}
par(mfrow=c(1,1),las=1,mar=c(3.6,3.6,1,1),oma=c(0,0,0,0),mgp=c(2.5,1,0))
SSplotComparisons(mysummary,legendlabels=modelnames,endyr=c(2014,2014),new=F,minbthresh=0,subplot=4,legend=T,btarg=-0.4,mcmc=c(F,F),legendloc="topright")
abline(h=c(0.1,0.4),lty=2,col=rgb(0,0,0,0.5))
axis(2,at=c(0.1,0.4),cex.axis=0.8)
if(doPNG) {dev.off()}

doPNG <- T
ht <- 3.25; wd<- 6.5
if(doPNG) {png(file.path(figDir,"Sensitivities/run1.2_depl.png"),height=ht,width=wd,pointsize=10,units="in",res=300)}
if(!doPNG) {windows(width=wd,height=ht)}
par(mfrow=c(1,1),las=1,mar=c(3.6,3.6,1,1),oma=c(0,0,0,0),mgp=c(2.5,1,0))
SSplotComparisons(mysummary,legendlabels=modelnames,endyr=c(2014,2014),new=F,minbthresh=0,subplot=4,legend=T,btarg=-0.4,mcmc=c(F,F),legendloc="topright")
abline(h=c(0.1,0.4),lty=2,col=rgb(0,0,0,0.5))
axis(2,at=c(0.1,0.4),cex.axis=0.8)
if(doPNG) {dev.off()}




doPNG <- F
ht <- 3.25; wd<- 6.5
if(doPNG) {png("Meetings/SRG/Figures/etro21_recr.png"),height=ht,width=wd,pointsize=10,units="in",res=300)}
if(!doPNG) {windows(width=wd,height=ht)}
par(mfrow=c(1,1),las=1,mar=c(3.6,3.6,1,1),oma=c(0,0,0,0),mgp=c(2.5,1,0))
SSplotComparisons(mysummary,legendlabels=modelnames,endyr=2014:2009,new=F,minbthresh=0,subplots=8,legend=T,btarg=-0.4,mcmc=c(T,T,T),legendloc="topleft")
axis(1,seq(1975,2015,10))
if(doPNG) {dev.off()}


avgWt <- SS_output(dir=file.path(SSdir,"2014hake_SRG1.8_avgWtForecast"),covar=F)
avgWt$mcmc <- SSgetMCMC(dir=file.path(SSdir,"2014hake_SRG1.8_MCMC_avgWtForecast"),writecsv=F)[[1]]
mymodels <- list(avgWt,base)
modelnames <- c("Average Wt-at-age","base 2014")
mysummary <- SSsummarize(mymodels)
mysummary$mcmc <- list(avgWt$mcmc,base$mcmc)

doPNG <- T
ht <- 3.25; wd<- 6.5
if(doPNG) {png("Meetings/SRG/Figures/avgWtCatch.png"),height=ht,width=wd,pointsize=10,units="in",res=300)}
if(!doPNG) {windows(width=wd,height=ht)}
par(mfrow=c(1,1),las=1,mar=c(3.6,3.6,1,1),oma=c(0,0,0,0),mgp=c(2.5,1,0))
SSplotComparisons(mysummary,legendlabels=modelnames,endyr=c(2014,2014),new=F,minbthresh=0,subplot=14,legend=T,btarg=-0.4,mcmc=c(T,T),legendloc="topright",
                    densitynames=c("ForeCatch_2014"))
if(doPNG) {dev.off()}









doPNG <- T
ht <- 3.25; wd<- 6.5
if(doPNG) {png(file.path(figDir,"Retros/retro21_recr.png"),height=ht,width=wd,pointsize=10,units="in",res=300)}
if(!doPNG) {windows(width=wd,height=ht)}
par(mfrow=c(1,1),las=1,mar=c(3.6,3.6,1,1),oma=c(0,0,0,0),mgp=c(2.5,1,0))
SSplotComparisons(mysummary,legendlabels=modelnames,endyr=2014:2009,new=F,minbthresh=0,subplots=8,legend=T,btarg=-0.4,mcmc=c(T,T,T),legendloc="topleft")
axis(1,seq(1975,2015,10))
if(doPNG) {dev.off()}














pdf(file.path(figDir,"Retros/21_retros.pdf")
SSplotComparisons(mysummary, legendlabels=modelnames,endyr=2014-(13:0),new=F,minbthresh = 0,
                densitynames = c("SPB_Virgin","R0","NatM_p_1_Fem_GP_1","SR_BH_steep","Recr_2008","Recr_2009","Recr_2010","Recr_2011"),mcmc=rep(T,14))
#plot and compare selex
columns <- 1:9
# setup colors, points, and line types
if(length(mymodels) > 3) col <- rc(length(mymodels)+1,alpha=0.6)[-1]
if(length(mymodels) < 3) col <- rc(length(mymodels),alpha=0.6)
if(length(mymodels) == 3) col <- c(rgb(0,0,1,0.6),rgb(1,0,0,0.6),rgb(0,205/255,0,0.6))
tmp <- mysummary$agesel
#commSelex <- tmp[tmp$factor=="Asel" & tmp$year==2013 & tmp$fleet==1,as.character(0:20)]
acSelex <- tmp[tmp$factor=="Asel" & tmp$year==2013 & tmp$fleet==2,as.character(0:20)]
par(mfrow=c(1,1))
#plot(as.numeric(names(commSelex))[columns],commSelex[1,columns],type="b",col=col[1],xlab="Age",ylab="Selectivity",lwd=3,pch=20,main="Commercial")
#for(i in 2:nrow(commSelex)) {
#    lines(as.numeric(names(commSelex))[columns],commSelex[i,columns],col=col[i],lwd=3,type="b",pch=20)
#}
plot(as.numeric(names(acSelex))[columns],acSelex[1,columns],type="b",col=col[1],xlab="Age",ylab="Selectivity",lwd=3,pch=20,main="Survey")
for(i in 2:nrow(acSelex)) {
    lines(as.numeric(names(acSelex))[columns],acSelex[i,columns],col=col[i],lwd=3,type="b",pch=20)
}
legend("bottomright",modelnames,col=col,pch=20,lty=1)
dev.off()





SSdir <- "Models"
run32 <- SS_output(dir=file.path(SSdir,"2014hake_32_survey2013_1.8_MCMC"),covar=F)
mcmc32 <- SSgetMCMC(dir=file.path(SSdir,"2014hake_32_survey2013_1.8_MCMC"),writecsv=F)$model1
run32$mcmc <- data.frame(mcmc32)

mymodels <- list(run32,base)
modelnames <- c("1.8million t","base 2014")
mysummary <- SSsummarize(mymodels)
mysummary$mcmc <- list(run32$mcmc,base$mcmc)

doPNG <- T
ht <- 3.25; wd<- 6.5
if(doPNG) {png(file.path(figDir,"run32_depl.png"),height=ht,width=wd,pointsize=10,units="in",res=300)}
if(!doPNG) {windows(width=wd,height=ht)}
par(mfrow=c(1,1),las=1,mar=c(3.6,3.6,1,1),oma=c(0,0,0,0),mgp=c(2.5,1,0))
SSplotComparisons(mysummary,legendlabels=modelnames,endyr=c(2014,2014),new=F,minbthresh=0,subplots=4,legend=T,btarg=-0.4,mcmc=c(T,T),legendloc="topleft",xlim=c(1966,2014))
abline(h=c(0.1,0.4),lty=2,col=rgb(0,0,0,0.4))
axis(1,seq(1975,2015,10))
axis(2,at=c(0.1,0.4),cex.axis=0.7)
if(doPNG) {dev.off()}
if(doPNG) {png(file.path(figDir,"run32_catch.png"),height=ht,width=wd,pointsize=10,units="in",res=300)}
if(!doPNG) {windows(width=wd,height=ht)}
par(mfrow=c(1,1),las=1,mar=c(3.6,3.6,1,1),oma=c(0,0,0,0),mgp=c(2.5,1,0))
SSplotComparisons(mysummary,legendlabels=modelnames,endyr=c(2014,2014),new=F,minbthresh=0,subplot=14,legend=T,btarg=-0.4,mcmc=c(T,T),legendloc="topright",
                    densitynames=c("ForeCatch_2014"),densityxlabs="Default harvest catch in 2014 (thousands t)",
                    labels=c("Year","Spawning biomass (t)","Spawning depletion","Age-0 recruits (1,000s)","Recruitment deviations","Index",
                             "Log index","SPR ratio","","Management target","Minimum stock size threshold"))
text(median(base$mcmc$ForeCatch_2014),0.6*mean(par()$usr[3:4]),paste("Median",round(median(base$mcmc$ForeCatch_2014)/1e6,3),sep=" = "),srt=90,adj=c(0.5,1))
text(median(run32$mcmc$ForeCatch_2014),0.6*mean(par()$usr[3:4]),paste("Median",round(median(run32$mcmc$ForeCatch_2014)/1e6,3),sep=" = "),srt=90,adj=c(0.5,-0.5))
if(doPNG) {dev.off()}

ht <- 3.25; wd<- 6.5
if(doPNG) {png(file.path(figDir,"run32_depl.png"),height=ht,width=wd,pointsize=10,units="in",res=300)}
if(!doPNG) {windows(width=wd,height=ht)}
par(mfrow=c(1,1),las=1,mar=c(3.6,3.6,1,1),oma=c(0,0,0,0),mgp=c(2.5,1,0))
SSplotComparisons(mysummary,legendlabels=modelnames,endyr=c(2014,2014),new=F,minbthresh=0,subplots=2,legend=T,btarg=-0.4,mcmc=c(T,T),legendloc="topleft",xlim=c(1966,2014))
abline(v=2014-5)
abline(h=c(0.1,0.4),lty=2)
axis(1,seq(1975,2015,10))
axis(2,at=c(0.1,0.4),cex.axis=0.7)
if(doPNG) {dev.off()}



median(base$mcmc$ForeCatch_2014)
median(run32$mcmc$ForeCatch_2014)















SSdir <- "Models"
run29 <- SS_output(dir=file.path(SSdir,"2014hake_29_highTVfrom21"),covar=F)
run30 <- SS_output(dir=file.path(SSdir,"2014hake_30_TVselex1975start_from21"),covar=F)

#################################################
#################################################
# Fishery selectivity for sensitivity
selex <- base$ageselex[base$ageselex$factor=="Asel" & base$ageselex$fleet==1,-c(1,2,4,5,6,7)]
allSel <- data.frame(year=c(1963,1966:2013))
allSel <- merge(selex,allSel,by.x=1,by.y=1,all.x=F,all.y=T)
for(i in 1:nrow(allSel)) {
    if(all(is.na(allSel[i,-1]))) {
        allSel[i,-1] <- allSel[1,-1]
    }
}
rownames(allSel) <- allSel[,1]
allSel_base <- allSel[,-1]

selex <- run29$ageselex[run29$ageselex$factor=="Asel" & run29$ageselex$fleet==1,-c(1,2,4,5,6,7)]
allSel <- data.frame(year=c(1963,1966:2013))
allSel <- merge(selex,allSel,by.x=1,by.y=1,all.x=F,all.y=T)
for(i in 1:nrow(allSel)) {
    if(all(is.na(allSel[i,-1]))) {
        allSel[i,-1] <- allSel[1,-1]
    }
}
rownames(allSel) <- allSel[,1]
allSel29 <- allSel[,-1]

selex <- run30$ageselex[run30$ageselex$factor=="Asel" & run30$ageselex$fleet==1,-c(1,2,4,5,6,7)]
allSel <- data.frame(year=c(1963,1966:2013))
allSel <- merge(selex,allSel,by.x=1,by.y=1,all.x=F,all.y=T)
for(i in 1:nrow(allSel)) {
    if(all(is.na(allSel[i,-1]))) {
        allSel[i,-1] <- allSel[1,-1]
    }
}
rownames(allSel) <- allSel[,1]
allSel30 <- allSel[,-1]


cexax <- 1
doPNG <- T
ht <- 8; wd<-6.5 
if(doPNG) {png(file.path(figDir,"TVselexSens1.png"),height=ht,width=wd,pointsize=10,units="in",res=300)}
if(!doPNG) {windows(width=wd,height=ht)}
par(mfrow=c(1,3),oma=c(0,1.1,0,0))
ind <- 1:12
tmp <- allSel_base[,ind]
mountains(tmp,xvec=ind-1,yvec=as.numeric(row.names(tmp)),rev=T,cex.axis=0.8,axes=F)
title(main="Base model\n(start 1991, low SD)",cex=1.1)
axis(1,at=ind-1,label=c(as.character(0:10),"11+"),cex.axis=cexax-0.1)
axis(2,at=-1*(1966:2013),label=1966:2013,cex.axis=cexax,las=1)
axis(2,at=-1963,"Equilibrium",cex.axis=cexax,las=1)
tmp <- allSel29[,ind]
mountains(tmp,xvec=ind-1,yvec=as.numeric(row.names(tmp)),rev=T,cex.axis=0.8,axes=F)
title(main="High SD\n(start 1991)",cex=1.1)
axis(1,at=ind-1,label=c(as.character(0:10),"11+"),cex.axis=cexax-0.1)
axis(2,at=-1*(1966:2013),label=1966:2013,cex.axis=cexax,las=1)
axis(2,at=-1963,"Equilibrium",cex.axis=cexax,las=1)
tmp <- allSel30[,ind]
mountains(tmp,xvec=ind-1,yvec=as.numeric(row.names(tmp)),rev=T,cex.axis=0.8,axes=F)
title(main="Start 1975\n(low SD)",cex=1.1)
axis(1,at=ind-1,label=c(as.character(0:10),"11+"),cex.axis=cexax-0.1)
axis(2,at=-1*(1966:2013),label=1966:2013,cex.axis=cexax,las=1)
axis(2,at=-1963,"Equilibrium",cex.axis=cexax,las=1)
mtext("Year",side=2,outer=T,line=-0.9)
mtext("Age",side=1,outer=T,line=-1.2)
if(doPNG) {dev.off()}
