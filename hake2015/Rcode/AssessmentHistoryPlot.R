
#############################################################
#History of assessments plot

stop("\n  This file should not be sourced!")

# paths on Ian's computers (other folks can add their own statements)
if (system("hostname", intern=TRUE) %in% c("NWCDW01724920","NWCLW01724829","ian-THINK") ){
  hakedir <- "C:/SS/hake/Hake_2015/"
  figDir  <- file.path(hakedir,"WriteUp/Figures")
  SSdir   <- file.path(hakedir, "Models")
}

# paths on Allan's computers (other folks can add their own statements)
if (system("hostname", intern=TRUE) %in% c("NWCDW01724919") ){
  hakedir <- "C:/NOAA2015/Hake"
  figDir  <- file.path(hakedir,"WriteUp/Figures")
  SSdir   <- file.path(hakedir, "Models")
}

library(r4ss)
doPNG <- F

addpoly <- function(yrvec, lower, upper, color){ # add shaded uncertainty intervals behind line
	lower[lower<0] <- 0 # max of value or 0
	shadeCol <- rgb(t(col2rgb(color)),alpha=0.2*255,maxColorValue=255)
	polygon(x=c(yrvec,rev(yrvec)),y=c(lower,rev(upper)), border=NA,col=shadeCol)
	lines(yrvec,lower,lty=3,col=color)
	lines(yrvec,upper,lty=3,col=color)
}

base <- SS_output(dir=file.path(SSdir,"2015hake_basePreSRG"),covar=F)
mcmc <- SSgetMCMC(dir=file.path(SSdir,"2015hake_basePreSRG_mcmc"),writecsv=F)
base$mcmc <- data.frame(mcmc$model1)

#The csv file should be set up with blanks for SPB in the most recent year

xx <- read.csv(file.path(SSdir,"HakeAssessHistory.csv"),as.is=T)
xx <- xx[xx$Value=="SB million mt",]
xx <- xx[!(xx$Model=="TINSS STAR update" | xx$Model=="TINSS Post-STAR" | xx$Model=="Base lowCI" | xx$Model=="Base highCI" ),]
xx[xx$Model=="TINSS SSC Final","Model"] <- "TINSS"
yearInd <- grep("X",names(xx))
years <- as.numeric(substring(names(xx[yearInd]),2))
latestAssess <- base$mcmc[,grep("SPB",names(base$mcmc))][,-c(1,2)]
latestYrs <- as.numeric(substring(names(latestAssess),5))
xx[nrow(xx),paste("X",years[years%in%latestYrs],sep="")] <- apply(latestAssess,2,median)[paste("SPB_",years[years%in%latestYrs],sep="")]/2e6
slower <- apply(latestAssess,2,quantile,probs=0.025)[paste("SPB_",years[years%in%latestYrs],sep="")]/2e6
supper <- apply(latestAssess,2,quantile,probs=0.975)[paste("SPB_",years[years%in%latestYrs],sep="")]/2e6

yrs <- sort(unique(xx$Year))
#cols <- rep(gray(c(0.4,0.5,0.6,0.7)),6)
cols=c(rgb(0.1,0.1,0.44),rgb(1,0.1,0.6),rgb(1,0.8,0),rgb(0,1,1),rgb(0.5,0,0.5),rgb(0.18,0.55,0.34),rgb(0,0,0.8),rgb(0,0.8,0.8),
       rgb(0.25,0.88,0.82,0.7),rgb(0.5,1,0.8,0.7),rgb(1,0.84,0,0.7,0.7),rgb(0,0.75,1,0.7),rgb(1,0,1,0.7),rgb(0.5,0.5,0.2,1),
       rgb(0.85,0.65,0.13,0.7),rgb(0.27,0.51,0.71),rgb(0.13,0.70,0.67),rgb(1,0,1),rgb(1,0,0),rgb(0,0,1))

lwds <- c(rep(1,nrow(xx)-1),3)
pchs <- rep(c(18,15,17,4,20,3),4) #repeat it more than necessary
legCol <- legPch <- rep(NA,nrow(xx))

ht <- 3.75; wd<- 6.5
if(doPNG) {png(file.path(figDir,"AssessmentHistory.png"),height=ht,width=wd,pointsize=10,units="in",res=300,family="serif")}
if(!doPNG) {windows(width=wd,height=ht)}
par(mar=c(3,3,1,6)+0.1, mgp=c(2, 1, 0))
plot(range(years),range(xx[,yearInd],na.rm=T),type="n",xlab="Year",ylab="Spawning Biomass (million t)",las=1,xlim=c(min(years),max(years)),cex.axis=0.9,cex.lab=1)
for(i in 1:nrow(xx)) {
    legCol[i] <- cols[yrs==xx$Year[i]]
    if(sum(xx$Year==xx$Year[i])>1) {  #put a symbol on it to differentiate years
        legPch[i] <- pchs[i]
    }
    lines(years,xx[i,yearInd],col=legCol[i],lwd=lwds[i],pch=legPch[i],type="o",cex=0.7)
}
legCol[i] <- rgb(0,0,0,0.8)
lwds[i] <- 3
lines(years,xx[i,yearInd],col=legCol[i],lwd=lwds[i])
addpoly(years[years%in%latestYrs],slower,supper,"black")

legend(2017.2,6.85,paste(xx$Year,xx$Model),col=legCol,lwd=lwds+1,pch=legPch,cex=0.7,bty="n",xpd=NA)
if(doPNG) {dev.off()}

