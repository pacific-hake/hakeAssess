### Forecast figures

stop("\n  This file should not be sourced!")

# paths on Ian's computers (other folks can add their own statements)
if (system("hostname", intern=TRUE) %in% c("NWCDW01724920","NWCLW01724829","ian-THINK") ){
  hakedir <- "C:/SS/hake/Hake_2015/"
  rDir    <- "c:/GitHub/hakeAssess/hake2015/Rcode/"
  figDir  <- file.path(hakedir,"WriteUp/Figures")
  SSdir   <- file.path(hakedir, "Models")
}

# paths on Allan's computers (other folks can add their own statements)
if (system("hostname", intern=TRUE) %in% c("NWCDW01724919") ){
  hakedir <- "C:/NOAA2015/Hake"
  rDir    <- "~/GitHub/hakeAssess/hake2015/Rcode"
  figDir  <- file.path(hakedir,"WriteUp/Figures")
  tableDir<- file.path(hakedir,"WriteUp/Tables")
  SSdir   <- file.path(hakedir, "Models")
}

#devtools::install_github("r4ss/r4ss")
library(r4ss)


base <- SS_output(dir=file.path(SSdir,"2015hake_basePreSRG"),covar=F)
mcmc <- SSgetMCMC(dir=file.path(SSdir,"2015hake_basePreSRG_mcmc"),writecsv=F)
base$mcmc <- data.frame(mcmc$model1)


## Forecasts
models <- paste(SSdir,"2015hake_basePreSRG_mcmc_decisionTable",
	            c("1_2015hake_basePreSRG_mcmc_0","2_2015hake_basePreSRG_mcmc_180",
	            	"4_2015hake_basePreSRG_mcmc_428","7_2015hake_basePreSRG_mcmc_defaultHR"),sep="/")
tmp <- SSgetMCMC(dir=models,writecsv=F)
doPNG <- T
mymodels <- list(base,base,base,base); modelnames <- c("No Fishing","180,000 t","428,000 t","Default: 804,576 t")
mysummary <- SSsummarize(mymodels)
mysummary$mcmc <- tmp
ht <- 3.25; wd<- 6.5
if(doPNG) {png(file.path(figDir,"forecasts.png"),height=ht,width=wd,pointsize=10,units="in",res=300)}
if(!doPNG) {windows(width=wd,height=ht)}
par(mar=c(4.5,4,1,1))
SSplotComparisons(mysummary, legendlabels=modelnames,endyr=2017,densitynames=c("Bratio_2015"),new=F,minbthresh=0,subplots=4,
	              plot=T,mcmc=rep(T,4),xlim=c(2009,2017),legendloc="topleft",
                  labels=c("Year","Spawning biomass (t)","Relatvie Spawning Biomass","Age-0 recruits (1,000s)","Recruitment deviations","Index",
                 "Log index","SPR ratio","Density","",""),btarg=-0.4,staggerpoints= 1990, spacepoints=200)
abline(h=c(0.1,0.4),lty=2,col="grey")
axis(2,at=c(0.1,0.4),las=1,cex.axis=0.8)
axis(1,at=seq(2009,2017,2))
if(doPNG){dev.off()}


doPNG <- T

#draw a figure of metrics
metrics <- read.csv(file.path(tableDir,"metrics2015.csv"))
metrics <- metrics[,-1]
metrics$Catch <- metrics$Catch/1e3
if(!is.numeric(metrics[1,2])) {
	tmp <- as.character(metrics[1,2])
	if(substring(tmp,nchar(tmp))=="%") {
		for(i in 2:ncol(metrics)) {
			metrics[,i] <- as.numeric(substring(as.character(metrics[,i]),1,nchar(as.character(metrics[,i]))-1))/100
		}
	}
}
catches <- metrics$Catch
ht <- 3.75; wd<- 6.5
if(doPNG) {png(file.path(figDir,"metrics2015.png"),height=ht,width=wd,pointsize=10,units="in",res=300)}
if(!doPNG) {windows(width=wd,height=ht)}
par(mfrow=c(1,1),las=1,mar=c(3.6,3.6,1,1),oma=c(0,0,0,0),mgp=c(2.5,1,0))
plot(metrics$Catch,metrics[,2],ylim=c(0,1),xaxt="n",ylab="Probability",xlab="Catch in 2015 ('000 t)",type="b",lty=2,pch=16)
lines(metrics$Catch,metrics[,3],col="blue",type="b",lty=2,pch=17)
lines(metrics$Catch,metrics[,4],col="green",type="b",lty=2,pch=17)
lines(metrics$Catch,metrics[,5],col="orange",type="b",lty=2,pch=17)
lines(metrics$Catch,metrics[,6],col="red",type="b",lty=2,pch=15)
lines(metrics$Catch,metrics[,7],col="tan",type="b",lty=2,pch=18)
abline(h=0.5,lty=2,lwd=1,col="grey")
legend("topleft",
	   legend=c("P(B2016 < B2015): Stock declines in 2016","P(2015 Fishing Intensity > Target of 40%)",
	   	        "P(C2016 < C2015): F40% catch declines in 2016","P(B2016 < B40%)","P(B2016 < B25%)","P(B2016 < B10%)"),
	   col=c("black","red","tan","blue","green","orange"),lty=1,lwd=2,pch=c(16,15,18,17,17,17),cex=0.7,bty="n")
axis(1,at=round(metrics$Catch,0),cex.axis=0.9,las=2)
if(doPNG){dev.off()}

metrics <- read.csv(file.path(tableDir,"metrics2016.csv"))
metrics <- metrics[,-1]
metrics$Catch <- metrics$Catch/1e3
if(!is.numeric(metrics[1,2])) {
	tmp <- as.character(metrics[1,2])
	if(substring(tmp,nchar(tmp))=="%") {
		for(i in 2:ncol(metrics)) {
			metrics[,i] <- as.numeric(substring(as.character(metrics[,i]),1,nchar(as.character(metrics[,i]))-1))/100
		}
	}
}
metrics <- metrics[order(metrics$Catch),]
catches <- metrics$Catch
ht <- 3.75; wd<- 6.5
if(doPNG) {png(file.path(figDir,"metrics2016.png"),height=ht,width=wd,pointsize=10,units="in",res=300)}
if(!doPNG) {windows(width=wd,height=ht)}
par(mfrow=c(1,1),las=1,mar=c(3.6,3.6,1,1),oma=c(0,0,0,0),mgp=c(2.5,1,0))
plot(metrics$Catch,metrics[,2],ylim=c(0,1),xaxt="n",ylab="Probability",xlab="Catch in 2016 ('000 t)",type="b",lty=2,pch=16)
lines(metrics$Catch,metrics[,3],col="blue",type="b",lty=2,pch=17)
lines(metrics$Catch,metrics[,4],col="green",type="b",lty=2,pch=17)
lines(metrics$Catch,metrics[,5],col="orange",type="b",lty=2,pch=17)
lines(metrics$Catch,metrics[,6],col="red",type="b",lty=2,pch=15)
lines(metrics$Catch,metrics[,7],col="tan",type="b",lty=2,pch=18)
abline(h=0.5,lty=2,lwd=1,col="grey")
legend("topleft",
	   legend=c("P(B2017 < B2016): Stock declines in 2017","P(2016 Fishing Intensity > Target of 40%)",
	   	        "P(C2017 < C2016): F40% catch declines in 2017","P(B2017 < B40%)","P(B2017 < B25%)","P(B2017 < B10%)"),
	   col=c("black","red","tan","blue","green","orange"),lty=1,lwd=2,pch=c(16,15,18,17,17,17),cex=0.7,bty="n")
axis(1,at=round(metrics$Catch,0),cex.axis=0.9,las=2)
if(doPNG){dev.off()}


