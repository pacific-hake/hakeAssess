#########
# MCMC diagnostics

#devtools::install_github("r4ss/r4ss", ref="hake2015mods_Allan")
library(r4ss)
library(gtools)

# path on Allan's computer
try(setwd("C:/NOAA2015/Hake"))
# path on one of Ian's computers (others to be added later)
if(system("hostname") %in% "ian-THINK" ){
  setwd("C:/SS/hake/Hake_2015/")
}
SSdir <- file.path(getwd(),"Models")
doPNG <- F

plotMcmcVar <- function(x1,x2,colName) {
	x1 <- x1[,colName]
	plot(x1,type="n",xlab="Iteration",ylab=colName)
	lines(running(x1, fun=median, allow.fewer=TRUE, width=nrow(ach)), col="black")
	lines(running(x1, fun=quantile, allow.fewer=TRUE, width=nrow(ach), probs=0.025), col="grey")
	lines(running(x1, fun=quantile, allow.fewer=TRUE, width=nrow(ach), probs=0.975), col="grey")
	x2 <- x2[,colName]
	lines(running(x2, fun=median, allow.fewer=TRUE, width=nrow(ach)), col=rgb(0,0,1,0.8))
	lines(running(x2, fun=quantile, allow.fewer=TRUE, width=nrow(ach), probs=0.025), col=rgb(0,0,1,0.5))
	lines(running(x2, fun=quantile, allow.fewer=TRUE, width=nrow(ach), probs=0.975), col=rgb(0,0,1,0.5))
	acf(x1,lag.max=10,ylim=c(-0.2,0.2),lwd=3)
	tmp <- acf(x2,lag.max=10,plot=F)
	points(tmp$lag[,1,1]+0.1,tmp$acf[,1,1],type="h",col="blue",lwd=3)
}


#no burnin and no thinning
hakeMCMC.ach <- SSgetMCMC(dir=paste(SSdir,"2015hake_basePreSRG_mcmc12e6_Allan",sep="/"), writecsv=T,
                   keystrings = c("NatM", "R0", "steep", "Q_extraSD"),
                   nuisancestrings = c("Objective_function", "SPB_", "InitAge", "RecrDev", "Recr"))
hakeMCMC.igt <- SSgetMCMC(dir=paste(SSdir,"2015hake_basePreSRG_mcmc12e6_Ian",sep="/"), writecsv=T,
                   keystrings = c("NatM", "R0", "steep", "Q_extraSD"),
                   nuisancestrings = c("Objective_function", "SPB_", "InitAge", "RecrDev", "Recr"))
#compare the two parallel runs
ach <- cbind(read.csv(paste(SSdir,"2015hake_basePreSRG_mcmc12e6_Allan","keyposteriors.csv",sep="/")),
             read.csv(paste(SSdir,"2015hake_basePreSRG_mcmc12e6_Allan","nuisanceposteriors.csv",sep="/")))
igt <- cbind(read.csv(paste(SSdir,"2015hake_basePreSRG_mcmc12e6_Ian","keyposteriors.csv",sep="/")),
             read.csv(paste(SSdir,"2015hake_basePreSRG_mcmc12e6_Ian","nuisanceposteriors.csv",sep="/")))

windows(height=7,width=10)
par(mfrow=c(4,2),mar=c(3,4,1,1),oma=c(1,0,1,0))
plotMcmcVar(ach,igt,colName="NatM_p_1_Fem_GP_1")
plotMcmcVar(ach,igt,colName="SPB_Virgin")
plotMcmcVar(ach,igt,colName="SPB_2015")
plotMcmcVar(ach,igt,colName="Recr_2010")
mtext("Iteration",side=1,line=-0.7,outer=T)
mtext("No burnin, no thin",side=3,line=-0.6,outer=T)

#burnin and thin
hakeMCMC.ach <- SSgetMCMC(dir=paste(SSdir,"2015hake_basePreSRG_mcmc12e6_Allan",sep="/"), writecsv=T,
                   keystrings = c("NatM", "R0", "steep", "Q_extraSD"),
                   nuisancestrings = c("Objective_function", "SPB_", "InitAge", "RecrDev", "Recr"),
                   burnin=401,thin=2)
hakeMCMC.igt <- SSgetMCMC(dir=paste(SSdir,"2015hake_basePreSRG_mcmc12e6_Ian",sep="/"), writecsv=T,
                   keystrings = c("NatM", "R0", "steep", "Q_extraSD"),
                   nuisancestrings = c("Objective_function", "SPB_", "InitAge", "RecrDev", "Recr"),
                   burnin=401,thin=2)
#compare the two parallel runs
ach <- cbind(read.csv(paste(SSdir,"2015hake_basePreSRG_mcmc12e6_Allan","keyposteriors.csv",sep="/")),
             read.csv(paste(SSdir,"2015hake_basePreSRG_mcmc12e6_Allan","nuisanceposteriors.csv",sep="/")))
igt <- cbind(read.csv(paste(SSdir,"2015hake_basePreSRG_mcmc12e6_Ian","keyposteriors.csv",sep="/")),
             read.csv(paste(SSdir,"2015hake_basePreSRG_mcmc12e6_Ian","nuisanceposteriors.csv",sep="/")))

windows(height=7,width=10)
par(mfrow=c(4,2),mar=c(3,4,1,1),oma=c(1,0,1,0))
plotMcmcVar(ach,igt,colName="NatM_p_1_Fem_GP_1")
plotMcmcVar(ach,igt,colName="SPB_Virgin")
plotMcmcVar(ach,igt,colName="SPB_2015")
plotMcmcVar(ach,igt,colName="Recr_2010")
mtext("Iteration",side=1,line=-0.7,outer=T)
mtext("Burnin=401, Thin=2",side=3,line=-0.6,outer=T)













ht <- 4; wd<- 4.5

if(doPNG) {png("Writeup/Figures/mcmcM.png",height=ht,width=wd,pointsize=10,units="in",res=300)}
if(!doPNG) {windows(width=wd,height=ht)}
par(mar=c(5,3.5,0,0.5),oma=c(0,2.5,0.2,0))
mcmc.out(paste(SSdir,"2015hake_basePreSRG_mcmc12e6_Allan/",sep="/"),run="",numparams=1,closeall=F,new=F,colNames=c("NatM_p_1_Fem_GP_1"))
mtext("M (natural mortality)",side=2,outer=T,line=1.5,cex=1.1)
if(doPNG){dev.off()}

if(doPNG) {png("Writeup/Figures/mcmcR0.png",height=ht,width=wd,pointsize=10,units="in",res=300)}
if(!doPNG) {windows(width=wd,height=ht)}
par(mar=c(5,3.5,0,0.5),oma=c(0,2.5,0.2,0))
mcmc.out(paste(SSdir,"2015hake_basePreSRG_mcmc12e6_Allan/",sep="/"),run="",numparams=1,closeall=F,new=F,colNames=c("SR_LN.R0."))
mtext("ln(R0) (initial recruitment)",side=2,outer=T,line=1.5,cex=1.1)
if(doPNG){dev.off()}

if(doPNG) {png("Writeup/Figures/mcmcSteep.png",height=ht,width=wd,pointsize=10,units="in",res=300)}
if(!doPNG) {windows(width=wd,height=ht)}
par(mar=c(5,3.5,0,0.5),oma=c(0,2.5,0.2,0))
mcmc.out(paste(SSdir,"2015hake_basePreSRG_mcmc12e6_Allan/",sep="/"),run="",numparams=1,closeall=F,new=F,colNames=c("SR_BH_steep"))
mtext("h (steepness)",side=2,outer=T,line=1.5,cex=1.1)
if(doPNG){dev.off()}

if(doPNG) {png("Writeup/Figures/mcmcQextra.png",height=ht,width=wd,pointsize=10,units="in",res=300)}
if(!doPNG) {windows(width=wd,height=ht)}
par(mar=c(5,3.5,0,0.5),oma=c(0,2.5,0.2,0))
mcmc.out(paste(SSdir,"2015hake_basePreSRG_mcmc12e6_Allan/",sep="/"),run="",numparams=1,closeall=F,new=F,colNames=c("Q_extraSD_2_Acoustic_Survey"))
mtext("Extra SD in survey",side=2,outer=T,line=1.5,cex=1.1)
if(doPNG){dev.off()}


















#mcmc.out(paste(SSdir,"2015hake_basePreSRG_mcmc12e6_Allan/",sep="/"),run="",numparams=4)

vars <- c("NatM_p_1_Fem_GP_1","SR_LN.R0.","SR_BH_steep","Q_extraSD_2_Acoustic_Survey","Objective_function","SPB_Virgin","SPB_2015")
mc <- mcmc.list(mcmc(ach[,vars]),mcmc(igt[,vars]))






#####Some other stuff not tested for 2015

if(doPNG) {png("Writeup/Figures/mcmcDiagnostics.png",height=ht,width=wd,pointsize=10,units="in",res=300)}
if(!doPNG) {windows(width=wd,height=ht)}
par(mar=c(5,4,0,0.5),oma=c(0,0,0.5,0.5))
mcmc.nuisance(paste(SSdir,"2015hake_basePreSRG_mcmc12e6_Allan/",sep="/"),run="",labelstrings=c("NatM","R0", "steep", "Q_extraSD","SPB_","Bratio_","RecrDev_"),bothfiles=T)
if(doPNG){dev.off()}

stats1 <- mcmc.nuisance(paste(SSdir,"2015hake_basePreSRG_mcmc12e6_Ian/",sep="/"),run="",labelstrings=c("NatM","R0", "steep", "Q_extraSD","SPB_","Bratio_","RecrDev_"),bothfiles=T,thin=1,burn=0,  printstats=TRUE)
stats2 <- mcmc.nuisance(paste(SSdir,"2015hake_basePreSRG_mcmc12e6_Ian/",sep="/"),run="",labelstrings=c("NatM","R0", "steep", "Q_extraSD","SPB_","Bratio_","RecrDev_"),bothfiles=T,thin=2,burn=401,printstats=TRUE)
# which parameter failed test
stats2$Label[stats2$heidelwelsch=="Failed"]
# trace plot of that parameter
plot(hakeMCMC.igt$model1$Main_RecrDev_1975,type='l')

#scatterplot of key params and derived quants
ht <- 6.5; wd<- 6.5
if(doPNG) {png("Writeup/Figures/mcmcPairs.png",height=ht,width=wd,pointsize=10,units="in",res=300)}
if(!doPNG) {windows(width=wd,height=ht)}
pairs(data.frame(base$mcmc$Object,base$mcmc$NatM,base$mcmc$SR_LN,base$mcmc$SR_BH_steep,base$mcmc$Q,base$mcmc$Recr_2010,base$mcmc$Recr_2011,base$mcmc$Bratio_2014,base$mcmc$ForeCatch_2014),
    labels=c("Obj_fun","M","ln(R0)","h","surv_SD","Recr 2010","Recr 2011","Depl 2014","2014 Catch"),pch=".",cex.labels=1.2,xaxt="n",yaxt="n",las=1,gap=0.5,oma=c(0,0,0,0))
if(doPNG){dev.off()}


