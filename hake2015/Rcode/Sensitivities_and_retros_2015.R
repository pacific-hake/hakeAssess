### code for sensitivities and retrospectives from Ian in 2015 hake stock assessment

stop("\n  This file should not be sourced!")

#devtools::install_github('r4ss/r4ss')
require(r4ss)

# paths on Ian's computers (other folks can add their own statements)
if (system("hostname", intern=TRUE) %in% c("NWCDW01724920","NWCLW01724829","ian-THINK") ){
  hakedir <- "C:/SS/hake/Hake_2015/"
  rDir    <- "c:/GitHub/hakeAssess/hake2015/Rcode/"
  figDir  <- file.path(hakedir,"WriteUp/Figures")
  SSdir   <- file.path(hakedir, "Models")
  doMaps  <- FALSE
}

comparisonLabels <-
  c("Year", "Spawning biomass (mt)", "Relative spawning biomass", 
    "Age-0 recruits (1,000s)", "Recruitment deviations", 
    "Index", "Log index", "SPR ratio", "Density", "Management target", 
    "Minimum stock size threshold",
    "Female spawning biomass (million t)", # this value changed for hake 2015
    "Harvest rate")

### run retrospective analyses
# note: don't run this in your main directory--make a copy in case something goes wrong
retrodir <- "C:/ss/hake/Hake_2015/Models/2015hake_basePreSRG_retroMLE"

## retrospective analyses
SS_doRetro(masterdir=retrodir, oldsubdir="", newsubdir="retrospectives", years=0:-15)

retroModels <- SSgetoutput(dirvec=file.path(retrodir, "retrospectives",
                               paste0("retro",0:-15)))
retroSummary <- SSsummarize(retroModels[1:15])
endyrvec <- retroSummary$endyrs + 1 + 0:-15
SSplotComparisons(retroSummary, endyrvec=endyrvec,
                  legendlabels=paste("Data",0:-15,"years"))

ht <- 3.75; wd<- 6.5
if(doPNG) {png(file.path(figDir,"squidPlot_2015.png"),
               height=ht,width=wd,pointsize=10,units="in",res=300)}
if(!doPNG) {windows(width=wd,height=ht)}
par(mar=c(4,4,1,1)+0.1)
SSplotRetroRecruits(retroSummary,endyrvec,cohorts=1999:2012,ylim=NULL,uncertainty=FALSE,
           main="",
           mcmcVec=FALSE,devs=TRUE,
           relative=FALSE,labelyears=TRUE,legend=FALSE,leg.ncols=4)
if(doPNG) {dev.off()}

ht <- 3.75; wd<- 6.5
if(doPNG) {png(file.path(figDir,"squidPlot_relative_2015.png"),
               height=ht,width=wd,pointsize=10,units="in",res=300)}
if(!doPNG) {windows(width=wd,height=ht)}
par(mar=c(4,4,1,1)+0.1)
SSplotRetroRecruits(retroSummary,endyrvec,cohorts=1999:2012,ylim=NULL,uncertainty=FALSE,
           main="",
           mcmcVec=FALSE,devs=TRUE,
           relative=TRUE,labelyears=TRUE,legend=FALSE,leg.ncols=4)
if(doPNG) {dev.off()}




### read base model if not already in workspace from other code
if (!exists("base")){
  base <- SS_output(dir=file.path(SSdir,"2015hake_basePreSRG"))
}

### read sensitivities
sens_autoCorRecr     <- SS_output(dir=file.path(SSdir,
                                      "2015hake_basePreSRG_sens_autoCorRecr"))
sens_constantWtAtAge <- SS_output(dir=file.path(SSdir,
                                      "2015hake_basePreSRG_sens_constantWtAtAge"))
sens_LorenzenM       <- SS_output(dir=file.path(SSdir,
                                      "2015hake_basePreSRG_sens_LorenzenM"))
sens_Squid1_add      <- SS_output(dir=file.path(SSdir,
                                      "2015hake_basePreSRG_sens_Squid1_age0-4add"))
sens_Squid2          <- SS_output(dir=file.path(SSdir,
                                      "2015hake_basePreSRG_sens_Squid2"))
sens_Squid1_mult     <- SS_output(dir=file.path(SSdir,
                                      "2015hake_basePreSRG_sens_Squid1_age0-4mult"))

# plot of mortality over time for squid models
plot(0,xlim=c(1966,2014),ylim=c(0,0.4),type='n',xlab='Year',ylab='M')
lines(base$MGparmAdj$Year, base$MGparmAdj$NatM_p_1_Fem_GP_1, lwd=2, col=1)
lines(sens_Squid1_add$MGparmAdj$Year, sens_Squid1_add$MGparmAdj$NatM_p_1_Fem_GP_1,
      lwd=2,col=2)
lines(sens_Squid1_add$MGparmAdj$Year, sens_Squid1_add$MGparmAdj$NatM_p_2_Fem_GP_1,
      lwd=2,col=2,lty=2)
lines(sens_Squid1_mult$MGparmAdj$Year, sens_Squid1_mult$MGparmAdj$NatM_p_1_Fem_GP_1,
      lwd=2,col=3)
lines(sens_Squid1_mult$MGparmAdj$Year, sens_Squid1_mult$MGparmAdj$NatM_p_2_Fem_GP_1,
      lwd=2,col=3,lty=2)
lines(sens_Squid2$MGparmAdj$Year, sens_Squid2$MGparmAdj$NatM_p_1_Fem_GP_1,
      lwd=2,col=4)

# comparisons of squid models
squidM_summary <- SSsummarize(list(base, sens_Squid1_add,
                                   sens_Squid1_mult))
squidM_names <- c("base", "sens_Squid1_add", "sens_Squid1_mult")
SStableComparisons(squidM_summary, modelnames=squidM_names)
SSplotComparisons(squidM_summary, legendlabels=squidM_names, indexfleets=2)
### looks like multiplicative model has the most sensible results

# model comparison plots
sens_M_summary <- SSsummarize(list(base, sens_LorenzenM, sens_Squid1_mult))
baseMval <- round(base$parameters["NatM_p_1_Fem_GP_1","Value"],3)
sens_M_names <- c(paste0("Base model (M estimated at ",baseMval,")"),
                  "Lorenzen M (decreasing with age)",
                  "M related to Humboldt Squid")

SSplotComparisons(sens_M_summary,legendlabels=sens_M_names,
                  spacepoints=3000, # this removes points on lines
                  labels=comparisonLabels, # change label on spawn bio plot
                  endyr=endYr,new=F,minbthresh=0,btarg=-0.4,
                  subplots=4,col=colvec,legendloc="topright",
                  pwidth=6.5, pheight=3.75, ptsize=10,
                  par=list(las=1,mar=c(3.6,3.6,1,1),oma=c(0,0,0,0),mgp=c(2.5,1,0)))

colvec <- c(1,4,2)

ht <- 3.75; wd<- 6.5
if(doPNG) {png(file.path(figDir,"Sensitivities/sens_M_depl.png"),
               height=ht,width=wd,pointsize=10,units="in",res=300)}
if(!doPNG) {windows(width=wd,height=ht)}
par(mfrow=c(1,1),las=1,mar=c(3.6,3.6,1,1),oma=c(0,0,0,0),mgp=c(2.5,1,0))
SSplotComparisons(sens_M_summary,legendlabels=sens_M_names,
                  spacepoints=3000, # this removes points on lines
                  labels=comparisonLabels, # change label on spawn bio plot
                  endyr=endYr,new=F,minbthresh=0,btarg=-0.4,
                  subplots=4,col=colvec,legendloc="topright")
if(doPNG){dev.off()}

ht <- 3.75; wd<- 6.5
if(doPNG) {png(file.path(figDir,"Sensitivities/sens_M_spb.png"),
               height=ht,width=wd,pointsize=10,units="in",res=300)}
if(!doPNG) {windows(width=wd,height=ht)}
par(mfrow=c(1,1),las=1,mar=c(3.6,3.6,1,1),oma=c(0,0,0,0),mgp=c(2.5,1,0))
SSplotComparisons(sens_M_summary,legendlabels=sens_M_names,
                  spacepoints=3000, # this removes points on lines
                  labels=comparisonLabels, # change label on spawn bio plot
                  endyr=endYr,new=F,minbthresh=0,btarg=-0.4,
                  subplots=2,col=colvec,legendloc="topright")
if(doPNG){dev.off()}

ht <- 3.75; wd<- 6.5
if(doPNG) {png(file.path(figDir,"Sensitivities/sens_M_recr.png"),
               height=ht,width=wd,pointsize=10,units="in",res=300)}
if(!doPNG) {windows(width=wd,height=ht)}
par(mfrow=c(1,1),las=1,mar=c(3.6,3.6,1,1),oma=c(0,0,0,0),mgp=c(2.5,1,0))
SSplotComparisons(sens_M_summary,legendlabels=sens_M_names,
                  spacepoints=3000, # this removes points on lines
                  labels=comparisonLabels, # change label on spawn bio plot
                  endyr=endYr,new=F,minbthresh=0,btarg=-0.4,
                  subplots=8,col=colvec,legendloc="topright")
if(doPNG){dev.off()}


# plot of M vs. age and year from 3 models
ht <- 3.25; wd <- 6.5
if(doPNG) {png(file.path(figDir,"Sensitivities/sensitivities_for_NatM.png"),
               height=ht,width=wd,pointsize=8,units="in",res=300)}
if(!doPNG) {windows(width=wd,height=ht)}
#
par(mfrow=c(1,2),mar=c(4.2,3,.2,.2),oma=c(0,1,0,0))
# M with age plot
par(las=1)
SSplotBiology(sens_LorenzenM, subplot=11, colvec=4, labels = c("Length (cm)", 
        "Age", "Maturity", "Mean weight (kg) in last year", 
        "Spawning output", "Length (cm, middle of the year)", 
        "Natural mortality", "Female weight (kg)", "Female length (cm)", 
        "Fecundity", "Default fecundity label", "Year"))
# more complex call to get squid M in year = 2009
lines(x=0:20, c(rep(max(sens_Squid1_mult$MGparmAdj$NatM_p_1_Fem_GP_1), 5),
          rep(sens_Squid1_mult$MGparmAdj$NatM_p_2_Fem_GP_1[1], 16)),
      lwd=2, col=2, lty=1, type='o', pch=21, bg='white')
# add base level of M for squid model
lines(x=0:20, c(rep(sens_Squid1_mult$MGparmAdj$NatM_p_1_Fem_GP_1[1], 5),
          rep(sens_Squid1_mult$MGparmAdj$NatM_p_2_Fem_GP_1[1], 16)),
      lwd=2, col=2, lty=1, type='o', pch=21, bg=2)
# subplot 11 doesn't appear for base model as M is constant with age
lines(x=0:20, rep(base$parameters["NatM_p_1_Fem_GP_1","Value"],21),
      lty=1, col=1, lwd=3)
# add legend
legend('topright', lwd=c(3,2,2,2), pch=c(NA,1,21,21), lty=c(1,1,1,1),
       col=c(1,4,2,2), pt.bg=c(1,1,'white',2), bty='n', inset=0.05,
       legend=c(sens_M_names[1:2],
           "M related to Humboldt Squid (2009)",
           "M related to Humboldt Squid (average year)"))
#                                        
# M with time plot
# plot of mortality over time for squid models (with same axis range)
plot(0,xlim=c(1966,2014),ylim=c(par()$usr[3], par()$usr[4]),yaxs='i',
     type='n',xlab='Year',ylab='')
for(age in 0:5){
  lines(sens_LorenzenM$MGparmAdj$Year,
        rep(sens_LorenzenM$endgrowth$M[sens_LorenzenM$endgrowth$Age==age],
            length(sens_LorenzenM$MGparmAdj$Year)),
        lwd=2,col=4,lty=age+1)
}
# add base model
lines(base$MGparmAdj$Year, base$MGparmAdj$NatM_p_1_Fem_GP_1, col=1, lwd=3)
# add squid for young and old
lines(sens_Squid1_mult$MGparmAdj$Year, sens_Squid1_mult$MGparmAdj$NatM_p_2_Fem_GP_1,
      lwd=2,col=2,lty=2)
lines(sens_Squid1_mult$MGparmAdj$Year, sens_Squid1_mult$MGparmAdj$NatM_p_1_Fem_GP_1,
      lwd=2,col=2,lty=1)
abline(h=0,col='grey')
#
mtext(side=2, line=0, outer=TRUE, "Natural mortality (M)", las=0)
#
legend('topright', lwd=c(3,2,2,2), lty=c(1,1,1),
       col=c(1,4,2), bty='n', inset=0.05,
       legend=c(sens_M_names[1],"Lorenzen M (lines for ages 0-5)",
           "M related to Humboldt Squid"))
if(doPNG) {dev.off()}
