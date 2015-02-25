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
  SSdir2014 <- "C:/ss/hake/Hake_2014/runs/"
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
if(FALSE){ # don't run a bunch of models by accident
  SS_doRetro(masterdir=retrodir, oldsubdir="", newsubdir="retrospectives", years=0:-15)
}
retroModels <- SSgetoutput(dirvec=file.path(retrodir, "retrospectives",
                               paste0("retro",0:-15)))
retroSummary <- SSsummarize(retroModels[1:15])
endyrvec <- retroSummary$endyrs + 1 + 0:-15
SSplotComparisons(retroSummary, endyrvec=endyrvec,
                  legendlabels=paste("Data",0:-15,"years"))

# squid plot
ht <- 3.75; wd<- 6.5
if(doPNG) {png(file.path(figDir,"squidPlot_2015.png"),
               height=ht,width=wd,pointsize=10,units="in",res=300)}
if(!doPNG) {windows(width=wd,height=ht)}
par(mar=c(4,4,1,1)+0.1)
SSplotRetroRecruits(retroSummary,endyrvec,cohorts=1999:2012,ylim=NULL,uncertainty=FALSE,
                    main="", mcmcVec=FALSE, devs=TRUE, relative=FALSE,
                    labelyears=TRUE,legend=FALSE,leg.ncols=4)
if(doPNG) {dev.off()}

# squid plot of recruit dev strength relative to most recent estimate
ht <- 3.75; wd<- 6.5
if(doPNG) {png(file.path(figDir,"squidPlot_relative_2015.png"),
               height=ht,width=wd,pointsize=10,units="in",res=300)}
if(!doPNG) {windows(width=wd,height=ht)}
par(mar=c(4,4,1,1)+0.1)
tmp <- SSplotRetroRecruits(retroSummary,endyrvec,cohorts=1999:2012,ylim=NULL,uncertainty=FALSE,
                           main="", mcmcVec=FALSE, devs=TRUE, relative=TRUE,
                           labelyears=TRUE, legend=FALSE,leg.ncols=4)
if(doPNG) {dev.off()}


### retro plot with MCMC
base <- SS_output(dir=file.path(SSdir,"2015hake_basePreSRG"))
mcmc <- SSgetMCMC(dir=file.path(SSdir,"2015hake_basePreSRG_mcmc12e6"),writecsv=FALSE)
if(nrow(mcmc$model1)!=999){
  stop("MCMC is not 999 rows! Make sure you did this on purpose.")
}
base$mcmc <- data.frame(mcmc$model1)

# repeating base model, which isn't ideal, but quicker
retro.list <- list(base, base, base, base, base, base)
retro.summ <- SSsummarize(retro.list)
retro.summ$mcmc[[1]] <- base$mcmc
for(i in 1:5){
  retro.summ$mcmc[[i+1]] <- SSgetMCMC(dir=file.path(SSdir,
                                        paste0("2015hake_basePreSRG_retros/retro-",i)))[[1]]
}
# retro names
retro_names <- c("Base model",
                 "-1 year",
                 paste(-2:-5,"year"))
# retro table
compareHakeTable(retro.summ, modelnames=retro_names,
                 csv=FALSE, models = "all",
                 mcmc=TRUE)

# retro plot
dir.create(file.path(figDir, "retro_plots"))
SSplotComparisons(retro.summ,legendlabels=retro_names,
                  plotdir=file.path(figDir, "retro_plots"),
                  mcmc=rep(TRUE,6),
                  png=TRUE,
                  plot=FALSE,
                  indexUncertainty=TRUE,
                  spacepoints=3000, # this removes points on lines
                  labels=comparisonLabels, # change label on spawn bio plot
                  endyr=2015-0:5,new=F,minbthresh=0.1,btarg=0.4,
                  subplots=1:20,legendloc="topleft",legendncol=2,
                  pwidth=6.5, pheight=3.75, ptsize=10,
                  par=list(mar=c(3.6,3.6,1,1),oma=c(0,0,0,0),mgp=c(2.5,1,0)))
# redo recruit plot to avoid legend overlap
SSplotComparisons(retro.summ,legendlabels=retro_names,
                  plotdir=file.path(figDir, "retro_plots"),
                  mcmc=rep(TRUE,6),
                  png=TRUE,
                  plot=FALSE,
                  indexUncertainty=TRUE,
                  spacepoints=3000, # this removes points on lines
                  labels=comparisonLabels, # change label on spawn bio plot
                  endyr=2015-0:5,new=F,minbthresh=0.1,btarg=0.4,
                  subplots=7,legendloc="topleft",legendncol=1,
                  pwidth=6.5, pheight=3.75, ptsize=10,
                  par=list(mar=c(3.6,3.6,1,1),oma=c(0,0,0,0),mgp=c(2.5,1,0)))


### comparison of 2014 and 2015 models
# assuming that base and mcmc already loaded above
base2014 <- SS_output(dir=file.path(SSdir2014,
                          "2014hake_21_TVselex1991start_cleanInputFiles"))
mcmc2014 <- SSgetMCMC(dir=file.path(SSdir2014,
                          "2014hake_21_TVselex1991start_MCMC"),
                      writecsv=FALSE)
if(nrow(mcmc2014$model1)!=999){
  stop("MCMC is not 999 rows! Make sure you did this on purpose.")
}
base2014$mcmc2014 <- data.frame(mcmc2014$model1)
summary14_15 <- SSsummarize(list(base, base2014))


### read base model if not already in workspace from other code
if (!exists("base")){
  base <- SS_output(dir=file.path(SSdir,"2015hake_basePreSRG"))
}

### read sensitivities
# autocorrelation
sens_autoCorRecr     <- SS_output(dir=file.path(SSdir,"Sensitivities",
                                      "2015hake_basePreSRG_sens_autoCorRecr"))
sens_autoCorRecr2    <- SS_output(dir=file.path(SSdir,"Sensitivities",
                                      "2015hake_basePreSRG_sens_autoCorRecr_-0.3"))
sens_constantWtAtAge <- SS_output(dir=file.path(SSdir,"Sensitivities",
                                      "2015hake_basePreSRG_sens_constantWtAtAge"))
# lorenzen M
sens_LorenzenM       <- SS_output(dir=file.path(SSdir,"Sensitivities",
                                      "2015hake_basePreSRG_sens_LorenzenM"))
# humboldt squid stuff
sens_Squid1_add      <- SS_output(dir=file.path(SSdir,"Sensitivities",
                                      "2015hake_basePreSRG_sens_Squid1_age0-4add"))
sens_Squid2          <- SS_output(dir=file.path(SSdir,"Sensitivities",
                                      "2015hake_basePreSRG_sens_Squid2"))
sens_Squid1_mult     <- SS_output(dir=file.path(SSdir,"Sensitivities",
                                      "2015hake_basePreSRG_sens_Squid1_age0-4mult"))
# survey stuff
sens_surv_low2013    <- SS_output(dir=file.path(SSdir,"Sensitivities",
                                      "2015hake_basePreSRG_sens_lower2013survey"))
sens_surv_low2013no2009 <- SS_output(dir=file.path(SSdir,"Sensitivities",
                                         "2015hake_basePreSRG_sens_no2009survey_lower2013survey"))
sens_surv_no2009     <- SS_output(dir=file.path(SSdir,"Sensitivities",
                                      "2015hake_basePreSRG_sens_no2009survey"))

# table of many sensitivities
sens_summary <- SSsummarize(list(base,
                                 sens_LorenzenM,
                                 sens_Squid1_mult,
                                 sens_constantWtAtAge,
                                 sens_autoCorRecr,
                                 sens_surv_low2013))
sens_names <- c("Base model",
                     "Lorenzen M (decreasing with age)",
                     "M related to Humboldt Squid",
                     "Time-invariance weight at age",
                     "Reduced 2013 survey",
                     "Lower 2013 survey")
# retro table
compareHakeTable(sens_summary, modelnames=sens_names,
                 csv=FALSE, models = "all",
                 mcmc=FALSE)

# comparison of survey sensitivities
sens_surv_summary <- SSsummarize(list(base, sens_surv_no2009, sens_surv_low2013,
                                      sens_surv_low2013no2009))
sens_surv_names <- c("Base model",
                     "No 2009 survey",
                     "Reduced 2013 survey",
                     "No 2009 survey and reduced 2013 survey")
colvec <- c(1,2,3,4)
SSplotComparisons(sens_surv_summary,legendlabels=sens_surv_names,
                  plotdir=file.path(figDir, "sensitivities_surveys"),
                  png=TRUE,
                  plot=FALSE,
                  indexUncertainty=TRUE,
                  spacepoints=3000, # this removes points on lines
                  labels=comparisonLabels, # change label on spawn bio plot
                  endyr=endYr,new=F,minbthresh=0,btarg=-0.4,
                  subplots=1:20,col=colvec,legendloc="topleft",
                  pwidth=6.5, pheight=3.75, ptsize=10,
                  par=list(mar=c(3.6,3.6,1,1),oma=c(0,0,0,0),mgp=c(2.5,1,0)))

# comparison of autocorellation sensitivities
sens_autoCor_summary <- SSsummarize(list(base,
                                         sens_autoCorRecr,
                                         sens_autoCorRecr2))
autCorEst <- sens_autoCorRecr$parameters["SR_autocorr","Value"]
autCorFix <- sens_autoCorRecr2$parameters["SR_autocorr","Value"]
sens_autoCor_names <- c("Base model",
                     paste("Estimated recruit. autocor. =",round(autCorEst,2)),
                     paste("Fixed recruit. autocor. =",round(autCorFix,2)))
colvec <- c(1,2,4)
dir.create(file.path(figDir, "sensitivities_autoCor"))
SSplotComparisons(sens_autoCor_summary,legendlabels=sens_autoCor_names,
                  plotdir=file.path(figDir, "sensitivities_autoCor"),
                  png=TRUE,
                  plot=FALSE,
                  indexUncertainty=TRUE,
                  spacepoints=3000, # this removes points on lines
                  labels=comparisonLabels, # change label on spawn bio plot
                  endyr=endYr,new=F,minbthresh=0,btarg=-0.4,
                  subplots=1:20,col=colvec,legendloc="topleft",
                  pwidth=6.5, pheight=3.75, ptsize=10,
                  par=list(mar=c(3.6,3.6,1,1),oma=c(0,0,0,0),mgp=c(2.5,1,0)))




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




#########################################################
### R0 profile
#########################################################

# vector of values to profile over
R0.vec <- seq(15.5,14.0,-.2)
Nprof.R0 <- length(R0.vec)

mydir <- file.path(SSdir, "Profiles/2015hake_basePreSRG_R0profile")


starter <- SS_readstarter(file.path(mydir, 'starter.ss'))
# change control file name in the starter file
starter$ctlfile <- "control_modified.ss"
# make sure the prior likelihood is calculated
# for non-estimated quantities
starter$prior_like <- 1
# write modified starter file
SS_writestarter(starter, dir=mydir, overwrite=TRUE)
# run SS_profile command
#  profile <- SS_profile(dir=mydir, # directory

  # note! had to change phase of SelPar1 from 2 to 1
  
  profile <- SS_profile(dir=getwd(), # directory
                        model="ss3",
                        masterctlfile="control.ss_new",
                        newctlfile="control_modified.ss",
                        string="R0",
                        profilevec=R0.vec)


  # read the output files (with names like Report1.sso, Report2.sso, etc.)
  setwd('C:/ss/thornyheads/runs/')
  prof.R0.models <- SSgetoutput(dirvec=mydir, keyvec=1:Nprof.R0)
  # summarize output
  prof.R0.summary <- SSsummarize(prof.R0.models)
  
  #  sbase <- SS_output('SST_BASE_pre-STAR')
  prof.R0.models$MLE <- sbase
  prof.R0.summary <- SSsummarize(prof.R0.models)
  # END OPTIONAL COMMANDS
  
  # plot profile using summary created above
  png(file.path(mydir,"R0_profile_plot.png"),width=7,height=4.5,res=300,units='in')
  par(mar=c(5,4,1,1))
  SSplotProfile(prof.R0.summary,           # summary object
                profile.string = "R0", # substring of profile parameter
                profile.label=expression(log(italic(R)[0])),ymax=15,
                pheight=4.5,print=FALSE,plotdir=mydir) # axis label
  baseval <- round(sbase$parameters$Value[grep("R0",sbase$parameters$Label)],2)
  baselab <- paste(baseval,sep="")
  axis(1,at=baseval,label=baselab)
  dev.off()
  # make timeseries plots comparing models in profile
  labs <- paste("log(R0) =",R0.vec[c(1,3,5,7,9,11)])
  labs[3] <- paste("log(R0) =",baseval,"(Base model)")
  SSplotComparisons(prof.R0.summary,legendlabels=labs,
                    models=c(1,3,12,7,9,11),
                    pheight=4.5,png=TRUE,plotdir=mydir,legendloc='bottomleft')

  round(range(prof.R0.summary$quants[prof.R0.summary$quants$Label=="SPB_Virgin",1:11]))
  ## 42263 312282
  round(100*range(prof.R0.summary$quants[prof.R0.summary$quants$Label=="Bratio_2013",1:11]),1)
  ## 39.2 87.9
  
  Qvec <- rep(NA,12)
  for(i in 1:12)
    Qvec[i] <- unique(prof.R0.summary$indices$Calc_Q[prof.R0.summary$indices$FleetNum==9 &
                                                     prof.R0.summary$indices$imodel==i])
  R0.vec2 <- c(R0.vec,baseval)
  plot(R0.vec2[1:11],Qvec[1:11],type='l',lwd=3,ylim=c(0,2),las=1,yaxs='i',
       xlab=expression(log(italic(R)[0])),ylab="Catchability of NWFSC Combo Survey")
  points(R0.vec2[12],Qvec[12],pch=16,cex=2)
  axis(1,at=baseval,label=baselab)
  axis(2,at=round(Qvec[12],2),las=1)

  deplvec <- prof.R0.summary$quants[prof.R0.summary$quants$Label=="Bratio_2013",1:12]
  B0vec <- prof.R0.summary$quants[prof.R0.summary$quants$Label=="SPB_Virgin",1:12]/1e3

  
  png(file.path(mydir,"R0_profile_dependencies.png"),width=7,height=9,res=300,units='in')
  par(mfrow=c(3,1),mar=c(1,4,1,1),oma=c(4,0,0,0),cex=.9)
  plot(R0.vec2[1:11],B0vec[1:11],type='l',lwd=3,ylim=c(0,max(B0vec)),las=1,yaxs='i',xaxs='i',
       xlab="",ylab=expression(paste(italic(B)[0]," (1000 mt)")))
  points(R0.vec2[12],B0vec[12],pch=16,cex=2)
  abline(h=B0vec[12],lty=3)
  axis(2,at=round(B0vec[12],2),las=1,padj=1.3)
  axis(1,at=baseval,label=baselab)
  abline(v=baseval,lty=3)

  plot(R0.vec2[1:11],deplvec[1:11],type='l',lwd=3,ylim=c(0,1),las=1,yaxs='i',xaxs='i',
       xlab="",ylab="Spawning depletion")
  points(R0.vec2[12],deplvec[12],pch=16,cex=2)
  abline(h=deplvec[12],lty=3)
  axis(1,at=baseval,label=baselab)
  abline(v=baseval,lty=3)

  plot(R0.vec2[1:11],Qvec[1:11],type='l',lwd=3,ylim=c(0,2),las=1,yaxs='i',xaxs='i',
       xlab="",ylab="Catchability of NWFSC Combo Survey")
  points(R0.vec2[12],Qvec[12],pch=16,cex=2)
  axis(1,at=baseval,label=baselab)
  axis(2,at=round(Qvec[12],2),las=1)
  abline(h=Qvec[12],lty=3)
  abline(v=baseval,lty=3)
  mtext(side=1,line=2,outer=TRUE,expression(log(italic(R)[0])))
  dev.off()
  
