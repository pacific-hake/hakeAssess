# base vs. lower survey value MCMC
sens_lowAc     <- SS_output(file.path(SSdir, "2015hake_basePreSRG_lowAc_MLE"))
sens_lowAc_nlq <- SS_output(file.path(SSdir, "2015hake_basePreSRG_lowAc_Ian_nonLinearQ_parm"))
sens_lowAc_nlq$mcmc <- SSgetMCMC(file.path(SSdir, '2015hake_basePreSRG_lowAc_Ian_nonLinearQ_mcmc11e6'),
                                 writecsv=FALSE)[[1]]

sens_lowAc_summary <- SSsummarize(list(base,sens_lowAc,sens_lowAc_nlq))
sens_lowAc_summary$mcmc[[1]] <- base$mcmc
sens_lowAc_summary$mcmc[[2]] <- sens_lowAc$mcmc
sens_lowAc_summary$mcmc[[3]] <- sens_lowAc_nlq$mcmc

sens_lowAc_names <- c("Base model",
                     "Lower '12 & '13 survey",
                     "Lower '12 & '13 survey, non-linear Q")
colvec <- c(1,2,3)
dir.create(file.path(figDir, "sensitivities_surveys_mcmc_nlq"))
SSplotComparisons(sens_lowAc_summary,legendlabels=sens_lowAc_names,
                  plotdir=file.path(figDir, "sensitivities_surveys_mcmc_nlq"),
                  png=TRUE,
                  plot=FALSE,
                  mcmcVec=rep(TRUE,3),
                  col=colvec,
                  indexUncertainty=TRUE,
                  spacepoints=3000, # this removes points on lines
                  labels=comparisonLabels, # change label on spawn bio plot
                  endyr=endYr,new=F,minbthresh=0,btarg=-0.4,
                  subplots=c(1:10,13:20),legendloc="topright",
                  pwidth=6.5, pheight=3.75, ptsize=10,
                  par=list(mar=c(3.6,3.6,1,1),oma=c(0,0,0,0),mgp=c(2.5,1,0)))
SSplotComparisons(sens_lowAc_summary,legendlabels=sens_lowAc_names,
                  subplots=11:12, indexPlotEach=TRUE, shadealpha=0.7,
                  plotdir=file.path(figDir, "sensitivities_surveys_mcmc_nlq"),
                  png=TRUE,
                  plot=FALSE,
                  mcmcVec=rep(FALSE,3),
                  col=colvec,
                  indexUncertainty=TRUE,
                  spacepoints=3000, # this removes points on lines
                  labels=comparisonLabels, # change label on spawn bio plot
                  endyr=endYr,new=F,minbthresh=0,btarg=-0.4,
                  legendloc="topright",
                  pwidth=6.5, pheight=3.75, ptsize=10,
                  par=list(mar=c(3.6,3.6,1,1),oma=c(0,0,0,0),mgp=c(2.5,1,0)))


# base vs. lower survey value MCMC
sens_lowAc      <- SS_output(file.path(SSdir, "2015hake_basePreSRG_lowAc_MLE"))
sens_lowAc$mcmc <- SSgetMCMC(file.path(SSdir, '2015hake_basePreSRG_lowAcBio_mcmc'),
                             writecsv=FALSE)[[1]]
## lowAcAllan <- SSgetMCMC(file.path(SSdir,
##                                   'fromAllan_2-26-9pm',
##                                   '2015hake_basePreSRG_lowAcBio_mcmc6mil_extraSurv'),
##                                   writecsv=FALSE)[[1]]
sens_lowAc_summary <- SSsummarize(list(base,sens_lowAc))
sens_lowAc_summary$mcmc[[1]] <- base$mcmc
#sens_lowAc_summary$mcmc[[2]] <- sens_lowAc_short
sens_lowAc_summary$mcmc[[2]] <- sens_lowAc$mcmc
names(sens_lowAc_summary$mcmc[[1]])[4] <- "SR_LN(R0)"
sens_lowAc_names <- c("Base model",
                      "Modified '12 & '13 survey")
sens_lowAc_names_MLE <- c("Base model MLE",
                          "Modified '12 & '13 survey MLE")
colvec <- c(1,2)
dir.create(file.path(figDir, "sensitivities_surveys_mcmc"))
SSplotComparisons(sens_lowAc_summary,legendlabels=sens_lowAc_names,
                  plotdir=file.path(figDir, "sensitivities_surveys_mcmc"),
                  png=TRUE,
                  plot=FALSE,
                  mcmcVec=rep(TRUE,2),
                  col=colvec,
                  indexUncertainty=TRUE,
                  spacepoints=3000, # this removes points on lines
                  labels=comparisonLabels, # change label on spawn bio plot
                  densitynames=c("SPB_Virgin", "R0", "Bratio_2015", "SPB_2015",
                      "Recr_2010","ForeCatch_2015","OFLCatch_2015"),
                  densityxlabs=c("Equilibrium spawning biomass (B0)",
                      "log(R0)", "2015 relative spawning biomass",
                      "2015 spawning biomass",
                      "2010 recruitment (billions)",
                      "2015 default harvest catch limit ('1000 t)",
                      "2015 overfishing limit (no 40:10 adjustment)"),
                  endyr=endYr,new=F,minbthresh=0,btarg=-0.4,
                  subplots=c(1:10,13:20),legendloc="topright",
                  pwidth=6.5, pheight=3.75, ptsize=10,
                  par=list(mar=c(3.6,3.6,1,1),oma=c(0,0,0,0),mgp=c(2.5,1,0)))

# MLE index fits
doPNG <- T
ht<-4;wd=6.5
if(doPNG) {png(file.path(figDir,"acousticBioFit_modifiedSurvey.png"),height=ht,width=wd,pointsize=10,units="in",res=300)}
if(!doPNG) {windows(height=ht,width=wd)}
par(las=1,mar=c(5, 4, 1, 1) + 0.1,cex.axis=0.9)
plot(0, type='n', xlim=c(1994,2014), xaxs='i', ylim=c(0,5.5e6), yaxs='i', axes=FALSE,
     xlab="Year",ylab="Biomass index (million t)")
cpue <- dat$CPUE[dat$CPUE$index > 0,]
## segments(x0 = cpue$year,
##          y0=qlnorm(.025,meanlog=log(cpue$ob),sdlog=cpue$se_log),
##          y1=qlnorm(.975,meanlog=log(cpue$ob),sdlog=cpue$se_log),
##          lwd=3, lend=1)
#SSplotIndices(base, subplot=2, add=TRUE, col3=rgb(1,0,0,.7))
SSplotComparisons(sens_lowAc_summary,legendlabels=sens_lowAc_names_MLE,
                  add=TRUE,
                  type="l",
                  lwd=3,
                  indexQlabel=TRUE,
                  indexQdigits=3,
                  subplots=11, indexPlotEach=TRUE, shadealpha=0.7,
                  plotdir=file.path(figDir, "sensitivities_surveys_mcmc"),
                  png=FALSE,
                  plot=TRUE,
                  mcmcVec=rep(FALSE,2),
                  col=colvec,
                  indexUncertainty=TRUE,
                  spacepoints=3000, # this removes points on lines
                  labels=comparisonLabels, # change label on spawn bio plot
                  endyr=endYr,new=F,minbthresh=0,btarg=-0.4,
                  legendloc="topright",
                  pwidth=6.5, pheight=3.75, ptsize=10,
                  par=list(mar=c(3.6,3.6,1,1),oma=c(0,0,0,0),mgp=c(2.5,1,0)))
axis(1, at=base$cpue$Yr[base$cpue$Use==1], cex.axis=0.8, tcl=-0.6)
axis(1, at=2012, cex.axis=0.8, tcl=-0.6) # numbers were tight so it didn't appear
axis(1, at=1990:2020, lab=rep("",length(1990:2020)), cex.axis=0.8, tcl=-0.2)
box()
axis(2, at=(0:5)*1e6, lab=0:5, las=1)
if(doPNG) {dev.off()}





