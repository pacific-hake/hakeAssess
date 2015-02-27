# base vs. lower survey value MCMC
sens_lowAc     <- SS_output(file.path(SSdir, "2015hake_basePreSRG_lowAc_MLE"))
sens_lowAc_nlq <- SS_output(file.path(SSdir, "2015hake_basePreSRG_lowAc_Ian_nonLinearQ_parm"))
sens_lowAc_short     <- SSgetMCMC(file.path(SSdir, '2015hake_basePreSRG_lowAc_Ian_mcmc5.2e6'),
                                  writecsv=FALSE)[[1]]
sens_lowAc_nlQ_short <- SSgetMCMC(file.path(SSdir, '2015hake_basePreSRG_lowAc_Ian_nonLinearQ_mcmc4.9e6'),
                                  writecsv=FALSE)[[1]]

sens_lowAc_summary <- SSsummarize(list(base,sens_lowAc,sens_lowAc))
sens_lowAc_summary$mcmc[[1]] <- base$mcmc
sens_lowAc_summary$mcmc[[2]] <- sens_lowAc_short
sens_lowAc_summary$mcmc[[3]] <- sens_lowAc_nlQ_short

sens_lowAc_names <- c("Base model",
                     "Lower '12 & '13 survey",
                     "Lower '12 & '13 survey, non-linear Q")
colvec <- c(1,2,3)
dir.create(file.path(figDir, "sensitivities_surveys_mcmc"))
SSplotComparisons(sens_lowAc_summary,legendlabels=sens_lowAc_names,
                  plotdir=file.path(figDir, "sensitivities_surveys_mcmc"),
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
                  plotdir=file.path(figDir, "sensitivities_surveys2"),
                  png=TRUE,
                  plot=FALSE,
                  mcmcVec=rep(TRUE,3),
                  col=colvec,
                  indexUncertainty=TRUE,
                  spacepoints=3000, # this removes points on lines
                  labels=comparisonLabels, # change label on spawn bio plot
                  endyr=endYr,new=F,minbthresh=0,btarg=-0.4,
                  legendloc="topright",
                  pwidth=6.5, pheight=3.75, ptsize=10,
                  par=list(mar=c(3.6,3.6,1,1),oma=c(0,0,0,0),mgp=c(2.5,1,0)))


# base vs. lower survey value MCMC
sens_lowAc        <- SS_output(file.path(SSdir, "2015hake_basePreSRG_lowAc_MLE"))
sens_lowAc_short  <- SSgetMCMC(file.path(SSdir, '2015hake_basePreSRG_lowAc_Ian_mcmc5.2e6'),
                               writecsv=FALSE)[[1]]

sens_lowAc_summary <- SSsummarize(list(base,sens_lowAc))
sens_lowAc_summary$mcmc[[1]] <- base$mcmc
sens_lowAc_summary$mcmc[[2]] <- sens_lowAc_short
names(sens_lowAc_summary$mcmc[[1]])[4] <- "SR_LN(R0)"
sens_lowAc_names <- c("Base model",
                     "Lower '12 & '13 survey")
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
                  densitynames=c("SPB_Virgin", "R0", "Bratio_2015",
                      "Recr_2010","ForeCatch_2015","OFLCatch_2015"),
                  endyr=endYr,new=F,minbthresh=0,btarg=-0.4,
                  subplots=c(1:10,13:20),legendloc="topright",
                  pwidth=6.5, pheight=3.75, ptsize=10,
                  par=list(mar=c(3.6,3.6,1,1),oma=c(0,0,0,0),mgp=c(2.5,1,0)))
SSplotComparisons(sens_lowAc_summary,legendlabels=sens_lowAc_names,
                  subplots=11:12, indexPlotEach=TRUE, shadealpha=0.7,
                  plotdir=file.path(figDir, "sensitivities_surveys_mcmc"),
                  png=TRUE,
                  plot=FALSE,
                  mcmcVec=rep(TRUE,2),
                  col=colvec,
                  indexUncertainty=TRUE,
                  spacepoints=3000, # this removes points on lines
                  labels=comparisonLabels, # change label on spawn bio plot
                  endyr=endYr,new=F,minbthresh=0,btarg=-0.4,
                  legendloc="topright",
                  pwidth=6.5, pheight=3.75, ptsize=10,
                  par=list(mar=c(3.6,3.6,1,1),oma=c(0,0,0,0),mgp=c(2.5,1,0)))


