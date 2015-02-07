compareHakeTable <- function(summaryoutput, modelnames,
                             csv=FALSE, models = "all",
                             mcmc=TRUE){
  ### function started in 2013 to make tables of hake quantities to compare models
  ### basically a wrapper for SStableComparisons
  likenames <- NULL
  names <- c("R0", "steep", "NatM", "Q_calc", "Q_extra",
             "BLANK",
             "Recr_2008",
             "Recr_2010",
             "SPB_Virg",
             "Bratio_2009",
             "Bratio_2015",
             "SPRratio_2014",
             "BLANK",
             "SSB_SPRtgt",
             "Fstd_SPRtgt",           
             "TotYield_SPRtgt")

  # theoretically things could get rounded off to a chosen precision,
  # but this isn't working yet
  #digits <- c(2,3,3,3,3,10,2,2,0,3,3,10,0,3,0)
  digits <- NULL
  
  csv = FALSE
  csvdir = "workingdirectory"
  csvfile <- "parameter_comparison_table.csv"

  # run SStableComparisons with a specific set of parameters
  tmp <- SStableComparisons(summaryoutput, models=models, likenames=likenames,
                            names=names, modelnames=modelnames, csv=csv,
                            csvfile=csvfile,
                            csvdir=csvdir,
                            verbose=TRUE,
                            digits=digits,
                            mcmc=mcmc)

  return(tmp)
}

# figDir hopefully defined elsewhere
# on Ian's computers it is defined as
#   hakedir <- "C:/SS/hake/Hake_2015/"
#   figDir  <- file.path(hakedir,"WriteUp/Figures")

## compareHakeFigs <-
##   function(dir=figDir,legendorder="default",
##            mcmcVec,pointsize=10,col="default",
##            scenario="sensitivity1",...){
##   ### function from 2013 to make figures of sensitivities
##   ### basically a wrapper for SSplotComparisons

##   comparisonLabels <-
##     c("Year", "Spawning biomass (mt)", "Relative spawning biomass", 
##       "Age-0 recruits (1,000s)", "Recruitment deviations", 
##       "Index", "Log index", "SPR ratio", "Density", "Management target", 
##       "Minimum stock size threshold",
##       "Female spawning biomass (million t)", # this value changed for hake 2015
##       "Harvest rate")
    
##   ##col=c("blue","red","green3","black")
##   ##shadecol=rgb(c(0,1,0,0),c(0,0,1,0),c(1,0,0,0),alpha=0.1)
##   ## col="default"
##   ## shadecol="default"
##   ht <- 3.25; wd<- 6.5
##   if(doPNG) {png(paste(dir,scenario,"_SPB.png",sep=""),height=ht,width=wd,pointsize=pointsize,units="in",res=300)}
##   if(!doPNG) {windows(width=wd,height=ht)}
##   SSplotComparisons(mysummary,legendlabels=modelnames,endyr=2014,new=F,minbthresh=0,
##                     labels=comparisonLabels,
##                     par=list(las=1, mar=c(3.6,3.6,1,1),oma=c(0,0,0,0),mgp=c(2.5,1,0)),
##                     subplots=4, mcmcVec=mcmcVec, legendorder=legendorder,
##                     legend=T,col=col,shadecol=shadecol,btarg=-0.4,...)
##   abline(h=c(0.1,0.4),lty=2)
##   axis(2,at=c(0.1,0.4),cex.axis=0.8)
##   if(doPNG){dev.off()}

##   if(doPNG) {png(paste(dir,scenario,"_Recr.png",sep=""),height=ht,width=wd,pointsize=pointsize,units="in",res=300)}
##   if(!doPNG) {windows(width=wd,height=ht)}
##   SSplotComparisons(mysummary,legendlabels=modelnames,endyr=2014,new=F,minbthresh=0,
##                     labels=comparisonLabels,
##                     par=list(las=1, mar=c(3.6,3.6,1,1),oma=c(0,0,0,0),mgp=c(2.5,1,0)),
##                     subplots=8,mcmcVec=mcmcVec,legendorder=legendorder,
##                     legend=T,col=col,shadecol=shadecol,btarg=-0.4,...)
##   if(doPNG){dev.off()}

##   if(doPNG) {png(paste(dir,scenario,"_SpawnBio.png",sep=""),height=ht,width=wd,pointsize=pointsize,units="in",res=300)}
##   if(!doPNG) {windows(width=wd,height=ht)}
##   SSplotComparisons(mysummary,legendlabels=modelnames,endyr=2014,new=F,minbthresh=0,
##                     labels=comparisonLabels,
##                     par=list(las=1, mar=c(3.6,3.6,1,1),oma=c(0,0,0,0),mgp=c(2.5,1,0)),
##                     subplots=2,mcmcVec=mcmcVec,legendorder=legendorder,
##                     legend=T,col=col,shadecol=shadecol,btarg=-0.4,...)
##   if(doPNG){dev.off()}
## }
