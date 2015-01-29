#install.packages("devtools")
devtools::install_github("r4ss/r4ss")
library(r4ss)

SSdir <- "C:\\NOAA2015\\Hake\\Models"

hake2014 <- SS_output(dir=paste(SSdir,"0.0_2014hake_21_TVselex1991start",sep="/"),covar=T,verbose=F)
hake2014.01 <- SS_output(dir=paste(SSdir,"0.1_2014hake_21_SS324U_ADMB11.2",sep="/"),covar=T,verbose=F)
hake2014.02 <- SS_output(dir=paste(SSdir,"0.2_2014hake_21_UpdateCatch",sep="/"),covar=T,verbose=F)
hake2014.03 <- SS_output(dir=paste(SSdir,"0.3_2014hake_21_fixWtAtAge",sep="/"),covar=T,verbose=F)
hake2014.04 <- SS_output(dir=paste(SSdir,"0.4_2014hake_21_updateComps",sep="/"),covar=T,verbose=F)
hake2015.1 <- SS_output(dir=paste(SSdir,"01_2015hake_add2014data",sep="/"),covar=T,verbose=F,ncols=43)
hake2015.2 <- SS_output(dir=paste(SSdir,"02_2015hake_redoFishComps",sep="/"),covar=T,verbose=F,ncols=43)


#Bridge to new version and updating old data
mymodels <- list(hake2014,hake2014.01,hake2014.02,hake2014.03,hake2014.04)
modelnames <- c("hake2014_base","SS324U","Update Catch","Fix WtAtAge","Update Comps")
mysummary <- SSsummarize(mymodels)
pdf(paste(SSdir,"Figures/update2013data.pdf",sep="/"))
SSplotComparisons(mysummary, legendlabels=modelnames,endyr=2015,new=F)
dev.off()

#Bridge to add 2014
mymodels <- list(hake2014.04,hake2015.1)
modelnames <- c("Update 2013 data","Add 2014 data")
mysummary <- SSsummarize(mymodels)
pdf(paste(SSdir,"Figures/add2014data.pdf",sep="/"))
SSplotComparisons(mysummary, legendlabels=modelnames,endyr=2015,new=F)
dev.off()

SS_plots(hake2015.1,uncertainty=T)


#Bridge to fix Comp data
mymodels <- list(hake2015.1,hake2015.2)
modelnames <- c("Add 2014 data","New age comps")
mysummary <- SSsummarize(mymodels)
pdf(paste(SSdir,"Figures/fixFishComps.pdf",sep="/"))
SSplotComparisons(mysummary, legendlabels=modelnames,endyr=2015,new=F,
				  densitynames=c("NatM_p_1_Fem_GP_1","Main_RecrDev_2010","Late_RecrDev_2011","SPB_Virgin","Bratio_2015","ForeCatch_2015","Recr_2010","Recr_2011"))
dev.off()

