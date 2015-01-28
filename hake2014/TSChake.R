dont source

source("C:/NOAA2014/Hake/WriteUp/Rcode/WriteUpFunctions.R")

library(r4ss)

setwd("C:/NOAA2014/Hake")
figDir <- "WriteUp\\Figures"
update_r4ss_files(local="Models/Rcode/r4ss",save=F)

SSdir <- "Models"
base <- SS_output(dir=file.path(SSdir,"2014hake_21_TVselex1991start"),covar=T)
mcmc <- SSgetMCMC(dir=file.path(SSdir,"2014hake_21_TVselex1991start_MCMC"),writecsv=F)
base$mcmc <- data.frame(mcmc$model1)

TSCplot(base,ylimDepl=c(0,1.2),MCMC=T,makePNG=file.path(figDir,"TSCplot.png"))
