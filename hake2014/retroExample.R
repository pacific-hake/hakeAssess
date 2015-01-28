
################################################
#Retros on 21
retro01 <- SS_output(dir=file.path(SSdir,"2014hake_21_retros/2014hake_21_retro01"),covar=F)
retro02 <- SS_output(dir=file.path(SSdir,"2014hake_21_retros/2014hake_21_retro02"),covar=F)
retro03 <- SS_output(dir=file.path(SSdir,"2014hake_21_retros/2014hake_21_retro03"),covar=F)
retro04 <- SS_output(dir=file.path(SSdir,"2014hake_21_retros/2014hake_21_retro04"),covar=F)
retro05 <- SS_output(dir=file.path(SSdir,"2014hake_21_retros/2014hake_21_retro05"),covar=F)
retro06 <- SS_output(dir=file.path(SSdir,"2014hake_21_retros/2014hake_21_retro06"),covar=F)
retro07 <- SS_output(dir=file.path(SSdir,"2014hake_21_retros/2014hake_21_retro07"),covar=F)
retro08 <- SS_output(dir=file.path(SSdir,"2014hake_21_retros/2014hake_21_retro08"),covar=F)
retro09 <- SS_output(dir=file.path(SSdir,"2014hake_21_retros/2014hake_21_retro09"),covar=F)
retro10 <- SS_output(dir=file.path(SSdir,"2014hake_21_retros/2014hake_21_retro10"),covar=F)
retro11 <- SS_output(dir=file.path(SSdir,"2014hake_21_retros/2014hake_21_retro11"),covar=F)
retro12 <- SS_output(dir=file.path(SSdir,"2014hake_21_retros/2014hake_21_retro12"),covar=F)
retro13 <- SS_output(dir=file.path(SSdir,"2014hake_21_retros/2014hake_21_retro13"),covar=F)

retroDir <- file.path(SSdir,"2014hake_21_retros")
mcmc <- SSgetMCMC(dir=rev(file.path(retroDir,dir(retroDir))),writecsv=F)
mcmc[[length(mcmc)+1]] <- base$mcmc
mymodels <- list(retro13,retro12,retro11,retro10,retro09,retro08,retro07,retro06,retro05,retro04,retro03,retro02,retro01,base)
modelnames <- c("Retro -13","Retro -12","Retro -11","Retro -10","Retro -9","Retro -8","Retro -7","Retro -6","Retro -5","Retro -4","Retro -3","Retro -2","Retro -1","base 2014")
mysummary <- SSsummarize(mymodels)
mysummary$mcmc <- mcmc
pdf("Writeup/Figures/Retros/21_retros.pdf")
SSplotComparisons(mysummary, legendlabels=modelnames,endyr=2014-(13:0),new=F,minbthresh = 0,
                densitynames = c("SPB_Virgin","R0","NatM_p_1_Fem_GP_1","SR_BH_steep","Recr_2008","Recr_2009","Recr_2010","Recr_2011"),mcmc=rep(T,14))
#plot and compare selex
columns <- 1:9
# setup colors, points, and line types
if(length(mymodels) > 3) col <- rc(length(mymodels)+1,alpha=0.6)[-1]
if(length(mymodels) < 3) col <- rc(length(mymodels),alpha=0.6)
if(length(mymodels) == 3) col <- c(rgb(0,0,1,0.6),rgb(1,0,0,0.6),rgb(0,205/255,0,0.6))
tmp <- mysummary$agesel
#commSelex <- tmp[tmp$factor=="Asel" & tmp$year==2013 & tmp$fleet==1,as.character(0:20)]
acSelex <- tmp[tmp$factor=="Asel" & tmp$year==2013 & tmp$fleet==2,as.character(0:20)]
par(mfrow=c(1,1))
#plot(as.numeric(names(commSelex))[columns],commSelex[1,columns],type="b",col=col[1],xlab="Age",ylab="Selectivity",lwd=3,pch=20,main="Commercial")
#for(i in 2:nrow(commSelex)) {
#    lines(as.numeric(names(commSelex))[columns],commSelex[i,columns],col=col[i],lwd=3,type="b",pch=20)
#}
plot(as.numeric(names(acSelex))[columns],acSelex[1,columns],type="b",col=col[1],xlab="Age",ylab="Selectivity",lwd=3,pch=20,main="Survey")
for(i in 2:nrow(acSelex)) {
    lines(as.numeric(names(acSelex))[columns],acSelex[i,columns],col=col[i],lwd=3,type="b",pch=20)
}
legend("bottomright",modelnames,col=col,pch=20,lty=1)
dev.off()

SSplotRetroDevs(mysummary,endyrvec=2014-(14:1),cohorts=1999:2013,mcmcVec=rep(F,14))
SSplotRetroRecruits(mysummary,endyrvec=2014-(13:0),cohorts=1999:2010,devs=F,mcmcVec=rep(T,14))
