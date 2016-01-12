#see WtAtAgeCollate.R

library(r4ss)
source("C:/NOAA2016/Hake/Data/Rcode/wtatage_calculations.R")
setwd("C:/NOAA2016/Hake/Data/LengthWeightAge")

yrs <- 2008:2015
dat <- getdata(file="LWAdata_1975to2007.csv",outlierplot=T,outlierPlotName="plots/wtAgeOutliers1975to2007.png",elimUnsexed=F)
for(yr in yrs) {
  dat <- rbind(dat,getdata(file=paste0("LWAdata_",yr,".csv"),outlierplot=T,outlierPlotName=paste0("plots/wtAgeOutliers",yr,".png"),elimUnsexed=F))
}

dat.yr <- split(dat,dat$Year)
ac <- dat[dat$Source=="Acoustic Canada" | dat$Source=="Acoustic U.S.",]
ac.yr <- split(ac,ac$Year)
fs <- dat[!substring(dat$Source,1,8)=="Acoustic",]
fs.yr <- split(fs,fs$Year)

meanWtAtAge.fn <- function(xx,ageLevs=1:20) {
    xx.age <- split(xx$Weight_kg,factor(xx$Age_yrs,levels=ageLevs))
    out <- unlist(lapply(xx.age,mean,na.rm=T))
    return(out)
}


x <- as.data.frame(lapply(ac.yr,meanWtAtAge.fn))
windows(height=7,width=11)
ages <- 1:10
yrs <- as.numeric(names(ac.yr))
plot(1,1,xlim=c(min(yrs),max(yrs)+1),ylim=c(0,max(x[ages,],na.rm=T)),ylab="Mean Wt-at-age (kg)",xlab="Year",las=1,main="Acoustic survey weight-at-age")
cols <- rich.colors.short(20)
for(i in ages) {
    lines(yrs,x[i,],type="b",pch=16,col=cols[i],lwd=3)
    text(max(yrs),x[i,ncol(x)],i,adj=-1)
}

x <- as.data.frame(lapply(fs.yr,meanWtAtAge.fn))
windows(height=7,width=11)
ages <- 2:10
yrs <- as.numeric(names(fs.yr))
plot(1,1,xlim=c(min(yrs),max(yrs)+1),ylim=c(0,max(x[ages,],na.rm=T)),ylab="Mean Wt-at-age (kg)",xlab="Year",las=1,main="Fishery weight-at-age")
cols <- rich.colors.short(20)
for(i in ages) {
    lines(yrs,x[i,],type="b",pch=16,col=cols[i],lwd=3)
    text(max(yrs),x[i,ncol(x)],i,adj=-1,col=cols[i])
}


x <- as.data.frame(lapply(dat.yr,meanWtAtAge.fn))
windows(height=7,width=11)
ages <- 1:15
yrs <- as.numeric(names(dat.yr))
plot(1,1,xlim=c(min(yrs),max(yrs)+1),ylim=c(0,max(x[ages,],na.rm=T)),ylab="Mean Wt-at-age (kg)",xlab="Year",las=1,main="All weight-at-age")
cols <- rich.colors.short(20)
for(i in ages) {
    lines(yrs,x[i,],type="b",pch=16,col=cols[i],lwd=3)
    text(max(yrs),x[i,ncol(x)],i,adj=-1)
}

table(dat$Year,dat$Age_yrs)



#Empirical WtAtAge
source("C:/NOAA2016/Hake/Data/Rcode/wtatage_calculations.R")

h2 <- dat

h2fish <- h2[!(h2$Source %in% c("Acoustic Canada","Acoustic U.S.","Acoustic Poland")),]
h2survey <- h2[h2$Source %in% c("Acoustic Canada","Acoustic U.S.","Acoustic Poland"),]

  # count the number of empty bins
  h3 <- h2[h2$Year>=1975,]
  h3$Age_yrs[h3$Age_yrs>15] <- 15

  #### making input files for SS with the holes still present
  wtage_All <- make_wtage_matrix(h2,fleetoption=2) # make matrix
  wtage_All_wMean <- make_wtage_matrix(h2,fleetoption=2,getmean=TRUE) # make matrix

  # new method does only linear interpolation within each age (only works with all data)
  wtageInterp1_All         <- dointerpSimple(wtage_All)

  #### do 2nd interpolation (actually an extrapolation at the edges)
  ### there is no 23rd column to remove, but the commands work anyway
  # all data
  wtageInterp2_All <- fill_wtage_matrix(wtageInterp1_All[,-23])
  wtageInterp2_All$Note <- fill_wtage_matrix(wtage_All[,-23])$Note

  # matrices for plotting
  mat_Interp1_All <- t(as.matrix(wtageInterp1_All[,-c(1:6,23)]))
  mat_Interp2_All <- t(as.matrix(wtageInterp2_All[,-c(1:6,23)]))

  # write output combining all fleets closer to format used by SS
  wtage_All_wMean$Note <- c(paste("# Mean from ",min(wtage_All_wMean[-1,1]),"-",max(wtage_All_wMean[,1]),sep=""),wtageInterp2_All$Note)
  wtageInterp2_All <- rbind(wtage_All_wMean[1,], wtageInterp2_All)


  # adding ages 16-20 as repeats of age 15
  wtage_extended <- wtageInterp2_All
  wtage_extended <- wtage_extended[,c(1:22,22,22,22,22,22)] # dropped column 23 with comments
  wtage_extended[,-(1:6)] <- round(wtage_extended[,-(1:6)],4)
  names(wtage_extended)[23:27] <- paste("a",16:20,sep="")
  names <- names(wtage_extended)

  #change argument Year in makeimage to last year of data before running next line
  make_wtatage_plots()
  write_wtatage_file(year=2015)


















  # count the number of empty bins
  h3 <- h2[h2$Year>=1975,]
  h3$Age_yrs[h3$Age_yrs>15] <- 15

  ## wtage1 <- make_wtage_matrix(h2) # make matrix
  wtage_Separate <- make_wtage_matrix(h2,fleetoption=1) # make matrix
  wtage_All <- make_wtage_matrix(h2,fleetoption=2) # make matrix
  wtage_All_wMean <- make_wtage_matrix(h2,fleetoption=2,getmean=T) # make matrix
  wtage_All_Midyear <- make_wtage_matrix(h2,fleetoption=2,months=5:7) # make matrix
  wtage_Total <- rbind(wtage_All,wtage_Separate)
  ## wtage2 <- fill_wtage_matrix(wtage1) # interpolate empty values
  ## wtage2b <- fill_wtage_matrix(wtage_All) # interpolate empty values
  wtage_Fish <- wtage_Separate[wtage_Separate$fleet==1,]
  wtage_Surv <- wtage_Separate[wtage_Separate$fleet==2,]

  # do first interpolation step
  interp_All <- dointerp(h2)
  interp_All_Midyear <- dointerp(h2[h2$Month%in%5:7,] )
  interp_Fish <- dointerp(h2fish)
  interp_Surv <- dointerp(h2survey)

  # copy tables
  wtageInterp1_All <- wtage_All
  wtageInterp1_All_Midyear <- wtage_All_Midyear
  wtageInterp1_Fish <- wtage_Fish
  wtageInterp1_Surv <- wtage_Surv

  # fill in new values
  wtageInterp1_All[-(1:6)]  <- interp_All
  wtageInterp1_All_Midyear[-(1:6)]  <- interp_All_Midyear
  wtageInterp1_Fish[-(1:6)] <- interp_Fish
  wtageInterp1_Surv[-(1:6)] <- interp_Surv

  # do 2nd interpolation
  # all data
  wtageInterp2_All <- fill_wtage_matrix(wtageInterp1_All[,-23])
  wtageInterp2_All$Note <- fill_wtage_matrix(wtage_All[,-23])$Note
  mat_Interp2_All <- t(as.matrix(wtageInterp2_All[,-c(1:6,23)]))

  # all data midyear
  wtageInterp2_All_Midyear <- fill_wtage_matrix(wtageInterp1_All_Midyear[,-23])
  wtageInterp2_All_Midyear$Note <- fill_wtage_matrix(wtage_All_Midyear[,-23])$Note
  mat_Interp2_All_Midyear <- t(as.matrix(wtageInterp2_All_Midyear[,-c(1:6,23)]))

  # fishery
  wtageInterp2_Fish <- fill_wtage_matrix(wtageInterp1_Fish[,-23])
  wtageInterp2_Fish$Note <- fill_wtage_matrix(wtage_Fish[,-23])$Note
  mat_Interp2_Fish <- t(as.matrix(wtageInterp2_Fish[,-c(1:6,23)]))

  # survey
  wtageInterp2_Surv <- fill_wtage_matrix(wtageInterp1_Surv[,-23])
  wtageInterp2_Surv$Note <- fill_wtage_matrix(wtage_Surv[,-23])$Note
  mat_Interp2_Surv <- t(as.matrix(wtageInterp2_Surv[,-c(1:6,23)]))

  wtageInterp2_All_Midyear$fleet <- -1
  # combine things
  wtageInterp2_Total <- rbind(wtageInterp2_All_Midyear, wtageInterp2_All, wtageInterp2_Fish, wtageInterp2_Surv)
  # write output
  write.csv(wtageInterp2_Total,paste('wtatage_interpolated_',format(Sys.time(),"%Y.%m.%d"),'.csv',sep=""),row.names=F)

  wtage_All_wMean$Note <- c("# Mean from 1975-2012",wtageInterp2_All$Note)
  wtageInterp2_All <- rbind(wtage_All_wMean[1,], wtageInterp2_All)
  write.csv(wtageInterp2_All,'wtatage_all_data_interpolated.csv',row.names=F)

  # fleet -1 = all data from months 5-7
  # fleet 0 = all data (should be start of year)
  # fleet 1 = fishery only
  # fleet 2 = survey only

  # alternate method that Ian T. didn't like, filling in by adjacent ages instead of adjacent years
  #wtage2d <- fill_wtage_matrix2(wtage_All1[,-23])
  #wtage2d$Note <- wtage2b$Note
  #mat2d <- t(as.matrix(wtage2d[,-c(1:6,23)]))



  # make plots
  # plot of all data with mean
  meanvec <- as.numeric(wtage_All_wMean[1,7+0:15])
  mat <- t(as.matrix(wtage_All[,7+0:15]))

  png("empirical_wtatage_alldata_1_nointerp.png",width=7,height=9,units='in',res=400)
  makeimage(mat=mat,meanvec=meanvec,main="Mean weight at age (all data)")
  dev.off()

  png("empirical_wtatage_alldata_2_interp.png",width=7,height=9,units='in',res=400)
  makeimage(mat=t(interp_All),meanvec=meanvec,main="Mean weight at age with interpolation (all data)")
  dev.off()

  png("empirical_wtatage_alldata_3_interp_extrap.png",width=7,height=9,units='in',res=400)
  makeimage(mat=mat_Interp2_All,meanvec=meanvec,main="Mean weight at age with interpolation & extrapolation (all data)")
  dev.off()

  png("empirical_wtatage_alldata_4_interp_extrap_shade.png",width=7,height=9,units='in',res=400)
  makeimage(mat=mat_Interp2_All,interpmat=mat,dofont=FALSE,dorect=TRUE,
            meanvec=meanvec,main="Mean weight at age with interpolation & extrapolation (all data)")
  dev.off()

  png("empirical_wtatage_alldata_5_interp_extrap_bold.png",width=7,height=9,units='in',res=400)
  makeimage(mat=mat_Interp2_All,interpmat=mat,dofont=TRUE,dorect=FALSE,
            meanvec=meanvec,main="Mean weight at age with interpolation & extrapolation (all data)")
  dev.off()

}
