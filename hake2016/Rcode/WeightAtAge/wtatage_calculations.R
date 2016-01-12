# calculations related to creating the wtatage.ss input file for Stock Synthesis
# used for the hake stock assessments starting in 2011 (up to 2013 so far)
# written by Ian Taylor with feedback from other members of the current and former JTC

# note: source this file and then copy and paste some of the code in the
# if(FALSE) section at the bottom
# or change to if(TRUE) and source the file

# none of this is very generalized and commits all sorts of bad errors like
# using absolute paths and changing the working directory
# feel free to fix up as you wish

getdata <- function(file,removeOutliers=T,outlierplot=FALSE,outlierPlotName="wt_at_age_outliers.png",elimUnsexed=FALSE) {

  h <- read.csv(file)

  outlierL <- rep(FALSE,nrow(h))
  outlierL[h$Year==2003 & h$Month==9 & h$Weight_kg < 0.52 & h$Length_cm > 45] <- TRUE
  outlierL[h$Weight_kg > (20e-6)*h$Length_cm^3] <- TRUE
  outlierL[h$Weight_kg < (2e-6)*h$Length_cm^3] <- TRUE

  table(h$Source,outlierL)
  #### old outlier table from 2011 data set
  ##                 FALSE  TRUE
  ## Acoustic Canada  7635     1
  ## Acoustic Poland  2092     2
  ## Acoustic U.S.   32111     4
  ## ATSEA           42256    10
  ## CAN_domestic    21872   239
  ## CAN_JV           1486     1
  ## SHORE           73409    29
  ## US_FOREIGN      39977    23
  ## US_JV           54673    28

  #### new outlier table for 2012
  ## Acoustic Canada  5847     0
  ## Acoustic Poland  2092     2
  ## Acoustic U.S.   20423     4
  ## ATSEA           39137    11
  ## CAN_domestic     1556     0
  ## CAN_JV            506     1
  ## SHORE           41077    19
  ## US_FOREIGN      29767    11
  ## US_JV           29421    10

  # plot of outliers
  if(outlierplot){
    cat("Looking for outliers\n")
    x <- 0:150
    png(outlierPlotName,width=10,height=7,units='in',res=400)
    par(mfrow=c(1,3))
    plot(h$Length_cm,h$Weight_kg,pch=16,col=rgb(0,0,0,.2),xlab="Length",ylab="Weight",main="All outliers")
    points(h$Length_cm[outlierL],h$Weight_kg[outlierL],pch=16,col=2)
    lines(x,2e-6*x^3,col=4)
    lines(x,20e-6*x^3,col=4)
    plot(h$Age_yrs, h$Weight_kg, pch=16, col=rgb(0,0,0,.2), xlab="Age",ylab="Weight",main="Weight vs Age (log space)",log="xy")
    points(h$Age_yrs[outlierL],h$Weight_kg[outlierL],pch=16,col=2)
    plot(h$Age_yrs, h$Length_cm, pch=16, col=rgb(0,0,0,.2), xlab="Age",ylab="Length",main="Length vs Age")
    points(h$Age_yrs[outlierL],h$Length_cm[outlierL],pch=16,col=2)
    # h.temp <- h[h$Year==2003 & h$Month==9,]
    # h.temp2 <- h[h$Year==2003 & h$Month==9 & outlier,]
    # plot(h.temp$Length_cm,h.temp$Weight_kg,pch=16,col=rgb(0,0,0,.2),xlab="Length",ylab="Weight",main="Outliers from September 2003")
    # points(h.temp2$Length_cm,h.temp2$Weight_kg,pch=16,col=2)
    # rect(45,0,100,.52,border=4)
    dev.off()
    cat("Plot saved to",outlierPlotName,"\n")
  }
  h <- cbind(h,outlierL)
  if(removeOutliers) {
    h <- h[!outlierL,]
  }

  # eliminate unsexed fish (a tiny fraction),
  if(elimUnsexed) {
    cat("eliminating all unsexed fish (a tiny fraction),\n",
        "almost all are Poland Accoustic which was only in 1977\n")
    print(table(h$Sex))
    cat("fraction unsexed =",mean(!h$Sex %in% c("F","M")),"\n")
    h <- h[h$Sex %in% c("F","M"),]
  }
  ##           F     M     U
  ##       83693 83635   301
  ## fraction unsexed = 0.01470917

  h$Source <- as.factor(as.character(h$Source)) # gets rid of factor for Poland
  ## Acoustic Canada Acoustic U.S.    ATSEA     SHORE      US_FOREIGN   US_JV
  ##            7625         31894    42266     73438           39668   54559

  table(h$Source,h$Month)
  ##                     1     2     3     4     5     6     7     8     9    10    11    12
  ## Acoustic Canada     0     0     0     0     0     0  1859  5412   352     0     0     0
  ## Acoustic U.S.       0     0     0     0     0  1884 14330 15204   471     0     0     0
  ## ATSEA               0     0    80  5922 14769  6606  3094  2761  2898  3934  1566   616
  ## CAN_domestic       21   303  1248    20   402  3224  4967  4010  4184  2295   868     0
  ## CAN_JV              0     0     0     0     0     0     0   270   539   674     0     0
  ## SHORE            4667 10496 12059  6564  2973  8923 11477  6549  1187  2799  3125  2561
  ## US_FOREIGN          0     0     0     0     0  7904  9259 10902  7630  3935     0     0
  ## US_JV               0     0     0  4017  9794 14792 10149  9635  5048  1062     0     0

  # eliminate age 99 fish (not necessarily present)
  h$Age_yrs[h$Age_yrs==99] <- NA

  return(invisible(h))
}

make_wtage_matrix <- function(dat,fleetoption=1,
                              months=1:12,getmean=FALSE){
  # make input file wtatage.ss for hake
  #agecalcs
  ha <- dat[!is.na(dat$Age_yrs) & dat$Month %in% months,]

  agebinspop  <- 0:15
  N_agebins <- length(agebinspop)
  Ncols <- N_agebins + 6

  FleetNames <- c(rep("Fishery",7),"Acoustic_Survey","Acoustic_Survey")
  FleetNames2 <- c("US_FOREIGN","US_JV","ATSEA","SHORE","",
                   "CAN_JV","CAN_domestic","Acoustic U.S.","Acoustic Canada")
  if(fleetoption==1) Fleet_ID <- c(rep(1,7), 2, 2)
  if(fleetoption==2) Fleet_ID <- c(rep(0,9))
  fleetinfo <- data.frame(ID=Fleet_ID,name_WLdata=FleetNames2,name_model=FleetNames)
  ##   ID     name_WLdata      name_model
  ## 1  1      US_FOREIGN         Fishery
  ## 2  1           US_JV         Fishery
  ## 3  1           ATSEA         Fishery
  ## 4  1           SHORE         Fishery
  ## 5  1                         Fishery
  ## 6  1          CAN_JV         Fishery
  ## 7  1    CAN_domestic         Fishery
  ## 8  2   Acoustic U.S. Acoustic_Survey
  ## 9  2 Acoustic Canada Acoustic_Survey
  nfleets <- length(unique(Fleet_ID))

  wtage <- data.frame(matrix(NA,nrow=0,ncol=Ncols))
  for(i in 1:nfleets){
    ID <- unique(Fleet_ID)[i]
    for(y in sort(unique(ha$Year))){
      htemp <- ha[as.character(ha$Source) %in% fleetinfo$name_WLdata[fleetinfo$ID==ID]
                  & ha$Year==y,]
      sampsizes <- rep(0,N_agebins)
      means <- rep(NA,N_agebins)
      for(iage in 1:N_agebins){
        if(iage < N_agebins){
          vals <- htemp$Weight_kg[htemp$Age_yrs==agebinspop[iage]]
        }else{
          vals <- htemp$Weight_kg[htemp$Age_yrs>=agebinspop[iage]]
        }
        n <- length(vals)
        if(n>0){
          sampsizes[iage] <- n
          means[iage] <- mean(vals)
        }
      }
      if(sum(sampsizes)>0){
        newdat <- data.frame(t(c(y,1,1,1,1,ID,means)))
        wtage <- rbind(wtage,newdat)
        #print(newdat)
      }
    } # end year loop

    if(getmean){
      # add value of mean across whole series
      htemp <- ha[as.character(ha$Source) %in% fleetinfo$name_WLdata[fleetinfo$ID==ID],]
      sampsizes <- rep(0,N_agebins)
      means <- rep(NA,N_agebins)
      for(iage in 1:N_agebins){
        if(iage < N_agebins){
          vals <- htemp$Weight_kg[htemp$Age_yrs==agebinspop[iage]]
        }else{
          vals <- htemp$Weight_kg[htemp$Age_yrs>=agebinspop[iage]]
        }
        n <- length(vals)
        if(n>0){
          sampsizes[iage] <- n
          means[iage] <- mean(vals)
        }
      }
      if(sum(sampsizes)>0){
        newdat <- data.frame(t(c(-1940,1,1,1,1,ID,means)))
        wtage <- rbind(newdat,wtage)
        #print(newdat)
      }
    }
  } # end fleet loop
  names(wtage) <- c('#Yr','seas','gender','GP','bseas','fleet',
                    paste("a",agebinspop,sep=""))
  rownames(wtage) <- 1:nrow(wtage)
  cat('\n')
  fleetinfo <<- fleetinfo
  return(wtage)
}

fill_wtage_matrix <- function(wtage){
  # fills in NA values with average of adjacent years
  wtage$Note <- ""
  nages <- ncol(wtage)-6
  for(irow in 1:nrow(wtage)){
    isNA <- (1:nages)[is.na(wtage[irow,-(1:6)])]
    if(length(isNA)>0){
      wtage$Note[irow] <- paste("# interpolated ages",paste(isNA-1,collapse=","))
      for(iage in isNA){
        if(irow>1) earliervals <- wtage[1:(irow-1),iage+6] else earliervals <- NA
        if(irow<nrow(wtage))
          latervals <- wtage[(irow+1):nrow(wtage),iage+6] else latervals <- NA
        lastearlier <- rev(earliervals[!is.na(earliervals)])[1]
        firstlater <- latervals[!is.na(latervals)][1]
        if(is.na(lastearlier)) lastearlier <- firstlater
        if(is.na(firstlater)) firstlater <- lastearlier

        wtage[irow,iage+6] <- mean(lastearlier,firstlater,na.rm=TRUE)
      }
    }
  }
  return(wtage)
}

fill_wtage_matrix2 <- function(wtage){
  # fills in NA values with average of adjacent years
  nages <- ncol(wtage)-6
  for(irow in 1:nrow(wtage)){
    isNA <- (1:nages)[is.na(wtage[irow,-(1:6)])]
    if(length(isNA)>0){
      for(iage in isNA){
        if(iage>1) earliervals <- wtage[irow,(1:(iage-1))+6] else earliervals <- NA
        if(iage<nages)
          latervals <- wtage[irow,((iage+1):nages)+6] else latervals <- NA
        lastearlier <- rev(earliervals[!is.na(earliervals)])[1]
        firstlater <- latervals[!is.na(latervals)][1]
        if(is.na(lastearlier)) lastearlier <- firstlater
        if(is.na(firstlater)) firstlater <- lastearlier
        print(lastearlier)
        print(firstlater)
        wtage[irow,iage+6] <- mean(lastearlier,firstlater,na.rm=TRUE)
      }
    }
  }
  return(wtage)
}


rich.colors.short <- function(n, alpha = 1){
  x <- seq(0, 1, length = n)
  r <- 1/(1 + exp(20 - 35 * x))
  g <- pmin(pmax(0, -0.8 + 6 * x - 5 * x^2), 1)
  b <- dnorm(x, 0.25, 0.15)/max(dnorm(x, 0.25, 0.15))
  rgb.m <- matrix(c(r, g, b), ncol = 3)
  rich.vector <- apply(rgb.m, 1, function(v) rgb(v[1],v[2],v[3],alpha = alpha))
}

#need to update the yrvec here because not an argument in makewtatagaeplots function
makeimage <- function(agevec=0:15,yrvec=1975:2015,mat,interpmat=NULL,
                      meanvec=NULL,yrvec2=NULL,main="",dofont=TRUE,dorect=FALSE){
  if(is.null(meanvec)){
    meanvec <- mat[,1]
    mat <- mat[,-1]
  }
  par(mar=c(4.2,4.2,4,1)+.1)
  print(dim(mat))
  yrvec2 <- c(1973,1974,yrvec)
  mat2 <- cbind(meanvec,NA,mat)
  image(x=0:15,y=yrvec2,z=mat2,axes=FALSE,xlab='Age',ylab='Year',
        col=rainbow(60)[1:50], main=main,breaks=seq(0,4,length=51))
  # add text
  zdataframe <- expand.grid(yr=yrvec2,age=agevec)
  zdataframe$z <- c(t(mat2))
  if(!is.null(interpmat)){
    interpmat2 <- cbind(meanvec,NA,interpmat)
    zdataframe$interp <- c(t(interpmat2))
  }else{
    zdataframe$interp <- 0
  }
  zdataframe$font <- 1
  if(dofont) zdataframe$font <- ifelse(is.na(zdataframe$interp),2,1)

  ztext <- format(round(zdataframe$z,2))
  ztext[ztext=="  NA"] <- ""
  ztext[ztext=="   NA"] <- ""
  text(x=zdataframe$age,y=zdataframe$yr,label=ztext,font=zdataframe$font,cex=.7)
  interp <- zdataframe[is.na(zdataframe$interp) & zdataframe$yr!=1974,]
  if(dorect)
    rect(interp$age-.5,interp$yr-.5,
         interp$age+.5,interp$yr+.5,col=rgb(0,0,0,.3),density=20)
  # finish plot
  axis(1,at=0:15,cex.axis=.7);
  axis(2,at=c(1973,yrvec),
       lab=c("mean",yrvec),las=1,cex.axis=.7)
}

dointerp <- function(dat,plot=FALSE){
  # linear interpolation of missing values
  # in 2013 this was replace with the "dointerpSimple" function below
  library(akima)
  if(plot) library(rgl)
  print(dim(dat))
  # data
  dat <- dat[!is.na(dat$Age_yr),]

  x <- dat$Age_yrs
  y <- dat$Year
  z <- dat$Weight_kg
  x[x>15] <- 15

  # bivariate linear interpolation
  # interp:
  akima.li <- interp(x, y, z,
                     xo=0:max(x),
                     yo=sort(unique(y)),
                     duplicate="mean",extrap=FALSE)
  samp <- sample(1:length(x),1e4)
  if(plot){
    rgl.clear()
    rgl.spheres(x[samp], z[samp], y[samp], 0.2,color="red")
    rgl.bbox()
    aspect3d(x=1.5,y=1,z=1.5)
    # interp surface:
    rgl.surface(akima.li$x,akima.li$y,akima.li$z,color="green",alpha=c(0.5))
  }
  return(t(akima.li$z))
}

dointerpSimple <- function(df,skipcols=1:6){
  cols <- setdiff(1:ncol(df),skipcols)
  n <- nrow(df)
  for(icol in cols){
    df[,icol] <- approx(x=1:n, xout=1:n, y=df[,icol])$y
  }
  return(df)
}


make_wtatage_plots <- function(){
  # make plots
  # plot of all data with mean
  meanvec <- as.numeric(wtage_All_wMean[1,7+0:15])
  mat <- t(as.matrix(wtage_All[,7+0:15]))

  png("empirical_wtatage_2013_alldata_1_nointerp.png",width=7,height=9,units='in',res=400)
  makeimage(mat=mat,meanvec=meanvec,main="Mean weight at age (all data)")
  dev.off()

  png("empirical_wtatage_2013_alldata_2_interp.png",width=7,height=9,units='in',res=400)
  makeimage(mat=mat_Interp1_All,meanvec=meanvec,main="Mean weight at age with interpolation (all data)")
  dev.off()

  png("empirical_wtatage_2013_alldata_3_interp_extrap.png",width=7,height=9,units='in',res=400)
  makeimage(mat=mat_Interp2_All,meanvec=meanvec,main="Mean weight at age with interpolation & extrapolation (all data)")
  dev.off()

  png("empirical_wtatage_2013_alldata_4_interp_extrap_shade.png",width=7,height=9,units='in',res=400)
  makeimage(mat=mat_Interp2_All,interpmat=mat,dofont=FALSE,dorect=TRUE,
            meanvec=meanvec,main="Mean weight at age with interpolation & extrapolation (all data)")
  dev.off()

  png("empirical_wtatage_2013_alldata_5_interp_extrap_bold.png",width=7,height=9,units='in',res=400)
  makeimage(mat=mat_Interp2_All,interpmat=mat,dofont=TRUE,dorect=FALSE,
            meanvec=meanvec,main="Mean weight at age with interpolation & extrapolation (all data)")
  dev.off()

}

write_wtatage_file <- function(year){
  # stuff copied from SS_writedat for printing tables
  on.exit({if(sink.number()>0) sink()}) # only needed if this is put into a function

  outfile <- paste('wtatage_',year,'created_',format(Sys.time(),'%d-%b-%Y_%H.%M'),'.ss',sep='')
  printdf <- function(dataframe){
    # function to print data frame with hash mark before first column name
    names(dataframe)[1] <- paste("#_",names(dataframe)[1],sep="")
    print(dataframe, row.names=FALSE, strip.white=TRUE)
  }

  oldwidth <- options()$width
  oldmax.print <- options()$max.print
  options(width=5000,max.print=9999999)

  cat("opening connection to",outfile,"\n")
  zz <- file(outfile, open="at")
  sink(zz)

  nrows_per_matrix <- nrow(wtage_extended)
  nrows_total <- 1 + 4*nrows_per_matrix

  header <- c("# empirical weight-at-age Stock Synthesis input file for hake",
              "# created by code in the R script: wtatage_calculations.R",
              paste("# creation date:",Sys.time()),
              "###################################################",
              paste(nrows_total,"# Number of lines of weight-at-age input to be read"),
              "20 # Maximum age",
              "",
              "#Maturity x Fecundity: Fleet = -2 (Values unchanged from 2012 Stock Assessment)",
              "")
  writeLines(header)

  matfec <- wtage_extended[1,]
  matfec[1,6] <- -2
  matfec[1,-(1:6)] <- c(0.0000, 0.0000, 0.1003, 0.2535, 0.3992, 0.5180, 0.6131, 0.6895, 0.7511, 0.8007, 0.8406, 0.8724, 0.8979, 0.9181, 0.9342, 0.9469, 0.9569, 0.9649, 0.9711, 0.9761, 0.9830)
  names(matfec) <- names
  printdf(matfec)

  writeLines("#All matrices below use the same values, pooled across all data sources")

  for(ifleet in -1:2){
    wtage_extended$fleet <- ifleet
    if(ifleet==-1) note <- "#Weight at age for population in middle of the year: Fleet = -1"
    if(ifleet==0)  note <- "#Weight at age for population at beginning of the year: Fleet = 0"
    if(ifleet==1)  note <- "#Weight at age for Fishery: Fleet = 1"
    if(ifleet==2)  note <- "#Weight at age for Survey: Fleet = 2"

    writeLines(c("",note))
    printdf(wtage_extended)
  }
  writeLines("# End of wtatage.ss file")

  # restore defaults
  options(width=oldwidth,max.print=oldmax.print)
  sink()
  close(zz)
  cat("file written to",outfile,"\n")
}



if(FALSE){
  # source this file
  # source('c:/SS/hake/Hake_2013/wtatage_calculations.R')

  ####### old stuff

  ## wtage_Separate <- make_wtage_matrix(h2,fleetoption=1) # make matrix
  ## wtage_All_Midyear <- make_wtage_matrix(h2,fleetoption=2,months=5:7) # make matrix
  ## wtage_Fish <- wtage_Separate[wtage_Separate$fleet==1,]
  ## wtage_Surv <- wtage_Separate[wtage_Separate$fleet==2,]

  #### calculating the interpolations for the holes,
  ####  but without additional columns needed by SS

  ## #### old method using Akima
  ## # copy tables
  ## wtageInterp1_All <- wtage_All
  ## wtageInterp1_All_Midyear <- wtage_All_Midyear
  ## wtageInterp1_Fish <- wtage_Fish
  ## wtageInterp1_Surv <- wtage_Surv

  ## # do first interpolation step
  ## interp_All <- dointerp(h2)
  ## interp_All_Midyear <- dointerp(h2[h2$Month%in%5:7,] )
  ## interp_Fish <- dointerp(h2fish)
  ## interp_Surv <- dointerp(h2survey)

  ## # fill in new values (to replace holes in data)
  ## wtageInterp1_All[-(1:6)]  <- interp_All
  ## wtageInterp1_All_Midyear[-(1:6)]  <- interp_All_Midyear
  ## wtageInterp1_Fish[-(1:6)] <- interp_Fish
  ## wtageInterp1_Surv[-(1:6)] <- interp_Surv

  ## ### abandoned code to produce different values for each fleet
  ## # all data midyear
  ## wtageInterp2_All_Midyear <- fill_wtage_matrix(wtageInterp1_All_Midyear[,-23])
  ## wtageInterp2_All_Midyear$Note <- fill_wtage_matrix(wtage_All_Midyear[,-23])$Note
  ## mat_Interp2_All_Midyear <- t(as.matrix(wtageInterp2_All_Midyear[,-c(1:6,23)]))

  ## # fishery
  ## wtageInterp2_Fish <- fill_wtage_matrix(wtageInterp1_Fish[,-23])
  ## wtageInterp2_Fish$Note <- fill_wtage_matrix(wtage_Fish[,-23])$Note
  ## mat_Interp2_Fish <- t(as.matrix(wtageInterp2_Fish[,-c(1:6,23)]))

  ## # survey
  ## wtageInterp2_Surv <- fill_wtage_matrix(wtageInterp1_Surv[,-23])
  ## wtageInterp2_Surv$Note <- fill_wtage_matrix(wtage_Surv[,-23])$Note
  ## mat_Interp2_Surv <- t(as.matrix(wtageInterp2_Surv[,-c(1:6,23)]))

  ## wtageInterp2_All_Midyear$fleet <- -1
  ## # combine things
  ## wtageInterp2_Total <- rbind(wtageInterp2_All_Midyear, wtageInterp2_All, wtageInterp2_Fish, wtageInterp2_Surv)
  ## # write output with separate values for each fleet/timeperiod (not used in recent assessments)
  ## write.csv(wtageInterp2_Total,'wtatage_interpolated_separate_fleets_1-8-2013.csv',row.names=FALSE)

  year=2013
  h2 <- getdata(year=year)
  h2fish <- h2[!(h2$Source %in% c("Acoustic Canada","Acoustic U.S.")),]
  h2survey <- h2[h2$Source %in% c("Acoustic Canada","Acoustic U.S."),]

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

  make_wtatage_plots()
  write_wtatage_file()
}
