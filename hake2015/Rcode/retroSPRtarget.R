# functions to look at Shannon Mann's question about how the hake fishery would have
# performed if previous catches all followed the default harvest control rule

getSPR <- function(startline=502, years=1966:2017){
  # get the series of SPR ratios in a fast way
  # probably better to replace startline with something automatic
  SPRratio.table <- read.table(file.path(dir, "Report.sso"),
                               header=FALSE, skip=startline-1,
                               nrows=length(years))
  SPRratio.table <- data.frame(label=SPRratio.table$V1, value=SPRratio.table$V2)
  SPRratio.table$year <- as.numeric(substring(SPRratio.table$label,
                                              nchar("SPRratio_")+1))
  return(SPRratio.table)
}


getSPR_1year <- function(year){
  SPRratio.table <- getSPR()
  return(SPRratio.table$value[SPRratio.table$year==year])
}

changeCatch <- function(year, new){
  # it's possible to store a single data file to modify, but read/write is quick
  dat <- SS_readdat(file.path(dir, "2015hake_data_SPRtarg.SS"),verbose=FALSE)
  dat$catch$Fishery[dat$catch$year==year] <- new
  SS_writedat(dat, file.path(dir, "2015hake_data_SPRtarg.SS"),
              verbose=FALSE, overwrite=TRUE)
}

doStuff <- function(x, y=2001){
  # change catch in year y to x
  changeCatch(year=y, new=x)
  # run SS
  old.wd <- getwd()
  setwd(dir)
  system.tmp <- system("ss3 -maxfn 0 -nohess -nox", intern=TRUE)
  setwd(old.wd)
  # get SPR ratio for that year
  SPRratio <- getSPR_1year(year=y)
  print(SPRratio)
  return((1-SPRratio)^2)
}

if(FALSE){
  dir <- 'C:/ss/hake/Hake_2015/Models/2015hake_basePreSRG_SPRtarg1990plus'
  # read starter file
  start <- SS_readstarter(file.path(dir, "starter.ss"))
  # change to read initial values from .par file
  start$init_values_src <- 1
  # change data file input
  start$datfile <- "2015hake_data_SPRtarg.SS"
  SS_writestarter(start, dir=dir, overwrite=TRUE)

  # read data file and write with new name (could have just copied)
  dat <- SS_readdat(file.path(dir, "2015hake_data.SS"))
  SS_writedat(dat, file.path(dir, "2015hake_data_SPRtarg.SS"), overwrite=TRUE)

  for(y in 1990:2014){
    print(y)
    opt.out <- optimize(f=doStuff, lower=1000, upper=1e6, y=y, tol=0.01)
    print(opt.out)
  }

  #base <- SS_output('C:/ss/hake/Hake_2015/Models/2015hake_basePreSRG')
  base.SPRtarg <- SS_output(dir,covar=FALSE)
  SSplotComparisons(SSsummarize(list(base, base.SPRtarg)), png=TRUE, plotdir=dir)
  SSplotCatch(base.SPRtarg,subplot=1)
  lines(base$timeseries$Yr, base$timeseries$"dead(B):_1", col=2, lwd=3)
  mean(base$timeseries$"dead(B):_1"[base$timeseries$Yr %in% 1990:2014])
  mean(base.SPRtarg$timeseries$"dead(B):_1"[base.SPRtarg$timeseries$Yr %in% 1990:2014])
  SSplotNumbers(base.SPRtarg,plot=FALSE,print=TRUE,pwidth=6.5,pheight=6,
                plotdir=base.SPRtarg$inputs$dir)

}
