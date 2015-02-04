# This script creates the table E.5 in the executive summary of the Pacifi Hake assessment
# Place the directories containing MCMC runs with different catch streams in the directory
# ../metricsTableRuns
#
# Table text from 2012 assessment:
# Table e.5. Probabilities of various management metrics given different catch alternatives. Catch
# alternatives are based on: 1) arbitrary constant catch levels of 0, 50,000, 100,000, 150,000, and 200,000
# mt, 2) the median values estimated via the default harvest control rule (the F40% default harvest rate and
# SB 40:10 reduction) for the base case, and the status quo catch target.

#rm(list=ls(all=T))
#require(r4ss)
#update_r4ss_files()


HakeMetricsTable <- function(models,
                             dirNames,
                             year=(as.numeric(strsplit(as.character(Sys.Date()),"-")[[1]][1]))+1,
                             decPlaces=0,
                             makePercentages=T){
  # models is a list of mcmc results
  # year - default is the current date's year + 1 so if you run this in January 2013 then year = 2014.


  metric <- function(x,year) {
    out <- rep(NA,5)
    out[1] <- max(x[,paste("ForeCatch",year-1,sep="_")])  #use max in case the first value is not the entered catch (due to low biomass and catch take entire catch)
    out[2] <- sum(x[,paste("SPB",year,sep="_")] > x[,paste("SPB",year-1,sep="_")])/nrow(x)
    out[3] <- sum(x[,paste("Bratio",year,sep="_")] < 0.40)/nrow(x)
    out[4] <- sum(x[,paste("Bratio",year,sep="_")] < 0.25)/nrow(x)
    out[5] <- sum(x[,paste("Bratio",year,sep="_")] < 0.10)/nrow(x)
    out[6] <- sum(x[,paste("SPRratio",year-1,sep="_")] > 1.00)/nrow(x)
    print(out[[1]])
    #print(x[,paste("ForeCatch",year-1,sep="_")])
    out[7] <- sum(x[,paste("ForeCatch",year,sep="_")] < out[1])/nrow(x)
    return(out)
  }

  cat("Metrics table for",year,"\n")
  out <- t(as.data.frame(lapply(models,metric,year=year)))
  out <- out[order(out[,1]),]

  if(makePercentages){
    out[,2:7] <- out[,2:7]*100.0  # make percentages
    out <- round(out,decPlaces)
    out[,2:7] = paste(out[,2:7],"%",sep="")
  }else{
    out <- round(out,decPlaces)
  }

  colnames(out) <- c("Catch",
                     paste("P(SB",year,"<SB",year-1,")",sep=""),
                     paste("P(SB",year,"<SB40%)",sep=""),
                     paste("P(SB",year,"<SB25%)",sep=""),
                     paste("P(SB",year,"<SB10%)",sep=""),
                     paste("P(relSPR",year-1,">40%)",sep=""),
                     paste("P(C",year,"<C",year-1,")",sep=""))
  rownames(out) <- getModelNamesFromDirNames(dirNames)
  return(out)
}

HakeMetricsTableRisk <- function(models,
                             dirNames,
                             year=(as.numeric(strsplit(as.character(Sys.Date()),"-")[[1]][1]))+1,
                             decPlaces=0,
                             makePercentages=T){
  # models is a list of mcmc results
  # year - default is the current date's year + 1 so if you run this in January 2013 then year = 2014.

  metric <- function(x,year) {
    out <- rep(NA,5)
    out[1] <- max(x[,paste("ForeCatch",year-1,sep="_")])  #use max in case the first value is not the entered catch (due to low biomass and catch take entire catch)
    out[2] <- sum(x[,paste("SPB",year,sep="_")] < x[,paste("SPB",year-1,sep="_")])/nrow(x)
    out[3] <- sum(x[,paste("Bratio",year,sep="_")] < 0.40)/nrow(x)
    out[4] <- sum(x[,paste("Bratio",year,sep="_")] < 0.25)/nrow(x)
    out[5] <- sum(x[,paste("Bratio",year,sep="_")] < 0.10)/nrow(x)
    out[6] <- sum(x[,paste("SPRratio",year-1,sep="_")] > 1.00)/nrow(x)
    print(out[[1]])
    print(x[,paste("ForeCatch",year-1,sep="_")])
    out[7] <- sum(x[,paste("ForeCatch",year,sep="_")] < out[1])/nrow(x)
    return(out)
  }
  print(head(models[[1]]))
  out <- t(as.data.frame(lapply(models,metric,year=year)))
  return(out)
  out <- out[order(out[,1]),]

  if(makePercentages){
    out[,2:7] <- out[,2:7]*100.0  # make percentages
    out <- round(out,decPlaces)
    out[,2:7] = paste(out[,2:7],"%",sep="")
  }else{
    out <- round(out,decPlaces)
  }

  colnames(out) <- c("Catch",
                     paste("P(SB",year,"<SB",year-1,")",sep=""),
                     paste("P(SB",year,"<SB40%)",sep=""),
                     paste("P(SB",year,"<SB25%)",sep=""),
                     paste("P(SB",year,"<SB10%)",sep=""),
                     paste("P(relSPR",year-1,">40%)",sep=""),
                     paste("P(C",year,"<C",year-1,")",sep=""))
  if(!is.null(dirNames)) {rownames(out) <- getModelNamesFromDirNames(dirNames)}
  return(out)
}


getModelNamesFromDirNames <- function(dirNames){
  # takes the listing of directories, splits it on "_" and retrieves the
  # last item, and creates a list of these.
  splitNames <- strsplit(dirNames,"_")
  modelNames <- NULL
  for(model in 1:length(splitNames)){
    modelNames <- c(modelNames,splitNames[[model]][length(splitNames[[model]])])
  }
  return(modelNames)
}

if(F) {
#setwd("C:/NOAA2013/Hake/Models")
######modelsPath   <- file.path("..","metricsTableRuns")
modelsPath   <- file.path("metricsTableRuns")
models       <- list.dirs(modelsPath)[-1]
mcmc         <- SSgetMCMC(models,writecsv=F)
#metricsTable <- HakeMetricsTable(mcmc,models,makePercentages=F,decPlaces=4)
metricsTable <- HakeMetricsTable(mcmc,models)
write.csv(metricsTable,"metricsTable.csv",quote=F)
}
