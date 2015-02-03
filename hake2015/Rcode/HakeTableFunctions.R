#These are functions to create the executive summary tables for the 2013 hake assessment
#They are named with a descriptive name, followed by a dot and "ex" for executive summary or "dc" for document

#library(r4ss)
#update_r4ss_files(local="C:/NOAA2012/Hake/Models/Rcode/R4SS")



#SSdir <- "Z:\\Hake_2011\\Ian T\\Candidates_1_13_11"
#EA <- SS_output(dir=paste(SSdir,"Empirical_Age",sep="/"))
#tmp <- SSgetMCMC(dir=paste(SSdir,"Empirical_Age_mcmc_5mill",sep="/"),writecsv=F)
#EA$mcmc <- data.frame(tmp$model1[,c("SPB_Virgin",paste("SPB",1966:2011,sep="_"),paste("Bratio",1967:2011,sep="_"))])


DerivedQuants.ex <- function(models,variable="SPB_",years=2012:2014,mcmc=rep(T,length(models)),probs=c(0.05,0.25,0.5,0.75,0.95),digits=4,scalar=rep(1,length(models)),csvFileName=NULL) {
###############################################################################
# Outputs for multiple models in a table the probs you specify and the median or MLE depending if mcmc is true or false, respectively
# models is a list of the models to output
# variable is the name of the variable up to the year ("SPB_" in SPB_1999, for example)
#   must be one of c("SPB_","Bratio_","Recr_","F_","SPRratio_")
# if mcmc=T, it looks for a table called mcmc with headers that match SS labeling
#   it returns the median and quantiles at probs
# if mcmc=F, it looks for a derived_quants matrix and the variable name separated from year by "_"
#    it returns the MLE with quantiles at probs determined from SD
###############################################################################

    
    if(substring(variable,nchar(variable))!="_") stop("It is expecting a '_' at the end of your variable name.")

    nmodels <- length(models)
    
    #set up holders
    model.out <- vector(mode="list",length=nmodels)
    outModels <- NULL
    for(i in 1:nmodels) {
        model.out[[i]] <- matrix(NA,nrow=length(years),ncol=length(probs))
        if(variable=="SPB_" && !is.null(models[[i]]$nsexes) && models[[i]]$nsexes==1) {
            cat("Dividing spawning biomass by 2 for model",i,"to output female spawning biomass in a single sex model\n")
            scalar[i] <- 2*scalar[i]
        }
        if(mcmc[i]) {
            if(is.null(models[[i]]$mcmc)) stop("mcmc table in models[[",i,"]] does not exist. Create a list element called mcmc with the derived quantites labels as headers.")
            quants <- models[[i]]$mcmc[,grep(variable,names(models[[i]]$mcmc))]/scalar[i]
            yrs <- suppressWarnings(as.numeric(substring(names(quants),nchar(variable)+1)))  #any that are not a number (like Virgin) will give an NA and warning
            vals <- apply(quants[,yrs %in% years],2,quantile,probs=probs)
            if(length(dim(vals))>1) {
                if(ncol(vals) != length(years)) stop("It seems that the years are not available in models[[",i,"]]$mcmc for your variable. The years available are shown above",cat(yrs,sep="\n"))
                model.out[[i]] <- t(vals)
                dimnames(model.out[[i]]) <- list(years,as.character(probs))
            } else {
                model.out[[i]] <- matrix(vals,ncol=1,dimnames=list(years,as.character(probs)))
            }
        }else {
            if(is.null(models[[i]]$derived_quants)) stop("derived_quants table in models[[",i,"]] does not exist.")
            quants <- models[[i]]$derived_quants[grep(variable,models[[i]]$derived_quants$LABEL), ]
            yrs <- suppressWarnings(as.numeric(substring(quants$LABEL,nchar(variable)+1)))  #any that are not a number (like Virgin) will give an NA and warning
            MLE <- quants[yrs %in% years,"Value"]/scalar[i]
            if(length(MLE) != length(years)) stop("It seems that the years are not available in models[[",i,"]]$derived_quants for your variable. The years available are shown above",cat(yrs,sep="\n"))
            SD <- quants[yrs %in% years,"StdDev"]/scalar[i]
            #lower <- qnorm(probs[1],MLE,SD)
            #upper <- qnorm(probs[2],MLE,SD)    #NEED TO TEST MLE TO MAKE SURE IT WORKS
            probs <- sort(probs)
            model.out[[i]] <- matrix(qnorm(probs[1],MLE,SD),ncol=1)
            for(j in probs[-1]) {
                model.out[[i]] <- cbind(model.out[[i]],qnorm(j,MLE,SD))  #cbind(lower,MLE,upper)                
            }
            dimnames(model.out[[i]]) <- list(years,as.character(probs))
        }
        return(cbind(outModels,model.out[[i]]))
        if(i==1) {
            outModels <- cbind(outModels,model.out[[i]])
        }else {
            outModels <- cbind(outModels,NA,model.out[[i]])
        }
    }
    outModels <- round(outModels,digits)
    
    if(is.null(csvFileName)) {
        return(outModels)
    }else {
        if(substring(csvFileName,nchar(csvFileName)-4) != ".csv") csvFileName <- paste(csvFileName,"csv",sep=".")
        write.csv(outModels,file=csvFileName)
        cat("Wrote the csv file",csvFileName,"\n")
        invisible(outModels)
    }
}

#HakeTables.ex(models=list(EA),variable="Bratio",mcmc=c(F))
#HakeTables.ex(models=list(EA,EA),mcmc=c(F,T))
#HakeTables.ex(models=list(EA,EA,EA),variable="Bratio_",mcmc=c(F,T,F))
#HakeTables.ex(models=list(EA,EA),variable="Recr_",mcmc=c(F,T))
#HakeTables.ex(models=list(EA,EA),variable="SPRratio_",mcmc=c(F,T))
#HakeTables.ex(models=list(EA),years=1957:1967) #test to see if properly gives error for non-years



DerivedQuantsTables.ex <- function(models,variable="SPB_",years=2003:2012,mcmc=rep(T,length(models)),probs=c(0.025,0.975),digits=4,scalar=rep(1,length(models)),csvFileName=NULL) {
###############################################################################
# Outputs for two models in a table the probs you specify and the median or MLE depending if mcmc is true or false, respectively
# models is a list of the models to output
# variable is the name of the variable up to the year ("SPB_" in SPB_1999, for example)
#   must be one of c("SPB_","Bratio_","Recr_","F_","SPRratio_")
# if mcmc=T, it looks for a table called mcmc with headers that match SS labeling
#   it returns the median and quantiles at probs
# if mcmc=F, it looks for a derived_quants matrix and the variable name separated from year by "_"
#    it returns the MLE with quantiles at probs determined from SD
###############################################################################

    if(length(probs)!=2) {stop("Need a probs vector with 2 values")}
    if(probs[1]>probs[2] | probs[1]>0.5 | probs[2]<0.5) {cat("quantile probabilities equals",probs,". You may want to check this\n")}
    
    #nombres <- c("SPB_","Bratio_","Recr_","F_","SPRratio_","ForeCatch_")
    #if(!(variable %in% nombres)) stop(cat("The argument variable must be one of",nombres,sep="\n"))
    if(substring(variable,nchar(variable))!="_") stop("It is expecting a '_' at the end of your variable name.")

    nmodels <- length(models)
    
    #set up holders
    model.out <- vector(mode="list",length=nmodels)
    outModels <- NULL
    for(i in 1:nmodels) {
        model.out[[i]] <- matrix(NA,nrow=length(years),ncol=3)
        if(variable=="SPB_" && !is.null(models[[i]]$nsexes) && models[[i]]$nsexes==1) {
            cat("Dividing spawning biomass by 2 for model",i,"to output female spawning biomass in a single sex model\n")
            scalar[i] <- 2*scalar[i]
        }
        if(mcmc[i]) {
            if(is.null(models[[i]]$mcmc)) stop("mcmc table in models[[",i,"]] does not exist. Create a list element called mcmc with the derived quantites labels as headers.")
            quants <- models[[i]]$mcmc[,grep(variable,names(models[[i]]$mcmc))]/scalar[i]
            if(variable=="Recr_") {
                cat("Entering 'Recr_' searches for that exact phrase to eliminate ForeRecr and RecrDev\n")
                yrs <- suppressWarnings(as.numeric(substring(names(quants),nchar(variable)+1)))  #any that are not a number (like Virgin) will give an NA and warning
            } else {
                yrs <- as.numeric(unlist(lapply(strsplit(names(quants),"_"),function(x){x[length(x)]})))
            }
            vals <- apply(quants[,yrs %in% years],2,quantile,probs=c(probs[1],0.5,probs[2]))
            if(ncol(vals) != length(years)) stop("It seems that the years are not available in models[[",i,"]]$mcmc for your variable. The years available are shown above",cat(yrs,sep="\n"))
            model.out[[i]] <- t(vals)
            dimnames(model.out[[i]]) <- list(years,c(probs[1],"0.5",probs[2]))
        }else {
            if(is.null(models[[i]]$derived_quants)) stop("derived_quants table in models[[",i,"]] does not exist.")
            quants <- models[[i]]$derived_quants[grep(variable,models[[i]]$derived_quants$LABEL), ]
            if(variable=="Recr_") {
                cat("Entering 'Recr_' searches for that exact phrase to eliminate ForeRecr and RecrDev")
                yrs <- suppressWarnings(as.numeric(substring(quants$LABEL,nchar(variable)+1)))  #any that are not a number (like Virgin) will give an NA and warning
            } else {
                yrs <- as.numeric(unlist(lapply(strsplit(quants$LABEL,"_"),function(x){x[length(x)]})))
            }
#            yrs <- suppressWarnings(as.numeric(substring(quants$LABEL,nchar(variable)+1)))  #any that are not a number (like Virgin) will give an NA and warning
            MLE <- quants[yrs %in% years,"Value"]/scalar[i]
            if(length(MLE) != length(years)) stop("It seems that the years are not available in models[[",i,"]]$derived_quants for your variable. The years available are shown above",cat(yrs,sep="\n"))
            SD <- quants[yrs %in% years,"StdDev"]/scalar[i]
            lower <- qnorm(probs[1],MLE,SD)
            upper <- qnorm(probs[2],MLE,SD)
            model.out[[i]] <- cbind(lower,MLE,upper)
            dimnames(model.out[[i]]) <- list(years,c(probs[1],"MLE",probs[2]))
        }
        if(i==1) {
            outModels <- cbind(outModels,model.out[[i]])
        }else {
            outModels <- cbind(outModels,NA,model.out[[i]])
        }
    }
    outModels <- round(outModels,digits)
    
    if(is.null(csvFileName)) {
        return(outModels)
    }else {
        if(substring(csvFileName,nchar(csvFileName)-4) != ".csv") csvFileName <- paste(csvFileName,"csv",sep=".")
        write.csv(outModels,file=csvFileName)
        cat("Wrote the csv file",csvFileName,"\n")
        invisible(outModels)
    }
}


HakeDecisionTablesQuantiles.ex <- function(models,years=2013:2014,outVar="Bratio_",quantiles=c(0.05,0.25,0.5,0.75,0.95),scalar=1,csvFileName=NULL) {
    #creates decision tables from multiple model runs
    #uses the posterior of the 2008 year class
    
    nmodels <- length(models)

    outVals <- NULL
    outNotCatch <- NULL
    for(i in 1:nmodels) {
        #sort the mcmc by the sorting variable
        xx <- models[[i]]
        xx.list <- list(mcmc=xx)
        #get the values of interest, but grab only the middle column (median)
        vals <- DerivedQuants.ex(models=list(xx.list),years=years,variable=outVar,mcmc=rep(T,length(xx.list)),probs=quantiles)
        vals <- vals/scalar
        catch <- apply(xx[,paste("ForeCatch_",years,sep="")],2,function(x){(sum(!duplicated(x))-1)/length(x)})
        cat("The proportion of catches that are not equal (if fixed, some runs could not take catch) for model",i,"\n",catch,"\n",sep=" ")
        medCatch <- apply(xx[,paste("ForeCatch_",years,sep="")],2,median)
        catch <-  cbind(years,medCatch,catch)       
        vals <- cbind(years,medCatch,vals)
        outVals <- rbind(outVals,vals)
        outNotCatch <- rbind(outNotCatch,catch)
    }
    outVals <- outVals[order(outVals[,"medCatch"],outVals[,"years"]),]  #sort by median catch then year
    outNotCatch <- outNotCatch[order(outNotCatch[,"medCatch"],outNotCatch[,"years"]),]  #sort by median catch then year
    #dimnames(outVals) <- list(rep(years,nmodels),c("Year","Catch",as.character(quantiles)))

    if(is.null(csvFileName)) {
        return(list(outVals,outNotCatch))
    }else {
        print(substring(csvFileName,nchar(csvFileName)-3))
        if(substring(csvFileName,nchar(csvFileName)-3) != ".csv") csvFileName <- paste(csvFileName,"csv",sep=".")
        write.csv(outVals,file=csvFileName)
        cat("Wrote the csv file",csvFileName,"\n")
        print(outNotCatch)
        invisible(outVals)
    }
    
}



HakeDecisionTablesSort.ex <- function(models,years=2013:2014,sortVar="Recr_2010",outVar="Bratio_",percentages=c(0.10,0.20,0.40,0.20,0.10),scalar=1,csvFileName=NULL) {
    #creates decision tables from multiple model runs
    #uses the posterior of defined by the sortVar to sort by quantiles
    
    nmodels <- length(models)
    if(sum(percentages) != 1) cat("WARNING: your percentages do not sum to one\n")
    cumPerc <- c(0,cumsum(percentages))

    outVals <- NULL
    outNotCatch <- NULL
    metrics <- NULL
    for(i in 1:nmodels) {
        #sort the mcmc by the sorting variable
        xx <- models[[i]][order(models[[i]][,sortVar]),]
        xx.list <- NULL
        for(j in 1:(length(cumPerc)-1)) {
            print(((ceiling(nrow(xx)*cumPerc[j])+1):ceiling(nrow(xx)*cumPerc[j+1])))
            if(i==1) print(length((ceiling(nrow(xx)*cumPerc[j])+1):ceiling(nrow(xx)*cumPerc[j+1])))
            xx.list[[j]] <- list(mcmc=xx[(ceiling(nrow(xx)*cumPerc[j])+1):ceiling(nrow(xx)*cumPerc[j+1]),])
        }
        print(names(xx.list[[1]]))
        metrics <- rbind(metrics,HakeMetricsTableRisk2(xx.list,year=max(years),decPlaces=4))
        #get the values of interest, but grab only the middle column (median)
        vals <- DerivedQuantsTables.ex(models=xx.list,years=years,variable=outVar,mcmc=rep(T,length(xx.list)))
        vals <- vals[,grep("0\\.5",colnames(vals))]/scalar
        colnames(vals) <- percentages
        catch <- DerivedQuantsTables.ex(models=xx.list,years=years,variable="ForeCatch_",mcmc=rep(T,length(xx.list)))
        catch <- catch[,grep("0\\.5",colnames(catch))]
        #print(catch)
        if(any(apply(catch,1,function(x){x!=x[1]}),na.rm=T)) cat("WARNING: some catches are not equal for the various partitions\n")
        medCatch <- median(xx[,paste("ForeCatch_",years[1],sep="")])
        notCatch <- cbind(years,medCatch,t(apply(catch,1,function(x,medC){x!=medC},medC=medCatch)))
        vals <- cbind(years,medCatch,vals)
        outVals <- rbind(outVals,vals)
        outNotCatch <- rbind(outNotCatch,notCatch)
    }
    outVals <- outVals[order(outVals[,"medCatch"],outVals[,"years"]),]  #sort by median catch then year
    outNotCatch <- outNotCatch[order(outNotCatch[,"medCatch"],outNotCatch[,"years"]),]  #sort by median catch then year
    dimnames(outVals) <- dimnames(outNotCatch) <- list(rep(years,nmodels),c("Year","Catch",as.character(percentages)))
    rownames(metrics) <- rep(percentages,nrow(metrics)/length(percentages))

    if(is.null(csvFileName)) {
        return(list(outVals=outVals,outNotCatch=outNotCatch,metrics=metrics))
    }else {
        print(substring(csvFileName,nchar(csvFileName)-3))
        if(substring(csvFileName,nchar(csvFileName)-3) != ".csv") csvFileName <- paste(csvFileName,"csv",sep=".")
        write.csv(outVals,file=csvFileName)
        cat("Wrote the csv file",csvFileName,"\n")
        print(outNotCatch)
        invisible(list(outVals=outVals,outNotCatch=outNotCatch,metrics=metrics))
    }
    
}

HakeMetricsTableRisk2 <- function(models,
                             year=(as.numeric(strsplit(as.character(Sys.Date()),"-")[[1]][1]))+1,
                             decPlaces=0){
  # models is a list of mcmc results
  # year - default is the current date's year + 1 so if you run this in January 2013 then year = 2014.

  metric <- function(x,year) {
    out <- rep(NA,5)
    x <- x$mcmc
    out[1] <- max(x[,paste("ForeCatch",year-1,sep="_")])  #use max in case the first value is not the entered catch (due to low biomass and catch take entire catch)
    out[2] <- sum(x[,paste("SPB",year,sep="_")] < x[,paste("SPB",year-1,sep="_")])/nrow(x)
    out[3] <- sum(x[,paste("Bratio",year,sep="_")] < 0.40)/nrow(x)
    out[4] <- sum(x[,paste("Bratio",year,sep="_")] < 0.25)/nrow(x)
    out[5] <- sum(x[,paste("Bratio",year,sep="_")] < 0.10)/nrow(x)
    out[6] <- sum(x[,paste("SPRratio",year-1,sep="_")] > 1.00)/nrow(x)
    print(out[[1]])
    out[7] <- sum(x[,paste("ForeCatch",year,sep="_")] < out[1])/nrow(x)
    return(out)
  }
  print(length(models))
  out <- t(as.data.frame(lapply(models,metric,year=year)))
  out <- out[order(out[,1]),]

  colnames(out) <- c("Catch",
                     paste("P(SB",year,"<SB",year-1,")",sep=""),
                     paste("P(SB",year,"<SB40%)",sep=""),
                     paste("P(SB",year,"<SB25%)",sep=""),
                     paste("P(SB",year,"<SB10%)",sep=""),
                     paste("P(relSPR",year-1,">40%)",sep=""),
                     paste("P(C",year,"<C",year-1,")",sep=""))
  return(out)
}







if(F) {

HakeMetricsTable.fn <- function(models,year=2013) {
    #models is a list of mcmc results
    metric.fn <- function(x,year) {
        out <- rep(NA,5)
        out[1] <- x[1,paste("ForeCatch",year-1,sep="_")]
        out[2] <- sum(x[,paste("SPB",year,sep="_")] > x[,paste("SPB",year-1,sep="_")])/nrow(x)
        out[3] <- sum(x[,paste("Bratio",year,sep="_")] > 0.40)/nrow(x)
        out[4] <- sum(x[,paste("Bratio",year,sep="_")] > 0.25)/nrow(x)
        out[5] <- sum(x[,paste("Bratio",year,sep="_")] > 0.10)/nrow(x)
        out[6] <- sum(x[,paste("SPRratio",year-1,sep="_")] > 1.00)/nrow(x)
        return(out)
    }
    out <- t(as.data.frame(lapply(models,metric.fn,year=year)))
    out <- out[order(out[,1]),]
    colnames(out) <- c("Catch","P>Byear-1","P>0.4","P>0.25","P>0.1","P(relSPR>1)")
    return(out)
}










HakeDecisionTables2.ex <- function(models,years=2012:2014,outVar="Bratio_",scalar=1,csvFileName=NULL) {
#this function does not sort on a variable and split it into states. Instead, each of the models is a state
    #creates decision tables for a single model run
    #This ones uses only the median and is meant for decision tables with different models as columns
    
    nmodels <- length(models)  #this is actually catch streams

    outVals <- NULL
    outNotCatch <- NULL
    for(i in 1:nmodels) {
        xx <- models[[i]]
        xx.mcmc <- list(mcmc=xx)
        #get the values of interest, but grab only the middle column (median)
        vals <- DerivedQuantsTables.ex(models=list(xx.mcmc),years=years,variable=outVar,mcmc=rep(T,length(xx.list)))
        vals <- vals[,grep("0\\.5",colnames(vals))]/scalar

        catch <- DerivedQuantsTables.ex(models=list(xx.mcmc),years=years,variable="ForeCatch_",mcmc=rep(T,length(xx.list)))
        catch <- catch[,grep("0\\.5",colnames(catch))]
        medCatch <- apply(xx[,paste("ForeCatch_",years,sep="")],2,median)
        #medCatch <- median(xx[,paste("ForeCatch_",years[1],sep="")])
        notCatch <- cbind(years,medCatch,t(apply(catch,1,function(x,medC){x!=medC},medC=medCatch)))
        vals <- cbind(years,medCatch,vals)
        outVals <- rbind(outVals,vals)
        outNotCatch <- rbind(outNotCatch,notCatch)
    }
    outVals <- outVals[order(outVals[,"medCatch"],outVals[,"years"]),]  #sort by median catch then year
    outNotCatch <- outNotCatch[order(outNotCatch[,"medCatch"],outNotCatch[,"years"]),]  #sort by median catch then year
    dimnames(outVals) <- dimnames(outNotCatch) <- list(rep(years,nmodels),c("Year","Catch",as.character(percentages)))

    if(is.null(csvFileName)) {
        return(list(outVals,outNotCatch))
    }else {
        print(substring(csvFileName,nchar(csvFileName)-3))
        if(substring(csvFileName,nchar(csvFileName)-3) != ".csv") csvFileName <- paste(csvFileName,"csv",sep=".")
        write.csv(outVals,file=csvFileName)
        cat("Wrote the csv file",csvFileName,"\n")
        print(outNotCatch)
        invisible(list(outVals,outNotCatch))
    }
    
}











exploitFrac.ex <- function(models,columns=c("Yr","Hrate:_1"),years=2001:2010,csvFileName=NULL) {
    #This grabs the Hrate from teh timeseries column, but I'm not sure what Hrate is?
    #For exploitation rate of hake, I used F_ from derived quantities instead    
    outModels <- NULL
    for(i in 1:length(models)) {
        tmp <- models[[i]][,columns]

        if(i==1) {
            outModels <- cbind(outModels,as.matrix(tmp[tmp[,1] %in% years,2]))
        }else {
            outModels <- cbind(outModels,NA,as.matrix(tmp[tmp[,1] %in% years,2]))
        }
    }
    rownames(outModels) <- years
    if(is.null(csvFileName)) {
        return(outModels)
    }else {
        if(substring(csvFileName,nchar(csvFileName)-4) != ".csv") csvFileName <- paste(csvFileName,"csv",sep=".")
        write.csv(outModels,file=csvFileName)
        cat("Wrote the csv file",csvFileName,"\n")
        invisible(outModels)
    }
}

#exploitFrac.ex(list(EA$timeseries,EA$timeseries),columns=c("Yr","Hrate:_1"),years=2001:2010,csvFileName=NULL)



if(F) {


HakeQuantileTables.ex <- function(models,years=2012:2014,outVar="Bratio_",quantiles=c(0.05,0.25,0.5,0.75,0.95),scalar=1,csvFileName=NULL) {
    #creates quantile tables from multiple model runs
    #uses the posterior of the 2012 depletion by default
    
    nmodels <- length(models)
    if(any(quantiles > 1) | any(quantiles < 0)) stop("Quantiles must be between zero and 1.")

    outVals <- NULL
    sortedModels <- NULL
    for(i in 1:nmodels) {
        #sort the mcmc by the sorting variable
        x <- models[[i]]$mcmc
        xx <- x[order(x[,sortVar]),c(paste(outVar,years,sep=""),paste("ForeCatch_",years,sep=""))]
        vals <- apply(xx,2,quantile,probs=quantiles)
        outs <- t(vals[,paste(outVar,years,sep="")])
        catches <- vals["50%",paste("ForeCatch_",years,sep="")]
        outVals <- rbind(outVals,cbind(catches,outs))
    }
    vals <- DerivedQuantsTables.ex(models=sortedModels,years=years,variable=outVar,probs=quantiles,mcmc=rep(T,nmodels))/scalar

        #get the values of interest
        vals <- DerivedQuantsTables.ex(models=list(list(mcmc=xx)),years=years,variable=outVar,mcmc=c(T,T,T))/scalar
        catch <- DerivedQuantsTables.ex(models=list(xx.low,xx.mid,xx.hi),years=years,variable="ForeCatch_",mcmc=c(T,T,T))[,c(2,6,10)]
        if(!any(apply(catch,1,function(x){x==x[1]}))) cat("WARNING: some catches are not equal for the low, mid, and high partitions\n")
        medCatch <- median(catch[,1])
        vals <- cbind(medCatch,years,catch[,1],vals)
        outVals <- rbind(outVals,vals)
    
    outVals <- outVals[order(outVals[,1],outVals[,2]),]  #sort by median catch then year
    outVals <- outVals[,-1] #remove the median catch
    dimnames(outVals) <- list(rep(years,nmodels),c("Year","Catch","Low","Mid","High"))

    if(is.null(csvFileName)) {
        return(outVals)
    }else {
        if(substring(csvFileName,nchar(csvFileName)-4) != ".csv") csvFileName <- paste(csvFileName,"csv",sep=".")
        write.csv(outVals,file=csvFileName)
        cat("Wrote the csv file",csvFileName,"\n")
        invisible(outVals)
    }
    
}






    findDepl.fn <- function(x,years=years) {
        #x is an mcmc object that has been properly sorted
        #returns the median depletion for the years specified
        
    }


}




}
