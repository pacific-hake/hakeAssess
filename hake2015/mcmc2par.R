stop("don't source this file")

# read posteriors.sso file
posts <- read.table(file.path(SSdir, '2015hake_basePreSRG_mcmc12e6/posteriors.sso'),
                    header=TRUE)

# create a table of parameter values based on labels in parameters section of Report.sso
newpar <- data.frame(value=c(1, base$parameters$Value),
                     hash="#", label=c("dummy_parm", base$parameters$Label),
                     stringsAsFactors=FALSE)
# add hash before first column name
names(newpar)[1] <- "#value"
# change label for R0 parameter to match R's conversion in "posts"
newpar$label[newpar$label=="SR_LN(R0)"] <- "SR_LN.R0."

# write table of new files
write.table(x=newpar,
            file=file.path(SSdir, '2015hake_basePreSRG_parTest/ss3.par'),
            quote=FALSE, row.names=FALSE)
# run model using command: "ss3 -maxfn 0 -phase 10 -nohess"
parTestOut <- SS_output(file.path(SSdir, '2015hake_basePreSRG_parTest'),covar=FALSE)


# change working directory to make life simple
setwd(file.path(SSdir, '2015hake_basePreSRG_mcmc12e6_reports'))

start <- SS_readstarter('starter.ss')
if(start$init_values_src!=1){
  stop("change starter file to read from par file!")
}

# loop over rows of posteriors file
#for(irow in 1:100){
for(irow in 1:nrow(posts)){
  print(irow)
  # replace values in newpar table with posteriors values
  # (excluding 1 and 2 for "Iter" and "Objective_function")
  newpar[newpar$label %in% names(posts), 1] <- as.numeric(posts[irow, -(1:2)])
  write.table(x=newpar,
              file='ss3.par',
              quote=FALSE, row.names=FALSE)
  file.copy('ss3.par', paste0('ss3_input',irow,'.par'), overwrite=TRUE)
  # run SS
  system('ss3 -maxfn 0 -phase 10 -nohess', intern=TRUE)
  file.copy('ss3.par', paste0('ss3_output',irow,'.par'), overwrite=TRUE)
  file.copy('Report.sso', paste0('Report_',irow,'.sso'), overwrite=TRUE)
  file.copy('CompReport.sso', paste0('CompReport_',irow,'.sso'), overwrite=TRUE)
}

# make table to store likelihood components
like.info <- data.frame(Iter=posts$Iter, stringsAsFactors=FALSE)
for(lab in c("TOTAL",
             "Equil_catch",
             "Survey",
             "Age_comp",
             "Recruitment", 
             "Forecast_Recruitment", 
             "Parm_priors",
             "Parm_devs", 
             "Crash_Pen",
             "Age_comp_surv",
             "Age_comp_fishery")){
  like.info[[lab]] <- 0
}

for(irow in 1:nrow(posts)){
  likes <- read.table(paste0('Report_',irow,'.sso'), skip=86, nrows=17,
                      fill=TRUE, row.names=NULL, col.names=1:4, stringsAsFactors=FALSE)
  like.info[irow, 2:10] <- as.numeric(likes$X2[3:11])  # fleet-aggregated likelihoods
  like.info[irow, 11] <- as.numeric(likes[17, 3]) # fleet-specific age comp likelihoods
  like.info[irow, 12] <- as.numeric(likes[17, 4]) # fleet-specific age comp likelihoods
}

# read expected proportions and Pearson values for each age comp observations
comp.table <- read.table(paste0('CompReport.sso'), skip=21, header=TRUE,
                      fill=TRUE, stringsAsFactors=FALSE)
# loop to create columns Exp1, Exp2, ..., Exp999 and Pearson1, Pearson2, etc.
for(irow in 1:nrow(posts)){
  if(irow%%100==0){
    print(irow)
  }
  comps <- read.table(paste0('CompReport_',irow,'.sso'), skip=21, header=TRUE,
                      fill=TRUE, stringsAsFactors=FALSE)
  lab1 <- paste0("Pearson",irow)
  lab2 <- paste0("Exp",irow)
  comp.table[lab1] <- comps$Pearson
  comp.table[lab2] <- comps$Exp
}
# filter out values that are not included in agedbase within base model
comp.table <- comp.table[!is.na(comp.table$N) & comp.table$N>0,]

# median and quantiles of expected values and Pearsons
exp.table <- comp.table[,names(comp.table) %in% paste0("Exp",1:nrow(posts))]
Pearson.table <- comp.table[,names(comp.table) %in% paste0("Pearson",1:nrow(posts))]
exp.median <- apply(exp.table, MARGIN=1, FUN=median)
exp.low    <- apply(exp.table, MARGIN=1, FUN=quantile, probs=0.025)
exp.high   <- apply(exp.table, MARGIN=1, FUN=quantile, probs=0.975)
Pearson.median <- apply(Pearson.table, MARGIN=1, FUN=median)
Pearson.low    <- apply(Pearson.table, MARGIN=1, FUN=quantile, probs=0.025)
Pearson.high   <- apply(Pearson.table, MARGIN=1, FUN=quantile, probs=0.975)

# confirm that values match between mcmc tables and base model MLE table
table(base$agedbase$Obs==comp.table$Obs)
## TRUE 
##  750 

# make new model output object to modify
base.mcmc <- base
base.mcmc$agedbase$Exp <- exp.median
base.mcmc$agedbase$Exp.025 <- exp.low
base.mcmc$agedbase$Exp.975 <- exp.high
base.mcmc$agedbase$Pearson <- Pearson.median
base.mcmc$agedbase$Pearson.025 <- Pearson.low
base.mcmc$agedbase$Pearson.975 <- Pearson.high


# get survey fits
cpue.table <- NULL
for(irow in 1:nrow(posts)){
  cpue <- read.table(paste0('Report_',irow,'.sso'), skip=1243, nrows=21,
                     header=TRUE, stringsAsFactors=FALSE)
  lab1 <- paste0("Exp",irow)
  cpue.table <- cbind(cpue.table, cpue$Exp)
}
cpue.median <- apply(cpue.table, MARGIN=1, FUN=median)
cpue.025 <- apply(cpue.table, MARGIN=1, FUN=quantile, probs=0.025)
cpue.975 <- apply(cpue.table, MARGIN=1, FUN=quantile, probs=0.975)
