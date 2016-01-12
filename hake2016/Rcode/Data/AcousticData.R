require(XLConnect)

setwd("C:/NOAA2016/Hake/Data/AcousticSurvey")

###design-based
yrs <- c(1998,2001,2003,2005,2007,2009,2011,2012,2013,2015)
biom <- matrix(NA,nrow=length(yrs),ncol=2,dimnames=list(yrs,c("biomass","CV")))
comps <- matrix(NA,nrow=length(yrs),ncol=15,dimnames=list(yrs,paste("a",1:15,sep="")))
compColNames <- c("Year","Season","Fleet","Sex","Partition","AgeErr","LbinLo","LbinHi","nHauls","a1","a2","a3","a4","a5","a6","a7","a8","a9","a10","a11","a12","a13","a14","a15")
compsForSS <- matrix(NA,nrow=length(yrs),ncol=length(compColNames),dimnames=list(yrs,compColNames))
compsForSS[,c("Year")] <- yrs
compsForSS[,c("Season")] <- 1
compsForSS[,c("Fleet")] <- 2
compsForSS[,c("Sex","Partition")] <- 0
compsForSS[,c("AgeErr")] <- yrs-1972
compsForSS[,c("LbinLo","LbinHi")] <- -1
 
for(yr in yrs) {
	wb <- loadWorkbook(file.path("2015-12-12 Final/without extrapolation",yr,"un-kriged_len_age_biomass_table.xlsx"))
	dat <- readWorksheet(wb, sheet = "Sheet3", header = TRUE, startRow=2, endRow=42, rownames=1, drop="Subtotal")
	biom[as.character(yr),"biomass"] <- sum(dat[,-1])

	wb <- loadWorkbook(file.path("2015-12-12 Final/without extrapolation",yr,"un-kriged_len_age_abundance_table.xlsx"))
	dat <- readWorksheet(wb, sheet = "Sheet3", header = TRUE, startRow=2, endRow=42, rownames=1, drop=c("Unaged","Subtotal"))
	dat$X1 <- 0   #remove age 1 fish
	tmp <- apply(dat,2,sum)/sum(dat)
	tmp["X15"] <- sum(tmp[paste("X",15:20,sep="")])
	comps[as.character(yr),] <- tmp[paste("X",1:15,sep="")]
}



###kriged, no extrapolation
yrs <- c(1998,2001,2003,2005,2007,2009,2011,2012,2013,2015)
#biom <- matrix(NA,nrow=length(yrs),ncol=2,dimnames=list(yrs,c("biomass","CV")))
comps <- matrix(NA,nrow=length(yrs),ncol=15,dimnames=list(yrs,paste("a",1:15,sep="")))
compColNames <- c("Year","Season","Fleet","Sex","Partition","AgeErr","LbinLo","LbinHi","nHauls","a1","a2","a3","a4","a5","a6","a7","a8","a9","a10","a11","a12","a13","a14","a15")
compsForSS <- matrix(NA,nrow=length(yrs),ncol=length(compColNames),dimnames=list(yrs,compColNames))
compsForSS[,c("Year")] <- yrs
compsForSS[,c("Season")] <- 1
compsForSS[,c("Fleet")] <- 2
compsForSS[,c("Sex","Partition")] <- 0
compsForSS[,c("AgeErr")] <- yrs-1972
compsForSS[,c("LbinLo","LbinHi")] <- -1
 
for(yr in yrs) {
	# wb <- loadWorkbook(file.path("2015-12-12 Final/without extrapolation",yr,"kriged_len_age_biomass_table.xlsx"))
	# dat <- readWorksheet(wb, sheet = "Sheet3", header = TRUE, startRow=2, endRow=42, rownames=1, drop="Subtotal")
	# biom[as.character(yr),"biomass"] <- sum(dat[,-1])

	wb <- loadWorkbook(file.path("2015-12-12 Final/without extrapolation",yr,"kriged_len_age_abundance_table.xlsx"))
	dat <- readWorksheet(wb, sheet = "Sheet3", header = TRUE, startRow=2, endRow=42, rownames=1, drop=c("Subtotal"))
	dat$X1 <- 0   #remove age 1 fish
	tmp <- apply(dat,2,sum)/sum(dat)
	tmp["X15"] <- sum(tmp[paste("X",15:20,sep="")])
	comps[as.character(yr),] <- tmp[paste("X",1:15,sep="")]

	wb <- loadWorkbook(file.path("2015-12-12 Final/without extrapolation",yr,"aged_len_haul_counts_table.xlsx"))
	dat <- readWorksheet(wb, sheet = "Sheet3", header = TRUE, startRow=2, endRow=42, rownames=1, drop=c("Subtotal"))
	compsForSS[as.character(yr),"nHauls"] <- ncol(dat)

	compsForSS[as.character(yr),paste("a",1:15,sep="")] <- comps[as.character(yr),paste("a",1:15,sep="")]
}


###kriged, with extrapolation
yrs <- c(1998,2001,2003,2005,2007,2009,2011,2012,2013,2015)
#biom <- matrix(NA,nrow=length(yrs),ncol=2,dimnames=list(yrs,c("biomass","CV")))
comps <- matrix(NA,nrow=length(yrs),ncol=15,dimnames=list(yrs,paste("a",1:15,sep="")))
compColNames <- c("Year","Season","Fleet","Sex","Partition","AgeErr","LbinLo","LbinHi","nHauls","a1","a2","a3","a4","a5","a6","a7","a8","a9","a10","a11","a12","a13","a14","a15")
compsForSS <- matrix(NA,nrow=length(yrs),ncol=length(compColNames),dimnames=list(yrs,compColNames))
compsForSS[,c("Year")] <- yrs
compsForSS[,c("Season")] <- 1
compsForSS[,c("Fleet")] <- 2
compsForSS[,c("Sex","Partition")] <- 0
compsForSS[,c("AgeErr")] <- yrs-1972
compsForSS[,c("LbinLo","LbinHi")] <- -1
 
for(yr in yrs) {
	# wb <- loadWorkbook(file.path("2015-12-12 Final/without extrapolation",yr,"kriged_len_age_biomass_table.xlsx"))
	# dat <- readWorksheet(wb, sheet = "Sheet3", header = TRUE, startRow=2, endRow=42, rownames=1, drop="Subtotal")
	# biom[as.character(yr),"biomass"] <- sum(dat[,-1])

	wb <- loadWorkbook(file.path("2015-12-12 Final/with extrapolation",yr,"kriged_len_age_abundance_table.xlsx"))
	dat <- readWorksheet(wb, sheet = "Sheet3", header = TRUE, startRow=2, endRow=42, rownames=1, drop=c("Subtotal"))
	dat$X1 <- 0   #remove age 1 fish
	tmp <- apply(dat,2,sum)/sum(dat)
	tmp["X15"] <- sum(tmp[paste("X",15:20,sep="")])
	comps[as.character(yr),] <- tmp[paste("X",1:15,sep="")]

	wb <- loadWorkbook(file.path("2015-12-12 Final/with extrapolation",yr,"aged_len_haul_counts_table.xlsx"))
	dat <- readWorksheet(wb, sheet = "Sheet3", header = TRUE, startRow=2, endRow=42, rownames=1, drop=c("Subtotal"))
	compsForSS[as.character(yr),"nHauls"] <- ncol(dat)

	compsForSS[as.character(yr),paste("a",1:15,sep="")] <- comps[as.character(yr),paste("a",1:15,sep="")]
}



####################################################
# Create some plots
plotBars.fn <- function(x,y,gap=0,scalar=1e6,add=F,ciCol="black",ciLty=1,ciLwd=1,...) {
    #x is the x axis values (i.e., years)
    #y is a data frame with:
    ##value: estimate (point) to plot
    ##lo: lower CI
    ##hi: higher CI

    if(!add) plot(x,y$value/scalar,...)
    if(add) points(x,y$value/scalar,...)
    segments(x,y$lo/scalar,x,y$value/scalar-gap,col=ciCol,lty=ciLty,lwd=ciLwd)
    segments(x,y$hi/scalar,x,y$value/scalar+gap,col=ciCol,lty=ciLty,lwd=ciLwd) 
}


require(XLConnect)
setwd("C:/NOAA2016/Hake/Data/AcousticSurvey")
wb <- loadWorkbook("AcousticSurveyTimeSeries_2015.xlsx")
biom <- readWorksheet(wb, sheet = "Biomass", header = TRUE, startRow=1, endRow=12)
names(biom) <- c("Year","designBased","noExtrapolation","Extrapolation","assessment2015","CvNoExtrapolation","CvExtrapolation","CvAssessment2015")

windows(height=5, width=12)
par(mar=c(4,4.2,1,1))
old <- data.frame(year = biom$Year,
	              value=biom$assessment2015, 
	              lo=exp(log(biom$assessment2015) - 1.96*sqrt(log(biom$CvAssessment2015^2+1))),
	              hi=exp(log(biom$assessment2015) + 1.96*sqrt(log(biom$CvAssessment2015^2+1))))
plotBars.fn(old$year, old[,-1], gap=50, scalar=1, add=F, ciCol="black", ciLty=1, ciLwd=1, ylim=c(0,max(old[,-1],na.rm=T)), pch=16,type="b",xlab="Year",ylab="Index estimate (thousand mt)",las=1,xaxt="n")
axis(1,at=biom$Year)
noEx <- data.frame(year = biom$Year,
	              value=biom$noExtrapolation, 
	              lo=exp(log(biom$noExtrapolation) - 1.96*sqrt(log(biom$CvNoExtrapolation^2+1))),
	              hi=exp(log(biom$noExtrapolation) + 1.96*sqrt(log(biom$CvNoExtrapolation^2+1))))
plotBars.fn(noEx$year+0.1, noEx[,-1], gap=50, scalar=1, add=T, ciCol="red", ciLty=1, ciLwd=1, pch=15, type="b", col="red")
ex <- data.frame(year = biom$Year,
	              value=biom$Extrapolation, 
	              lo=exp(log(biom$Extrapolation) - 1.96*sqrt(log(biom$CvExtrapolation^2+1))),
	              hi=exp(log(biom$Extrapolation) + 1.96*sqrt(log(biom$CvExtrapolation^2+1))))
plotBars.fn(ex$year-0.1, ex[,-1], gap=50, scalar=1, add=T, ciCol="blue", ciLty=1, ciLwd=1, pch=17, type="b", col="blue")
legend("topleft", c("2015 assessment","New, No Extrapolation","New, With Extrapolation"), col=c("black","red","blue"), pch=c(16,15,17),lty=1)

wb <- loadWorkbook("AcousticSurveyTimeSeries_2015.xlsx")
oldAges <- readWorksheet(wb, sheet = "AgeComps", header = TRUE, startRow=2,endRow=12)
designAges <- readWorksheet(wb, sheet = "AgeComps", header = TRUE, startRow=16,endRow=26)
noExtrapAges <- readWorksheet(wb, sheet = "AgeComps", header = TRUE, startRow=30,endRow=40)
extrapAges <- readWorksheet(wb, sheet = "AgeComps", header = TRUE, startRow=44,endRow=54)

yrs <- extrapAges$X.year
windows(height=6, width=15)
par(mfrow=c(2,5),mar=c(4,4,1,1))
for(yr in yrs) {
	if(sum(oldAges$X.year==yr) == 0) {
		tmp <- rbind(noExtrapAges[noExtrapAges$X.year==yr,c(paste0("a",1:15))],
		          extrapAges[extrapAges$X.year==yr,c(paste0("a",1:15))])
    	barplot(as.matrix(tmp),beside=T,col=c("red","blue"),main=yr,ylim=c(0,75),ylab="",xlab="",names=1:15,las=1)
	} else {
		tmp <- rbind(oldAges[oldAges$X.year==yr,c(paste0("a",1:15))],
		          noExtrapAges[noExtrapAges$X.year==yr,c(paste0("a",1:15))],
		          extrapAges[extrapAges$X.year==yr,c(paste0("a",1:15))])
    	barplot(as.matrix(tmp),beside=T,col=c("gray","red","blue"),main=yr,ylim=c(0,75),ylab="",xlab="",names=1:15,las=1)
	}
	if(yr==1998) {
		legend("topleft",c("2015 assessment","New, No Extrapolation","New, With Extrapolation"), fill=c("gray","red","blue"),bty="n")
	}
}
mtext("Proportion of numbers-at-age",side=2,outer=T,line=-1.3)
mtext("Age",side=1,outer=T,line=-1.5)