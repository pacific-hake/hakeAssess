setwd("C:\\NOAA2015\\Hake\\Data\\Catches")
source("C:/NOAA2015/Hake/Data/Rcode/functions/HakeFigureFunctions.R")
doPNG <- T


#######################################
#Catch plot
catches <- read.csv("2015HakeCatches.csv")
catches[is.na(catches)] <- 0
catch <- catches[,c("CAN_forgn","CAN_JV","CAN_Shoreside","CAN_FreezeTrawl","US_foreign","US_JV","atSea_US_MS","atSea_US_CP","US_shore")]
cols <- c(         rgb(0,0.8,0),rgb(0,0.6,0),rgb(0.8,0,0),rgb(0.4,0,0),      rgb(0,0.2,0),rgb(0,0.4,0),rgb(0,0,0.7),rgb(0,0,0.4),rgb(0,0,1))
#catch <- catches[,c("US_foreign","CAN_forgn","US_JV","CAN_JV","CAN_Shoreside","CAN_FreezeTrawl","atSea_US_MS","atSea_US_CP","US_shore")]
#cols<- c(rgb(70,130,180,maxColorValue = 255),rgb(128,0,0,maxColorValue = 255),rgb(218,165,32,maxColorValue = 255),rgb(107,142,35,maxColorValue = 255),rgb(0,0,205,maxColorValue = 255),rgb(72,61,139,maxColorValue = 255),rgb(200,200,220,maxColorValue = 255),rgb(0.5,0.5,0.5))

legOrder <- c(6,5,2,1,4,3,NA,NA,9,8,7)  #c(9,8,7,NA,NA,4,3,6,5,2,1)

ht <- 4; wd<- 6.5
if(doPNG) {png("Figures/catches.png",height=ht,width=wd,pointsize=10,units="in",res=300)}
if(!doPNG) {windows(height=ht,width=wd)}
par(las=1,mar=c(4, 4, 6, 2) + 0.1,cex.axis=0.9)
tmp <- barplot(t(as.matrix(catch))/1000,beside=F,names=catches[,1],
        col=cols,xlab="Year",ylab="",cex.lab=1,xaxs="i",mgp=c(2.2,1,0))
axis(1,at=tmp,label=F,line=-0.12)
grid(NA,NULL,lty=1,lwd = 1)
mtext("Catch ('000 mt)",side=2,line=2.8,las=0,cex=1.3)
barplot(t(as.matrix(catch))/1000,beside=F,names=catches[,1],
        #legend.text=c("Canadian Foreign","Canadian Joint-Venture","Canadian Shoreside","Canadian Freezer Trawl",
       # 	          "U.S. Foreign","U.S. Joint-Venture","U.S. MS","U.S. CP","U.S. Shore-based")[legOrder],
        col=cols,
        #args.legend=list(x=60.5,y=430,bg="white",horiz=F,xpd=NA,cex=0.83,ncol=3,col=cols[legOrder]),
        xlab="Year",ylab="",cex.lab=1,xaxs="i",add=T,mgp=c(2.2,1,0))
legend(x=0,y=510,
	   c("Canadian Foreign","Canadian Joint-Venture","Canadian Shoreside","Canadian Freezer Trawl",
        	          "U.S. Foreign","U.S. Joint-Venture","U.S. MS","U.S. CP","U.S. Shore-based")[legOrder],
	   bg="white",horiz=F,xpd=NA,cex=1,ncol=3,fill=cols[legOrder],border=cols[legOrder],bty="n")
if(doPNG){dev.off()}
#run barplot a second time to overwrite grid lines

