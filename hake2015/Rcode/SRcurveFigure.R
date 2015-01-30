# plot of MCMC variability in SR relationship

## source("C:/NOAA2014/Hake/WriteUp/Rcode/WriteUpFunctions.R")
## r21mcmc <- cbind(read.table("C:/NOAA2014/Hake/Models/2014hake_21_TVselex1991start_MCMC/posteriors.sso",header=T),
##                  read.table("C:/NOAA2014/Hake/Models/2014hake_21_TVselex1991start_MCMC/derived_posteriors.sso",header=T))


addpoly <- function(yrvec, lower, upper, shadecol=rgb(0,0,0,.1),col=1){
  # add shaded uncertainty intervals behind line
  # modified from SSplotComparisons in r4ss package
  lower[lower<0] <- 0 # max of value or 0
  polygon(x=c(yrvec,rev(yrvec)),
          y=c(lower,rev(upper)),
          border=NA,col=shadecol)
  lines(yrvec,lower,lty=3,col=col)
  lines(yrvec,upper,lty=3,col=col)
}

bh <- function(h,B=seq(0,1.5,0.005)){
  R <- (4.*h*B) /
    ((1.-h)+(5.*h-1.)*B)
  return(R)
}

Rmat <- NULL
for(iter in r21mcmc$Iter){
  B <- seq(0,1.5,0.005)
  h <- r21mcmc$SR_BH_steep[r21mcmc$Iter==iter]
  R <- bh(h,B)
  Rmat <- cbind(Rmat,R)
}
Rquants <- apply(Rmat,1,quantile,probs=c(0.025,0.5,0.975))
adj <- exp(-.5*1.4^2) # bias adjustment

#pdf('c:/SS/hake/Hake_2013/runs/SpawnRecruit_MCMC.pdf')
png('c:/NOAA2014/Hake/WriteUp/Figures/SpawnRecruit_MCMC.png',res=300,width=6.5,height=6.5,units='in')

par(mar=c(4,4,1,4)+0.1)
plot(0,type='n',xlim=c(0,1.4),ylim=c(0,7),xaxs='i',yaxs='i',
     axes=FALSE,
     xlab="Depletion",ylab="Recruitment relative to R0")
axis(1,at=seq(0,2,.1))
axis(2,at=seq(0,par()$usr[4],1),las=1)
abline(h=1,v=1,lty=2,col=1)
abline(h=adj,lty=2,col=2)

addpoly(B,Rquants[1,],Rquants[3,])
addpoly(B,adj*Rquants[1,],adj*Rquants[3,],shadecol=rgb(1,0,0,.1),col=2)
lines(B,Rquants[2,],lwd=3)
lines(B,adj*Rquants[2,],lwd=3,col=2)

yrs <- 1966:2013
Rratio <- r21mcmc[names(r21mcmc) %in% c("Recr_Virgin",paste("Recr_",yrs,sep=""))]
mnRvirg <- median(Rratio[,1])
Bratio <- r21mcmc[names(r21mcmc) %in% c("SPB_Virgin",paste("SPB_",yrs,sep=""))]

# standardize relative to equilibrium
Bratio <- Bratio/Bratio[,1]
Rratio <- Rratio/Rratio[,1]
colvec <- rev(rich.colors.short(length(yrs)+10,alpha=0.8))[-(1:10)]


for(iyr in 1:length(yrs)){
  y <- yrs[iyr]
  Bs <- Bratio[,iyr+1] # +1 to account for equilbrium in first column
  Rs <- Rratio[,iyr+1]
  Bq <- quantile(Bs,prob=c(0.025,0.5,0.975))
  Rq <- quantile(Rs,prob=c(0.025,0.5,0.975))
  arrows(Bq[2],Rq[1],Bq[2],Rq[3],col=colvec[iyr],code=3,angle=90,length=.02)
  arrows(Bq[1],Rq[2],Bq[3],Rq[2],col=colvec[iyr],code=3,angle=90,length=.02)
  points(Bq[2],Rq[2],pch=21,bg=colvec[iyr],col=1)
  if(Rq[2]>1.5 | y>2007) text(y,x=Bq[2],y=Rq[2],cex=.5,adj=c(-.3,-.3))
}
box()
text(1.01,0.15+1,    "Mean recruitment",        cex=.8,pos=4)
text(1.01,0.15+1*adj,"Median recruitment",col=2,cex=.8,pos=4)
points(1,1,pch=16,cex=2)
points(1,1*adj,pch=16,cex=2,col=2)

axis(4,at=1:7,label=round((1:7)*mnRvirg/1e6,1),las=1,cex.axis=0.8)
mtext("Unfished equilibrium recruitment, R0 (billions)",side=4,line=2.5)

dev.off()
