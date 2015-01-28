## plot info related to proportions of hake age 4+ or 5+
## Ian Taylor, 1/15/2014


##  example use:
if(FALSE){
  # read model
  h06 <- SS_output(file.path(dir,"runs/2014hake_06_final2013Data"))
  # make plot of total biomass
  png(file.path(dir,"figs/2014hake_06_biomass_fraction_total.png"),
      res=300,units='in',width=8,height=10)
  biomass_fraction_plots(h06, selected=FALSE)
  dev.off()
  # make plot of selected biomass
  png(file.path(dir,"figs/2014hake_06_biomass_fraction_selected.png"),
      res=300,units='in',width=8,height=10)
  biomass_fraction_plots(h06, selected=TRUE)
  dev.off()
}



biomass_fraction_plots <- function(replist, selected=FALSE){
  #### biomass fraction of ages 4+
  # get weight at age
  wtatage <- replist$wtatage[replist$wtatage$fleet==1,-(2:6)]
  # make years positive
  names(wtatage)[1] <- "Yr" # avoid annoying mix of 'yr' and 'Yr'
  wtatage$Yr <- abs(wtatage$Yr)
  # get equilibrium value (mean across years for hake)
  wtatage.mean <- wtatage[1,]
  # get numbers at age
  natage <- replist$natage[replist$natage$"Beg/Mid"=="B",-c(1:6,8:11)]
  # fill in missing years in weight at age with equilibrium value
  for(y in setdiff(natage$Yr, wtatage$Yr)){
    tmp <- wtatage.mean
    tmp$Yr <- y
    wtatage <- rbind(wtatage,tmp)
  }
  wtatage <- wtatage[order(wtatage$Yr),]
  Yrs <- rownames(natage) <- natage$Yr
  # get fishery selectivity (method would differ if it were time-varying)
  sel <- replist$ageselex[replist$ageselex$fleet==1 &
                          replist$ageselex$factor=="Asel",c("year",paste(0:20))]
  if(nrow(sel)<10){
    # if not time-varying, repeat vector for all years
    sel <- matrix(as.numeric(sel[1,-1]),ncol=ncol(sel)-1,nrow=length(Yrs),byrow=TRUE)
  }else{
    # if time-varying, fiddle with years to make them match the numbers at age
    sel.init <- sel[sel$year==1963,]
    sel <- sel[sel$year!=1963,]
    for(y in 1965:1964){
      sel.init$year <- y
      sel <- rbind(sel.init, sel)
    }
    sel.2014 <- sel[sel$year==2014,]
    for(y in 2015:2016){
      sel.2014$year <- y
      sel <- rbind(sel, sel.2014)
    }
    if(any(sel$year!=Yrs)){
      stop("problem with selectivity")
    }else{
      sel <- sel[,-1]
    }
  }

  # calculate biomass at age
  batage <- natage[,-1]*wtatage[,-1]
  # selected biomass at age
  batage.sel <- batage*sel
  
  if(selected){
    B0plus <- as.numeric(apply(batage.sel, 1, sum))
    B4plus <- as.numeric(apply(batage.sel[, 0:20 >= 4], 1, sum))
    B5plus <- as.numeric(apply(batage.sel[, 0:20 >= 5], 1, sum))
  }else{
    B0plus <- as.numeric(apply(batage, 1, sum))
    B4plus <- as.numeric(apply(batage[, 0:20 >= 4], 1, sum))
    B5plus <- as.numeric(apply(batage[, 0:20 >= 5], 1, sum))
  }

  # define time-periods
  par(mfrow=c(2,1))
  main.yrs <- Yrs %in% 1966:2014
  fore.yrs <- Yrs >= 2014

  # plot timeseries of biomass
  plot(0, type='n', xlim=range(Yrs), ylim=c(0,ceiling(max(B0plus/1e6))), yaxs='i', las=1,
       xlab='Year', ylab='Biomass (millions of mt)')
  lines(Yrs[main.yrs], B0plus[main.yrs]/1e6, lwd=3, col=1, lty=1)
  lines(Yrs[main.yrs], B4plus[main.yrs]/1e6, lwd=3, col=2, lty=1)
  lines(Yrs[main.yrs], B5plus[main.yrs]/1e6, lwd=3, col=4, lty=1)
  lines(Yrs[fore.yrs], B0plus[fore.yrs]/1e6, lwd=2, col=1, lty='12')
  lines(Yrs[fore.yrs], B4plus[fore.yrs]/1e6, lwd=2, col=2, lty='12')
  lines(Yrs[fore.yrs], B5plus[fore.yrs]/1e6, lwd=2, col=4, lty='12')
  points(Yrs[1], B0plus[1]/1e6,lwd=3)
  points(Yrs[1], B4plus[1]/1e6,col=2,lwd=3)
  points(Yrs[1], B5plus[1]/1e6,col=4,lwd=3)
  axis(1, at=1964, lab="Equilibrium", cex.axis=.8)
  axis(1, at=2014)
  abline(v=c(1964,seq(1970,2010,10),2014),lty=3,col='grey')
  abline(h=1:5,lty=3,col='grey')
  legend('bottomleft', lwd=3, col=c(1,2,4), ncol=3,
         legend=c("All ages","Ages 4+","Ages 5+"),bg='white')
  title(main=ifelse(selected, "Estimated selected biomass", "Estimated total biomass"))
        
  # plot timeseries of fractions of biomass
  plot(0, type='n', xlim=range(Yrs), ylim=c(0,1), yaxs='i', las=1,
       xlab='Year', ylab='Fraction of biomass')
  lines(Yrs[main.yrs], B4plus[main.yrs]/B0plus[main.yrs], lwd=3, col=2)
  lines(Yrs[main.yrs], B5plus[main.yrs]/B0plus[main.yrs], lwd=3, col=4)
  lines(Yrs[fore.yrs], B4plus[fore.yrs]/B0plus[fore.yrs], lwd=2, col=2, lty='11')
  lines(Yrs[fore.yrs], B5plus[fore.yrs]/B0plus[fore.yrs], lwd=2, col=4, lty='11')
  points(Yrs[1], B4plus[1]/B0plus[1], lwd=3, col=2)
  points(Yrs[1], B5plus[1]/B0plus[1], lwd=3, col=4)
  axis(1, at=1964, lab="Equilibrium", cex.axis=.8)
  axis(1, at=2014)
  abline(v=c(1964,seq(1970,2010,10),2014),lty=3,col='grey')
  abline(h=seq(0,0.8,.2),lty=3,col='grey')
  legend('bottomleft', lwd=3, col=c(2,4), ncol=2,
         legend=c("Ages 4+","Ages 5+"),bg='white')
  title(main=ifelse(selected, "Estimated fractions of selected biomass", "Estimated fractions of total biomass"))
}
