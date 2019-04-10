setwd("~/Documents/GitHub/RceattleRuns/GOA/ATF 2018")

# Catch time series
catch <- read.csv("Groundfish Total Catch by Fishery_ATF.csv")

catch <- aggregate(catch$Weight.Posted..Sum., by=list(Category=catch$Year), FUN=sum)

# Survey biomass
biom <- read.csv("Biomass by Regulatory Area - GOA.csv")
biom <- aggregate(biom$Area.Biomass, by=list(Category=biom$Year), FUN=sum)

# Weighting factor by M

par(mfrow=c(1,2))
nages<-25
getProp<-function(Mf=.3,Mm=.3,nagesIN=nages){
  f<-exp(-Mf*(0:nagesIN))
  m<-exp(-Mm*(0:nagesIN))
  propf<-f/(f+m)
  propm<-m/(f+m)
  plot(propf,ylim=c(0,1),type="l",lty=1);lines(propm,lty=2,lwd=2)
  return(data.frame(age=0:nagesIN,propf,propm))
}
PP<-getProp(Mf=.27,Mm=.3)

Wf_age<-.2*exp((0:nages)*.2)
Wm_age<-.2*exp((0:nages)*.16)

W_age<-Wf_age*PP$propf+Wm_age*PP$propm
plot(0:nages,Wf_age,type="l",lty=1)
lines(0:nages,Wm_age,lty=2,lwd=2)
lines(0:nages,W_age,lwd=2,col="red")
