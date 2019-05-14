# File to set up the UobsWtAge array for the diet data in Rceattle
# Grant Adams 5-12-19

# Load data
load("Diet/GOA_agg.preyWT_sub.Rdata")

head(agg.preyWT)

# Clean data and extract summer monthsß
dat <- agg.preyWT
stmp<-c("W. Pollock","P. Cod","Arrowtooth", "P. Halibut")
tt<-match((dat$Species2),stmp)
rmn<-which(is.na(tt))
dat<-dat[-rmn,]
dat<-dat[dat$Mo>=6&dat$Mo<=9,]
dat<-dat[dat$BT>-5,]
dat<-dat[dat$DIG_mnbyW!=0&is.na(dat$DIG_mnbyW)==FALSE,]

# Assign age
dat$ageHat <- NA




predict.age3<-function(Wt,spnum1=1,regnum1=1,partable=VonBparm.table,maxage=30){
  if(any(is.na(match(spnum1,c(1,2,3)))==TRUE))
  {
    return(NA)
  }else{
    pos.na<-function(x){
      which(x>0)[1]
    }
    tmp<-paste(partable$species,partable$region,sep="_")
    tmp2<-paste(sp.lookup$sp1[spnum1],reg.lookup$r1[regnum1],sep="_")
    #rr<-which(partable$species==sp.lookup$sp1[spnum1]&partable$region==reg.lookup$r1[regnum1])
    rr<-match(tmp2,tmp)
    pars<-(data.frame(partable[rr,]))
    if(dim(pars)[1]==5 & dim(pars)[2]==1)
      pars<-t(pars)
    eval(parse(text=paste("mtmp<-LW.glm$",pars$species,sep="")))
    a<-as.numeric(coef(mtmp)[1])
    b<-as.numeric(coef(mtmp)[2])
    L.hat<-exp((log(Wt)+a)/b)
    
    H<-17.628 # pars$H
    K<-0.133 # pars$K
    t0<-0.115 # pars$t0
    d<-0.544 # pars$d
    Winf1<-pars$Winf
    sigma<-pars[,5]
    Winf<-(H/K)^(1/(1-d))
    age.prof<-seq(0,maxage,.1)
    nobs<-length(age.prof)
    W.hat<-rep(0,nobs)
    
    W.hat2<-matrix(0,length(Wt),nobs)
    age.hat<-rep(0,length(Wt))
    for(aa in 1:nobs)
      W.hat2[,aa]<-Winf*(1-exp(-K*(1-d)*(age.prof[aa]-t0)))^(1/(1-d))-Wt
    
    age.hat<-age.prof[apply(W.hat2,1,pos.na)]
    cc<-which(Wt>Winf)
    if(length(cc)>0){
      age.hat[cc]<-maxage
    }
    
    return(age.hat)
  }
}
