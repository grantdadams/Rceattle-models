# This code depends on :
# GOAPredPreyL.Rdata  # mean prop of prey length L spp in diets of preds length L
# GOA_preyWt.Rdata  # mean prop of prey spp in diets of preds
# lkup.Rdata   # species name lookup table
# 
# 
# This code will create:
# mnDiet = average proportion of prey X in the diet of pred Y
# mnDietbyYR = average proportion of prey X in the diet of pred Y by year
# mnPPL_Diet = (not annual b/c not enough data) sum of count and weight (from LW regression of prey size) of prey X size a in the diets of pred Y size j. 
# 
# The last step is to take mnPPL_Diet, convert it to proportions and multiply by mnDiet or mnDietbyYR and also fill in the wide matrix with zeros for bins where the values are zero.
# 
# Note: makeUobs_sub.R is the step where I remove observer data so just keep "update_data" = 0 and you'll be good to go without needing to run it .

# K. Holsman R script for creating Uobs for CEATTLE
# May 12 2019
# __________________________________________________


rm(list=ls())
mainG  <- "Data/Diet"
main  <-  "Data/Diet"
library(dplyr)
library(ggplot2)
library(tidyr)

# Update Switches:
update_data  <-   0  # update raw queries - set to 0 
update.LW     <-  1
update.pp     <-  1
update.ppbyL  <-  1

# plot switches
plot.mndiet   <-  1

if(update_data){
  source(file.path(mainG,"makeUobs_sub.R"))
}else{
  
  load(file.path(mainG,"GOAPredPreyL.Rdata"))
  load(file.path(mainG,"GOA_preyWt.Rdata"))
  load(file.path(mainG,"lkup.Rdata"))
}


# create matrix of prey pref based on size bins for each spp:
# _____________________________________
nspp<-4
ageLenbins<-list(
  "W. Pollock"=c(14,24 ,32 ,39 ,43 ,48, 51, 54 ,57 ,59 ,60 ,61) ,
  "P. Cod"=c(9,12,15,18,21,24,27,30,33,36,39,42,45,50,55,60,65,70,75,80,85,90,95,100,105),
  "Arrowtooth"=c(10,16,18,20,22,24,26,28,30,32,34,36,38,40,43,46,49,52,55,58,61,64,67,70,75),
  "P. Halibut"=c(10,16,18,20,22,24,26,28,30,32,34,36,38,40,43,46,49,52,55,58,61,64,67,70,75,100,105))
bins<-list()

for(sp in 1:nspp){
  bins[[sp]]<-c(0,( (c(ageLenbins[[sp]],1000)-c(0,ageLenbins[[sp]]))/2)+c(0,ageLenbins[[sp]]) )
}
names(bins)<-names(ageLenbins)

# _____________________________________
# summarize mean prop by prey spp by bins:
# _____________________________________
GOA_preyWt$Lbin<-GOA_preyWt$ageLenbin<-NA
sptable<-tapply(as.character(GOAPredPreyL$CEATTLE_PREY),GOAPredPreyL$preyNum,unique)
spnames<-as.character(lkup[match(sptable,lkup$name1),]$name2)

GOA_preyWt$predNum<-as.numeric(
  factor(GOA_preyWt$Species_name2,levels=c("W. Pollock","P. Cod","Arrowtooth","P. Halibut")))

sptable2<-tapply(as.character(GOA_preyWt$Species_name2),GOA_preyWt$predNum,unique)
spnames2<-as.character(lkup[match(sptable2,lkup$name1),]$name2)

for(sp in 1:nspp){
  sub<-as_tibble(GOA_preyWt[GOA_preyWt$Species_name2==names(bins)[sp],])
  nbins<-length(bins[[sp]])
  #sub$Lbin<-bins[[sp]][nbins]
  for(b in 2:(nbins)){
    sub$ageLenbin[sub$PredL>=bins[[sp]][b]&sub$PredL<bins[[sp]][b+1]]<-ageLenbins[[sp]][b-1]
    sub$Lbin[sub$PredL>=bins[[sp]][b]&sub$PredL<bins[[sp]][b+1]]<-bins[[sp]][b]
  }
  
  # now get CEATTLE GOA data:
  sub2<-sub%>%
    group_by(Type,Region,
             # Strata,    # <-- uncomment this to biomass weight by strata
             # Station,    # <-- uncomment this to biomass weight by Station
             Species_name2,predNum,Yr,Lbin,ageLenbin)%>%
    summarise(
      nobs = length(SST),
      mnRLAT = mean(RLAT),
      mnRLONG = mean(RLONG),
      mnMONTH = mean(Mo),
      
      SST_mn = mean(SST,na.rm=T),
      BT_mn = mean(BT,na.rm=T),
      SST_sd=sd(SST,na.rm=T),
      BT_sd = sd(BT,na.rm=T),
      
      PredL_mn= mean(PredL,na.rm=T),
      PredW_mn= mean(PredW,na.rm=T),
      Obs_TWT_mn= mean(Obs_TWT,na.rm=T),
      obsC_mn= mean(obsC,na.rm=T),
      
      PredL_sd= sd(PredL,na.rm=T),
      PredW_sd= sd(PredW,na.rm=T),
      Obs_TWT_sd= sd(Obs_TWT,na.rm=T),
      obsC_sd= sd(obsC,na.rm=T),
      
      
      obsC_mn = mean(obsC,na.rm=T),
      C1_mn = mean(C1,na.rm=T),
      pollock_mn=mean(pollock,na.rm=T),
      pcod_mn= mean(pcod,na.rm=T),
      atf_mn=mean(atf,na.rm=T),
      halibut_mn=mean(halibut,na.rm=T),
      
      obsC_sd = sd(obsC,na.rm=T),
      C1_sd = sd(C1,na.rm=T),
      pollock_sd=sd(pollock,na.rm=T),
      pcod_sd= sd(pcod,na.rm=T),
      atf_sd=sd(atf,na.rm=T),
      halibut_sd=sd(halibut,na.rm=T))
  #    %>%
  # select(Type,Species_name2,predNum,Yr,Region,
  # 	mnRLAT,mnRLONG,mnMONTH,
  # 	Lbin,ageLenbin,nobs,
  # 	BT_mn,SST_mn,PredL_mn,PredW_mn,Obs_TWT_mn,
  # 	obsC_mn,C1_mn,pollock_mn,pcod_mn,atf_mn,halibut_mn,
  # 	BT_sd,SST_sd,PredL_sd,PredW_sd,Obs_TWT_sd,
  # 	obsC_sd,C1_sd,pollock_sd,pcod_sd,atf_sd,halibut_sd)
  
  if (sp==1){
    mnDietbyYR<-sub2
  }else{
    mnDietbyYR<-rbind(mnDietbyYR,sub2)
  }
  sub2<-sub%>%
    group_by(Type,Region,
             # Strata,    # <-- uncomment this to biomass weight by strata
             # Station,    # <-- uncomment this to biomass weight by Station
             Species_name2,predNum,Lbin,ageLenbin)%>%
    summarise(
      nobs = length(SST),
      mnRLAT = mean(RLAT),
      mnRLONG = mean(RLONG),
      mnMONTH = mean(Mo),
      
      SST_mn = mean(SST,na.rm=T),
      BT_mn = mean(BT,na.rm=T),
      SST_sd=sd(SST,na.rm=T),
      BT_sd = sd(BT,na.rm=T),
      
      PredL_mn= mean(PredL,na.rm=T),
      PredW_mn= mean(PredW,na.rm=T),
      Obs_TWT_mn= mean(Obs_TWT,na.rm=T),
      obsC_mn= mean(obsC,na.rm=T),
      
      PredL_sd= sd(PredL,na.rm=T),
      PredW_sd= sd(PredW,na.rm=T),
      Obs_TWT_sd= sd(Obs_TWT,na.rm=T),
      obsC_sd= sd(obsC,na.rm=T),
      
      
      obsC_mn = mean(obsC,na.rm=T),
      C1_mn = mean(C1,na.rm=T),
      pollock_mn=mean(pollock,na.rm=T),
      pcod_mn= mean(pcod,na.rm=T),
      atf_mn=mean(atf,na.rm=T),
      halibut_mn=mean(halibut,na.rm=T),
      
      obsC_sd = sd(obsC,na.rm=T),
      C1_sd = sd(C1,na.rm=T),
      pollock_sd=sd(pollock,na.rm=T),
      pcod_sd= sd(pcod,na.rm=T),
      atf_sd=sd(atf,na.rm=T),
      halibut_sd=sd(halibut,na.rm=T))
  #    %>%
  # select(Type,Species_name2,predNum,Yr,Region,
  # 	mnRLAT,mnRLONG,mnMONTH,
  # 	Lbin,ageLenbin,nobs,
  # 	BT_mn,SST_mn,PredL_mn,PredW_mn,Obs_TWT_mn,
  # 	obsC_mn,C1_mn,pollock_mn,pcod_mn,atf_mn,halibut_mn,
  # 	BT_sd,SST_sd,PredL_sd,PredW_sd,Obs_TWT_sd,
  # 	obsC_sd,C1_sd,pollock_sd,pcod_sd,atf_sd,halibut_sd)
  
  if (sp==1){
    mnDiet<-sub2
  }else{
    mnDiet<-rbind(mnDiet,sub2)
  }
}


if(any(is.na(mnDiet$Lbin))) mnDiet<-mnDiet[!is.na(mnDiet$Lbin),]
if(any(is.na(mnDietbyYR$Lbin))) mnDietbyYR<-mnDietbyYR[!is.na(mnDietbyYR$Lbin),]


mnDiet<-data.frame(mnDiet)
mnDietbyYR<-data.frame(mnDietbyYR)

subp<-data.frame(mnDiet[mnDiet$predNum==1,])
subp<-data.frame(mnDietbyYR[mnDietbyYR$predNum==1,])

plot(subp$pollock_mn~subp$PredL_mn,type="b")

#mnDiet is the mean (across stations, strata, and years) proportion of each prey species in the age bin of each predator
#mnDietbyYR is annual mean (across stations, strata) proportion of each prey species in the age bin of each predator
save(mnDiet,file=file.path(mainG,"mnDiet.Rdata"))
save(mnDietbyYR,file=file.path(mainG,"mnDietbyYR.Rdata"))


# _____________________________________
# summarize mean prop length by prey spp by bins:
# _____________________________________
GOAPredPreyL$Lbin_pred<-GOAPredPreyL$ageLenbin_pred<-NA
GOAPredPreyL$Lbin_prey<-GOAPredPreyL$ageLenbin_prey<-NA

for(sp in 1:nspp){
  sub<-as_tibble(GOAPredPreyL[GOAPredPreyL$CEATTLE_PRED==names(bins)[sp],])
  nbins<-length(bins[[sp]])
  #sub$Lbin<-bins[[sp]][nbins]
  for(b in 2:(nbins)){
    sub$ageLenbin_pred[sub$PRED_LEN>=bins[[sp]][b]&sub$PRED_LEN<bins[[sp]][b+1]]<-ageLenbins[[sp]][b-1]
    sub$Lbin_pred[sub$PRED_LEN>=bins[[sp]][b]&sub$PRED_LEN<bins[[sp]][b+1]]<-bins[[sp]][b]
  }
  for(b in 2:(nbins)){
    sub$ageLenbin_prey[sub$PREY_SZ1_CM>=bins[[sp]][b]&sub$PREY_SZ1_CM<bins[[sp]][b+1]]<-ageLenbins[[sp]][b-1]
    sub$Lbin_prey[sub$PREY_SZ1_CM>=bins[[sp]][b]&sub$PREY_SZ1_CM<bins[[sp]][b+1]]<-bins[[sp]][b]
  }
  
  # now get CEATTLE GOA data:
  sub2<-sub%>%
    group_by(REGION,CEATTLE_PRED,CEATTLE_PREY,preyNum,
             ageLenbin_pred,Lbin_pred,ageLenbin_prey,Lbin_prey)%>%
    summarise(
      mnMONTH = mean(MONTH),
      PredL_mn= mean(PRED_LEN,na.rm=T),
      PreyL_mn= mean(PREY_SZ1_CM,na.rm=T),
      sumCNT  = sum(count,na.rm=T),
      TotpreyWt_kg_sum= sum(TotpreyWt_kg,na.rm=T))
  
  
  if (sp==1){
    mnPPL_Diet<-sub2
  }else{
    mnPPL_Diet<-rbind(mnPPL_Diet,sub2)
  }
  
}



mnPPL_Diet<-data.frame(mnPPL_Diet)
save(mnPPL_Diet,file=file.path(mainG,"mnPPL_Diet.Rdata"))


# convert long to wide now - Grant to pick up here 
# todo: fill in key with 0 values 
sp<-1
tmp<-data.frame(mnPPL_Diet[mnPPL_Diet$CEATTLE_PRED==names(bins)[sp],])
tmp$key<-paste(tmp$preyNum,tmp$CEATTLE_PREY,tmp$ageLenbin_prey)
df.w <-  spread( tmp,   key = key  ,   value = TotpreyWt_kg_sum, fill=0   )



# now take prop df.w and multiply by mnDiet to get Uobs






