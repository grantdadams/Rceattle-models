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
  
  load(file.path(mainG,"GOAPredPreyL.Rdata")) # Subset of 100 fish per year of prey lengths
  # PRED_LEN is in cm
  # PREY_SZ1_CM is prey length in cm
  load(file.path(mainG,"GOA_preyWt.Rdata")) # Gravimetric composition
  # C1 is daily ration
  # obsC is C1 / weight of predator
  # Species_name is the predator
  # Strata is the AFSC strata
  # FIXME: biomass weight this by strata
  load(file.path(mainG,"lkup.Rdata"))
  
}

GOAPredPreyL$CEATTLE_PREY <- as.character(GOAPredPreyL$CEATTLE_PREY)
GOAPredPreyL$CEATTLE_PRED <- as.character(GOAPredPreyL$CEATTLE_PRED)

# Adjust GOAPredPreyL for males
arrowtooth_prey <- GOAPredPreyL[which(GOAPredPreyL$CEATTLE_PREY == "Arrowtooth"),]
arrowtooth_prey$CEATTLE_PREY <- "ArrowtoothM"
GOAPredPreyL$preyNum[which(GOAPredPreyL$preyNum == 4)] <- 5
arrowtooth_prey$preyNum <- 4


arrowtooth_pred <- GOAPredPreyL[which(GOAPredPreyL$CEATTLE_PRED == "Arrowtooth"),]
arrowtooth_pred$CEATTLE_PRED <- "ArrowtoothM"


# Adjust females
GOAPredPreyL$CEATTLE_PREY[which(GOAPredPreyL$CEATTLE_PREY == "Arrowtooth")] <- "ArrowtoothF"
GOAPredPreyL$CEATTLE_PRED[which(GOAPredPreyL$CEATTLE_PRED == "Arrowtooth")] <- "ArrowtoothF"

GOAPredPreyL <- rbind(GOAPredPreyL, arrowtooth_prey)
GOAPredPreyL <- rbind(GOAPredPreyL, arrowtooth_pred)


# Adjust GOA_preyWt for male and female ATF
GOA_preyWt_prey <- GOA_preyWt[which(GOA_preyWt$Species_name2 == "Arrowtooth"),]
GOA_preyWt_prey$Species_name2 <- "ArrowtoothM"

GOA_preyWt$Species_name2[which(GOA_preyWt$Species_name2 == "Arrowtooth")] <- "ArrowtoothF"
GOA_preyWt <- rbind(GOA_preyWt, GOA_preyWt_prey)

# Step 1: Find Pred, pred age, prey, prey age preference using GOAPredPreyL
# -- FIXME: Do 1st and 2nd stage expansion
# -- Convert lengths to ages
# -- Take weighted average

# Step 2: Find pred stomach content by %
# -- FIXME: Do 1st and 2nd stage expansion

# create matrix of prey pref based on size bins for each spp:
# _____________________________________
nspp<-5
nages <- c(10,	12,	16, 16, 21)
ageLenbins<-list(
  "W. Pollock"= 1:10 ,
  "P. Cod"= 1:12,
  "ArrowtoothF"= 1:16,
  "ArrowtoothM"= 1:16,
  "P. Halibut"= 1:21)

# Get length bins based on vonB
vonB_params <- matrix(NA, 3, nspp)
rownames(vonB_params) <- c("Linf", "k", "t0")
colnames(vonB_params) <- c("W. Pollock",
                           "P. Cod",
                           "ArrowtoothF",
                           "ArrowtoothM",
                           "P. Halibut")
vonB_params[,1] <- c(65.2, 0.3, 0) # Pollock
vonB_params[,2] <- c(99.46, 0.1966, -0.11) # Pcod
vonB_params[,3] <- c(66.44564, 0.1535, -0.6253) # Female ATF
vonB_params[,4] <- c(49.48802, 0.2161, -0.7776) # Male ATF
vonB_params[,5] <- c(66.44564, 0.1535, -0.6253) # Female ATF FIXME

bins<-list()

for(sp in 1:nspp){
  for(age in 1:nages[sp]){
    # Length bins are 0 to length-at-age 1.5, length-at-age 1.5 to 2.5, ..., length-at-age (max age - 0.5) to 1000
    bins[[sp]]<- c(0, vonB_params[1,sp] * (1 -  exp(-vonB_params[2,sp] * ((1:(nages[sp] - 1)) - vonB_params[3,sp]))), 1000)
  }
}
names(bins)<-names(ageLenbins)

# _____________________________________
# summarize mean prop by prey spp by bins:
# _____________________________________
GOA_preyWt$Lbin<-GOA_preyWt$ageLenbin<-NA
sptable<-tapply(as.character(GOAPredPreyL$CEATTLE_PREY),GOAPredPreyL$preyNum,unique)
spnames<-as.character(lkup[match(sptable,lkup$name1),]$name2)

GOA_preyWt$predNum<-as.numeric(
  factor(GOA_preyWt$Species_name2,levels=c("W. Pollock","P. Cod","ArrowtoothF", "P. Halibut", "ArrowtoothM")))

sptable2<-tapply(as.character(GOA_preyWt$Species_name2),GOA_preyWt$predNum,unique)
spnames2<-as.character(lkup[match(sptable2,lkup$name1),]$name2)

for(sp in 1:nspp){
  sub<-as_tibble(GOA_preyWt[GOA_preyWt$Species_name2==names(bins)[sp],])
  nbins<-length(bins[[sp]])
  #sub$Lbin<-bins[[sp]][nbins]
  for(b in 1:(nbins-1)){
    sub$ageLenbin[sub$PredL>=bins[[sp]][b]&sub$PredL<bins[[sp]][b+1]]<-ageLenbins[[sp]][b]
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
  for(b in 1:(nbins-1)){
    sub$ageLenbin_pred[sub$PRED_LEN>=bins[[sp]][b]&sub$PRED_LEN<bins[[sp]][b+1]]<-ageLenbins[[sp]][b]
    sub$Lbin_pred[sub$PRED_LEN>=bins[[sp]][b]&sub$PRED_LEN<bins[[sp]][b+1]]<-bins[[sp]][b]
  }
  for(b in 1:(nbins-1)){
    sub$ageLenbin_prey[sub$PREY_SZ1_CM>=bins[[sp]][b]&sub$PREY_SZ1_CM<bins[[sp]][b+1]]<-ageLenbins[[sp]][b]
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
mnPPL_Diet_ga <- mnPPL_Diet
mnPPL_Diet_ga$TotpreyWt_kg_prop <- NA

for(pred in 1:4){
  for(pred_age in 1:nages[pred]){
    for(prey in 1:4){
      for(prey_age in 1:nages[prey]){
        # See if data exits
        sub <- mnPPL_Diet[which(mnPPL_Diet$CEATTLE_PRED == names(bins)[pred] &
                                  mnPPL_Diet$CEATTLE_PREY == names(bins)[prey] &
                                  mnPPL_Diet$ageLenbin_pred == pred_age &
                                  mnPPL_Diet$ageLenbin_prey == prey_age),]
        
        # Add rows if it does not
        if(nrow(sub) == 0){
          # Colnames =  REGION, CEATTLE_PRED, CEATTLE_PREY ,preyNum, ageLenbin_pred, Lbin_pred, ageLenbin_prey, Lbin_prey, mnMONTH, PredL_mn, PreyL_mn, sumCNT, TotpreyWt_kg_sum
          added_vals <- c("GOA", names(bins)[pred], names(bins)[prey], prey, pred_age, bins[[pred]][pred_age], prey_age, bins[[prey]][prey_age], -999, -999, -999, 0, 0, 0)
          mnPPL_Diet_ga <- rbind(mnPPL_Diet_ga, added_vals)
        }
      }
      # Turn weight of prey-at-age in pred-at-age to % composition by weight
      pred_at_age_wt <- (as.numeric(mnPPL_Diet_ga$TotpreyWt_kg_sum[which(mnPPL_Diet_ga$CEATTLE_PRED == names(bins)[pred] &
                                                                           mnPPL_Diet_ga$CEATTLE_PREY == names(bins)[prey] &
                                                                           mnPPL_Diet_ga$ageLenbin_pred == pred_age)]))
      pred_at_age_wt_sum <- sum(pred_at_age_wt)
      if(pred_at_age_wt_sum > 0){
        pred_at_age_wt_prop <- pred_at_age_wt / pred_at_age_wt_sum
      }
      mnPPL_Diet_ga$TotpreyWt_kg_prop[which(mnPPL_Diet_ga$CEATTLE_PRED == names(bins)[pred] &
                                              mnPPL_Diet_ga$CEATTLE_PREY == names(bins)[prey] &
                                              mnPPL_Diet_ga$ageLenbin_pred == pred_age)] = pred_at_age_wt_prop
    }
  }
}

# Make UObs
mnPPL_Diet_ga$Est_Prey_wt_by_length <- NA
mnPPL_Diet_ga$Est_prop_by_wt_prey <- NA
mn_diet_names <- c("pollock_mn", "pcod_mn", "atf_mn", "atf_mn")
Uobs <- array(0, dim = c(3, 3, 2, 2, 21, 21))
for(pred in 1:4){
  for(pred_age in 1:nages[pred]){
    for(prey in 1:4){
      
      # Subset proportion of prey length in pred-at-length
      mnPPL_Diet_rows <- which(mnPPL_Diet_ga$CEATTLE_PRED == names(bins)[pred] &
                                 mnPPL_Diet_ga$CEATTLE_PREY == names(bins)[prey] &
                                 mnPPL_Diet_ga$ageLenbin_pred == pred_age)
      
      # Find weight of prey in pred-at-length
      mnDiet_sub_row <- which(mnDiet$predNum == pred &
                                mnDiet$ageLenbin == pred_age)
      
      # Get weight of prey-at-length in pred-at-length
      mnPPL_Diet_ga$Est_Prey_wt_by_length[mnPPL_Diet_rows] <- as.numeric(mnPPL_Diet_ga$TotpreyWt_kg_prop[mnPPL_Diet_rows]) * mnDiet[mnDiet_sub_row, mn_diet_names[prey]] * mnDiet$Obs_TWT_mn[mnDiet_sub_row]
    }
    # Normalize for all prey/pre-at-length
    # Subset proportion of prey length in pred-at-length
    mnPPL_Diet_rows <- which(mnPPL_Diet_ga$CEATTLE_PRED == names(bins)[pred] &
                               mnPPL_Diet_ga$ageLenbin_pred == pred_age)
    
    # Normalize across all prey/prey-at-length
    if(mnDiet$Obs_TWT_mn[mnDiet_sub_row] > 0){
      mnPPL_Diet_ga$Est_prop_by_wt_prey[mnPPL_Diet_rows] <- mnPPL_Diet_ga$Est_Prey_wt_by_length[mnPPL_Diet_rows] / mnDiet$Obs_TWT_mn[mnDiet_sub_row]
    }
    
    # Assign to Uobs
    for(prey in 1:4){
      for(prey_age in 1:nages[prey]){
        mnPPL_Diet_row <- which(mnPPL_Diet_ga$CEATTLE_PRED == names(bins)[pred] &
                                  mnPPL_Diet_ga$CEATTLE_PREY == names(bins)[prey] &
                                  mnPPL_Diet_ga$ageLenbin_pred == pred_age &
                                  mnPPL_Diet_ga$ageLenbin_prey == prey_age)
        
        prey_sex = 1
        if(prey == 4){
          prey = 3
          prey_sex = 2
        }
        
        pred_sex = 1
        if(pred == 4){
          pred = 3
          pred_sex = 2
        }
        
        Uobs[pred, prey, pred_sex, prey_sex, pred_age, prey_age] <- mnPPL_Diet_ga$Est_prop_by_wt_prey[mnPPL_Diet_row]
        
        # Prey ATF plust group
        # Make all ages > 16 have the same diet because missing length bins (similar size so OK)
        if(prey_age == 16 & prey == 3){
          Uobs[pred, 3, pred_sex, prey_sex, pred_age, 17:21] <- mnPPL_Diet_ga$Est_prop_by_wt_prey[mnPPL_Diet_row]
        }
        
        # Predator ATF plust group
        # Make all ages > 16 have the same diet because missing length bins (similar size so OK)
        if(pred_age == 16 & pred == 3){
          Uobs[3, prey, pred_sex, prey_sex, 17:21, prey_age] <- mnPPL_Diet_ga$Est_prop_by_wt_prey[mnPPL_Diet_row]
        }
      }
    }
  }
}


save(Uobs, file = "1981_2015_GOA_UobsWtAge_twoSex.Rdata")

# Convert to different format
nsex <- c(1,1,2)
nages <- c(10,12,21,21)
uobsdf <- data.frame(matrix(NA, nrow = 5000, ncol = 9))
ind = 1

for(pred in 1:3){
  for(prey in 1:3){
    for(pred_sex in 1:nsex[pred]){
      for(prey_sex in 1:nsex[prey]){ 
        for(pred_age in 1:nages[pred]){
          for(prey_age in 1:nages[prey]){
            
            uobsdf[ind, 1] <- pred
            uobsdf[ind, 2] <- prey
            uobsdf[ind, 3] <- pred_sex
            uobsdf[ind, 4] <- prey_sex
            uobsdf[ind, 5] <- pred_age
            uobsdf[ind, 6] <- prey_age
            uobsdf[ind, 7] <- 0
            uobsdf[ind, 8] <- 20
            uobsdf[ind, 9] <- Uobs[pred, prey, pred_sex, prey_sex, pred_age, prey_age]
            ind <- ind + 1
          }
        }
      }
    }
  }
}

uobsdf <- uobsdf[complete.cases(uobsdf),]
colnames(uobsdf) <- c("Pred", "Prey", "Pred_sex", "Prey_sex", "Pred_age", "Prey_age", "Year", 
                      "Sample_size", "Stomach_proportion_by_weight")
write.csv(uobsdf, file = "1981_2015_GOA_uobsdf_2sex.csv")



