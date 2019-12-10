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
qamainG  <- "Data/Diet"
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

#############################################
# ATF
#############################################
# Adjust GOAPredPreyL for males
arrowtooth_prey_males <- GOAPredPreyL[which(GOAPredPreyL$CEATTLE_PREY == "Arrowtooth"),]
arrowtooth_prey_males$CEATTLE_PREY <- "ArrowtoothM"

GOAPredPreyL$preyNum[which(GOAPredPreyL$preyNum == 4)] <- 5 # Halibut # to 5 and ATF M to 4
arrowtooth_prey_males$preyNum <- 4


arrowtooth_pred_males <- GOAPredPreyL[which(GOAPredPreyL$CEATTLE_PRED == "Arrowtooth"),]
arrowtooth_pred_males$CEATTLE_PRED <- "ArrowtoothM"


# Adjust females
GOAPredPreyL$CEATTLE_PREY[which(GOAPredPreyL$CEATTLE_PREY == "Arrowtooth")] <- "ArrowtoothF"
GOAPredPreyL$CEATTLE_PRED[which(GOAPredPreyL$CEATTLE_PRED == "Arrowtooth")] <- "ArrowtoothF"

GOAPredPreyL <- rbind(GOAPredPreyL, arrowtooth_prey_males)
GOAPredPreyL <- rbind(GOAPredPreyL, arrowtooth_pred_males)


# Adjust GOA_preyWt for male and female ATF
GOA_preyWt_prey_males <- GOA_preyWt[which(GOA_preyWt$Species_name2 == "Arrowtooth"),]
GOA_preyWt_prey_males$Species_name2 <- "ArrowtoothM"

GOA_preyWt$Species_name2[which(GOA_preyWt$Species_name2 == "Arrowtooth")] <- "ArrowtoothF"

GOA_preyWt <- rbind(GOA_preyWt, GOA_preyWt_prey_males)

######################################
# HALIBUT
######################################
# Adjust GOAPredPreyL for males
halibut_prey_males <- GOAPredPreyL[which(GOAPredPreyL$CEATTLE_PREY == "P. Halibut"),]
halibut_prey_males$CEATTLE_PREY <- "P. HalibutM"
halibut_prey_males$preyNum <- 6


halibut_pred_males <- GOAPredPreyL[which(GOAPredPreyL$CEATTLE_PRED == "P. Halibut"),]
halibut_pred_males$CEATTLE_PRED <- "P. HalibutM"


# Adjust females
GOAPredPreyL$CEATTLE_PREY[which(GOAPredPreyL$CEATTLE_PREY == "P. Halibut")] <- "P. HalibutF"
GOAPredPreyL$CEATTLE_PRED[which(GOAPredPreyL$CEATTLE_PRED == "P. Halibut")] <- "P. HalibutF"

GOAPredPreyL <- rbind(GOAPredPreyL, halibut_prey_males)
GOAPredPreyL <- rbind(GOAPredPreyL, halibut_pred_males)


# Adjust GOA_preyWt for male and female ATF
GOA_preyWt_prey_males <- GOA_preyWt[which(GOA_preyWt$Species_name2 == "P. Halibut"),]
GOA_preyWt_prey_males$Species_name2 <- "P. HalibutM"

GOA_preyWt$Species_name2[which(GOA_preyWt$Species_name2 == "P. Halibut")] <- "P. HalibutF"

GOA_preyWt <- rbind(GOA_preyWt, GOA_preyWt_prey_males)



# Step 1: Find Pred, pred age, prey, prey age preference using GOAPredPreyL
# -- FIXME: Do 1st and 2nd stage expansion
# -- Convert lengths to ages
# -- Take weighted average

# Step 2: Find pred stomach content by %
# -- FIXME: Do 1st and 2nd stage expansion

# create matrix of prey pref based on size bins for each spp:
# _____________________________________
nspp<-6
nages <- c(10,	12,	16, 12, 16, 20)
ageLenbins<-list(
  "W. Pollock"= 1:nages[1] ,
  "P. Cod"= 1:nages[2],
  "ArrowtoothF"= 1:nages[3],
  "ArrowtoothM"= 1:nages[4],
  "P. HalibutF"= 1:nages[5],
  "P. HalibutM"= 1:nages[6])

# Get length bins based on vonB
vonB_params <- matrix(NA, 3, nspp)
rownames(vonB_params) <- c("Linf", "k", "t0")
colnames(vonB_params) <- c("W. Pollock",
                           "P. Cod",
                           "ArrowtoothF",
                           "ArrowtoothM",
                           "P. HalibutF",
                           "P. HalibutM")
vonB_params[,1] <- c(65.2, 0.3, 0) # Pollock
vonB_params[,2] <- c(99.46, 0.1966, -0.11) # Pcod
vonB_params[,3] <- c(66.44564, 0.1535, -0.6253) # Female ATF
vonB_params[,4] <- c(49.48802, 0.2161, -0.7776) # Male ATF

bins<-list()

for(sp in 1:4){
    # Length bins are 0 to length-at-age 1.5, length-at-age 1.5 to 2.5, ..., length-at-age (max age - 0.5) to 1000
    bins[[sp]]<- c(0, vonB_params[1,sp] * (1 -  exp(-vonB_params[2,sp] * (((2:(nages[sp]))) - vonB_params[3,sp]))), ifelse(sp == 4,  49.35173, 1000 ))
}

# Female Halibut - from average weight-at-age converted to length-at-age (1977-2018)
bins[[5]] <- c(0, 20.98414476, 26.05467079, 38.95059085, 48.59624731, 65.45655466, 81.01367555, 84.96023659, 91.1632273, 96.18399758, 101.9296683, 109.759309, 115.4166574, 120.9402115, 124.4322066, 131.5274968, 1000)

# 137.2351486, 140.6212772, 144.592606, 150.7565766, 158.1289786, 145.0375124, 154.7621974, 168.5098466, 160.16937, 166.5282808, 172.1523869, 177.2017792, 181.7709937, 185.9147644

# Male halibut - from average weight-at-age converted to length-at-age (1977-2018)
bins[[6]] <- c(0, 22.46023889, 27.70955325, 38.07012417, 47.2093022, 59.74949349, 71.18448793, 74.64537761, 79.12410539, 83.35769724, 86.21980379, 90.12457879, 91.19153085, 94.27554381, 96.26814735, 98.24412573, 102.0207376, 103.6909081, 104.9241492, 110.9688789, 133.7289825)
 # 111.4698576, 115.7636147, 119.924815, 122.7530789, 110.626339, 116.5099047, 121.5751708, 126.0433758, 130.0621139, 133.7289825, 1000


names(bins)<-names(ageLenbins)

# _____________________________________
# summarize mean prop by prey spp by bins:
# _____________________________________
GOA_preyWt$Lbin<-GOA_preyWt$ageLenbin<-NA
sptable<-tapply(as.character(GOAPredPreyL$CEATTLE_PREY),GOAPredPreyL$preyNum,unique)
spnames<-as.character(lkup[match(sptable,lkup$name1),]$name2)

GOA_preyWt$predNum<-as.numeric(
  factor(GOA_preyWt$Species_name2,levels=c("W. Pollock","P. Cod","ArrowtoothF", "ArrowtoothM", "P. HalibutF", "P. HalibutM")))

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
save(mnDiet,file=file.path(mainG,"mnDiet_halibut.Rdata"))
save(mnDietbyYR,file=file.path(mainG,"mnDietbyYR_halibut.Rdata"))


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
      sumPred = length(unique(PREDJOIN)),
      TotpreyWt_kg_sum= sum(TotpreyWt_kg,na.rm=T))
  
  
  if (sp==1){
    mnPPL_Diet<-sub2
  }else{
    mnPPL_Diet<-rbind(mnPPL_Diet,sub2)
  }
  
}



mnPPL_Diet<-data.frame(mnPPL_Diet)
save(mnPPL_Diet,file=file.path(mainG,"mnPPL_Diet_halibut.Rdata"))


# convert long to wide now - Grant to pick up here 
# todo: fill in key with 0 values 
mnPPL_Diet_ga <- mnPPL_Diet
mnPPL_Diet_ga$TotpreyWt_kg_prop <- NA

for(pred in 1:nspp){
  for(pred_age in 1:nages[pred]){
    for(prey in 1:nspp){
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

mnPPL_Diet_ga$TotpreyWt_kg_prop <- as.numeric(mnPPL_Diet_ga$TotpreyWt_kg_prop)
mnPPL_Diet_ga$TotpreyWt_kg_prop[which(is.na(mnPPL_Diet_ga$TotpreyWt_kg_prop))] <- 0




# Make UObs
mnPPL_Diet_ga$Est_Prey_wt_by_length <- NA
mnPPL_Diet_ga$Est_prop_by_wt_prey <- NA
mn_diet_names <- c("pollock_mn", "pcod_mn", "atf_mn", "atf_mn", "halibut_mn", "halibut_mn")

Uobs <- array(0, dim = c(4, 4, 2, 2, 30, 30))
for(pred in 1:nspp){
  for(pred_age in 1:nages[pred]){
    for(prey in 1:nspp){
      
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
  }
}

write.csv(mnPPL_Diet_ga, file = "mnPPL_Diet_ga_with_halibut.csv")

# Assign to Uobs
for(pred in 1:nspp){
  for(pred_age in 1:nages[pred]){
    for(prey in 1:nspp){
      for(prey_age in 1:nages[prey]){
        
        
        mnPPL_Diet_row <- which(mnPPL_Diet_ga$CEATTLE_PRED == names(bins)[pred] &
                                  mnPPL_Diet_ga$CEATTLE_PREY == names(bins)[prey] &
                                  mnPPL_Diet_ga$ageLenbin_pred == pred_age &
                                  mnPPL_Diet_ga$ageLenbin_prey == prey_age)
        
        # Adjust for ATF males
        prey_sex = 1
        pred_sex = 1
        
        prey_use <- prey
        pred_use <- pred
        
        if(prey == 4){
          prey_use = 3
          prey_sex = 2
        }
        if(pred == 4){
          pred_use = 3
          pred_sex = 2
        }
        
        # Adjust for halibut females
        if(prey == 5){
          prey_use = 4
          prey_sex = 1
        }
        if(pred == 5){
          pred_use = 4
          pred_sex = 1
        }
        
        # Adjust for halibut males
        if(prey == 6){
          prey_use = 4
          prey_sex = 2
        }
        if(pred == 6){
          pred_use = 4
          pred_sex = 2
        }
        
        Uobs[pred_use, prey_use, pred_sex, prey_sex, pred_age, prey_age] <- mnPPL_Diet_ga$Est_prop_by_wt_prey[mnPPL_Diet_row]
        
        # Prey ATF and Halibut plust group
        # Make all ages > 16 have the same diet because missing length bins (similar size so OK)
        if(prey_age == nages[prey] & prey_use %in% c(3,4)){
          Uobs[pred_use, prey_use, pred_sex, prey_sex, pred_age, (nages[prey]+1):30] <- mnPPL_Diet_ga$Est_prop_by_wt_prey[mnPPL_Diet_row]
        }
        
        # Predator ATF and Halibut plust group
        # Make all ages > 16 have the same diet because missing length bins (similar size so OK)
        if(pred_age == nages[pred] & pred_use %in% c(3,4)){
          Uobs[pred_use, prey_use, pred_sex, prey_sex, (nages[pred]+1):30, prey_age] <- mnPPL_Diet_ga$Est_prop_by_wt_prey[mnPPL_Diet_row]
        }
      }
    }
  }
}


save(Uobs, file = "1981_2015_GOA_UobsWtAge_twoSex.Rdata")

# Convert to different format
nsex <- c(1,1,2,2)
nages <- c(10,12,21,30)
uobsdf <- data.frame(matrix(NA, nrow = 10000, ncol = 9))
ind = 1

for(pred in 1:4){
  for(prey in 1:4){
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
write.csv(uobsdf, file = "1981_2015_GOA_w_halibut_uobsdf_2sex.csv")



