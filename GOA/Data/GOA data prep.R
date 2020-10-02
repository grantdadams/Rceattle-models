library(Rceattle)
library(r4ss)
library(plyr)

data(BS2017SS) # ?BS2017SS for more information on the data 
GOA2018SS <- BS2017SS

###########################################################
# READ IN DATA
###########################################################
# PCOD
pcod_dat <- r4ss::SS_readdat_3.30(file = "Data/data-raw/GOA/Pcod 2018/data2018.dat")
pcod_report <- r4ss::SS_output(dir = "Data/Pcod 2018/GOApcod_Appendix2.3/2018 GOA Pacific cod figures and Files/Model18.09.35/")


##################################################
# GENERAL SPECIFICATIONS
##################################################
# styr
GOA2018SS$styr[1] <- 1970 # 1961 is the start year for ATF
GOA2018SS$styr[2] <- 1977 # 1977 is the start year for cod
GOA2018SS$styr[3] <- 1961 # 1961 is the start year for ATF
#FIXME - check with other species

# endyr
GOA2018SS$endyr <- 2018 # 2018 is the end year for cod, ATF
#FIXME - check with other species

# nspp
GOA2018SS$nspp <- 3 #FIXME add in halibut later

# nyrs
GOA2018SS$nyrs <- length(GOA2018SS$styr : GOA2018SS$endyr)

# Nages
GOA2018SS$nages[1] <- 10 #COD
GOA2018SS$nages[2] <- pcod_dat$Nages #COD
GOA2018SS$nages[3] <- 21 #ATF


# stom_tau
#FIXME do not change


##################################################
# FISHERY DATA
##################################################
# Clear data
GOA2018SS$fsh_comp <- GOA2018SS$fsh_comp[-c(1:nrow(GOA2018SS$fsh_comp)),]

#---------------------------------------------
# fsh_control

# Pcod
# 3 fisheries
pcod_fsh_control <- GOA2018SS$fsh_control
pcod_fsh_control
pcod_fsh_control$Fishery_name <- c("Pcod_trawl", "Pcod_longline", "Pcod_pot") # in datfile Trawl = 1, LL = 2, pot = 3
pcod_fsh_control$Fishery_code <- c(1:3) # 1:3 fishery name
pcod_fsh_control$Species <- rep(2, 3) # Setting 2 = cod
pcod_fsh_control$Selectivity <- rep(3, 3) # Using double-logistic for everything
pcod_fsh_control$Nselages <- rep(NA, 3) # Can set to NA because not using non-parametric selectivity
pcod_fsh_control$Comp_type <- c(1, 1, 1) # Using age comp because there is more data
pcod_fsh_control$Comp_N_bins <- rep(20, 3)# 20 ages for age comp
pcod_fsh_control$Catch_units <- rep(1, 3) # Catch is weight

GOA2018SS$fsh_control <- pcod_fsh_control


# Pollock
pollock_fsh_control <- readxl::read_excel()

#---------------------------------------------
# fsh_emp_sel

# Pollock has none
# PCOD has none

#---------------------------------------------
# fsh_biom
head(GOA2018SS$fsh_biom)

# PCod
pcod_dat$fleetinfo
pcod_fsh_biom <- pcod_dat$catch
pcod_fsh_biom <- pcod_fsh_biom[which(pcod_fsh_biom$V1 > 0),]

# re-order for CEATTLE
pcod_fsh_biom <- data.frame( Fishery_name = rep(NA, nrow(pcod_fsh_biom)),
                             Fishery_code = as.numeric(pcod_fsh_biom$V3) + 1,
                             Species = rep(2, nrow(pcod_fsh_biom)),
                             Sex = rep(0, nrow(pcod_fsh_biom)),
                             Year = as.numeric(pcod_fsh_biom$V1),
                             Month = as.numeric(pcod_fsh_biom$V2),
                             Observation = as.numeric(pcod_fsh_biom$V4) * 1000, # convert tons to kg
                             CV = as.numeric(pcod_fsh_biom$V5))
# Change survey name
pcod_fsh_biom$Fishery_name[which(pcod_fsh_biom$Fishery_code == 2)] <- "Pcod_trawl" 
pcod_fsh_biom$Fishery_name[which(pcod_fsh_biom$Fishery_code == 3)] <- "Pcod_longline" 
pcod_fsh_biom$Fishery_name[which(pcod_fsh_biom$Fishery_code == 4)] <- "Pcod_pot"

GOA2018SS$fsh_biom <- pcod_fsh_biom



#---------------------------------------------
# fsh_comp

# Pcod length comp
# BT(4) is age, LL(5) is length
pcod_dat$fleetinfo
pcod_fsh_age_comp <- pcod_dat$lencomp
pcod_fsh_age_comp <- pcod_fsh_age_comp[which(pcod_fsh_age_comp$FltSvy %in% c(1:3)),] # Get the ones the trawl(1), LL (2), pot(3)

# Get info
pcod_fsh_comp <- 
  data.frame( Fishery_name = rep(NA, nrow(pcod_fsh_age_comp)),
              Fishery_code = as.numeric(pcod_fsh_age_comp$FltSvy)+1,
              Species = rep(2, nrow(pcod_fsh_age_comp)),
              Sex = rep(0, nrow(pcod_fsh_age_comp)),
              Age0_Length1 = rep(1, nrow(pcod_fsh_age_comp)),
              Year = as.numeric(pcod_fsh_age_comp$Yr),
              Month = as.numeric(pcod_fsh_age_comp$Seas),
              Sample_size = as.numeric(pcod_fsh_age_comp$Nsamp))

# Change survey name
pcod_fsh_comp$Fishery_name[which(pcod_fsh_comp$Fishery_code == 2)] <- "Pcod_trawl" 
pcod_fsh_comp$Fishery_name[which(pcod_fsh_comp$Fishery_code == 3)] <- "Pcod_longline" 
pcod_fsh_comp$Fishery_name[which(pcod_fsh_comp$Fishery_code == 4)] <- "Pcod_pot"

# get comp
pcod_comp <- pcod_fsh_age_comp[,7:ncol(pcod_fsh_age_comp)]
pcod_comp[] <- lapply(pcod_comp, function(x) as.numeric(as.character(x)))
colnames(pcod_comp) <- paste0("Comp_", 1:ncol(pcod_comp))

# combine
pcod_fsh_comp <- cbind(pcod_fsh_comp, pcod_comp)
GOA2018SS$fsh_comp <- rbind.fill(GOA2018SS$fsh_comp, pcod_fsh_comp)




##################################################
# SURVEY DATA
##################################################
# rearange arrays
GOA2018SS$srv_emp_sel  <- GOA2018SS$srv_emp_sel[-c(1:nrow(GOA2018SS$srv_emp_sel)),]
GOA2018SS$srv_comp <- GOA2018SS$srv_comp[-c(1:nrow(GOA2018SS$srv_comp)),]


# srv_control #FltSRV 4 = BT Trawl, 5 = LL SRV
pcod_srv_control <- GOA2018SS$srv_control
pcod_srv_control <- pcod_srv_control[-c(2,4),]
pcod_srv_control$Survey_name <- c("Pcod_bt_survey", "Pcod_ll_survey")
pcod_srv_control$Survey_code <- c(1:2) # 1:2 Survey name
pcod_srv_control$Species <- rep(2, 2) # Setting 2 = cod
pcod_srv_control$Selectivity <- rep(3, 2) # Using double-logistic for everything
pcod_srv_control$Nselages <- rep(NA, 2) # Can set to NA because not using non-parametric selectivity
pcod_srv_control$Comp_type <- c(1, 1) # Using age comp because there is more data
pcod_srv_control$Comp_N_bins <- rep(20, 2)# 20 ages for age comp
pcod_srv_control$Catch_units <- rep(2, 2) # Survey is numbers

GOA2018SS$srv_control <- pcod_srv_control



#---------------------------------------------
# srv_emp_sel
# PCOD has none



#---------------------------------------------
# srv_biom
head(GOA2018SS$srv_biom)

# PCod
pcod_dat$fleetinfo
pcod_srv_biom <- pcod_dat$CPUE
pcod_srv_biom <- pcod_srv_biom[which(pcod_srv_biom$index %in% c(4,5)),] # Get the ones the BT and longline

# re-order for CEATTLE
pcod_srv_biom <- data.frame( Survey_name = rep(NA, nrow(pcod_srv_biom)),
                             Survey_code = as.numeric(pcod_srv_biom$index) -3,
                             Species = rep(2, nrow(pcod_srv_biom)),
                             Year = pcod_srv_biom$year,
                             Month = pcod_srv_biom$seas,
                             Observation = pcod_srv_biom$obs,
                             CV = pcod_srv_biom$se_log)
# Change survey name
pcod_srv_biom$Survey_name[which(pcod_srv_biom$Survey_code == 1)] <- "Pcod_bt_survey"
pcod_srv_biom$Survey_name[which(pcod_srv_biom$Survey_code == 2)] <- "Pcod_ll_survey"

GOA2018SS$srv_biom <- pcod_srv_biom


#---------------------------------------------
# srv_comp

# Pcod age comp
# BT(4) is age, LL(5) is length
pcod_dat$fleetinfo
pcod_srv_age_comp <- pcod_dat$agecomp
pcod_srv_age_comp <- pcod_srv_age_comp[which(pcod_srv_age_comp$FltSvy %in% c(-4,5)),] # Get the ones the BT and longline

# Get info
pcod_srv_comp <-
  data.frame( Survey_name = rep(NA, nrow(pcod_srv_age_comp)),  
                                 Survey_code = -1 * as.numeric(pcod_srv_age_comp$FltSvy) + 1,
                                 Species = rep(2, nrow(pcod_srv_age_comp)),
                                 Year = as.numeric(pcod_srv_age_comp$Yr),
                                 Month = as.numeric(pcod_srv_age_comp$Seas),
              Sample_size = as.numeric(pcod_srv_age_comp$Nsamp))

# Change survey name
pcod_srv_comp$Survey_name[which(pcod_srv_comp$Survey_code == 5)] <- "Pcod_bt_survey"
pcod_srv_comp$Survey_name[which(pcod_srv_comp$Survey_code == 6)] <- "Pcod_ll_survey"

# get comp
pcod_comp <- pcod_srv_age_comp[,which(colnames(pcod_srv_age_comp) %in% paste0("a", 1:100))]
colnames(pcod_comp) <- paste0("Comp_", 1:ncol(pcod_comp))

# combine
pcod_srv_comp <- cbind(pcod_srv_comp, pcod_comp)
GOA2018SS$srv_comp <- rbind.fill(GOA2018SS$srv_comp, pcod_srv_comp)

write.csv(pcod_srv_comp, file = "pcod_srv_comp.csv")

# Pcod length comp
# BT(4) is age, LL(5) is length
pcod_dat$fleetinfo
pcod_srv_age_comp <- pcod_dat$lencomp
pcod_srv_age_comp <- pcod_srv_age_comp[which(pcod_srv_age_comp$FltSvy%in% c(4,5)),] # Get the ones the BT and longline

# Get info
pcod_srv_comp <- 
  data.frame( Survey_name = rep(NA, nrow(pcod_srv_age_comp)),
              Survey_code = as.numeric(pcod_srv_age_comp$FltSvy) -3,
              Species = rep(2, nrow(pcod_srv_age_comp)),
              Year = as.numeric(pcod_srv_age_comp$Yr),
              Month = as.numeric(pcod_srv_age_comp$Seas),
              Sample_size = as.numeric(pcod_srv_age_comp$Nsamp))

# Change survey name
pcod_srv_comp$Survey_name[which(pcod_srv_comp$Survey_code == 1)] <- "Pcod_bt_survey"
pcod_srv_comp$Survey_name[which(pcod_srv_comp$Survey_code == 2)] <- "Pcod_ll_survey"

# get comp
pcod_comp <- pcod_srv_age_comp[,7:ncol(pcod_srv_age_comp)]
pcod_comp[] <- lapply(pcod_comp, function(x) as.numeric(as.character(x)))
colnames(pcod_comp) <- paste0("Comp_", 1:ncol(pcod_comp))

# combine
pcod_srv_comp <- cbind(pcod_srv_comp, pcod_comp)
GOA2018SS$srv_comp <- rbind.fill(GOA2018SS$srv_comp, pcod_srv_comp)



##################################################
# AGE AND GROWTH
##################################################
# rearange arrays
GOA2018SS$wt <- array(NA, dim = c(GOA2018SS$nyrs, max(GOA2018SS$nages), GOA2018SS$nspp) )
GOA2018SS$Mn_LatAge <- matrix(NA, nrow = GOA2018SS$nspp, ncol = max(GOA2018SS$nages))
GOA2018SS$pmature <- matrix(NA, nrow = GOA2018SS$nspp, ncol = max(GOA2018SS$nages))
GOA2018SS$propF <- matrix(NA, nrow = GOA2018SS$nspp, ncol = max(GOA2018SS$nages))
GOA2018SS$aLW <- matrix(NA, nrow = 2, ncol = GOA2018SS$nspp)

#---------------------------------------------
# age_trans_matrix 
# FIXME ask steve about this...


#---------------------------------------------
# wt
# FIXME: Ask steve about empirical weight-at-age
# using von Bert parameters and weight-at-length parameters from:
# Barbeaux, S., Aydin, K., Fissel, B., Holsman, K., Laurel, B., and Palsson, W. 2018. Chapter 2 : Assessment of the Pacific cod stock in the Gulf of Alaska.
# Von bertalanffy parameters from pg 23 = 
pcod_Linf = 99.46; pcod_k = 0.1966; pcod_t0 = -0.11 # Length is cm
# Weight-at-length parameters from pg 23: 
pcod_alpha = 5.631*10^(-6); pcod_beta = 3.1306 # weight is kg
pcod_ages <- 1:GOA2018SS$nages[2]

# Fill in growth
for(i in 1:nrow(GOA2018SS$wt[,,2])){
  GOA2018SS$wt[i,pcod_ages, 2] <- pcod_alpha * (pcod_Linf * (1 -  exp(-pcod_k * (pcod_ages - pcod_t0)))) ^ pcod_beta
}


#---------------------------------------------
# "aLW"  
# Used for time-varying length-based suitability estimation

# Pcod
GOA2018SS$aLW[,2] <- c(pcod_alpha, pcod_beta)


#---------------------------------------------
# "Mn_LatAge"

# Pollock
pllock_linf = 65.2 #cm
pollock_k <- 0.3
GOA2018SS$Mn_LatAge[1,1:10] <- (pllock_linf * (1 -  exp(-pollock_k * (1:10))))

# Pcod
GOA2018SS$Mn_LatAge[2,pcod_ages] <- (pcod_Linf * (1 -  exp(-pcod_k * (pcod_ages - pcod_t0))))


#---------------------------------------------
# "pmature" 

# Pcod
# Pcod uses a length based maturity schedule with the following parameters
# Pcod also mature at a smaller size in the GOA
pcod_l50 = 53.7 #cm #  pg. 24
pcod_slope = -0.27365 # Slope of logistic eq pg. 24
pcod_intercept = -pcod_l50 * pcod_slope # L50 = - intercept/slope
curve(1/(1+exp(-(pcod_intercept + pcod_slope * x))), from = 0 , to = 120) # This looks off
curve(1/(1+exp((pcod_intercept + pcod_slope * x))), from = 0 , to = 120) # This is reasonable

GOA2018SS$pmature[2,pcod_ages] <- 1 / (1 + exp( pcod_intercept + pcod_slope * GOA2018SS$Mn_LatAge[2,pcod_ages]))

# Pollock 
GOA2018SS$pmature[1,1:10] <- c(0.00000,	0.00043,	0.02158,	0.29393,	0.59592,	0.84358,	0.92750,	0.96872,	0.98761,	0.99314)


#---------------------------------------------
# "propF"  

# Pcod
GOA2018SS$propF[2,pcod_ages] <- 0.5 #FIXME, ask steve about this

##################################################
# STOMACH DATA
##################################################
# "Uobs"             
#FIXME - extract from diet data


# "UobsWt"           
#FIXME - extract from diet data


# "UobsAge"          
#FIXME - extract from diet data


# "UobsWtAge"       
#FIXME - extract from diet data

# "other_food" 
# Don't change for now, makes little impact

##################################################
# ENVIRONMENTAL
##################################################
# "nTyrs" 
GOA2018SS$nTyrs <- GOA2018SS$nyrs
#FIXME check with other species


# "Tyrs"
# Taking from CFSR bottom temperature
# FIXME - talk to K

# "BTempC"          
# FIXME - talk to K



##################################################
# BIOENERGETICS
##################################################
# "C_model"          
# Dont change use the Wisconsin model
GOA2018SS$C_model

# "fday" 
#FIXME - get from K

# "Pyrs" 
#FIXME - get from K

# "Pvalue"           
# FIXME - talk to K

# "Ceq"              
# Dont change use the Thornton and Lessem

# "CA"               
# FIXME - talk to K

# "CB"              
# FIXME - talk to K

# "Qc"               
# FIXME - talk to K

# "Tco"             
# FIXME - talk to K

# "Tcm"              
# FIXME - talk to K

# "Tcl"              
# FIXME - talk to K

# "CK1"              
# FIXME - talk to K

# "CK4"
# FIXME - talk to K


# "S_a"              
# Not used


##################################################
# MORTALITY
##################################################         

# "M1_base"         
# FIXME - talk to K
       

# ALK
library(r4ss)
output <- SS_output(dir = "C:/Users/Grant Adams/Documents/GitHub/RceattleRuns/GOA/Pcod 2018/GOApcod_Appendix2.3/2018 GOA Pacific cod figures and Files/Model18.10.44")
SSplotAgeMatrix(output)

ALK <- output$ALK
dim(ALK)
alk_list <- list()
rotate <- function(x) t(apply(x, 2, rev))
alk_list[[1]] <- as.data.frame(rotate(ALK[,,1]))
alk_list[[2]] <- as.data.frame(rotate(ALK[,,2]))
alk_list[[1]] <- cbind(rownames(alk_list[[1]]), alk_list[[1]])
alk_list[[2]] <- cbind(rownames(alk_list[[2]]), alk_list[[2]])

library(writexl)
write_xlsx(alk_list, path = "GOA/Pcod 2018/PCod_ALK.xlsx")
