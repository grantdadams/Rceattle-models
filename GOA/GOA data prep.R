library(Rceattle)
library(r4ss)

data(BS2017SS) # ?BS2017SS for more information on the data 
GOA2018SS <- BS2017SS

###########################################################
# PCod
###########################################################
# Read in data
pcod_dat <- r4ss::SS_readdat_3.30(file = "Data/data-raw/GOA/Pcod 2018/data2018.dat")


# styr
GOA2018SS$styr <- pcod_dat$styr # 1977 is the start year for cod
#FIXME - check with other species

# endyr
GOA2018SS$endyr <- pcod_dat$endyr # 2018 is the start year for cod 
#FIXME - check with other species

# nspp
GOA2018SS$nspp <- 3 #FIXME add in halibut later

# nyrs
GOA2018SS$nyrs <- length(GOA2018SS$styr : GOA2018SS$endyr)

# Nages
GOA2018SS$nages[2] <- pcod_dat$Nages


# stom_tau
#FIXME do not change


# fsh_control
# 3 fisheries
pcod_fsh_control <- GOA2018SS$fsh_control
pcod_fsh_control
pcod_fsh_control$Fishery_name <- c("Pcod_longline", "Pcod_pot", "Pcod_trawl")
pcod_fsh_control$Fishery_code <- c(1:3) # 1:3 fishery name
pcod_fsh_control$Species <- rep(2, 3) # Setting 2 = cod
pcod_fsh_control$Selectivity <- rep(3, 3) # Using double-logistic for everything
pcod_fsh_control$Nselages <- rep(NA, 3) # Can set to NA because not using non-parametric selectivity
pcod_fsh_control$Comp_type <- c(1, 1, 1) # Using age comp because there is more data
pcod_fsh_control$Comp_N_bins <- rep(20, 3)# 20 ages for age comp
pcod_fsh_control$Catch_units <- rep(1, 3) # Catch is weight

GOA2018SS$fsh_control <- pcod_fsh_control


# srv_control
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


# age_trans_matrix 
# FIXME ask steve about this...


# wt
GOA2018SS$wt <- array(NA, dim = c(GOA2018SS$nyrs, max(GOA2018SS$nages), GOA2018SS$nspp) )
# FIXME: Ask steve about empirical weight-at-age
# using von Bert parameters and weight-at-length parameters from:
# Barbeaux, S., Aydin, K., Fissel, B., Holsman, K., Laurel, B., and Palsson, W. 2018. Chapter 2 : Assessment of the Pacific cod stock in the Gulf of Alaska.
# Von bertalanffy parameters from pg 23 = 
pcod_Linf = 99.46; pcod_k = 0.1966; pcod_t0 = -0.11
# Weight-at-length parameters from pg 23: 
pcod_alpha = 5.631*10^(-6); pcod_beta = 3.1306
pcod_ages <- 1:GOA2018SS$nages[2]

# Fill in growth
for(i in 1:nrow(GOA2018SS$wt[,,2])){
  GOA2018SS$wt[i,pcod_ages, 2] <- pcod_alpha * (pcod_Linf * (1 -  exp(-pcod_k * (pcod_ages - pcod_t0)))) ^ pcod_beta
}


# "fday" 
#FIXME - get from K


# "Pyrs" 
#FIXME - get from K


# "Uobs"             
#FIXME - extract from diet data


# "UobsWt"           
#FIXME - extract from diet data


# "UobsAge"          
#FIXME - extract from diet data


# "UobsWtAge"       
#FIXME - extract from diet data


# "Mn_LatAge"
GOA2018SS$Mn_LatAge <- matrix(NA, nrow = GOA2018SS$nspp, ncol = max(GOA2018SS$nages))
GOA2018SS$Mn_LatAge[2,pcod_ages] <- (pcod_Linf * (1 -  exp(-pcod_k * (pcod_ages - pcod_t0))))


# "nTyrs"           


# "Tyrs"             


# "BTempC"          


# "other_food"       


# "C_model"          


# "Pvalue"           


# "Ceq"              


# "CA"               


# "CB"              


# "Qc"               


# "Tco"             


# "Tcm"              


# "Tcl"              


# "CK1"              


# "CK4"


# "S_a"              


# "aLW"              


# "M1_base"         


# "propF"           


# "pmature"          


# fsh_emp_sel

# srv_emp_sel

# fsh_comp

# srv_comp

# fsh_biom

# srv_biom
