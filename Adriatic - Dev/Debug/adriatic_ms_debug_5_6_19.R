
library(Rceattle)

################################################
# Data
################################################
# Example
# To run the 2017 single species assessment for the Bering Sea, a data file must first be loaded:
# data(BS2017SS) # ?BS2017SS for more information on the data
# Write data to excel
# Rceattle::write_excel(data_list = BS2017SS, file = "BS2017SS.xlsx")


# Read the data in
adriatic_data <- Rceattle::read_data(file = "Adriatic_v9.xlsx")


################################################
# Estimation
################################################
inits <- build_params(adriatic_data)
inits$ln_mn_rec <- c(9,9,9)

# Then the model can be fit by setting `msmMode = 0` using the `Rceattle` function:
ss_run <- Rceattle::fit_mod(data_list = adriatic_data,
                            inits = NULL, # Initial parameters = 0
                            file = NULL, # Don't save
                            debug = 0, # Estimate. Set to 1 to not estimate.
                            random_rec = FALSE, # No random recruitment
                            msmMode = 0, # Single species mode
                            silent = TRUE)
# Type ?fit_mod for more details

# Then the model can be fit by setting `msmMode = 0` using the `Rceattle` function:
ms_run <- Rceattle::fit_mod(data_list = adriatic_data,
                            inits = ss_run$estimated_params, # Initial parameters = 0
                            file = NULL, # Don't save
                            debug = 0, # Estimate. Set to 1 to not estimate.
                            random_rec = FALSE, # No random recruitment
                            msmMode = 1, # Single species mode
                            silent = TRUE,
                            niter = 6)
# Type ?fit_mod for more details




# Update stomach content data
N0 <- matrix(1, nrow = 3, ncol = max(adriatic_data$nages))
for(sp in 1:3){
  for(age in 2:adriatic_data$nages[[sp]]){
    if(age < adriatic_data$nages[[sp]]){
      N0[sp, age] <- N0[sp, age - 1] * exp(-adriatic_data$M1_base[sp,age - 1])
    }
    if(age == adriatic_data$nages[[sp]]){
      N0[sp, age] <- N0[sp, age - 1] * exp(-adriatic_data$M1_base[sp,age - 1]) / (1 - exp(-adriatic_data$M1_base[sp,age]))
    }
  }
}
W0 <- N0 * t(adriatic_data$wt[1,,])

uobswt <- adriatic_data$UobsWtAge

# Scale new wt at age
for(rsp in 1:3){
  

  
  for(r_age in 1:adriatic_data$nages[[rsp]]){
    


    for(ksp in 1:3){
      stom_sum <- sum(uobswt[rsp,ksp,r_age,], na.rm = TRUE)
      for(k_age in 1:adriatic_data$nages[[ksp]]){
      uobswt[rsp,ksp,r_age,k_age] <- uobswt[rsp,ksp,r_age,k_age] * W0[ksp, k_age]
      }
    }

    
    if(sum(uobswt[rsp,,r_age,], na.rm = TRUE) > 0){
      uobswt[rsp,,r_age,] <- uobswt[rsp,,r_age,] / sum(uobswt[rsp,,r_age,], na.rm = TRUE)
    }
  }
}

adriatic_data_update <- adriatic_data
adriatic_data_update$UobsWtAge <- uobswt

mod_list <- list()


iters <- c(3:20)

for( i in 1:length(iters)){
  iif(i == 1){
    inits <- ss_run$estimated_params
  }
  if(i > 1){
    inits = mod_list[[i-1]]$estimated_params
  }
  mod_list[[i]] <- Rceattle::fit_mod(data_list = adriatic_data_update,
                                     inits = inits, # Initial parameters from single species ests
                                     file = NULL, # Don't save
                                     debug = 0, # Do not estimate. Set to zero to estimate.
                                     niter = iters[i], # 10 iterations around population and predation dynamics
                                     random_rec = FALSE, # No random recruitment
                                     msmMode = 1, # MSVPA based
                                     suitMode = 0, # empirical suitability
                                     silent = FALSE,
                                     minNByage = 0)
}


plot_biomass(mod_list)
plot_recruitment(mod_list)
plot_mort(mod_list, age = 1)
plot_mort(mod_list, age = 2)
plot_mort(mod_list, age = 3)
plot_mort(mod_list, age = 4)
plot_mort(mod_list, age = 6)


mort <- sapply(mod_list, function(x) x$quantities$M2[1,,5])
rownames(mort) <- c(paste0("Age",0:10))
colnames(mort) <- c(paste0(iters, "iters"))
round(mort,4)

NByage <- sapply(mod_list, function(x) x$quantities$NByage[1,,5])
rownames(NByage) <- c(paste0("Age",0:10))
colnames(NByage) <- c(paste0(iters, "iters"))
round(NByage,4)


AvgN <- sapply(mod_list, function(x) x$quantities$AvgN[1,,5])
rownames(AvgN) <- c(paste0("Age",0:10))
colnames(AvgN) <- c(paste0(iters, "iters"))
round(AvgN,4)


avail_food <- sapply(mod_list, function(x) x$quantities$avail_food[1,,5])
rownames(avail_food) <- c(paste0("Age",0:10))
colnames(avail_food) <- c(paste0(iters, "iters"))
round(avail_food)
