
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
# Then the model can be fit by setting `msmMode = 0` using the `Rceattle` function:
ss_run <- Rceattle::fit_mod(data_list = adriatic_data,
                            inits = NULL, # Initial parameters = 0
                            file = NULL, # Don't save
                            debug = 0, # Estimate. Set to 1 to not estimate.
                            random_rec = FALSE, # No random recruitment
                            msmMode = 0, # Single species mode
                            silent = TRUE,
                            recompile = FALSE  )
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
    stom_sum <- sum(uobswt[rsp,,r_age,], na.rm = TRUE)
    uobswt[rsp,,r_age,] <- uobswt[rsp,,r_age,] * W0[rsp, r_age]
    
    if(sum(uobswt[rsp,,r_age,], na.rm = TRUE) > 0){
    uobswt[rsp,,r_age,] <- uobswt[rsp,,r_age,] / sum(uobswt[rsp,,r_age,], na.rm = TRUE) * stom_sum
    }
  }
}

adriatic_data_update <- adriatic_data
adriatic_data_update$UobsWtAge <- uobswt

ms_run10 <- Rceattle::fit_mod(data_list = adriatic_data_update,
                              inits = ss_run$estimated_params, # Initial parameters from single species ests
                              file = NULL, # Don't save
                              debug = 0, # Do not estimate. Set to zero to estimate.
                              niter = 6, # 10 iterations around population and predation dynamics
                              random_rec = FALSE, # No random recruitment
                              msmMode = 1, # MSVPA based
                              suitMode = 0, # empirical suitability
                              silent = FALSE)


