


setwd("~/GitHub/RceattleRuns/GOA/Model runs/GOA_22.1.1")
######################### 
# Read in data
#########################
GOA_data <- Rceattle::read_data( file = "Data/Bridging/GOA_18_5_1_data_1977-2018_no_halibut.xlsx")
base <- bridging_fun(GOA_data)

# 1: Add updated catch (using 2018 weights for 2019-2022)
GOA_data <- Rceattle::read_data( file = "Data/Bridging/GOA_22_1_1_data_1977-2022_b1.xlsx")
GOA_data$endyr <- 2022
c1 <- bridging_fun(GOA_data)
plot_biomass(c1)


# 2: Add updated weight-at-age
GOA_data <- Rceattle::read_data( file = "Data/Bridging/GOA_22_1_1_data_1977-2022_b2.xlsx")
c2 <- bridging_fun(GOA_data)
plot_biomass(c2)


# 3: Add updated survey data
GOA_data <- Rceattle::read_data( file = "Data/Bridging/GOA_22_1_1_data_1977-2022_b3.xlsx")
c3 <- bridging_fun(GOA_data)
plot_biomass(c3)


# 4: Add updated comp data (pollock and arrowtooth only)
GOA_data <- Rceattle::read_data( file = "Data/Bridging/GOA_22_1_1_data_1977-2022_b4.xlsx")
c4 <- bridging_fun(GOA_data)
plot_biomass(c4)


# 5: Change cod to max-age 10
GOA_data <- Rceattle::read_data( file = "Data/Bridging/GOA_22_1_1_data_1977-2022_b5.xlsx")
c5 <- bridging_fun(GOA_data)
plot_biomass(c5)


# 6: Update cod comp data
GOA_data <- Rceattle::read_data( file = "Data/Bridging/GOA_22_1_1_data_1977-2022_b6.xlsx")
c6 <- bridging_fun(GOA_data)
plot_biomass(c6)


# 7: Add in new cod age data
GOA_data <- Rceattle::read_data( file = "Data/Bridging/GOA_22_1_1_data_1977-2022_b7.xlsx")
c7 <- bridging_fun(GOA_data)
plot_biomass(c7)


# 8: Add in new age-transition matrix
GOA_data <- Rceattle::read_data( file = "Data/Bridging/GOA_22_1_1_data_1977-2022_b8.xlsx")
c8 <- bridging_fun(GOA_data)
plot_biomass(c8)


# 9: Add in new surveys
GOA_data <- Rceattle::read_data( file = "Data/Bridging/GOA_22_1_1_data_1977-2022_b9.xlsx")
c9 <- bridging_fun(GOA_data)
plot_biomass(c9)


# 10: Update penalties for time-varying Q for pollock
GOA_data <- Rceattle::read_data( file = "Data/Bridging/GOA_22_1_1_data_1977-2022_b10.xlsx")
c10 <- bridging_fun(GOA_data)
plot_biomass(c10)


# 11: Estimate new surveys
GOA_data$fleet_control$Fleet_type[17:18] <- 2
c11 <- bridging_fun(GOA_data)
plot_biomass(c11)


bridging_fun <- function(GOA_data){
  library(Rceattle)
  library(readxl)
  # Set up model list
  # The three models for 1977 to 2022 are: 
  # •	Model 1: a model that does not include predation to represent a base single-species model. M is fixed
  # •	Model 2: a model that does not include predation to represent a base single-species model. M is estimated
  # •	Model 3: a mutlispecies model that did not include halibut predation.
  
  mydata_list <- list(
    GOA_data, # 1 - single-species SAFE M 
    GOA_data, # 2 - single-species Estimated M 
    GOA_data # 3 - Multi-species no hal - Est M1
  )
  
  
  # Set up inits vectors
  inits_M1_df <- data.frame(
    Model = 1:3,
    MsmMode = c(0, 0, 1), # Short 
    EstM1 = c(0, 1, 1), # Short
    InitModel = c(NA, 1, 2) # Short
  ) 
  
  
  # Set up M1 estimation switches
  for(i in 1:length(mydata_list)){
    mydata_list[[i]]$est_M1 = c(0,0,0)
    if(inits_M1_df$EstM1[i] == 1){
      mydata_list[[i]]$est_M1 = c(1,2,1)
    }
  }
  
  
  
  ################################################
  # Single species
  ################################################
  mod_list_all <- list()
  
  print("Model 1 - Run single-species fix M")
  mod_list_all[[1]] <- Rceattle::fit_mod(data_list = mydata_list[[1]],
                                         inits = NULL, # Initial parameters = 0
                                         file = NULL, # Don't save
                                         estimateMode = 0, # Estimate
                                         random_rec = FALSE, # No random recruitment
                                         msmMode = 0, # Single species mode
                                         verbose = 0,
                                         phase = "default")
  
  print("Model 2 - Run single-species est M")
  mod_list_all[[2]] <- Rceattle::fit_mod(data_list = mydata_list[[2]],
                                         inits = mod_list_all[[1]]$estimated_params, # Initial parameters = 0
                                         file = NULL, # Don't save
                                         estimateMode = 0, # Estimate
                                         random_rec = FALSE, # No random recruitment
                                         msmMode = 0, # Single species mode
                                         verbose = 0,
                                         phase = "default")
  
  
  ################################################
  print("Model 3 - Run multi-species")
  ################################################
  mod_list_all[[3]] <- Rceattle::fit_mod(
    data_list = mydata_list[[3]],
    inits = mod_list_all[[2]]$estimated_params, # Initial parameters = 0
    file = NULL, # Don't save
    estimateMode = 0, # Estimate
    random_rec = FALSE, # No random recruitment
    msmMode = 1, # Multi species mode
    verbose = 0,
    niter = 3,
    updateM1 = FALSE,
    use_gradient = TRUE,
    phase = NULL)
  
  return(mod_list_all)
}


plot_biomass(mod_list_all)
