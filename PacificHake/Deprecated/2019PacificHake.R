library(Rceattle)
library(readxl)

################################################
# Data
################################################
mydata_hake <- Rceattle::read_data( file = "Data/2019PacificHake.xlsx")

################################################
# Fit initial model
################################################
mydata_hake$est_M1 <- 0
hake_base <- Rceattle::fit_mod(data_list = mydata_hake,
                               inits = NULL, # Initial parameters = 0
                               file = NULL, # Don't save
                               estimateMode = 1,
                               msmMode = 0, # Single species mode
                               phase = "default")

mydata_hake$est_M1 <- 1
hake_baseM <- Rceattle::fit_mod(data_list = mydata_hake,
                               inits = NULL, # Initial parameters = 0
                               file = NULL, # Don't save
                               estimateMode = 1,
                               msmMode = 0, # Single species mode
                               phase = "default")

mydata_hake$est_M1 <- 1
hake_ms <- Rceattle::fit_mod(data_list = mydata_hake,
                               inits = NULL, # Initial parameters = 0
                               file = NULL, # Don't save
                               estimateMode = 1,
                               msmMode = 1, # Multi species mode
                               niter = 3,
                               phase = "default")

# -- Plot
plot_biomass(hake_base)
plot_ssb(hake_base)
plot_recruitment(hake_base)
plot_catch(hake_base)
plot_selectivity(hake_base)
plot_logindex(hake_base)

################################################
# Reweight initial model
################################################
# - Update composition weights using McAllister-Ianelli method
# - This is what they did prior to dirichlet-multinomial
hake_reweighted <- hake_base
for(i in 1:3){
    mydata_hake_weighted <- mydata_hake
    mydata_hake_weighted$fleet_control$Comp_weights <- hake_reweighted$data_list$fleet_control$Est_weights_mcallister

    hake_reweighted <- Rceattle::fit_mod(data_list = mydata_hake_weighted,
                                         inits = hake_reweighted$estimated_params, # Initial parameters = 0
                                         file = NULL, # Don't save
                                         random_rec = FALSE, # No random recruitment
                                         msmMode = 0, # Single species mode
                                         phase = NULL)
}

# -- Plot
plot_biomass(hake_reweighted) # Age-1+ biomass
plot_ssb(hake_reweighted) # Age-1+ ssb
plot_recruitment(hake_reweighted) # Age-1 rec
plot_catch(hake_reweighted)
plot_selectivity(hake_reweighted)
plot_index(hake_reweighted)


################################################
# Compare with SS  model
################################################
# - Fix N
mydata_hake_fixed <- mydata_hake
mydata_hake_fixed$estDynamics = 1 # Set model to use NByageFixed
mydata_hake_fixed$fleet_control$Selectivity <- c(0,0) # Use empirical selectivity

# - Fix q to value from SS
hake_fixed_inits <- build_params(mydata_hake_fixed) # Make a parameter object for initial values (can use hake_base$estimated_params)
hake_fixed_inits$ln_srv_q <- c(NA, log(1.08815)) # Set q to MLE from SS

hake_fixed <- Rceattle::fit_mod(data_list = mydata_hake_fixed,
                                inits = hake_fixed_inits, # Initial parameters from base (for some reason it bugs out if I dont do this)
                                map = NULL,
                                file = NULL, # Don't save
                                estimateMode = 4, # Do not estimate
                                random_rec = FALSE, # No random recruitment
                                msmMode = 0, # Single species mode
                                verbose = 2,
                                phase = NULL)


# -- Plot comparison
mod_list <- list(hake_base, hake_reweighted, hake_fixed)
mod_names = c("Base", "Base - reweighted", "Fixed")
plot_biomass(mod_list, model_names = mod_names) # Age-1+ biomass
plot_ssb(mod_list, model_names = mod_names)  # Age-1+ ssb
plot_index(mod_list, model_names = mod_names)
