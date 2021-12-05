library(Rceattle)
library(readxl)

################################################
# Data
################################################
mydata_hake <- Rceattle::read_data( file = "Data/2019PAcificHake.xlsx")

################################################
# Fit single-species models
################################################
hake_base <- Rceattle::fit_mod(data_list = mydata_hake,
                               inits = NULL, # Initial parameters = 0
                               file = NULL, # Don't save
                               debug = 0, # Estimate
                               random_rec = FALSE, # No random recruitment
                               random_sel = FALSE,
                               msmMode = 0, # Single species mode
                               verbose = 1,
                               phase = "default")

# -- Plot
plot_biomass(hake_base)
plot_ssb(hake_base)
plot_recruitment(hake_base)
plot_catch(hake_base)
plot_selectivity(hake_base)
plot_logindex(hake_base)

# -- Reweight
# Update composition weights using McAllister-Ianelli method
hake_reweighted <- hake_base
for(i in 1:3){
    mydata_hake_weighted <- mydata_hake
    mydata_hake_weighted$fleet_control$Comp_weights <- hake_reweighted$data_list$fleet_control$Est_weights_mcallister

    hake_reweighted <- Rceattle::fit_mod(data_list = mydata_hake_weighted,
                                         inits = hake_reweighted$estimated_params, # Initial parameters = 0
                                         file = NULL, # Don't save
                                         debug = FALSE, # Estimate
                                         random_rec = FALSE, # No random recruitment
                                         msmMode = 0, # Single species mode
                                         phase = NULL,
                                         verbose = 1)
}

plot_biomass(hake_reweighted)
plot_ssb(hake_reweighted)
plot_recruitment(hake_base)
plot_catch(hake_base)
plot_selectivity(hake_base)
plot_index(hake_base)


# - Fix N
mydata_hake_fixed <- mydata_hake
mydata_hake_fixed$estDynamics = 1
hake_fixed <- Rceattle::fit_mod(data_list = mydata_hake_fixed,
                                inits = NULL, # Initial parameters = 0
                                file = NULL, # Don't save
                                debug = 1, # Estimate
                                random_rec = FALSE, # No random recruitment
                                msmMode = 0, # Single species mode
                                verbose = 2,
                                phase = "default")
# -- Plot
plot_biomass(atf_base)
plot_recruitment(atf_base)
plot_catch(atf_base)
plot_selectivity(atf_base)
plot_logindex(atf_base)

persp(x = 1:8, y = 1966:2019, z = hake_base$quantities$sel[1, 1,1:8,], theta = 220, zlab = "Selectivity", xlab = "Age")
