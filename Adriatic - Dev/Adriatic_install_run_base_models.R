# Grant Adams, Kirstin Holsman, Andre Punt - April 2019
# Function to run CEATTLE model in TMB
# Citation:
# Holsman, K. K., Ianelli, J., Aydin, K., Punt, A. E., and Moffitt, E. A. 2015. A comparison of fisheries biological reference points estimated from temperature-specific multi-species and single-species climate-enhanced stock assessment models. Deep-Sea Research Part II: Topical Studies in Oceanography, 134: 360–378.

# Install devtools if you don't already have it
install.packages("devtools")
# On Windows install the Rtools matching your R-version.Install rtools (https://cran.r-project.org/bin/windows/Rtools/).
# Install TMB 
# Instructions can be found here for non-pc: https://github.com/kaskr/adcomp/wiki/Download
install.packages('TMB', type = 'source')
# Try "TMB::runExample(all = TRUE)" to see if TMB works
# Most common errors are related to package versioning or installation failure
# Please make sure that the r session is restarted
# For example, installing the packages "glue" and "Rcpp" may have to be done independently

# Install Rceattle
devtools::install_github("grantdadams/Rceattle", auth_token = "4925b42ac46f1e0aefd671e9dc0c1cf1b3157017")



library(Rceattle)

################################################
# Data
################################################
# Example
# To run the 2017 single species assessment for the Bering Sea, a data file must first be loaded:
# data(BS2017SS) # ?BS2017SS for more information on the data
# Write data to excel
# Rceattle::write_data(data_list = BS2017SS, file = "BS2017SS.xlsx")


# Read the data in
data <- Rceattle::read_data(file = "Data/Adriatic_base.xlsx")


################################################
# Single species model estimation
################################################

# The single-species model can be fit by setting `msmMode = 0` using the `Rceattle` function:
ss_run <- Rceattle::fit_mod(data_list = data,
                            inits = NULL, # Initial parameters = 0
                            file = NULL, # Don't save
                            debug = 0, # Estimate. Set to 1 to not estimate.
                            msmMode = 0, # Single species mode
                            silent = TRUE,
                            recompile = FALSE)
# Type ?fit_mod for more details


# The you can plot the model results using using  
species_names <- c("Hake", "Sardine", "Anchovy")
plot_biomass(Rceattle =  ss_run, species = species_names)
plot_recruitment(Rceattle =  ss_run, species = species_names)

# Plot diagnostics
plot_catch(Rceattle =  ss_run)
plot_index(Rceattle =  ss_run)
plot_srv_comp(Rceattle =  ss_run)
plot_fsh_comp(Rceattle =  ss_run)
plot_selectivity(ss_run)


################################################
# Multi-species model estimation
################################################

# For the a multispecies model the following can be specified:
ms_run <- Rceattle::fit_mod(data_list = data,
                            inits = ss_run$estimated_params, # Initial parameters from single species ests. Generally won't converge if this is not done
                            file = NULL, # Don't save
                            debug = 0, # Estimate. Set to 1 to debug.
                            niter = 3, # 3 iterations around population and predation dynamics
                            msmMode = 1, # MSVPA based
                            suitMode = 0, # Empirical suitability
                            silent = TRUE)

# We can plot both runs:
mod_list <- list(ss_run, ms_run)
mod_names <- c("Single-spp", "Multi-spp")

# Plot biomass trajectory
plot_biomass(Rceattle = mod_list, model_names = mod_names, species = species_names)
plot_recruitment(Rceattle = mod_list, model_names = mod_names, add_ci = TRUE, species = species_names)

plot_selectivity(Rceattle = mod_list, model_names = mod_names)
plot_mort(Rceattle = mod_list, model_names = mod_names, age = 2)

