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
# installing the packages "glue" and "Rcpp" may have to be done independently

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
# Rceattle::write_excel(data_list = BS2017SS, file = "BS2017SS.xlsx")


# Read the data in
adriatic_data <- Rceattle::read_excel(file = "Adriatic_v8.xlsx")


################################################
# Estimation
################################################
inits <- build_params(adriatic_data)

# Then the model can be fit by setting `msmMode = 0` using the `Rceattle` function:
ss_run <- Rceattle::fit_mod(data_list = adriatic_data,
                            inits = inits, # Initial parameters = 0
                            file = NULL, # Don't save
                            debug = 0, # Estimate. Set to 1 to not estimate.
                            random_rec = FALSE, # No random recruitment
                            msmMode = 0, # Single species mode
                            silent = TRUE,
                            recompile = TRUE)
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


# For the a multispecies model starting from the single species parameters, the following can be specified to load the data:
data("BS2017MS") # Note: the only difference is the residual mortality is lower
# Or we can use the previous data set

ms_run <- Rceattle::fit_mod(data_list = adriatic_data,
                            inits = ss_run$estimated_params, # Initial parameters from single species ests
                            file = NULL, # Don't save
                            debug = 0, # Do not estimate. Set to zero to estimate.
                            niter = 4, # 10 iterations around population and predation dynamics
                            random_rec = FALSE, # No random recruitment
                            msmMode = 1, # MSVPA based
                            suitMode = 0, # empirical suitability
                            silent = FALSE)

# We can plot both runs as well:
mod_list <- list(ss_run, ms_run)
mod_names <- c("SS", "MS")

# Plot biomass trajectory
plot_biomass(Rceattle = mod_list, model_names = mod_names, species = species_names)
plot_recruitment(Rceattle = mod_list, model_names = mod_names, add_ci = TRUE, species = species_names)

plot_selectivity(Rceattle = mod_list, model_names = mod_names)
plot_mort(Rceattle = mod_list, model_names = mod_names, age = 2)


################################################
# Projection
################################################

# PROJECTION 1: CHANGING F
# Rceattle automatically projects the population forward when estimating
# To check to see what the F rates are for each fishery we can check:
adriatic_data$fsh_control$proj_F
adriatic_data$projyr # Year the population is projected forward

# We can then change the F - For example, mean historical F
adriatic_data$fsh_control$proj_F <- c(0.2342936, 0.513, 0.0774777)

# Re-run, without estimating
ms_run_proj <- Rceattle::fit_mod(data_list = adriatic_data,
                                 inits = ms_run$estimated_params, # Initial parameters from single species ests
                                 file = NULL, # Don't save
                                 debug = TRUE, # Do not estimate. Not changing parameters right now
                                 niter = 10, # 10 iterations around population and predation dynamics
                                 random_rec = FALSE, # No random recruitment
                                 msmMode = 1, # MSVPA based
                                 suitMode = 0, # empirical suitability
                                 silent = TRUE)




# PROJECTION 1: CHANGING FUTURE RECRUITMENT
# Change recruitment deviations - NOTE: the workflow for this may change in the future

# Get years from projection
nyrs <- adriatic_data$endyr - adriatic_data$styr + 1
nyrs_proj <- adriatic_data$projyr - adriatic_data$styr + 1
yrs_proj <- (nyrs + 1):nyrs_proj

# Replace future rec_devs with numbers
ms_run$estimated_params$rec_dev[,yrs_proj] <- replace(
  ms_run$estimated_params$rec_dev[,yrs_proj],
  values = rnorm( length(ms_run$estimated_params$rec_dev[,yrs_proj]),
                  mean = 0,
                  sd = adriatic_data$sigma_rec_prior) # Assumed value from penalized likelihood
)

ms_run_proj2 <- Rceattle::fit_mod(data_list = adriatic_data,
                                  inits = ms_run$estimated_params, # Initial parameters from single species ests
                                  file = NULL, # Don't save
                                  debug = TRUE, # Do not estimate. Not changing parameters right now
                                  niter = 10, # 10 iterations around population and predation dynamics
                                  random_rec = FALSE, # No random recruitment
                                  msmMode = 1, # MSVPA based
                                  suitMode = 0, # empirical suitability
                                  silent = TRUE)


# plot
mod_list <- list(ss_run, ms_run, ms_run_proj, ms_run_proj2)
mod_names <- c("SS", "MS no F", "MS mean historical F", "MS mean historical F w random rec")

# Plot biomass trajectory
plot_biomass(Rceattle = mod_list, model_names = mod_names, incl_proj = TRUE)
plot_recruitment(Rceattle = mod_list, model_names = mod_names, add_ci = TRUE, incl_proj = TRUE, species = species_names)
