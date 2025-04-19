################################################
# Libraries
################################################
# devtools::install_github("grantdadams/Rceattle", ref = "dev-name-change") # New branch, will be merged into "dev"
library(Rceattle)
library(dplyr)
library(readxl)


################################################
# Data
################################################
new_hakedata <- Rceattle::read_data(file = "Data/NEW_ATF_hake_intrasp_250207.xlsx")


# Single-species
hake_ss <- Rceattle::fit_mod(data_list = new_hakedata,
                             inits = NULL, # Initial parameters = 0
                             file = NULL, # Don't save
                             estimateMode = 0,
                             verbose = 1,
                             msmMode = 0, # Single species mode
                             phase = TRUE,
                             initMode = 3) # Fished start with init devs


# MSVPA multi-species
hake_ms <- Rceattle::fit_mod(data_list = new_hakedata,
                             inits = hake_ss$estimated_params, # Initial parameters from single-species model
                             file = NULL, # Don't save
                             estimateMode = 0,
                             verbose = 1,
                             msmMode = 1, # Multi species mode
                             phase = FALSE,
                             initMode = 3) # Fished start with init devs


# Create initial parameter list:
inits = hake_ms$estimated_params
map = hake_ms$map # gam_a, gam_b, and log_phi are turned off here
hake_ms$map$mapList$log_gam_a


# Create a list with missing parameters (invented):
# Set weight ratio parameters
inits$log_gam_a = c(0, log(0.7))  # Mean log weight ratio for ATF, 0 for other species (pred/prey)
inits$log_gam_b = c(0, 0)  # Standard deviation of log weight ratio for ATF, 0 for other species

# Set vulnerability matrix (log transformed)
nspp <- 2  # ATF and hake
vulnerability_matrix <- matrix(0, nrow = nspp, ncol = nspp)
vulnerability_matrix[2, 1] <- 1.0
inits$log_phi <- log(vulnerability_matrix)


# Now, I have the parameters needed in the model
# try to fit model:
new_hakedata$diet_data$Sample_size <- 0 # Set sample size to zero so diet data is not included in likelihood
hake_ls <- Rceattle::fit_mod(data_list = new_hakedata,
                             inits = hake_ms$estimated_params, # Initial parameters from multi-species model
                             map = map, # Using previous map because we are not estimating new parameters
                             file = NULL, # Don't save
                             estimateMode = 0,
                             verbose = 1,
                             msmMode = 1, # Multi species mode
                             phase = FALSE,
                             suitMode = 4, # Weight-based lognormal suitability
                             initMode = 3) # Fished start with init devs

# Plot
plot_biomass(list(hake_ss, hake_ms, hake_ls), model_names = c("Single-spp", "MSVPA", "Lognormal"))
plot_b_eaten(list(hake_ss, hake_ms, hake_ls), model_names = c("Single-spp", "MSVPA", "Lognormal"))

# JNLL
hake_ls$quantities$jnll_comp
hake_ls$quantities$suitability[2,1,10,1:20,1:3]
