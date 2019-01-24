devtools::install_github("grantdadams/Rceattle", auth_token = "4925b42ac46f1e0aefd671e9dc0c1cf1b3157017")

library(Rceattle)
data(BS2017SS)
?BS2017SS



ss_run <- Rceattle(data_list = BS2017SS,
                   inits = NULL, # Initial parameters = 0
                   file_name = NULL, # Don't save
                   debug = 0, # Estimate
                   random_rec = FALSE, # No random recruitment
                   msmMode = 0, # Single species mode
                   avgnMode = 0,
                   silent = TRUE)


# Multi-species
data(BS2017MS)
ms_run <- Rceattle(data_list = BS2017MS,
                   inits = ss_run$estimated_params, # Initial parameters from ss run
                   file_name = NULL, # Don't save
                   debug = 0, # Estimate
                   random_rec = FALSE, # No random recruitment
                   niter = 10, # Number of iterations around predation/pop dy functions
                   msmMode = 1, # Multi-species holsman mode
                   avgnMode = 0,
                   silent = TRUE)


plot_biomass(ceattle_list =  list(ss_run, ms_run), model_names = c("SS", "MS"))
plot_recruitment(ceattle_list =  list(ss_run, ms_run), model_names = c("SS", "MS"))
