library(Rceattle)
setwd("C:/Users/Grant Adams/Documents/GitHub/RceattleRuns/GOA/GOA_18.1.4 - Time Varying")

# Updated the ALK

################################################
# Data
################################################
# Read the data in
mydata <- Rceattle::read_data( file = "GOA_18.1.4.xlsx")
mydata$fsh_control$Nselages[15]

################################################
# Estimation
################################################
ss_run_base <- Rceattle::fit_mod(data_list = mydata,
                            inits = NULL, # Initial parameters = 0
                            file = NULL, # Don't save
                            debug = 0, # Estimate
                            random_rec = FALSE, # No random recruitment
                            msmMode = 0, # Single species mode
                            silent = FALSE)

file_name <- "Figures/Base/Base"
plot_index(ss_run_base, file = file_name)
plot_catch(ss_run_base, file = file_name)
Rceattle::plot_srv_comp(ss_run_base, file = file_name)
Rceattle::plot_fsh_comp(ss_run_base, file = file_name)
plot_biomass(ss_run_base, file = file_name)
plot_ssb(ss_run_base, file = file_name, add_ci = TRUE)
plot_recruitment(ss_run_base, file = file_name, add_ci = TRUE)
plot_selectivity(ss_run_base, file = file_name)
write_results(ss_run_base, file = paste0(file_name, ".xlsx"))



################################################
# Model 1 - Add Pollock Random Walk to Selectivity
################################################
mydata$fsh_control$Time_varying_sel[1] = 1
mydata$fsh_control$Sel_sd_prior[1] = 0.05

ss_run_mod1 <- Rceattle::fit_mod(data_list = mydata,
                                 inits = NULL, # Initial parameters = 0
                                 file = NULL, # Don't save
                                 debug = 0, # Estimate
                                 random_rec = FALSE, # No random recruitment
                                 msmMode = 0, # Single species mode
                                 silent = TRUE)

file_name <- "Figures/RW1/RW1"
plot_index(ss_run_mod1, file = file_name)
plot_catch(ss_run_mod1, file = file_name)
Rceattle::plot_srv_comp(ss_run_mod1, file = file_name)
Rceattle::plot_fsh_comp(ss_run_mod1, file = file_name)
plot_biomass(ss_run_mod1, file = file_name)
plot_ssb(ss_run_mod1, file = file_name, add_ci = TRUE)
plot_recruitment(ss_run_mod1, file = file_name, add_ci = TRUE)
plot_selectivity(ss_run_mod1, file = file_name)
write_results(ss_run_mod1, file = paste0(file_name, ".xlsx"))


################################################
# Model 2 - Add Pollock Random Walk to Acoustic Catchability
################################################
mydata$srv_control$Estimate_q[2] = 1
mydata$srv_control$Log_q_prior[2] = -13

ss_run_mod2 <- Rceattle::fit_mod(data_list = mydata,
                                 inits = NULL, # Initial parameters = 0
                                 file = NULL, # Don't save
                                 debug = 0, # Estimate
                                 random_rec = FALSE, # No random recruitment
                                 msmMode = 0, # Single species mode
                                 silent = FALSE)

mydata$srv_control$Time_varying_q[2] = 1
mydata$srv_control$Q_sd_prior[2] = 0.05
ss_run_mod2$estimated_params$ln_sigma_srv_q[2] <- log(0.05)

ss_run_mod2 <- Rceattle::fit_mod(data_list = mydata,
                                 inits = ss_run_mod2$estimated_params, # Initial parameters = 0
                                 file = NULL, # Don't save
                                 debug = 0, # Estimate
                                 random_rec = FALSE, # No random recruitment
                                 msmMode = 0, # Single species mode
                                 silent = FALSE)

file_name <- "Figures/RW2/RW2"
plot_index(ss_run_mod2, file = file_name)
plot_catch(ss_run_mod2, file = file_name)
Rceattle::plot_srv_comp(ss_run_mod2, file = file_name)
Rceattle::plot_fsh_comp(ss_run_mod2, file = file_name)
plot_biomass(ss_run_mod2, file = file_name)
plot_ssb(ss_run_mod2, file = file_name, add_ci = TRUE)
plot_recruitment(ss_run_mod2, file = file_name, add_ci = TRUE)
plot_selectivity(ss_run_mod2, file = file_name)
write_results(ss_run_mod2, file = paste0(file_name, ".xlsx"))


################################################
# Model 3 - Add Pollock Random Walk to BT Catchability
################################################
mydata$srv_control$Estimate_q[3] = 1
mydata$srv_control$Log_q_prior[3] = -13

ss_run_mod3 <- Rceattle::fit_mod(data_list = mydata,
                                 inits = NULL, # Initial parameters = 0
                                 file = NULL, # Don't save
                                 debug = 0, # Estimate
                                 random_rec = FALSE, # No random recruitment
                                 msmMode = 0, # Single species mode
                                 silent = FALSE)

mydata$srv_control$Time_varying_q[3] = 1
mydata$srv_control$Q_sd_prior[3] = 0.01
ss_run_mod3$estimated_params$ln_sigma_srv_q[3] <- log(0.01)

ss_run_mod3 <- Rceattle::fit_mod(data_list = mydata,
                                 inits = ss_run_mod3$estimated_params, # Initial parameters = 0
                                 file = NULL, # Don't save
                                 debug = 0, # Estimate
                                 random_rec = FALSE, # No random recruitment
                                 msmMode = 0, # Single species mode
                                 silent = FALSE)

file_name <- "Figures/RW3/RW3"
plot_index(ss_run_mod3, file = file_name)
plot_catch(ss_run_mod3, file = file_name)
Rceattle::plot_srv_comp(ss_run_mod3, file = file_name)
Rceattle::plot_fsh_comp(ss_run_mod3, file = file_name)
plot_biomass(ss_run_mod3, file = file_name)
plot_ssb(ss_run_mod3, file = file_name, add_ci = TRUE)
plot_recruitment(ss_run_mod3, file = file_name, add_ci = TRUE)
plot_selectivity(ss_run_mod3, file = file_name)
write_results(ss_run_mod3, file = paste0(file_name, ".xlsx"))


################################################
# Model 4 - Add Pollock Random Walk to ADFG Catchability
################################################
mydata$srv_control$Estimate_q[4] = 1
mydata$srv_control$Log_q_prior[4] = -13

ss_run_mod4 <- Rceattle::fit_mod(data_list = mydata,
                                 inits = NULL, # Initial parameters = 0
                                 file = NULL, # Don't save
                                 debug = 0, # Estimate
                                 random_rec = FALSE, # No random recruitment
                                 msmMode = 0, # Single species mode
                                 silent = TRUE)

mydata$srv_control$Time_varying_q[4] = 1
mydata$srv_control$Q_sd_prior[4] = 0.05
ss_run_mod4$estimated_params$ln_sigma_srv_q[4] <- log(0.05)

ss_run_mod4 <- Rceattle::fit_mod(data_list = mydata,
                                 inits = ss_run_mod4$estimated_params, # Initial parameters = 0
                                 file = "Figures/RW4/RW4", # Don't save
                                 debug = 0, # Estimate
                                 random_rec = FALSE, # No random recruitment
                                 msmMode = 0, # Single species mode
                                 silent = TRUE)

file_name <- "Figures/RW4/RW4"
plot_index(ss_run_mod4, file = file_name)
plot_catch(ss_run_mod4, file = file_name)
Rceattle::plot_srv_comp(ss_run_mod4, file = file_name)
Rceattle::plot_fsh_comp(ss_run_mod4, file = file_name)
plot_biomass(ss_run_mod4, file = file_name)
plot_ssb(ss_run_mod4, file = file_name, add_ci = TRUE)
plot_recruitment(ss_run_mod4, file = file_name, add_ci = TRUE)
plot_selectivity(ss_run_mod4, file = file_name)
write_results(ss_run_mod4, file = paste0(file_name, ".xlsx"))

retro <- retrospective(Rceattle = ss_run_mod4, peels = 10)

Rceattle <- ss_run_mod4
peels = 10

# Get objects
mod_list <- list(Rceattle)
data_list <- Rceattle$data_list
endyr <- data_list$endyr
styr <- data_list$styr
projyr <- data_list$projyr
nyrs_proj <- projyr - styr + 1

# Run across retrospective bits
ind <- 2
for (i in 1:peels) {
  data_list$endyr <- endyr - i
  nyrs <- (endyr - i) - styr + 1
  
  # Adjust initial parameters
  inits <- Rceattle$estimated_params
  inits$rec_dev[, (nyrs + 1):nyrs_proj] <- 0
  inits$F_dev <- inits$F_dev[, 1:nyrs]
  
  inits$ln_srv_q_dev <- inits$ln_srv_q_dev[, 1:nyrs]
  inits$ln_srv_q_dev_re <- inits$ln_srv_q_dev_re[, 1:nyrs]
  
  inits$srv_sel_inf_dev <- inits$srv_sel_inf_dev[, ,1:nyrs]
  inits$srv_sel_inf_dev_re <- inits$srv_sel_inf_dev_re[, ,1:nyrs]
  inits$srv_sel_slp_dev <- inits$srv_sel_slp_dev[, ,1:nyrs]
  inits$srv_sel_slp_dev_re <- inits$srv_sel_slp_dev_re[, ,1:nyrs]
  
  inits$fsh_sel_inf_dev <- inits$fsh_sel_inf_dev[, ,1:nyrs]
  inits$fsh_sel_inf_dev_re <- inits$fsh_sel_inf_dev_re[, ,1:nyrs]
  inits$fsh_sel_slp_dev <- inits$fsh_sel_slp_dev[, ,1:nyrs]
  inits$fsh_sel_slp_dev_re <- inits$fsh_sel_slp_dev_re[, ,1:nyrs]
  
  # Adjust map parameters
  map <- Rceattle$map
  map[[2]]$rec_dev[, (nyrs + 1):nyrs_proj] <- 0
  map[[2]]$F_dev <- map[[2]]$F_dev[, 1:nyrs]
  
  map[[2]]$ln_srv_q_dev <- map[[2]]$ln_srv_q_dev[, 1:nyrs]
  map[[2]]$ln_srv_q_dev_re <- map[[2]]$ln_srv_q_dev_re[, 1:nyrs]
  
  map[[2]]$srv_sel_inf_dev <- map[[2]]$srv_sel_inf_dev[, ,1:nyrs]
  map[[2]]$srv_sel_inf_dev_re <- map[[2]]$srv_sel_inf_dev_re[, ,1:nyrs]
  map[[2]]$srv_sel_slp_dev <- map[[2]]$srv_sel_slp_dev[, ,1:nyrs]
  map[[2]]$srv_sel_slp_dev_re <- map[[2]]$srv_sel_slp_dev_re[, ,1:nyrs]
  
  map[[2]]$fsh_sel_inf_dev <- map[[2]]$fsh_sel_inf_dev[, ,1:nyrs]
  map[[2]]$fsh_sel_inf_dev_re <- map[[2]]$fsh_sel_inf_dev_re[, ,1:nyrs]
  map[[2]]$fsh_sel_slp_dev <- map[[2]]$fsh_sel_slp_dev[, ,1:nyrs]
  map[[2]]$fsh_sel_slp_dev_re <- map[[2]]$fsh_sel_slp_dev_re[, ,1:nyrs]
  
  for (i in 1:length(map[[2]])) {
    map[[1]][[i]] <- factor(map[[2]][[i]])
  }
  
  
  # Refit
  newmod <- suppressMessages(suppressWarnings(Rceattle::fit_mod(data_list = data_list, inits = inits, file = NULL, debug = 0, map = map,
                                                                niter = data_list$niter, random_rec = data_list$random_rec, msmMode = data_list$msmMode, suitMode = data_list$suitMode,
                                                                avgnMode = data_list$avgnMode, silent = TRUE)))
  
  # Refit model If converged
  if (!is.null(newmod$opt$Convergence_check)) {
    if (newmod$opt$Convergence_check != "The model is definitely not converged") {
      mod_list[[ind]] <- newmod
      ind <- ind + 1
    }
  }
}

# Calculate Mohs rho for each species
objects <- c("biomass", "biomassSSB", "R")

mohns <- data.frame(matrix(0, nrow = length(objects), ncol = 1 + data_list$nspp))
colnames(mohns) <- c("Object", paste0("Species_", 1:data_list$nspp))
mohns$Object <- objects
