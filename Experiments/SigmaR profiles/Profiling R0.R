# Code to profile over R0 when rec devs are treated as random effects vs fixed effects
library(Rceattle)


# ESTIMATION ----
# * EBS Combined ----
data("BS2017SS")
ebs_run <- Rceattle::fit_mod(data_list = BS2017SS,
                             inits = NULL, # Initial parameters = 0
                             file = NULL, # Don't save
                             estimateMode = 0, # Estimate
                             random_rec = FALSE, # No random recruitment
                             msmMode = 0, # Single species mode
                             fit_control = fit_control(phase = TRUE, verbose = 1))

# -- Treat recruitment as random effects
ebs_run_re <- Rceattle::fit_mod(data_list = BS2017SS,
                                inits = ebs_run$estimated_params, # Initial parameters from previous
                                file = NULL, # Don't save
                                estimateMode = 0, # Estimate
                                random_rec = TRUE, # Random recruitment
                                msmMode = 0, # Single species mode
                                fit_control = fit_control(phase = FALSE, verbose = 1))

# * EBS with Ricker ----
alpha = exp(c(4.121, 2.119, 1.553))
ebs_ricker_run <- Rceattle::fit_mod(
  data_list = BS2017SS,
  inits = NULL, # Initial parameters = 0
  file = NULL, # Don't save
  estimateMode = 1, # Estimate hindcast only
  M1Fun = build_M1(M1_model = 0,
                   M1_use_prior = FALSE,
                   M2_use_prior = FALSE),
  recFun = build_srr(srr_fun = 0,
                     srr_pred_fun = "Ricker",
                     proj_mean_rec = FALSE,
                     srr_est_mode = 1,
                     srr_prior = alpha,
                     srr_prior_sd = 0.2),
  random_rec = FALSE, # No random recruitment
  msmMode = 0, # Single species mode
  initMode = 3, # Start at fished equilibrium from Finit (biases alpha and beta otherwise)
  fit_control = fit_control(phase = TRUE, verbose = 1))

# -- Treat recruitment as random effects
ebs_ricker_run_re <- Rceattle::fit_mod(
  data_list = BS2017SS,
  inits = ebs_ricker_run$estimated_params, # Initial parameters from previous
  file = NULL, # Don't save
  estimateMode = 1, # Estimate hindcast only
  M1Fun = build_M1(M1_model = 0,
                   M1_use_prior = FALSE,
                   M2_use_prior = FALSE),
  recFun = build_srr(srr_fun = 0,
                     srr_pred_fun = "Ricker",
                     proj_mean_rec = FALSE,
                     srr_est_mode = 1,
                     srr_prior = alpha,
                     srr_prior_sd = 0.2),
  random_rec = TRUE, # Random recruitment
  msmMode = 0, # Single species mode
  initMode = 3, # Fished equilibrium from Finit
  fit_control = fit_control(phase = FALSE, verbose = 1))


# * EBS Yellowfin sole ----
mydata_yfs <- Rceattle::read_data( file = "C:/Users/grant.adams/GitHub/yfs_ss3/Rceattle runs/Data/yfs_single_species_2022.xlsx")
mydata_yfs$estDynamics = 0
mydata_yfs$index_data$Log_sd <- mydata_yfs$index_data$Log_sd/mydata_yfs$index_data$Observation

mydata_yfs$catch_data$Catch <- mydata_yfs$catch_data$Catch*1000


yfs_model <- Rceattle::fit_mod(data_list = mydata_yfs,
                               inits = NULL, # Initial parameters = 0
                               file = NULL, # Don't save
                               estimateMode = 0, # Estimate
                               random_rec = FALSE, # No random recruitment
                               msmMode = 0, # Single species mode
                               initMode = 3, # Fished equilibrium from Finit
                               fit_control = fit_control(phase = TRUE, verbose = 1))

yfs_model_re <- Rceattle::fit_mod(data_list = mydata_yfs,
                                  inits = yfs_model$estimated_params, # Initial parameters = 0
                                  file = NULL, # Don't save
                                  estimateMode = 0, # Estimate
                                  random_rec = TRUE, # Random recruitment
                                  msmMode = 0, # Single species mode
                                  initMode = 3, # Fished equilibrium from Finit
                                  fit_control = fit_control(phase = FALSE, verbose = 1))


# * GOA Combined ----
# data("GOA2018SS")
# GOA2018SS$fleet_control$Proj_F_proportion <- rep(1, nrow(GOA2018SS$fleet_control))
# goa_run <- Rceattle::fit_mod(data_list = GOA2018SS,
#                              inits = NULL, # Initial parameters = 0
#                              file = NULL, # Don't save
#                              estimateMode = 0, # Estimate
#                              random_rec = FALSE, # No random recruitment
#                              msmMode = 0, # Single species mode
#                              fit_control = fit_control(phase = TRUE, verbose = 1))
#
# # -- Treat recruitment as random effects
# goa_run_re <- Rceattle::fit_mod(data_list = GOA2018SS,
#                                 inits = goa_run$estimated_params, # Initial parameters from previous
#                                 file = NULL, # Don't save
#                                 estimateMode = 0, # Estimate
#                                 random_rec = TRUE, # Random recruitment
#                                 msmMode = 0, # Single species mode
#                                 fit_control = fit_control(phase = FALSE, getsd = FALSE, verbose = 1))

# * GOA Pollock ----
data("GOApollock")
GOApollock$styr = 1977 # The SAFE model starts at 1970, so change styr to 1970 to run the full time series model (data is in there). I start them all at 1977 because thats the years with overlap.
pollock_model <- Rceattle::fit_mod(
  data_list = GOApollock,
  inits = NULL, # Initial parameters = 0
  file = NULL, # Don't save
  estimateMode = 0,
  random_rec = FALSE, # No random recruitment
  msmMode = 0,
  fit_control = fit_control(phase = TRUE, verbose = 1)) # Use default phasing

pollock_model_re <- Rceattle::fit_mod(
  data_list = GOApollock,
  inits = pollock_model$estimated_params, # Initial parameters from previous
  file = NULL, # Don't save
  estimateMode = 0,
  random_rec = TRUE, # Random recruitment
  msmMode = 0,
  fit_control = fit_control(phase = FALSE, verbose = 1))


# * GOA Arrowtooth flounder ----
data("GOAatf")
GOAatf$styr = 1977 # The SAFE model starts at 1961, so change styr to 1961 to run the full time series model (data is in there). I start them all at 1977 because thats the years with overlap.
atf_model <- Rceattle::fit_mod(
  data_list = GOAatf,
  inits = NULL, # Initial parameters = 0
  file = NULL, # Don't save
  estimateMode = 0,
  random_rec = FALSE, # No random recruitment
  msmMode = 0,
  fit_control = fit_control(phase = TRUE, verbose = 1)) # Use default phasing

atf_model_re <- Rceattle::fit_mod(
  data_list = GOAatf,
  inits = atf_model$estimated_params, # Initial parameters from previous
  file = NULL, # Don't save
  estimateMode = 0,
  random_rec = TRUE, # Random recruitment
  msmMode = 0,
  fit_control = fit_control(phase = FALSE, verbose = 1))


# * GOA Cod ----
data("GOAcod")
GOAcod$maturity[1,2:13] <- 2 # Spawn wt from SS model includes sex-ratio and maturity already, so setting Pmature (age-at-maturity) to 2 to have CEATTLE calculations be the same
cod_model <- Rceattle::fit_mod(
  data_list = GOAcod,
  inits = NULL, # Initial parameters = 0
  file = NULL, # Don't save
  estimateMode = 0,
  random_rec = FALSE, # No random recruitment
  msmMode = 0,
  fit_control = fit_control(phase = TRUE, verbose = 1)) # Use default phasing

cod_model_re <- Rceattle::fit_mod(
  data_list = GOAcod,
  inits = cod_model$estimated_params, # Initial parameters from previous
  file = NULL, # Don't save
  estimateMode = 0,
  random_rec =TRUE, # Random recruitment
  msmMode = 0,
  fit_control = fit_control(phase = FALSE, verbose = 1))


# PROFILE ----
r0_mult <- seq(from = 0.75, to = 1.25, by = 0.01) # Multipliers on the fitted R0; 1 is the fitted model


# * Run profile ----
# Each grid point refits the hindcast with R0 (rec_pars column 1) fixed at the
# fitted value times the multiplier. $nll is NA where a refit did not converge.
# - EBS
ebs_list1 <- profile(ebs_run, param = "R0", slots = list(1), values = list(r0_mult), joint = "multiply", cores = NULL, getsd = FALSE)
ebs_list2 <- profile(ebs_run, param = "R0", slots = list(2), values = list(r0_mult), joint = "multiply", cores = NULL, getsd = FALSE)
ebs_list3 <- profile(ebs_run, param = "R0", slots = list(3), values = list(r0_mult), joint = "multiply", cores = NULL, getsd = FALSE)

ebs_re_list1 <- profile(ebs_run_re, param = "R0", slots = list(1), values = list(r0_mult), joint = "multiply", cores = NULL, getsd = FALSE)
ebs_re_list2 <- profile(ebs_run_re, param = "R0", slots = list(2), values = list(r0_mult), joint = "multiply", cores = NULL, getsd = FALSE)
ebs_re_list3 <- profile(ebs_run_re, param = "R0", slots = list(3), values = list(r0_mult), joint = "multiply", cores = NULL, getsd = FALSE)

# - EBS w/ Ricker
ebsr_list1 <- profile(ebs_ricker_run, param = "R0", slots = list(1), values = list(r0_mult), joint = "multiply", cores = NULL, getsd = FALSE)
ebsr_list2 <- profile(ebs_ricker_run, param = "R0", slots = list(2), values = list(r0_mult), joint = "multiply", cores = NULL, getsd = FALSE)
ebsr_list3 <- profile(ebs_ricker_run, param = "R0", slots = list(3), values = list(r0_mult), joint = "multiply", cores = NULL, getsd = FALSE)

ebsr_re_list1 <- profile(ebs_ricker_run_re, param = "R0", slots = list(1), values = list(r0_mult), joint = "multiply", cores = NULL, getsd = FALSE)
ebsr_re_list2 <- profile(ebs_ricker_run_re, param = "R0", slots = list(2), values = list(r0_mult), joint = "multiply", cores = NULL, getsd = FALSE)
ebsr_re_list3 <- profile(ebs_ricker_run_re, param = "R0", slots = list(3), values = list(r0_mult), joint = "multiply", cores = NULL, getsd = FALSE)

# - YFS
yfs_list1 <- profile(yfs_model, param = "R0", slots = list(1), values = list(r0_mult), joint = "multiply", cores = NULL, getsd = FALSE)
yfs_re_list1 <- profile(yfs_model_re, param = "R0", slots = list(1), values = list(r0_mult), joint = "multiply", cores = NULL, getsd = FALSE)

# - GOA
goa_list1 <- profile(pollock_model, param = "R0", slots = list(1), values = list(r0_mult), joint = "multiply", cores = NULL, getsd = FALSE)
goa_list2 <- profile(atf_model, param = "R0", slots = list(1), values = list(r0_mult), joint = "multiply", cores = NULL, getsd = FALSE)
goa_list3 <- profile(cod_model, param = "R0", slots = list(1), values = list(r0_mult), joint = "multiply", cores = NULL, getsd = FALSE)

goa_re_list1 <- profile(pollock_model_re, param = "R0", slots = list(1), values = list(r0_mult), joint = "multiply", cores = NULL, getsd = FALSE)
goa_re_list2 <- profile(atf_model_re, param = "R0", slots = list(1), values = list(r0_mult), joint = "multiply", cores = NULL, getsd = FALSE)
goa_re_list3 <- profile(cod_model_re, param = "R0", slots = list(1), values = list(r0_mult), joint = "multiply", cores = NULL, getsd = FALSE)

goa_list <- list(pollock_model_re, cod_model_re, atf_model_re)

# * Combine ----
ebs_jnll <- list(ebs_list1, ebs_list2, ebs_list3)
ebs_re_jnll <- list(ebs_re_list1, ebs_re_list2, ebs_re_list3)

ebsr_jnll <- list(ebsr_list1, ebsr_list2, ebsr_list3)
ebsr_re_jnll <- list(ebsr_re_list1, ebsr_re_list2, ebsr_re_list3)

goa_jnll <- list(goa_list1, goa_list3, goa_list2)
goa_re_jnll <- list(goa_re_list1, goa_re_list3, goa_re_list2)


# PLOT ----
par(mfrow = c(3,3))

# -- EBS
for(i in 1:3){
  y = ebs_jnll[[i]]$nll
  y = y - min(y, na.rm = TRUE)

  plot(y = y, x = r0_mult, ylab = "dNLL", xlab = "R0 multiplier", type = "l", main = paste("EBS", ebs_run$data_list$spnames[i]), col = "red", ylim = c(0,10))


  y = ebs_re_jnll[[i]]$nll
  y = y - min(y, na.rm = TRUE)
  lines(y = y, x = r0_mult, col = 1)


  abline(v = 1, lty = 2) # A multiplier of 1 is the fitted R0
}

legend("topright", c("Penalized likelihood", "Random effects", "Fitted model"), col = c(2,1,1), lty = c(1,1,2), bty = "n")


# w/ Ricker
for(i in 1:3){
  y = ebsr_jnll[[i]]$nll
  y = y - min(y, na.rm = TRUE)

  plot(y = y, x = r0_mult, ylab = "dNLL", xlab = "R0 multiplier", type = "l", main = paste("EBS-Ricker", ebs_ricker_run$data_list$spnames[i]), col = "red", ylim = c(0,10))


  y = ebsr_re_jnll[[i]]$nll
  y = y - min(y, na.rm = TRUE)
  lines(y = y, x = r0_mult, col = 1)

  abline(v = 1, lty = 2)
}


# -- GOA
for(i in 1:3){
  y = goa_jnll[[i]]$nll
  y = y - min(y, na.rm = TRUE)

  plot(y = y, x = r0_mult, ylab = "dNLL", xlab = "R0 multiplier", type = "l", main = paste("GOA", goa_list[[i]]$data_list$spnames[1]), col = "red", ylim = c(0,10))


  y = goa_re_jnll[[i]]$nll
  y = y - min(y, na.rm = TRUE)
  lines(y = y, x = r0_mult, col = 1)

  abline(v = 1, lty = 2)
}
