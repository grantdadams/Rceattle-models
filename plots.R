# Example
# To run the 2017 single species assessment for the Bering Sea, a data file must first be loaded:
library(Rceattle)
data(BS2017SS) # ?BS2017SS for more information on the data 

# Then the model can be fit by setting `msmMode = 0` using the `Rceattle` function:
ss_run <- Rceattle(data_list = BS2017SS,
                   inits = NULL, # Initial parameters = 0
                   file_name = NULL, # Don't save
                   debug = 0, # Estimate
                   random_rec = FALSE, # No random recruitment
                   msmMode = 0, # Single species mode
                   avgnMode = 0,
                   silent = TRUE)

# The you can plot the model results using using
plot_biomass(Rceattle =  ss_run)
plot_recruitment(Rceattle =  ss_run)


# For the 2017 multispecies model starting from the single species parameters, the following can be specified:
data(BS2017MS) # ?BS2017MS for more information on the data 

ms_run <- Rceattle(data_list = BS2017MS,
                   inits = ss_run$estimated_params, # Initial parameters from single species ests
                   file_name = NULL, # Don't save
                   debug = 0, # Estimate
                   niter = 10,
                   random_rec = FALSE, # No random recruitment
                   msmMode = 1, # Holsman et al empirical suitability
                   avgnMode = 0,
                   suitMode = 1,
                   silent = TRUE,
                   recompile = FALSE)


# The you can plot the model results using using
mod_list <- list(ss_run, ms_run)
mod_names <- c("SS", "MS")
plot_biomass(Rceattle =  mod_list, model_names = mod_names)
plot_recruitment(Rceattle =  mod_list, model_names = mod_names)

# Plot biomass
par(mfrow = c(3,1), mar = c(3,4,0.5,0.5))
for(sp in 1:3){
  
  srv_yrs = ms_run$data_list$yrs_srv_biom[sp,]
  plot(y = ms_run$data_list$srv_biom[sp,], x = ms_run$data_list$yrs_srv_biom[sp,], type = "l", lty = 1, lwd = 2, col = 1, ylab = "Biomass", xlab = NA)
  Upper95 <-
    (
      ms_run$data_list$srv_biom[sp,] + ms_run$data_list$srv_biom_se[sp,] * 1.92
    )
  Lower95 <-
    (
      ms_run$data_list$srv_biom[sp,] - ms_run$data_list$srv_biom_se[sp,] * 1.92
    )
  
  arrows(
    x0 = srv_yrs,
    y0 = Upper95,
    x1 = srv_yrs,
    y1 = Lower95,
    length = 0.05,
    angle = 90,
    code = 3,
    lwd = 2,
    col = 1
  )
  
  points(y = ms_run$quantities$srv_bio_hat[sp,1:36], x = ms_run$data_list$yrs_srv_biom[sp,], pch = 16, col = 3, cex = 2)
  if(sp == 1){
    legend("topright", c("Observed", "Estimate"), col = c(1,3), pch = 16, bty = "n")
  }
}

# Plot catch
par(mfrow = c(3,1), mar = c(3,4,0.5,0.5))
for(sp in 1:3){
  plot(y = ms_run$data_list$tcb_obs[sp,], x = 1979:2017, type = "l", lty = 1, lwd = 2, col = 1, ylab = "Catch (mt)", xlab = NA)
  #points(y = ms_run$quantities$tc_hat[sp,]/2, x = 1979:2017, pch = 16, col = 3, cex = 2)
  if(sp == 1){
    #legend("topright", c("Observed", "Estimate"), col = c(1,3), pch = 16, bty = "n")
  }
}

