library(Rceattle)

################################################
# Data
################################################
data(BS2017SS) # ?BS2017SS for more information on the data
data("BS2017MS") # Note: the only difference is the residual mortality is lower

ss_run <- Rceattle::fit_mod(data_list = BS2017SS,
                            inits = NULL, # Initial parameters = 0
                            file = NULL, # Don't save
                            debug = 0, # Estimate
                            random_rec = FALSE, # No random recruitment
                            msmMode = 0, # Single species mode
                            silent = TRUE)


ms_run <- Rceattle::fit_mod(data_list = BS2017SS,
                            inits = ss_run$estimated_params, # Initial parameters from single species ests
                            file = NULL, # Don't save
                            debug = 0, # Estimate
                            niter = 10, # 10 iterations around population and predation dynamics
                            random_rec = FALSE, # No random recruitment
                            msmMode = 1, # MSVPA based
                            suitMode = 0, # empirical suitability
                            silent = TRUE)



# Run retro
ss_retro <- Rceattle::retrospective(ss_run, peels = 10)
ms_retro <- retrospective(ms_run, peels = 10)

# Plots
plot_recruitment((ss_retro[[1]]), mohns = ss_retro[[2]], file = "BSAI/Retrospective/Figures/ss_retro", model_names = c("Single species"))
plot_biomass((ss_retro[[1]]), mohns = ss_retro[[2]], file = "BSAI/Retrospective/Figures/ss_retro", model_names = c("Single species"))


plot_recruitment((ms_retro[[1]]), mohns = ms_retro[[2]], file = "BSAI/Retrospective/Figures/ms_retro", model_names = c("Multi species"))
plot_biomass((ms_retro[[1]]), mohns = ms_retro[[2]], file = "BSAI/Retrospective/Figures/ms_retro", model_names = c("Multi species"))
plot_mort((ms_retro[[1]]),  file = "BSAI/Retrospective/Figures/ms_retro", model_names = c("Multi species"), M2_only = TRUE, age = 1)
plot_mort((ms_retro[[1]]),  file = "BSAI/Retrospective/Figures/ms_retro", model_names = c("Multi species"), M2_only = TRUE, age = 2)
plot_mort((ms_retro[[1]]),  file = "BSAI/Retrospective/Figures/ms_retro", model_names = c("Multi species"), M2_only = TRUE, age = 3)

# With diet estimation
mod_list <- list()
retro_list <- list()
for(i in 1:3){
  mod_list[[i]] <- Rceattle::fit_mod(data_list = BS2017SS,
                                     inits = ss_run$estimated_params, # Initial   parameters from single species ests
                                     file = NULL, # Don't save
                                     debug = 0, # Estimate
                                     niter = 10, # 10 iterations around population and predation dynamics
                                     random_rec = FALSE, # No random recruitment
                                     msmMode = 1, # MSVPA based
                                     suitMode = i, # empirical suitability
                                     silent = TRUE)
  retro_list[[i]] <- retrospective(mod_list[[i]], peels = 10)
  
  plot_recruitment((retro_list[[i]][[1]]), mohns = retro_list[[i]][[2]], file = paste0("BSAI/Retrospective/Figures/ms_retro_suit_",i), model_names = c(paste0("Multi species suit ",i)))
  plot_biomass((retro_list[[i]][[1]]), mohns = retro_list[[i]][[2]], file = paste0("BSAI/Retrospective/Figures/ms_retro_suit_",i), model_names = c(paste0("Multi species suit ",i)))
  plot_mort((retro_list[[i]][[1]]), file = paste0("BSAI/Retrospective/Figures/ms_retro_suit_",i), model_names = c(paste0("Multi species suit ",i)), M2_only = TRUE, age = 1)
  plot_mort((retro_list[[i]][[1]]), file = paste0("BSAI/Retrospective/Figures/ms_retro_suit_",i), model_names = c(paste0("Multi species suit ",i)), M2_only = TRUE, age = 2)
plot_mort((retro_list[[i]][[1]]), file = paste0("BSAI/Retrospective/Figures/ms_retro_suit_",i), model_names = c(paste0("Multi species suit ",i)), M2_only = TRUE, age = 3)
}


