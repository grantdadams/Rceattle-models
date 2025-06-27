library(Rceattle)

################################################
# Pollock
################################################
pollock23 <- Rceattle::read_data( file = "Data/Pollock_2023.xlsx")
pollock23$fleet_control$Comp_loglike = 0

# - Fit single-species models
pollock_base <- fit_mod(data_list = pollock23,
                        inits = NULL, # Initial parameters = 0
                        file = NULL, # Don't save
                        estimateMode = 0, # Estimate
                        random_q = FALSE,
                        random_rec = TRUE, # No random recruitment
                        msmMode = 0, # Single species mode
                        verbose = 1,
                        initMode = 1,
                        phase = TRUE)


# SAFE model ----
# Fixed initial age-structure and removed accumulation age
load("Data/Pollock23.Rdata")
safe <- pollock_base
nyrs <- length(pollock_base$data_list$styr:pollock_base$data_list$endyr)
safe$quantities$biomass[,1:nyrs] <- fit$rep$Etotalbio * 1e6
safe$quantities$ssb[,1:nyrs] <- fit$rep$Espawnbio * 1e6
safe$quantities$R[,1:nyrs] <- fit$rep$recruit * 1e6
safe$quantities$index_hat <- safe$quantities$index_hat / 1e6
safe$quantities$index_hat[1:(nyrs*6)] <- c(fit$rep$Eindxsurv1, fit$rep$Eindxsurv2, fit$rep$Eindxsurv3, fit$rep$Eindxsurv4, fit$rep$Eindxsurv5, fit$rep$Eindxsurv6) * 1e6


# Plot ----
mod_list <- list(pollock_base, safe)
model_names <- c("CEATTLE", "SAFE")

plot_biomass(mod_list, model_names = model_names)
plot_ssb(mod_list, model_names = model_names)
plot_recruitment(mod_list, model_names = model_names)
# plot_index(mod_list, model_names = model_names)
