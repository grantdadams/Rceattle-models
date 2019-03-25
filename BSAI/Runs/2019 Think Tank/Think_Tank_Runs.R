library(Rceattle)
data(BS2017SS) 


###################################################
# Run in single species mode
ss_no_re <- Rceattle(data_list = BS2017SS,
                   inits = NULL, # Initial parameters = 0
                   file_name = "2019 Think Tank/Models/ss_no_re", # Don't save
                   debug = 0, # Estimate
                   random_rec = FALSE, # No random recruitment
                   msmMode = 0, # Single species mode
                   avgnMode = 0,
                   silent = TRUE)

ss_re <- Rceattle(data_list = BS2017SS,
                   inits = NULL, # Initial parameters = 0
                   file_name = "2019 Think Tank/Models/ss_re", # Don't save
                   debug = 0, # Estimate
                   random_rec = TRUE, # No random recruitment
                   msmMode = 0, # Single species mode
                   avgnMode = 0,
                   silent = TRUE)


###################################################
# Run in multi species mode with random recruitment
data(BS2017MS)
ms_no_re <- Rceattle(data_list = BS2017MS,
                   inits = ss_no_re$estimated_params, # Initial parameters = 0
                   file_name = "2019 Think Tank/Models/ms_no_re", # Don't save
                   debug = 0, # Estimate
                   random_rec = FALSE, # No random recruitment
                   msmMode = 1, # Single species mode
                   avgnMode = 0,
                   silent = TRUE)

ms_re <- Rceattle(data_list = BS2017MS,
                   inits = ss_re$estimated_params, # Initial parameters = 0
                   file_name = "2019 Think Tank/Models/ms_re", # Don't save
                   debug = 0, # Estimate
                   random_rec = TRUE, # No random recruitment
                   msmMode = 1, # Single species mode
                   avgnMode = 0,
                   silent = TRUE)


library(Rceattle)
# Load models
load("2019 Think Tank/Models/ss_admb.RData")
ss_admb <- mod_objects
ss_admb$opt$AIC

load("2019 Think Tank/Models/ss_no_re.RData")
ss_no_re <- mod_objects
ss_no_re$opt$AIC

load("2019 Think Tank/Models/ss_re.RData")
ss_re <- mod_objects
ss_re$opt$AIC

load("FISH559/Models/Optim 3/ms_admb_10.RData")
ms_admb <- mod_objects
sum(ms_admb$quantities$jnll_comp) * 2 + 2 * (length(unlist(ms_admb$initial_params)) - 1)

load("2019 Think Tank/Models/ms_no_re.RData")
ms_no_re <- mod_objects
ms_no_re$opt$AIC

load("2019 Think Tank/Models/ms_re.RData")
ms_re <- mod_objects
ms_re$opt$AIC

load("C:/Users/Grant Adams/Documents/GitHub/Rceattle/data-raw/CEATTLE_results.Rdata")
ss_admb <- tmp
load("C:/Users/Grant Adams/Documents/GitHub/Rceattle/data-raw/BSAI/BS_MS_10_Loops_Files_Corrected/CEATTLE_results.Rdata")
ms_admb <- tmp

aic_vec <- c(ss_admb$obj_fun, ss_no_re$quantities$jnll, ss_re$quantities$jnll, ms_admb$obj_fun, ms_no_re$quantities$jnll, ms_re$quantities$jnll)

sigma_r <- rbind( rep(0.707, 3) ,rep(0.707, 3) , ss_re$sdrep$value[which(names(ss_re$sdrep$value) == "r_sigma")], rep(0.707, 3), rep(0.707, 3), ms_re$sdrep$value[which(names(ms_re$sdrep$value) == "r_sigma")] )

sigma_r_sd <- rbind( rep(0, 3) ,rep(0, 3) , ss_re$sdrep$sd[which(names(ss_re$sdrep$value) == "r_sigma")], rep(0, 3), rep(0, 3), ms_re$sdrep$sd[which(names(ms_re$sdrep$value) == "r_sigma")] )

results <- cbind(aic_vec, sigma_r, sigma_r_sd)


# Plot biomass

plot_biomass(Rceattle = list(ss_no_re, ss_re),
             tmp_list = list(ss_admb),
             file_name = "2019 Think Tank/ss_runs", model_names = c("TMB single spp pl", "TMB single spp re",  "ADMB single spp"),
             line_col = c("#272727", "#9B1D20", "#009FB7", adjustcolor( "red", alpha.f = 0.6)),
             species = c("Walleye pollock", "Pacific cod", "Arrowtooth flounder", rep(NA, 2)), lwd = 4)

plot_recruitment(Rceattle = list(ss_no_re, ss_re),
                 tmp_list = list(ss_admb),
                 file_name = "2019 Think Tank/ss_runs", model_names = c("TMB single spp pl", "TMB single spp re",  "ADMB single spp"),
                 line_col = c("#272727", "#9B1D20", "#009FB7"),
                 species = c("Walleye pollock", "Pacific cod", "Arrowtooth flounder"),
                 ci_col = adjustcolor( c("#272727", "#9B1D20", "#009FB7"), alpha.f = 0.8), lwd = 4)


# Plot multispecies
plot_biomass(Rceattle = list(ms_no_re, ms_re),
             tmp_list = NULL
             , file_name = "2019 Think Tank/ms_runs",
             model_names = c("TMB multi-spp pl", "TMB multi-spp re",  "ADMB multi-spp"),
             line_col = c("#F0C808", "#143642", "#84BC9C", adjustcolor( "red", alpha.f = 0.6)),
             species = c("Walleye pollock", "Pacific cod", "Arrowtooth flounder", rep(NA, 1)), lwd = 4)


plot_recruitment(Rceattle = list(ms_no_re, ms_re),
                 tmp_list = NULL,
                 file_name = "2019 Think Tank/ms_runs",
                 model_names = c("TMB multi-spp pl", "TMB multi-spp re",  "ADMB multi-spp"),
                 line_col = c("#F0C808", "#143642", "#84BC9C"),
                 species = c("Walleye pollock", "Pacific cod", "Arrowtooth flounder"),
                 ci_col = adjustcolor( c("#F0C808", "#143642", "#84BC9C"), alpha.f = 0.8), lwd = 4)


# Plot RE
plot_biomass(Rceattle = list(ms_re, ss_re)
             , file_name = "2019 Think Tank/re_runs",
             model_names = c("TMB multi-spp re", "TMB single-spp re"),
             line_col = c("#143642", "#9B1D20"),
             species = c("Walleye pollock", "Pacific cod", "Arrowtooth flounder"), lwd = 4)

plot_recruitment(Rceattle = list(ms_re, ss_re)
                 , file_name = "2019 Think Tank/re_runs",
                 model_names = c("TMB multi-spp re", "TMB single-spp re"),
                 line_col = c("#143642", "#9B1D20"),
                 species = c("Walleye pollock", "Pacific cod", "Arrowtooth flounder"),
                 ci_col = adjustcolor( c("#143642", "#9B1D20"), alpha.f = 0.8), lwd = 4)



plot_recruitment(Rceattle = list(ss_no_re, ss_re),
                 tmp_list = NULL,
                 file_name = "2019 Think Tank/ss_runs_short", model_names = c("TMB single spp pl", "TMB single spp re"),
                 line_col = c("#272727", "#9B1D20", "#009FB7"),
                 species = c("Walleye pollock", "Pacific cod", "Arrowtooth flounder"),
                 ci_col = adjustcolor( c("#272727", "#9B1D20", "#009FB7"), alpha.f = 0.8), lwd = 4)

plot_recruitment(Rceattle = list(ms_no_re, ms_re),
                 tmp_list = NULL,
                 file_name = "2019 Think Tank/ms_runs_short",
                 model_names = c("TMB multi-spp pl", "TMB multi-spp re"),
                 line_col = c("#F0C808", "#143642", "#84BC9C"),
                 species = c("Walleye pollock", "Pacific cod", "Arrowtooth flounder"),
                 ci_col = adjustcolor( c("#F0C808", "#143642", "#84BC9C"), alpha.f = 0.8), lwd = 4)


