library(Rceattle)
library(wesanderson)
library(ggplot2)
library(readxl)

# Load models
setwd("Model runs/GOA_18.5.1")

mod_list_all <- list()
re_mods <- list.files("Models/Random_effects_models_3iter_w_hessian")
for(i in 1:length(re_mods)){
  load(paste0("Models/Random_effects_models_3iter_w_hessian/", re_mods[i]))
  mod_no <- as.numeric(gsub('_', '', substr(re_mods[i], 21,22)))
  mod_list_all[[mod_no]] <- mod_re
}


# The long time-series models for 1977 to 2018 were: 
#   •	Model 1: a model that did not include predation (single-species models) representing a base model. 
# •	Model 2: a model that did not include halibut predation to allow comparisons in which halibut does not impact the dynamics of groundfish in the GOA. 
# •	Models 3-5: models that included pre-specified mid-year numbers-at-age of Pacific halibut from the coastwide long-time (1917-2018) series model developed by the IPHC. To account for a lack of information on halibut distribution prior to 1993, numbers-at-age prior to 1993 were multiplied by the 50th (model 3), 15th (model 4), and 85th (model 5) quantiles of the distribution of adult halibut in area 3 between 1993 and 2018. 
# •	Models 6-8: as for models 3-5 but using numbers-at-age of Pacific halibut from the areas-as-fleets long-time series model. 

# The three moderate term models for 1993 to 2018 were: 
#   •	Model 9: a model that does not include predation (model 9) to represent a base single-species model 
# •	Model 10: a mutlispecies model that did not include halibut predation (model 10). 
# •	Model 11: a mutlispecies model with relative abundance-at-age of Pacific halibut in area 3 multiplied by an estimated parameter to allow the model to estimate the relative contribution of Pacific halibut predation to describing the dynamics of pollock, Pacific cod, and arrowtooth flounder. 

# The four short term models for 1996 to 2018 were: 
#   •	Model 12: a model that does not include predation (model 9) to represent a base single-species model 
# •	Model 13: a mutlispecies model that did not include halibut predation (model 10). 
# •	Model 14: a model with pre-specified mid-year numbers-at-age of Pacific halibut from the coastwide short-time series model. 
# •	Model 15: as for models 11but using numbers-at-age of Pacific halibut from the areas-as-fleets short-time series model


mod_names_long <- c("1. SS long", "2. MS-No Halibut long", "3. MS-Coast long avg", "4. MS-Coast long low", "5. MS-Coast long high", "6. MS-AAF long avg", "7. MS-AAF long low", "8. MS-AAF long high")
mod_names_medium <- c("9. SS med", "10. MS-No Halibut med", "11. MS-Survey med")
mod_names_short <- c("12. SS short", "13. MS-No Halibut short", "14. MS-Coast short", "15. MS-AAF short")
mod_names_ss <- c(mod_names_long[1], mod_names_medium[1], mod_names_short[1])


ms_mod_list <- mod_list_all[c(2:8, 10:15)]
mod_list_long <- mod_list_all[1:8]
mod_list_medium <- mod_list_all[9:11]
mod_list_short <- mod_list_all[12:15]
mod_list_ss <- mod_list_all[c(1,9,12)]
mod_list_ms <- mod_list_all[c(2:8,10:11,13:15)]
mod_list_ms_hal <- mod_list_all[c(3:8,11,14:15)]
mod_list_ms_no_hal <- mod_list_all[c(2,10,13)]


mod_names_all <- c(mod_names_long, mod_names_medium, mod_names_short)
line_col <- oce::oce.colorsViridis(16)[1:15] 
line_col_long <- line_col[1:8]
line_col_medium <- line_col[9:11]
line_col_short <- line_col[12:15]


# Model average
mov_avg_ss <- model_average(mod_list_ss, uncertainty = TRUE, nboot = 10000)
save(mov_avg_ss, file = "Models/18.5.1_mov_avg_ss.RData")

mov_avg_ms_no_hal <- model_average(mod_list_ms_no_hal, uncertainty = TRUE, nboot = 10000)
save(mov_avg_ms_no_hal, file = "Models/18.5.1_mov_avg_ms_no_hal.RData")

save(mov_avg_ms_rel_hal, file = "Models/18.5.1_mod_avg_ms_rel_hal.RData")
mov_avg_ms_hal <- model_average(mod_list_all[c(3:8,14:15)], weights = c(1,1,1,1,1,1,3,3), uncertainty = TRUE, nboot = 10000)
save(mov_avg_ms_hal, file = "Models/18.5.1_mod_avg_ms_hal.RData")

mov_avg_ms_rel_hal <- mod_list_all[[11]]


mov_avg_ss$quantities$biomass[4,] <- NA
mov_avg_ss$quantities$biomassSSB[4,] <- NA
mov_avg_ms_no_hal$quantities$biomass[4,] <- NA
mov_avg_ms_no_hal$quantities$biomassSSB[4,] <- NA
mod_list_avg <- list(mov_avg_ss, mov_avg_ms_no_hal, mov_avg_ms_hal, mov_avg_ms_rel_hal)
mod_list_avg_names <- c("Avg 1-SS", "Avg 2-MS No Halibut", "Avg 3-MS Halibut", "Avg 4-MS Relative Halibut")

# Bounds diagnostics
rel_diff_upper <- abs(mod_list_all[[2]]$opt$diagnostics$Upper - mod_list_all[[2]]$opt$diagnostics$MLE)
rel_diff_upper[which(rel_diff_upper != Inf)]
rel_diff_lower <- abs(mod_list_all[[2]]$opt$diagnostics$Lower - mod_list_all[[2]]$opt$diagnostics$MLE)
rel_diff_lower[which(rel_diff_lower != Inf)]

# ######################### 
# # Compare with SAFE Models
# #########################
# # Columns = year, pollock, cod, atf
# safe2018biomass <- as.data.frame(read_xlsx("Data/2018_SAFE_biomass_estimate.xlsx", sheet = 1))
# safe2018ssb <- as.data.frame(read_xlsx("Data/2018_SAFE_biomass_estimate.xlsx", sheet = 2))
# safe2018rec <- as.data.frame(read_xlsx("Data/2018_SAFE_biomass_estimate.xlsx", sheet = 3))
# 
# # Assign data to CEATTLE object
# Mod_18_SAFE <- mod_list_all[[1]]
# # - Pollock and ATF
# Mod_18_SAFE$quantities$biomass[1:2,1:42] <- t(safe2018biomass[1:42,c(2,4)]) * 1000
# Mod_18_SAFE$quantities$biomassSSB[1:2,1:42] <- t(safe2018ssb[1:42,c(2,4)]) * 1000
# 
# # - Cod
# Mod_18_SAFE$quantities$biomass[3,1:42] <- t(safe2018biomass[1:42,c(3)])
# Mod_18_SAFE$quantities$biomassSSB[3,1:42] <- t(safe2018ssb[1:42,c(3)])
# 
# # Convert to age-3 biomass
# Mod_18_5_1_3plusBiomass <- mod_list_ss
# for(i in 1:length(Mod_18_5_1_3plusBiomass)){
#   Mod_18_5_1_3plusBiomass[[i]]$quantities$biomass[1,] <- colSums(Mod_18_5_1_3plusBiomass[[i]]$quantities$biomassByage[1,3:10,])
# }
# 
# plot_biomass(c(Mod_18_5_1_3plusBiomass, list(Mod_18_SAFE)), file =  "Results/Figures/18.5.1. Bridging weighted March 2021", model_names = c(mod_names_ss, "2018 SAFE"), right_adj = 0.27, line_col = NULL, species = c(1:3))
# 


######################### 
# TIME-SERIES PLOTS
#########################

# - Long time series
file_name <- "Results/Figures/Time_series/18.5.1_models_long"
plot_biomass(mod_list_long, file = file_name, model_names = mod_names_long, right_adj = 0.2, line_col = line_col_long)
plot_ssb(mod_list_long, file = file_name, model_names = mod_names_long, right_adj = 0.2, line_col = line_col_long)
plot_recruitment(mod_list_long, file = file_name, add_ci = TRUE, model_names = mod_names_long, right_adj = 0.27, line_col = line_col_long)
plot_index(mod_list_long, file = file_name, model_names = mod_names_long, right_adj = 0, line_col = line_col_long)
plot_logindex(mod_list_long, file = file_name, model_names = mod_names_long, right_adj = 0.25, top_adj = 0.27, line_col = line_col_long)
plot_catch(mod_list_long, file = file_name, model_names = mod_names_long, right_adj = 0.25, top_adj = 0, line_col = line_col_long)
plot_b_eaten(mod_list_long, file = file_name, add_ci = FALSE, model_names = mod_names_long, right_adj = 0.17, line_col = line_col, lwd = 2)


# Medium time-series
file_name <- "Results/Figures/Time_series/18.5.1_models_medium"
plot_biomass(mod_list_medium, file = file_name, model_names = mod_names_medium, right_adj = 0.17, line_col = line_col_medium)
plot_ssb(mod_list_medium, file = file_name, model_names = mod_names_medium, right_adj = 0.17, line_col = line_col_medium)
plot_recruitment(mod_list_medium, file = file_name, add_ci = TRUE, model_names = mod_names_medium, right_adj = 0.17, line_col = line_col_medium)
plot_index(mod_list_medium, file = file_name, model_names = mod_names_medium, right_adj = 0, line_col = line_col_medium)
plot_logindex(mod_list_medium, file = file_name, model_names = mod_names_medium, right_adj = 0.1, line_col = line_col_medium)
plot_catch(mod_list_medium, file = file_name, model_names = mod_names_medium, right_adj = 0, line_col = line_col_medium)


# Short time-series
file_name <- "Results/Figures/Time_series/18.5.1_models_short"
plot_biomass(mod_list_short, file = file_name, model_names = mod_names_short, right_adj = 0.17, line_col = line_col_short)
plot_ssb(mod_list_short, file = file_name, model_names = mod_names_short, right_adj = 0.17, line_col = line_col_short)
plot_recruitment(mod_list_short, file = file_name, add_ci = TRUE, model_names = mod_names_short, right_adj = 0.17, line_col = line_col_short)
plot_index(mod_list_short, file = file_name, model_names = mod_names_short, right_adj = 0, line_col = line_col_short)
plot_logindex(mod_list_short, file = file_name, model_names = mod_names_short, right_adj = 0.1, line_col = line_col_short)
plot_catch(mod_list_short, file = file_name, model_names = mod_names_short, right_adj = 0, line_col = line_col_short)


# All plots
file_name <- "Results/Figures/Time_series/18.5.1_models_all"
plot_biomass(mod_list_all, file = file_name, model_names = mod_names_all, right_adj = 0.2, line_col = line_col, lwd = 2)
plot_ssb(mod_list_all, file = file_name, add_ci = FALSE, model_names = mod_names_all, right_adj = 0.2, line_col = line_col, lwd = 2)
plot_b_eaten(mod_list_all, file = file_name, add_ci = FALSE, model_names = mod_names_all, right_adj = 0.2, line_col = line_col, lwd = 2, species = 1:3)
plot_b_eaten_prop(mod_list_all, file = file_name, add_ci = FALSE, model_names = mod_names_all, right_adj = 0.2, line_col = line_col, lwd = 2, species = 1:3)
plot_recruitment(mod_list_all, file = file_name, add_ci = FALSE, model_names = mod_names_all, right_adj = 0.2, line_col = line_col, lwd = 2)
plot_logindex(mod_list_all, file = file_name, model_names = 1:15, right_adj = 0.12, line_col = line_col)


# Model-average
line_col_avg <- oce::oce.colorsViridis(5)[1:4] 
file_name <- "Results/Figures/Time_series/Model_average"
mod_list_avg[[1]]$quantities$biomass[4,] <- NA
mod_list_avg[[1]]$quantities$biomassSSB[4,] <- NA
mod_list_avg[[2]]$quantities$biomass[4,] <- NA
mod_list_avg[[2]]$quantities$biomassSSB[4,] <- NA
mod_list_avg[[1]]$asymptotic_samples$biomass[4,,] <- NA
mod_list_avg[[2]]$asymptotic_samples$biomass[4,,] <- NA


plot_biomass(mod_list_avg, file = file_name, model_names = mod_list_avg_names, right_adj = 0, line_col = line_col_avg, lwd = 2, minyr = 1996, mod_avg = c(TRUE, TRUE, TRUE, FALSE), add_ci = TRUE, alpha = 0.35, species = 1:4)
plot_recruitment(Rceattle = mod_list_avg, file = file_name, width = 5, height = 6, model_names = mod_list_avg_names, right_adj = 0, line_col = line_col_avg, lwd = 2, minyr = 1996, mod_avg = c(TRUE, TRUE, TRUE, FALSE), add_ci = TRUE, alpha = 0.35)
plot_ssb(mod_list_avg, file = file_name, species = 1:3, width = 5, height = 6, model_names = mod_list_avg_names, right_adj = 0, line_col = line_col_avg, lwd = 2, minyr = 1996, mod_avg = c(TRUE, TRUE, TRUE, FALSE), add_ci = TRUE, alpha = 0.35)


plot_b_eaten(mod_list_avg[2:4], file = file_name, add_ci = FALSE, width = 5, top_adj = 0.15, mod_cex = 1, height = 6, model_names = mod_list_avg_names[2:4], right_adj = 0, line_col = line_col_avg[2:4], lwd = 2, species = 1:3, minyr = 1996)
plot_b_eaten_prop(mod_list_avg[2:4], file = file_name, add_ci = FALSE, width = 5, height = 6, model_names = mod_list_avg_names[2:4], right_adj = 0, line_col = line_col_avg[2:4], lwd = 2, species = 1:3, minyr = 1996)



#######################################################
# Likelihood components
#######################################################
# -- Joint likelihood
nll_all <- data.frame(
  model <- 1:length(mod_list_all),
  nll = sapply(mod_list_all, function(x) x$quantities$jnll),
  k = sapply(mod_list_all, function(x) length(x$obj$env$last.par)))
nll_all$aic <- 2*nll_all$k + 2*nll_all$nll
nll_all$daic <- NA
nll_all$daic[1:8] <- nll_all$aic[1:8] - min(nll_all$aic[1:8]) # Long
nll_all$daic[9:11] <- nll_all$aic[9:11] - min(nll_all$aic[9:11]) # Med
nll_all$daic[12:15] <- nll_all$aic[12:15] - min(nll_all$aic[12:15]) # Short
nll_all <- round(nll_all)
write.csv(nll_all, "Results/Tables/18.5.1.all_model_aic_jnll.csv")


# -- Table of likelihood component summary (index, comp, catch, etc)
jnll_summary <- data.frame(matrix(NA, nrow = nrow(mod_list_all[[1]]$quantities$jnll_comp), ncol = (length(mod_list_all)+2)))
jnll_summary[,1] = rownames(mod_list_all[[1]]$quantities$jnll_comp)

for(i in 1:length(mod_list_all)){
  jnll_summary[,i+1] <- rowSums(mod_list_all[[i]]$quantities$jnll_comp)
}
for(i in 1:length(mod_list_all)){
  jnll_summary[,i+1] <- as.numeric(jnll_summary[,i+1])
}
jnll_summary[16,] <- c("Rec", jnll_summary[12,2:16] + jnll_summary[13,2:16])
jnll_summary[17,] <- c("Sel pen", jnll_summary[5,2:16] + jnll_summary[6,2:16] + jnll_summary[7,2:16] + jnll_summary[8,2:16])
jnll_summary <- rbind(jnll_summary, c("jnll", sapply(mod_list_all, function(x) x$quantities$jnll)))
jnll_summary <- rbind(jnll_summary, c("k", sapply(mod_list_all, function(x) length(x$obj$env$last.par))))
for(i in 1:length(mod_list_all)){
  jnll_summary[,i+1] <- as.numeric(jnll_summary[,i+1])
}
jnll_summary = jnll_summary[-which(rowSums(jnll_summary[,2:14]) == 0),]
write.csv(jnll_summary, "Results/Tables/18.5.1.all_model_nll_components_summary.csv")


# -- Delta likelihood component summary (index, comp, catch, etc)
for(i in 1:(nrow(jnll_summary)-1)){
  jnll_summary[i,2:9] <- jnll_summary[i,2:9] - min(jnll_summary[i,2:9])
  jnll_summary[i,10:12] <- jnll_summary[i,10:12] - min(jnll_summary[i,10:12])
  jnll_summary[i,13:16] <- jnll_summary[i,13:16] - min(jnll_summary[i,13:16])
}
jnll_summary <- rbind(jnll_summary, c("DAIC", nll_all$daic))
jnll_summary <- round(jnll_summary, 3)
write.csv(jnll_summary, "Results/Tables/18.5.1.all_model_delta_nll_components_summary.csv")


# -- Table of all likelihood bits by fleet (index, comp, catch, etc)
jnll_summary <- data.frame(matrix(NA, nrow = length(mod_list_all[[1]]$quantities$jnll_comp), ncol = (length(mod_list_all)+3)))
jnll_summary[,1] = rep(rownames(mod_list_all[[1]]$quantities$jnll_comp), ncol(mod_list_all[[1]]$quantities$jnll_comp))
jnll_summary[,2] = paste0(rep(rownames(mod_list_all[[1]]$quantities$jnll_comp), ncol(mod_list_all[[1]]$quantities$jnll_comp)), "_$Sp/Srv/Fsh_", rep(1:ncol(mod_list_all[[1]]$quantities$jnll_comp), each = nrow(mod_list_all[[1]]$quantities$jnll_comp)))
jnll_summary[,3] <- rep(mod_list_all[[1]]$data_list$fleet_control$Fleet_name[mod_list_all[[1]]$data_list$fleet_control$Fleet_code], each = nrow(mod_list_all[[1]]$quantities$jnll_comp))

for(i in 1:length(mod_list_all)){
  jnll_summary[,i+3] <- c(mod_list_all[[i]]$quantities$jnll_comp)
}
jnll_summary <- jnll_summary[which(rowSums(jnll_summary[,4:16]) !=0),]
colnames(jnll_summary) <- c("Likelihood component", "Likelihood component2", "Fleet_name", 1:length(mod_list_all))
write.csv(jnll_summary, "Results/Tables/18.5.1.individual_nll_components.csv")

# -- Different in likelihood components
for(i in 1:nrow(jnll_summary)){
  jnll_summary[i,4:11] <- jnll_summary[i,4:11] - min(jnll_summary[i,4:11])
  jnll_summary[i,12:14] <- jnll_summary[i,12:14] - min(jnll_summary[i,12:14])
  jnll_summary[i,15:18] <- jnll_summary[i,15:18] - min(jnll_summary[i,15:18])
}
jnll_summary <- jnll_summary[which(rowSums(jnll_summary[,4:16]) !=0),]
# jnll_summary <- jnll_summary[which(round(rowSums(jnll_summary[,4:16]), 2) !=0),]
pop_pen <- which(jnll_summary[,1] == "Recruitment deviates" | jnll_summary[,1] == "Initial abundance deviates")
jnll_summary_pop_pen <- jnll_summary[pop_pen, ]
jnll_summary <- rbind(jnll_summary[-pop_pen, ], jnll_summary_pop_pen)
write.csv(jnll_summary, "Results/Tables/18.5.1.delta_individual_nll_components.csv")


#######################################################
# Biomass comparison
#######################################################
# - Long
ssb_diff_long <- lapply(mod_list_all[2:8], function(x) x$quantities$biomassSSB / mod_list_all[[1]]$quantities$biomassSSB)
biom_diff_long <- lapply(mod_list_all[2:8], function(x) x$quantities$biomass / mod_list_all[[1]]$quantities$biomass)
R_diff_long <- lapply(mod_list_all[2:8], function(x) x$quantities$R / mod_list_all[[1]]$quantities$R)

# -- All long ms models
mean(sapply(ssb_diff_long[1:7], function(x) mean(x[1,])))
mean(sapply(biom_diff_long[1:7], function(x) mean(x[1,])))
mean(sapply(R_diff_long[1:7], function(x) mean(x[1,])))

# -- No halibut Model 2
mean(ssb_diff_long[[1]][1,])
mean(biom_diff_long[[1]][1,])

# -- Halibut Models 3-8
mean(sapply(ssb_diff_long[2:7], function(x) mean(x[1,])))
mean(sapply(biom_diff_long[2:7], function(x) mean(x[1,])))


# - Medium models
ssb_diff_medium <- lapply(mod_list_all[10:11], function(x) x$quantities$biomassSSB / mod_list_all[[9]]$quantities$biomassSSB)
biom_diff_medium <- lapply(mod_list_all[10:11], function(x) x$quantities$biomass / mod_list_all[[9]]$quantities$biomass)
R_diff_medium <- lapply(mod_list_all[10:11], function(x) x$quantities$R / mod_list_all[[9]]$quantities$R)

# -- All medium models
mean(sapply(ssb_diff_medium[1:2], function(x) mean(x[1,])))
mean(sapply(biom_diff_medium[1:2], function(x) mean(x[1,])))
mean(sapply(R_diff_medium[1:2], function(x) mean(x[1,])))

# -- No halibut
mean(ssb_diff_medium[[1]][1,])
mean(biom_diff_medium[[1]][1,])


# - Short models
ssb_diff_short <- lapply(mod_list_all[13:15], function(x) x$quantities$biomassSSB / mod_list_all[[12]]$quantities$biomassSSB)
biom_diff_short <- lapply(mod_list_all[13:15], function(x) x$quantities$biomass / mod_list_all[[12]]$quantities$biomass)
R_diff_short <- lapply(mod_list_all[13:15], function(x) x$quantities$R / mod_list_all[[12]]$quantities$R)

# -- All short models
mean(sapply(ssb_diff_short[1:3], function(x) mean(x[1,])))
mean(sapply(biom_diff_short[1:3], function(x) mean(x[1,])))
mean(sapply(R_diff_short[1:3], function(x) mean(x[1,])))

# -- No halibut
mean(ssb_diff_short[[1]][1,])
mean(biom_diff_short[[1]][1,])

exp(mod_list_all[[11]]$estimated_params$ln_pop_scalar)

#######################################################
# Predation time-series
#######################################################
# Time series of total natural mortality
mod_list_avg_names <- c("Avg 1-SS", "Avg 2-MS No Halibut", "Avg 3-MS Halibut", "Avg 4-MS Relative Halibut")
# -- Model average
# Pollock
zmax <- max(sapply(mod_list_avg, function(x) max(x$quantities$M[1,1,1:10,])))

for(i in 1:length(mod_list_avg)){
  plot_mortality(Rceattle = mod_list_avg[[i]],
                 file = paste0("Results/Figures/M2 Plots/Pollock/Avg_",i),
                 incl_proj = FALSE,
                 zlim = c(0,zmax), minyr = 1996,
                 contour = FALSE, spp = 1, maxage = 10,
                 title = paste0("Model ",mod_list_avg_names[i]), height = 3)
  
}

# Cod
zmax <- max(sapply(mod_list_avg, function(x) max((x$quantities$M[3,1,1:12,]))))

for(i in 1:length(mod_list_avg)){
  plot_mortality(Rceattle = mod_list_avg[[i]],
                 file = paste0("Results/Figures/M2 Plots/Cod/Avg_",i),
                 incl_proj = FALSE,
                 zlim = c(0, zmax), minyr = 1996,
                 contour = FALSE, spp = 3, maxage = 12, log = FALSE,
                 title = paste0("Model ",mod_list_avg_names[i]), height =  3)
  
}

# ATF
zmax <- max(sapply(mod_list_avg, function(x) max((x$quantities$M[2,,1:30,]))))

for(i in 1:length(mod_list_avg)){
  plot_mortality(Rceattle = mod_list_avg[[i]],
                 file = paste0("Results/Figures/M2 Plots/ATF/Avg_",i),
                 incl_proj = FALSE,
                 zlim = c(0,zmax), minyr = 1996,
                 contour = FALSE, spp = 2, maxage = 30, log = FALSE,
                 title = paste0("Model ",mod_list_avg_names[i]))
  
}



# -- Individual models
# Pollock
zmax <- max(sapply(mod_list_all, function(x) max(x$quantities$M[1,1,1:10,])))

for(i in 1:length(mod_list_all)){
  plot_mortality(Rceattle = mod_list_all[[i]],
                 file = paste0("Results/Figures/M2 Plots/Pollock/18.5.1_mod_",i),
                 incl_proj = FALSE,
                 zlim = c(0,zmax),
                 contour = FALSE, spp = 1, maxage = 10,
                 title = paste0("Model ",mod_names_all[i]), height = ifelse(i < 9, 2.3, 3))
  
}

# Cod
zmax <- max(sapply(mod_list_all, function(x) max((x$quantities$M[3,1,1:12,]))))

for(i in 1:length(mod_list_all)){
  plot_mortality(Rceattle = mod_list_all[[i]],
                 file = paste0("Results/Figures/M2 Plots/Cod/18.5.1_mod_",i),
                 incl_proj = FALSE,
                 zlim = c(0, zmax),
                 contour = FALSE, spp = 3, maxage = 12, log = FALSE,
                 title = paste0("Model ",mod_names_all[i]), height = ifelse(i < 9, 2.3, 3))
  
}

# ATF
zmax <- max(sapply(mod_list_all, function(x) max((x$quantities$M[2,,1:30,]))))

for(i in 1:length(mod_list_all)){
  plot_mortality(Rceattle = mod_list_all[[i]],
                 file = paste0("Results/Figures/M2 Plots/ATF/18.5.1_mod_",i),
                 incl_proj = FALSE,
                 zlim = c(0,zmax),
                 contour = FALSE, spp = 2, maxage = 30, log = FALSE,
                 title = paste0("Model ",mod_names_all[i]), height = ifelse(i < 9, 4, 6))
  
}


#######################################################
# M-at-age
#######################################################
Mmat <- matrix(NA, nrow = 22, ncol = 4*4)
ind = 1

for(i in 1:length(mod_list_avg)){
  nyrs <- dim(mod_list_avg[[i]]$quantities$M)[4] - 1 # avoid projection years where no halibut
  Mmat[1,ind] = mod_list_avg[[i]]$quantities$M1[1,1,1]
  Mmat[2:11,ind] = rowMeans(mod_list_avg[[i]]$quantities$M[1,1,1:10,1:nyrs])
  ind = ind+1
  
  
  Mmat[1,ind] = mod_list_avg[[i]]$quantities$M1[2,1,1]
  Mmat[2:22,ind] = rowMeans(mod_list_avg[[i]]$quantities$M[2,1,1:21,1:nyrs])
  ind = ind+1
  
  Mmat[1,ind] = mod_list_avg[[i]]$quantities$M1[2,2,1]
  Mmat[2:22,ind] = rowMeans(mod_list_avg[[i]]$quantities$M[2,2,1:21,1:nyrs])
  ind = ind+1
  
  Mmat[1,ind] = mod_list_avg[[i]]$quantities$M1[3,1,1]
  Mmat[2:13,ind] = rowMeans(mod_list_avg[[i]]$quantities$M[3,1,1:12,1:nyrs])
  ind = ind+1
}
Mmat <- round(Mmat, 3)
Mmat <- as.data.frame(Mmat)
Mmat <- cbind(c("M1", 1:21), Mmat)
Mmat <- rbind(c("NA", rep(c("Pollock", "ATF Fem", "ATF Male", "Cod"), 4)), Mmat)
colnames(Mmat) <- c("Age", rep("Avg1", 4),rep("Avg2", 4),rep("Avg3", 4),rep("Avg4", 4))
write.csv(Mmat, file = "Results/Tables/Model_average_average_M_at_age.csv")



#######################################################
# Average M
#######################################################
nll_all$M_pollock = sapply(mod_list_all[1:15], function(x) mean(x$quantities$M[1,1,1:10,]))
nll_all$M_atff = sapply(mod_list_all[1:15], function(x) mean(x$quantities$M[2,1,1:21,]))
nll_all$M_atfm = sapply(mod_list_all[1:15], function(x) mean(x$quantities$M[2,2,1:21,]))
nll_all$M_cod = sapply(mod_list_all[1:15], function(x) mean(x$quantities$M[3,1,1:12,]))

nll_all$M2_pollock = sapply(mod_list_all[1:15], function(x) mean(x$quantities$M2[1,1,1:3,]))
nll_all$M2_atff = sapply(mod_list_all[1:15], function(x) mean(x$quantities$M2[2,1,1:3,]))
nll_all$M2_atfm = sapply(mod_list_all[1:15], function(x) mean(x$quantities$M2[2,2,1:3,]))
nll_all$M2_cod = sapply(mod_list_all[1:15], function(x) mean(x$quantities$M2[3,1,1:3,]))

nll_all$M1_pollock = sapply(mod_list_all[1:15], function(x) mean(x$quantities$M1[1,1,1:10]))
nll_all$M1_atff = sapply(mod_list_all[1:15], function(x) mean(x$quantities$M1[2,1,1:21]))
nll_all$M1_atfm = sapply(mod_list_all[1:15], function(x) mean(x$quantities$M1[2,2,1:21]))
nll_all$M1_cod = sapply(mod_list_all[1:15], function(x) mean(x$quantities$M1[3,1,1:12]))

write.csv(nll_all, "Results/Tables/18.5.1.all_model_aic_nll.csv")

# B_eaten average
c(mean(sapply(mod_list_all[c(2:8, 10:11, 13:15)], function(x) mean(colSums(x$quantities$B_eaten[1,1,1:10,1:length(x$data_list$styr:x$data_list$endyr)])))),
  mean(sapply(mod_list_all[c(2:8,10:11, 13:15)], function(x) mean(colSums(x$quantities$B_eaten[2,,,1:length(x$data_list$styr:x$data_list$endyr)])))),
  mean(sapply(mod_list_all[c(2:8,10:11, 13:15)], function(x) mean(colSums(x$quantities$B_eaten[3,1,,1:length(x$data_list$styr:x$data_list$endyr)])))))
