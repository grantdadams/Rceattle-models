





######################### Plots
# - SAFE vs SS
file_name <- "Figures/18.5.1/18.5.1_SAFE_vs_single_species"
mod_list_all[[1]]$quantities$biomass[1,1:42] <- colSums(mod_list_all[[1]]$quantities$biomassByage[1,3:10,1:42])
mod_list_all[[14]]$quantities$biomass[1:3,1:42] <- t(safe2018biomass[,2:4]) * 1000 * 0.453592
mod_list_all[[14]]$quantities$biomassSSB[1:3,1:42] <- t(safe2018ssb[,2:4]) * 1000 * 0.453592
plot_biomass(mod_list_all[c(1,14)], file = file_name, model_names = c("CEATTLE Single-species", "2018 SAFE (mt)"), right_adj = 0.27, line_col = c(line_col_long[1],1), species = 1)
plot_ssb(mod_list_all[c(1,14)], file = file_name, model_names = c("CEATTLE Single-species", "2018 SAFE (mt)"), right_adj = 0.27, line_col = c(line_col_long[1],1), species = 1)
plot_recruitment(mod_list_all[c(1,14)], file = file_name, add_ci = FALSE, model_names = c(mod_names_long[1], "2018 SAFE"), right_adj = 0.27, line_col = c(line_col_long[1],1), species = 1)