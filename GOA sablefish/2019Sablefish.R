library(Rceattle)
library(readxl)

################################################
# Data
################################################
mydata_sablefish <- Rceattle::read_data( file = "Data/2019Sablefish.xlsx")

################################################
# Fit single-species models
################################################
base <- Rceattle::fit_mod(data_list = mydata_sablefish,
                               inits = NULL, # Initial parameters = 0
                               file = NULL, # Don't save
                               estimateMode = 1, # Estimate
                               random_rec = FALSE, # No random recruitment
                               random_sel = FALSE,
                               msmMode = 0, # Single species mode
                               verbose = 2,
                               use_gradient = TRUE,
                               phase = "default")

plot_biomass(base)
plot_catch(base)
plot_recruitment(base)


gradient_check <- data.frame(names = names(base$obj$par), par = base$obj$par, gr = as.numeric(base$obj$gr()))
gradient_check

gradientList <- base$obj$env$parList(as.numeric(base$obj$gr()))
gradientList$sel_inf[1,,]
gradientList$sel_inf[2,,]


base$quantities$jnll_comp
sum()

#8,9,12,13 comp data have issues
check <- c(8,9,12,13)
mydata_sablefish$fleet_control$Fleet_type[check]

#
base$data_list$comp_data[which(base$data_list$comp_data$Fleet_code %in% check),9:69]
base$quantities$comp_hat[which(base$data_list$comp_data$Fleet_code %in% check),1:60]

sum(base$data_list$comp_data[which(base$data_list$comp_data$Fleet_code %in% check),9:38] - base$quantities$comp_hat[which(base$data_list$comp_data$Fleet_code %in% check),1:30])
base$data_list$comp_data[which(base$data_list$comp_data$Fleet_code %in% check),9:38]
sum(base$quantities$comp_hat[which(base$data_list$comp_data$Fleet_code %in% check),1:30])
sum(base$quantities$comp_obs[which(base$data_list$comp_data$Fleet_code %in% check),1:30])
sum(base$quantities$comp_obs[which(base$data_list$comp_data$Fleet_code %in% check),])
base$quantities$offset
base$data_list$fleet_control$Comp_weights


jnll_comp(2, flt) -= comp_weights(flt) * Type(comp_n(comp_ind, 1)) * (comp_obs(comp_ind, ln) + 0.00001) * log(comp_hat(comp_ind, ln) + 0.00001) ;


base$quantities$sel[8,,,1]
base$quantities$sel[9,,,1]
base$quantities$sel[12,,,1]
base$quantities$sel[13,,,1]
base$quantities$comp_hat[which(base$data_list$comp_data$Fleet_code %in% check),]


base$quantities$jnll_comp
base$quantities$jnll_comp



# -- Plot
plot_biomass(base)
plot_ssb(base)
plot_recruitment(base)
plot_catch(base)
plot_selectivity(base)
plot_logindex(base)

# -- Reweight
# Update composition weights using McAllister-Ianelli method
hake_reweighted <- base
for(i in 1:3){
    mydata_sablefish_weighted <- mydata_sablefish
    mydata_sablefish_weighted$fleet_control$Comp_weights <- hake_reweighted$data_list$fleet_control$Est_weights_mcallister

    hake_reweighted <- Rceattle::fit_mod(data_list = mydata_sablefish_weighted,
                                         inits = hake_reweighted$estimated_params, # Initial parameters = 0
                                         file = NULL, # Don't save
                                         debug = FALSE, # Estimate
                                         random_rec = FALSE, # No random recruitment
                                         msmMode = 0, # Single species mode
                                         phase = NULL,
                                         verbose = 1)
}

plot_biomass(hake_reweighted)
plot_ssb(hake_reweighted)
plot_recruitment(base)
plot_catch(base)
plot_selectivity(base)
plot_index(base)


# - Fix N
mydata_sablefish_fixed <- mydata_sablefish
mydata_sablefish_fixed$estDynamics = 1
check <- rearrange_dat(mydata_sablefish_fixed)
params <- build_params(mydata_sablefish_fixed)

hake_fixed <- Rceattle::fit_mod(data_list = mydata_sablefish_fixed,
                                inits = base$estimated_params, # Initial parameters = 0
                                map = NULL,
                                file = NULL, # Don't save
                                debug = 1, # Estimate
                                random_rec = FALSE, # No random recruitment
                                msmMode = 0, # Single species mode
                                verbose = 2,
                                phase = NULL)
param_check <- build_params(mydata_sablefish_fixed)
mydata_sablefish_fixed$msmMode = 0
map_check <- build_map(mydata_sablefish_fixed, param_check, debug = FALSE, random_rec = FALSE)

size <- c()
size2 <- c()
for(i in 1:length(param_check)){
    nametmp <- names(param_check)[i]
    size[i] <- sum(dim(map_check[[1]][nametmp]) != dim(base$map[[1]][nametmp]))
    size2[i] <- sum(length(map_check[[1]][nametmp]) != length(base$map[[1]][nametmp]))
}
cbind(size,size2, names(param_check))

# -- Plot
mod_list <- list(base, hake_fixed)
mod_names = c("Base", "Fixed")
plot_biomass(mod_list, model_names = mod_names)
plot_ssb(mod_list, model_names = mod_names)
plot_recruitment(mod_list, model_names = mod_names)
plot_catch(mod_list, model_names = mod_names)
plot_selectivity(mod_list, model_names = mod_names)
plot_logindex(mod_list, model_names = mod_names)

persp(x = 1:8, y = 1966:2019, z = base$quantities$sel[1, 1,1:8,], theta = 220, zlab = "Selectivity", xlab = "Age")
