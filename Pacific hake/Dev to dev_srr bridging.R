# Bridging from dev branch to dev_srr branch

library(Rceattle)
library(dplyr)


################################################
# Fit in dev branch ----
################################################
# Data
hakedata <- Rceattle::read_data(file = "hake_yr24_241125.xlsx")
hakedata$fleet_control$Accumatation_age_lower <- NA
hakedata$fleet_control$Accumatation_age_upper <- NA
hakedata$fleet_control <- hakedata$fleet_control %>%
    rename(Estimate_survey_sd = Estimate_index_sd, Survey_sd_prior = Index_sd_prior) %>%
    select("Fleet_name", "Fleet_code", "Fleet_type", "Species", "Selectivity_index", "Selectivity", "Nselages", "Time_varying_sel", "Sel_sd_prior", "Age_first_selected", "Accumatation_age_lower", "Accumatation_age_upper", "Weight1_Numbers2", "Weight_index", "Age_transition_index", "Q_index", "Estimate_q", "Q_prior", "Q_sd_prior", "Time_varying_q", "Time_varying_q_sd_prior", "Estimate_survey_sd", "Survey_sd_prior", "Estimate_catch_sd", "Catch_sd_prior", "proj_F_prop", "Comp_weights") %>%
    as.data.frame()
hakedata$styr <- 1966

hake_ss <- Rceattle::fit_mod(data_list = hakedata,
                                inits = NULL, # Initial parameters = 0
                                file = "hake ss dev", # Don't save
                                estimateMode = 0,
                                verbose = 1,
                                msmMode = 0, # Single species mode
                                phase = "default")


################################################
# Fit in dev_srr branch ----
################################################

load("~/Documents/GitHub/Rceattle-models/Pacific hake/ssv10.RData")

# Data
check_data <-  mod_objects$data_list
check_data$fleet_control <- check_data$fleet_control %>%
    rename(Estimate_index_sd = Estimate_survey_sd,  Index_sd_prior = Survey_sd_prior)
check_data$fleet_control$Age_max_selected = NA
check_data$fleet_control$Comp_loglike = -1
check_data$index_data <- check_data$srv_biom
check_data$catch_data <- check_data$fsh_biom
check_data$stom_prop_data <- check_data$UobsWtAge

write_data(check_data, file = "check.xlsx")
check_data = read_data(file = "check.xlsx")


hake_ss <- Rceattle::fit_mod(data_list = check_data,
                             inits = NULL, # Initial parameters = 0
                             file = NULL, # Don't save
                             estimateMode = 0,
                             verbose = 1,
                             msmMode = 0, # Single species mode
                             phase = TRUE)

# - Compare
plot_biomass(list(mod_objects, hake_ss))
plot_biomass(list(mod_objects, hake_ss))

# Fix parameters ----
inits <- mod_objects$estimated_params

names(hake_ss$estimated_params)[!names(hake_ss$estimated_params) %in% names(inits)]

inits$R_ln_sd <- inits$ln_rec_sigma
inits$ln_rec_sigma <- NULL

inits$index_ln_q <- inits$ln_srv_q
inits$ln_srv_q <- NULL

inits$index_q_dev <- inits$ln_srv_q_dev
inits$ln_srv_q_dev <- NULL

inits$index_q_ln_sd <- inits$ln_sigma_srv_q
inits$ln_sigma_srv_q <- NULL

inits$sel_dev_ln_sd <- inits$ln_sigma_sel
inits$ln_sigma_sel <- NULL

inits$index_q_dev_ln_sd <- inits$ln_sigma_time_varying_srv_q
inits$ln_sigma_time_varying_srv_q <- NULL

inits$index_ln_sd <- inits$ln_sigma_srv_index
inits$ln_sigma_srv_index <- NULL

inits$catch_ln_sd <- inits$ln_sigma_fsh_catch
inits$ln_sigma_fsh_catch <- NULL

inits$index_q_beta <- hake_ss$estimated_params$index_q_beta

inits$beta_rec_pars <- hake_ss$estimated_params$beta_rec_pars

inits$index_q_rho <- hake_ss$estimated_params$index_q_rho

names(hake_ss$estimated_params)[!names(hake_ss$estimated_params) %in% names(inits)]
names(inits)[!names(inits) %in% names(hake_ss$estimated_params)]

inits[c("ln_pop_scalar", "ln_sex_ratio_sigma", "logH_1", "logH_1a", "logH_1b", "logH_2", "logH_3", "H_4", "log_gam_a", "log_gam_b", "log_phi")] = NULL

map <- build_map(hake_ss$data_list, inits)
names(map$mapList)[!names(map$mapList) %in% names(inits)]
names(inits)[!names(inits) %in% names(map$mapList)]

hake_ss$data_list$fleet_control$Comp_loglike <- -1
ss_init <- Rceattle::fit_mod(data_list = hake_ss$data_list,
                            inits = inits, # Initial parameters = 0
                            file = NULL, # Don't save
                            estimateMode = 3, # Estimate
                            random_rec = FALSE, # No random recruitment
                            msmMode = 0, # Single species mode
                            phase = TRUE,
                            verbose = 1)
sum(ss_init$quantities$sel-mod_objects$quantities$sel)
sum(mod_objects$quantities$NByage-ss_init$quantities$N_at_age)
sum(mod_objects$quantities$comp_hat-ss_init$quantities$comp_hat)
sum(mod_objects$quantities$comp_n-ss_init$quantities$comp_n)
sum(mod_objects$quantities$comp_obs-ss_init$quantities$comp_obs, na.rm = T)

# The difference in composition likelihoods is because v10 did not evaluate the nll for composition data if comp obs = NA or comp_hat = 0, v11 sets comp_obs to 0 where NA and evaluates

round(ss_init$quantities$jnll_comp, 4)-round(mod_objects$quantities$jnll_comp, 4)

plot_biomass(list(ss_init, mod_objects))
plot_recruitment(list(ss_init, mod_objects))

mod_objects2 <- ss_init
mod_objects2$quantities$catch_hat <- mod_objects$quantities$fsh_bio_hat
plot_catch(list(mod_objects2, ss_init))
