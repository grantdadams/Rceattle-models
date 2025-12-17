##############################################################
### SIMPLE TESTINGS
##############################################################

library(Rceattle)
set.seed(123)

ATF_hakedata <- Rceattle::read_data(file = "dev/070725_ATF_Hake_model_Neff.xlsx")

ss_run <- Rceattle::fit_mod(data_list = ATF_hakedata,
                            inits = NULL, # Initial parameters = 0
                            file = NULL, # Don't save
                            estimateMode = 0, # Estimate
                            random_rec = FALSE, # No random recruitment
                            msmMode = 0, # Single species mode
                            phase = TRUE,
                            verbose = 1)


ms_run <- Rceattle::fit_mod(data_list = ATF_hakedata,
                            inits = ss_run$estimated_params, # Initial parameters from single species ests
                            M1Fun = build_M1(M1_model = 0,  #do not estimate mortality!
                                             updateM1 = FALSE,
                                             M1_use_prior = FALSE,
                                             M2_use_prior = FALSE),
                            file = NULL, # Don't save
                            estimateMode = 0, # Estimate
                            niter = 3, # 3 iterations around population and predation dynamics
                            random_rec = FALSE, # No random recruitment
                            msmMode = 1, # MSVPA based
                            suitMode = 0, # empirical suitability
                            initMode = 2, # Fished start with init devs
                            verbose = 1)


# Index and catch fits can show multiple models
plot_index(list(ss_run, ms_run), model_names = 1:2)
plot_logindex(list(ss_run, ms_run), model_names = 1:2)
plot_catch(list(ss_run, ms_run), model_names = 1:2)

# Composition plots can be only show one model at a time
plot_comp(ss_run) # - Produces many plots!

# Plot mortality-at-age
library(ggplot2)
plot_mortality(ms_run)


#####################################################################
# FIX SUITABILITY AND SUM across prey ages (NEW PART)
# Create initial parameter list:
test_data <- ATF_hakedata

inits = ms_run$estimated_params
map = ms_run$map # gam_a, gam_b, and log_phi are turned off here

# Create a list prey size preference
# Set weight ratio parameters
inits$log_gam_a = c(0, 3.006)  # Mean log weight ratio for ATF, 0 for other species (pred/prey)
inits$log_gam_b = c(0, 1.887)  # Standard deviation of log weight ratio for ATF, 0 for other species

# Set vulnerability matrix
inits$log_phi #Currently all set to 0.5 (keep it)
inits$log_phi[1,2] <- -999 #Fixing so hake do not prey on ATF
inits$log_phi[2,2] <- -999 # Set ATF do not feed on ATF
#inits$log_phi[2,1] <- 4

# Do this to estimate vulnerability and log_phi :
map$mapList$log_phi[] <- 1:length(map$mapList$log_phi) # Unique number for each parameter
map$mapList$log_phi[1,1] <- NA #so we dont estimate on hake on hake
map$mapList$log_phi[1,2] <- NA #so we dont estimate on hake on atf
map$mapList$log_phi[2,2] <- NA #so we dont estimate atf on atf

map$mapFactor$log_phi <- factor(map$mapList$log_phi)

atf_diet <- test_data$diet_data %>%
    dplyr::filter(Pred == 2 & Pred_sex == 1)

diet <- test_data$diet_data %>%
    dplyr::filter(Pred != 2)

test_data$diet_data <- diet %>%
    rbind(atf_diet %>%
              dplyr::mutate(Pred_sex = 0))

#In this model we are estimating M1
run_ms_LN_M <- Rceattle::fit_mod(data_list = test_data,
                                 inits = inits, # Initial parameters from single species ests
                                 map = map,
                                 M1Fun = build_M1(M1_model = 0,
                                                  updateM1 = TRUE,
                                                  M1_use_prior = FALSE,
                                                  M2_use_prior = FALSE),
                                 file = NULL, # Don't save
                                 estimateMode = 0, # Estimate
                                 niter = 3, # 3 iterations around population and predation dynamics
                                 random_rec = FALSE, # No random recruitment
                                 msmMode = 1, # MSVPA based
                                 loopnum = 5,
                                 phase = TRUE,
                                 suitMode = c(0, 4), # empirical + LN suitability
                                 initMode = 2,
                                 verbose = 1)


plot_b_eaten_prop(run_ms_LN_M)
dimnames(run_ms_LN_M$quantities$B_eaten)
hake_eaten_by_atf <- c()
for(yr in 1:dim(run_ms_LN_M$quantities$B_eaten)[5]){
    hake_eaten_by_atf[yr] <- sum(run_ms_LN_M$quantities$B_eaten[c(2,4), 1, , , yr])
}

run_ms_LN_M$quantities$jnll #3602.67
run_ms_LN_M$estimated_params$log_phi #0.5, 29.65
run_ms_LN_M$quantities$vulnerability #0, 1
run_ms_LN_M$quantities$vulnerability_other #0, 1
run_ms_LN_M$estimated_params$diet_comp_weights #1,1
run_ms_LN_M$data_list$Diet_weights_mcallister #1.39011120, 0.002650715


###plots
##plot single species models
mod_list <- list(ss_run, ms_run, run_ms_LN_M)
mod_names <- c("ss_run", "ms_run", "run_ms_LN_M")

# Plot biomass trajectory
plot_biomass(Rceattle = mod_list, model_names = mod_names) #Now biomass looks alike
plot_biomass(Rceattle = mod_list, model_names = mod_names, add_ci = TRUE)
#plot_depletionSSB(Rceattle = mod_list, model_names = mod_names) #this looks pretty different
plot_ssb(Rceattle = mod_list, model_names = mod_names, add_ci = TRUE)

diet_data <- run_ms_LN_M$data_list$diet_data
diet_data$Type <- "Obs"

diet_est <- run_ms_LN_M$data_list$diet_data
diet_est$Stomach_proportion_by_weight <- run_ms_LN_M$quantities$diet_hat[,2]
diet_est$Type <- "Est"
diet_data <-  rbind(diet_data, diet_est) %>%
    dplyr::filter(Pred == 2)

diet_data %>%
    ggplot(aes(x = Pred_age, y = Stomach_proportion_by_weight, colour = Type)) +
    geom_point() +
    facet_wrap(~Pred + Prey)

filled.contour(y = 1:test_data$nages[1], x = 1:test_data$nages[2], z = run_ms_LN_M$quantities$suitability[2,1,1:test_data$nages[2],1:test_data$nages[1],1])

filled.contour(y = 1:test_data$nages[1], x = 1:test_data$nages[2], z = run_ms_LN_M$quantities$suitability[4,1,1:test_data$nages[2],1:test_data$nages[1],1])


diet_data <- run_ms_LN_M$data_list$diet_data
diet_data$est <- run_ms_LN_M$quantities$diet_hat[,2]
diet_data <- diet_data %>%
    filter(Pred == 2)


-sum(diet_data$Sample_size * (diet_data$Stomach_proportion_by_weight + 0.00001) * log((0.5 + 0.00001)/(diet_data$Stomach_proportion_by_weight + 0.00001)))

0.5*log(0.2/0.5)





# ============================
# profile
log_phi <- seq(-10,35, length.out = 100)
mod_list <- list()

for(i in 78:length(log_phi)){
    run_ms_LN_M = run_ms_LN_M$estimated_params
    run_ms_LN_M = run_ms_LN_M$map # gam_a, gam_b, and log_phi are turned off here

    inits$log_phi[2,1] <- log_phi[i]
    map$mapList$log_phi[2,1] <- NA #so we dont estimate atf on atf

    map$mapFactor$log_phi <- factor(map$mapList$log_phi)

    #In this model we are estimating M1
    mod_list[[i]] <- Rceattle::fit_mod(data_list = test_data,
                                       inits = inits, # Initial parameters from single species ests
                                       map = map,
                                       M1Fun = build_M1(M1_model = 0,
                                                        updateM1 = TRUE,
                                                        M1_use_prior = FALSE,
                                                        M2_use_prior = FALSE),
                                       file = NULL, # Don't save
                                       estimateMode = 0, # Estimate
                                       niter = 3, # 3 iterations around population and predation dynamics
                                       random_rec = FALSE, # No random recruitment
                                       msmMode = 1, # MSVPA based
                                       loopnum = 1,
                                       phase = FALSE,
                                       suitMode = c(0, 4), # empirical + LN suitability
                                       initMode = 2,
                                       getsd = FALSE,
                                       verbose = 1)
}



ll_list <- lapply(mod_list, function(x) x$quantities$unweighted_jnll_comp)
ll_pieces <- data.frame(Model = NA, Catch = NA, Survey = NA, Scomp = NA, Fcomp = NA, Sel = NA, Rec = NA, stom = NA, total = NA)
for(i in 1:length(log_phi)){
    ll_pieces[i,] <- c(i, ll_list[[i]][22], ll_list[[i]][3], ll_list[[i]][4], ll_list[[i]][24], ll_list[[i]][6], ll_list[[i]][11] + ll_list[[i]][12], ll_list[[i]][40], sum(ll_list[[i]]))
}



#=================================================================================================
### increase weigth comps =============== (I THINK THIS MAKE THINGS WORST)
test_data <- ATF_hakedata

# Fixed weights, constrained preferences
test_data$Diet_comp_weights <- c(1, 4)

inits = ms_run$estimated_params
map = ms_run$map # gam_a, gam_b, and log_phi are turned off here
#ms_run$map$mapList$log_gam_a

inits$diet_comp_weights <- c(1, 4)

# Create a list prey size preference
# Set weight ratio parameters
inits$log_gam_a = c(0, 3.006)  # Mean log weight ratio for ATF, 0 for other species (pred/prey)
inits$log_gam_b = c(0, 1.887)  # Standard deviation of log weight ratio for ATF, 0 for other species

# Set vulnerability matrix
inits$log_phi #Currently all set to 0.5 (keep it)
inits$log_phi[1,2] <- -999 #Fixing so hake do not prey on ATF
inits$log_phi[2,2] <- -999 # Set ATF do not feed on ATF
#inits$log_phi[2,1] <- 4  # set a better starting value

# Do this to estimate vulnerability and log_phi :
map$mapList$log_phi[] <- 1:length(map$mapList$log_phi) # Unique number for each parameter
map$mapList$log_phi[1,1] <- NA #so we dont estimate on hake on hake
map$mapList$log_phi[1,2] <- NA #so we dont estimate on hake on atf
map$mapList$log_phi[2,2] <- NA #so we dont estimate atf on atf
#map$mapList$log_phi[2,1] <- NA

map$mapFactor$log_phi <- factor(map$mapList$log_phi)

# Run without reweighting
run_ms_LN_M_wg <- Rceattle::fit_mod(data_list = test_data,
                                    inits = inits, # Initial parameters from single species ests
                                    map = map,
                                    M1Fun = build_M1(M1_model = 0,
                                                     updateM1 = TRUE,
                                                     M1_use_prior = FALSE,
                                                     M2_use_prior = FALSE),
                                    file = NULL, # Don't save
                                    estimateMode = 0, # Estimate
                                    niter = 3, # 3 iterations around population and predation dynamics
                                    random_rec = FALSE, # No random recruitment
                                    msmMode = 1, # MSVPA based
                                    loopnum = 5,
                                    phase = TRUE,
                                    suitMode = c(0, 4), # empirical + LN suitability
                                    initMode = 2,
                                    verbose = 1)

run_ms_LN_M_wg$quantities$jnll #5333.507
run_ms_LN_M_wg$estimated_params$log_phi #0.5, 6.10
run_ms_LN_M_wg$quantities$vulnerability #0,997
run_ms_LN_M_wg$quantities$vulnerability_other #0.00221676
run_ms_LN_M_wg$estimated_params$diet_comp_weights #1,2
run_ms_LN_M_wg$data_list$Diet_weights_mcallister #0.961507022 0.002632437

###plots
##plot single species models
mod_list <- list(run_ms_LN_M_wg, ms_run, run_ms_LN_M)
mod_names <- c("run_ms_LN_M_wg", "ms_run", "run_ms_LN_M")

# Plot biomass trajectory (CHAOS!!!)
plot_biomass(Rceattle = mod_list, model_names = mod_names) #Now biomass looks alike
plot_biomass(Rceattle = mod_list, model_names = mod_names, add_ci = TRUE)
plot_depletionSSB(Rceattle = mod_list, model_names = mod_names) #this looks pretty different
plot_ssb(Rceattle = mod_list, model_names = mod_names, add_ci = TRUE)

plot_ration(run_ms_LN_M)
plot_ration(run_ms_LN_M_wg)

#=======================================================================================
## MODEL RE-Weigthing
# This function performs iterative reweighting of diet composition weights
perform_model_reweighting <- function(initial_model, test_data, inits, map,
                                      max_iterations = 10,
                                      verbose = TRUE) {

    # Initialize storage for results
    model_runs <- list()
    iteration_metrics <- data.frame(
        iteration = integer(),
        jnll = numeric(),
        weight_change = numeric()
    )

    # Store initial model
    current_model <- initial_model
    model_runs[[1]] <- current_model

    # Get initial weights
    previous_weights <- current_model$data_list$Diet_weights_mcallister

    if(verbose) {
        cat("Starting iterative reweighting process...\n")
        cat("Initial JNLL:", current_model$quantities$jnll_comp, "\n")
    }

    # Iterative reweighting loop - runs all iterations
    for(i in 1:max_iterations) {

        if(verbose) cat("\n--- Iteration", i, "---\n")

        # Update data and inits with new weights
        test_data$Diet_comp_weights <- current_model$data_list$Diet_weights_mcallister
        inits$diet_comp_weights <- current_model$data_list$Diet_weights_mcallister

        # Fit model with updated weights
        current_model <- Rceattle::fit_mod(data_list = test_data,
                                           inits = inits, # Initial parameters from single species ests
                                           map = map,
                                           M1Fun = build_M1(M1_model = 0,
                                                            updateM1 = TRUE,
                                                            M1_use_prior = FALSE,
                                                            M2_use_prior = FALSE),
                                           file = NULL, # Don't save
                                           estimateMode = 0, # Estimate
                                           niter = 3, # 3 iterations around population and predation dynamics
                                           random_rec = FALSE, # No random recruitment
                                           msmMode = 1, # MSVPA based
                                           loopnum = 5,
                                           phase = TRUE,
                                           suitMode = c(0, 4), # empirical + LN suitability
                                           initMode = 2,
                                           verbose = 1)

        # Store model
        model_runs[[i + 1]] <- current_model

        # Get current weights
        current_weights <- current_model$data_list$Diet_weights_mcallister

        # Calculate metrics for tracking
        weight_change <- max(abs(current_weights - previous_weights))
        current_jnll <- current_model$quantities$jnll_comp

        # Store metrics
        iteration_metrics <- rbind(iteration_metrics,
                                   data.frame(
                                       iteration = i,
                                       jnll = current_jnll,
                                       weight_change = weight_change
                                   ))

        if(verbose) {
            cat("JNLL:", current_jnll, "\n")
            cat("Max weight change:", weight_change, "\n")
            cat("Vulnerability:", current_model$quantities$vulnerability, "\n")
        }

        # Update previous weights for next iteration
        previous_weights <- current_weights
    }

    if(verbose) {
        cat("\nCompleted all", max_iterations, "iterations\n")
    }

    # Return results
    return(list(
        models = model_runs,
        iteration_metrics = iteration_metrics,
        final_model = current_model,
        n_iterations = max_iterations
    ))
}

# Usage example:
reweight_results <- perform_model_reweighting(
    initial_model = run_ms_LN_M, #run_ms_LN_M_wg,
    test_data = test_data,
    inits = inits,
    map = map,
    max_iterations = 5,
    verbose = TRUE
)

rw_model <- reweight_results$final_model
all_models <- reweight_results$models

reweight_results$iteration_metrics

rw_model$quantities$jnll #2204.869
rw_model$estimated_params$log_phi #0.5, 28.39
rw_model2$quantities$vulnerability
rw_model$estimated_params$diet_comp_weights #1,1
rw_model$data_list$Diet_weights_mcallister #1.39011120, 0.05770769

###plots
##plot single species models
mod_list <- list(rw_model, run_ms_LN_M)
mod_names <- c("rw_model", "run_ms_LN_M")

# Plot biomass trajectory
plot_biomass(Rceattle = mod_list, model_names = mod_names) #Now biomass looks alike
plot_biomass(Rceattle = mod_list, model_names = mod_names, add_ci = TRUE)
plot_depletionSSB(Rceattle = mod_list, model_names = mod_names) #this looks pretty different
plot_ssb(Rceattle = mod_list, model_names = mod_names, add_ci = TRUE)

plot_ration(Rceattle = mod_list, model_names = mod_names, add_ci = TRUE)
plot_b_eaten(Rceattle = mod_list, model_names = mod_names, add_ci = TRUE)
plot_b_eaten_prop(Rceattle = mod_list, model_names = mod_names, add_ci = TRUE)
plot_(Rceattle = mod_list, model_names = mod_names, add_ci = TRUE)

# Show first 5 values for ATF (species 2, sex 1, first 5 ages, year 1:5) ## SAME VALUES
print(run_ms_LN_M_wg$quantities$consumption_at_age[2, 1, 1:5, 1:5])
print(run_ms_LN_M$quantities$consumption_at_age[2, 1, 1:5, 1:5])

#Show suitability (ATF age 5 -> hake age 1) ## SAME VALUES
print(run_ms_LN_M_wg$quantities$suitability[2, 1, 5, 1:5, 1:5])
print(run_ms_LN_M$quantities$suitability[2, 1, 5, 1:5, 1:5])

#Hake abundance (age 1, year 1) ## DIFFERENT AS EXPECTED BASE ON DIFF IN BIOMASS AND RATION
print(run_ms_LN_M_wg$quantities$avgN_at_age[1, 1, 1, 1])
print(run_ms_LN_M$quantities$avgN_at_age[1, 1, 1, 1])

#ATF abundance (age 5, year 1) ## (VALUES DO NOT CHANGE)
print(run_ms_LN_M_wg$quantities$avgN_at_age[2, 1, 5, 1])
print(run_ms_LN_M$quantities$avgN_at_age[2, 1, 5, 1])

## Vulnerability, Vulnerability-other, log_phi, diet_comps_weigh, diet_weights_mcallister DIFFER BETWEEN MODELS
