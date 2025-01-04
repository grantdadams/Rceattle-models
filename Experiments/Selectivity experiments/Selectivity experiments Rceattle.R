
library(ggplot2)
library(Rceattle)
library(dplyr)
library(TMB)
library(numDeriv)
library(tidyr)
library(cowplot)


# Data ----
# Example
data("GOA2018SS")
GOA2018SS$est_sex_ratio <- rep(0, 3)
GOA2018SS$spnames <- paste("GOA", GOA2018SS$spnames)

data("BS2017SS")
BS2017SS$spnames <- paste("EBS", BS2017SS$spnames)
colnames(BS2017SS$fleet_control)[2:ncol(BS2017SS$fleet_control)] <- c("EBS pollock fishery",
                                                                      "EBS cod fishery",
                                                                      "EBS atf fishery",
                                                                      "EBS pollock bottom trawl",
                                                                      "EBS cod bottom trawl",
                                                                      "EBS atf bottom trawl",
                                                                      "EBS pollock EIT")

BS2017SS$est_sex_ratio <- rep(0, 3)


# Estimation ----
# * GOA ----
ss_run <- Rceattle::fit_mod(data_list = GOA2018SS,
                            inits = NULL, # Initial parameters = 0
                            file = NULL, # Don't save
                            estimateMode = 0, # Estimate
                            random_rec = FALSE, # No random recruitment
                            msmMode = 0, # Single species mode
                            phase = "default",
                            verbose = 1)


ss_run2 <- Rceattle::fit_mod(data_list = GOA2018SS,
                             TMBfilename = "src/ceattle_v01_11_sel_norm",
                             inits = NULL, # Initial parameters = 0
                             file = NULL, # Don't save
                             estimateMode = 0, # Estimate
                             random_rec = FALSE, # No random recruitment
                             msmMode = 0, # Single species mode
                             phase = "default",
                             verbose = 1)

GOA2018SS$sel_age <- apply(ss_run2$quantities$sel[,1,,1], 1, which.max) - 1
ss_run3 <- Rceattle::fit_mod(data_list = GOA2018SS,
                             TMBfilename = "src/ceattle_v01_11_sel_norm_age",
                             inits = NULL, # Initial parameters = 0
                             file = NULL, # Don't save
                             estimateMode = 0, # Estimate
                             random_rec = FALSE, # No random recruitment
                             msmMode = 0, # Single species mode
                             phase = "default",
                             verbose = 1)


t(rowsum(t(ss_run3$quantities$jnll_comp), ss_run3$data_list$fleet_control$Species))



# * EBS ----
ebs_run <- Rceattle::fit_mod(data_list = BS2017SS,
                            inits = NULL, # Initial parameters = 0
                            file = NULL, # Don't save
                            estimateMode = 0, # Estimate
                            random_rec = FALSE, # No random recruitment
                            msmMode = 0, # Single species mode
                            phase = "default",
                            verbose = 1)


ebs_run2 <- Rceattle::fit_mod(data_list = BS2017SS,
                             TMBfilename = "src/ceattle_v01_11_sel_norm",
                             inits = NULL, # Initial parameters = 0
                             file = NULL, # Don't save
                             estimateMode = 0, # Estimate
                             random_rec = FALSE, # No random recruitment
                             msmMode = 0, # Single species mode
                             phase = "default",
                             verbose = 1)

BS2017SS$sel_age <- apply(ebs_run2$quantities$sel[,1,,1], 1, which.max) - 1
ebs_run3 <- Rceattle::fit_mod(data_list = BS2017SS,
                             TMBfilename = "src/ceattle_v01_11_sel_norm_age",
                             inits = NULL, # Initial parameters = 0
                             file = NULL, # Don't save
                             estimateMode = 0, # Estimate
                             random_rec = FALSE, # No random recruitment
                             msmMode = 0, # Single species mode
                             phase = "default",
                             verbose = 1)

# Plots ----
plot_biomass(list(ss_run, ss_run2, ss_run3), model_names = c("1. Norm by max", "2. No norm", "3. Norm by age"), add_ci = T)
plot_f(list(ss_run, ss_run2, ss_run3), model_names = c("1. Norm by max", "2. No norm", "3. Norm by age"))
plot_catch(list(ss_run, ss_run2, ss_run3), model_names = c("1. Norm by max", "2. No norm", "3. Norm by age"))

dev.off()
plot_selectivity(list(ss_run, ss_run2, ss_run3), model_names = c("1. Norm by max", "2. No norm", "3. Norm by age"))
plot_selectivity(list(ss_run2, ss_run, ss_run2, ss_run3), model_names = c("1. Norm by max", "2. No norm", "3. Norm by age"))
plot_selectivity(list(ss_run3, ss_run, ss_run2, ss_run3), model_names = c("1. Norm by max", "2. No norm", "3. Norm by age"))

plot_selectivity(ss_run)
plot_selectivity(ss_run2)
plot_selectivity(ss_run3)



plot_f(list(ebs_run, ebs_run2, ebs_run3), model_names = c("1. Norm by max", "2. No norm", "3. Norm by age"))
plot_catch(list(ebs_run, ebs_run2, ebs_run3), model_names = c("1. Norm by max", "2. No norm", "3. Norm by age"))
plot_biomass(list(ebs_run, ebs_run2, ebs_run3), model_names = c("1. Norm by max", "2. No norm", "3. Norm by age"), add_ci = T)

dev.off()
ebs_run$quantities$sel[7,,,] <- 0:1
ebs_run2$quantities$sel[7,,,] <- 0:1
ebs_run3$quantities$sel[7,,,] <- 0:1
plot_selectivity(list(ebs_run, ebs_run2, ebs_run3), model_names = c("1. Norm by max", "2. No norm", "3. Norm by age"))
plot_selectivity(list(ebs_run2, ebs_run, ebs_run2, ebs_run3), model_names = c("1. Norm by max", "2. No norm", "3. Norm by age"))
plot_selectivity(list(ebs_run3, ebs_run, ebs_run2, ebs_run3), model_names = c("1. Norm by max", "2. No norm", "3. Norm by age"))


plot_selectivity(ebs_run)
plot_selectivity(ebs_run2)
plot_selectivity(ebs_run3)


# Likelihoods ----
get_quantities <- function(model, model_name = "value"){

  yrs <- model$data_list$styr:model$data_list$endyr

  ll_comp <- t(rowsum(t(model$quantities$jnll_comp[c(1:9, 13),]), model$data_list$fleet_control$Species))[-c(4),]
  ll_comp <- rbind(ll_comp, model$quantities$jnll_comp[c(11:12),1:3])
  rownames_ll <- rownames(ll_comp)


  ll_comp <- rbind(round(ll_comp, 2),
                   round(colSums(ll_comp), 2),
                   round(model$quantities$SB0[,ncol(model$quantities$SB0)], 0),
                   round(rowMeans(model$quantities$R[,1:length(yrs)], 0))
                   )
  rownames(ll_comp) <- c(rownames_ll, "JNLL", "SB0", "meanR")
  ll_comp <- as.data.frame(ll_comp)
  colnames(ll_comp) <- model$data_list$spnames
  ll_comp$quantity <- rownames(ll_comp)
  ll_comp$RowOrder <- 1:nrow(ll_comp)

  ll_comp <- ll_comp %>%
    pivot_longer(!c(quantity, RowOrder), names_to = "Species", values_to = model_name) %>%
    arrange(Species, RowOrder)

  return(as.data.frame(ll_comp))
}


goa_output <- get_quantities(ss_run, model_name = "1. Norm by max") %>%
  full_join(get_quantities(ss_run2, model_name = "2. No norm")) %>%
  full_join(get_quantities(ss_run3, model_name = "3. Norm by age"))
write.csv(goa_output, file = "Results/GOA_likelihood_components.csv")

ebs_output <- get_quantities(ebs_run, model_name = "1. Norm by max") %>%
  full_join(get_quantities(ebs_run2, model_name = "2. No norm")) %>%
  full_join(get_quantities(ebs_run3, model_name = "3. Norm by age"))
write.csv(ebs_output, file = "Results/EBS_likelihood_components.csv")


# Gradients ----
# * GOA ----
# Divide by max
grad_mle <- ss_run$obj$gr(ss_run$opt$par)
grad1 <- numDeriv::grad(func = ss_run$obj$f, x = ss_run$opt$par)
max(abs(grad1))
ss_run$opt$max_gradient
max(abs(grad_mle))
grad1 - grad_mle
names(grad1)[which(abs(grad1) == max(abs(grad1)))]

# No divide
grad_mle2 <- ss_run2$obj$gr(ss_run2$opt$par)
grad2 <- numDeriv::grad(func = ss_run2$obj$f, x = ss_run2$opt$par)
max(abs(grad2))
ss_run2$opt$max_gradient
max(abs(grad_mle2))
grad2 - grad_mle2
names(grad2)[which(abs(grad2) == max(abs(grad2)))]

# Divide by age
grad_mle3 <- ss_run3$obj$gr(ss_run3$opt$par)
grad3 <- numDeriv::grad(func = ss_run3$obj$f, x = ss_run3$opt$par)
max(abs(grad3))
ss_run3$opt$max_gradient
max(abs(grad_mle3))
grad3 - grad_mle3
names(grad3)[which(abs(grad3) == max(abs(grad3)))]

# Plot
goa_results <- data.frame(Param = names(ss_run$opt$par),
                      MLEmodel1 = ss_run$opt$par,
                      ADgrModel1 = c(ss_run$obj$gr(ss_run$opt$par)),
                      FDgrModel1 = grad1,
                      InitModel1 = ss_run$obj$par,
                      ADgrInitModel1 = c(ss_run$obj$gr(ss_run$obj$par)),
                      FDgrInitModel1 = numDeriv::grad(func = ss_run$obj$f, x = ss_run$obj$par),
                      MLEmodel2 = ss_run2$opt$par,
                      ADgrModel2 = c(ss_run2$obj$gr(ss_run2$opt$par)),
                      FDgrModel2 = grad2,
                      InitModel2 = ss_run2$obj$par,
                      ADgrInitModel2 = c(ss_run2$obj$gr(ss_run2$obj$par)),
                      FDgrInitModel2 = numDeriv::grad(func = ss_run2$obj$f, x = ss_run2$obj$par),
                      MLEmodel3 = ss_run3$opt$par,
                      ADgrModel3 = c(ss_run3$obj$gr(ss_run3$opt$par)),
                      FDgrModel3 = grad3,
                      InitModel3 = ss_run3$obj$par,
                      ADgrInitModel3 = c(ss_run3$obj$gr(ss_run3$obj$par)),
                      FDgrInitModel3 = numDeriv::grad(func = ss_run3$obj$f, x = ss_run3$obj$par))

# -- MLEs
goa_results_mle <- goa_results %>%
  mutate(ParN = 1:n(),
         ParName = paste0(ParN, Param),
         GRdiffModel1 = ADgrModel1 - FDgrModel1,
         GRdiffModel2 = ADgrModel2 - FDgrModel2,
         GRdiffModel3 = ADgrModel3 - FDgrModel3) %>%
  select(ParName, GRdiffModel1, GRdiffModel2, GRdiffModel3) %>%
  rename("1. Norm by max" = GRdiffModel1, "2. No norm" = GRdiffModel2, "3. Norm by age" = GRdiffModel3) %>%
  pivot_longer(!ParName, names_to = "Model", values_to = "Difference")

goa_results_mle %>%
  ggplot(aes(x = ParName, y = log10(abs(Difference)), colour = Model)) +
  geom_point() +
  ylab("log10 abs gradient difference at MLE") +
  facet_grid(~Model) +
  theme(legend.position="none")

# -- Inits
goa_results_init <- goa_results %>%
  mutate(ParN = 1:n(),
         ParName = paste0(ParN, Param),
         GRdiffModel1 = ADgrInitModel1 - FDgrInitModel1,
         GRdiffModel2 = ADgrInitModel2 - FDgrInitModel2,
         GRdiffModel3 = ADgrInitModel3 - FDgrInitModel3) %>%
  select(ParName, GRdiffModel1, GRdiffModel2, GRdiffModel3) %>%
  rename("1. Norm by max" = GRdiffModel1, "2. No norm" = GRdiffModel2, "3. Norm by age" = GRdiffModel3) %>%
  pivot_longer(!ParName, names_to = "Model", values_to = "Difference")

goa_results_init %>%
  ggplot(aes(x = ParName, y = log10(abs(Difference)), colour = Model)) +
  geom_point() +
  ylab("log10 abs gradient difference at init") +
  facet_grid(~Model) +
  theme(legend.position="none")




# * EBS ----
# Divide by max
grad_mle <- ebs_run$obj$gr(ebs_run$opt$par)
grad1 <- numDeriv::grad(func = ebs_run$obj$f, x = ebs_run$opt$par)
max(abs(grad1))
ebs_run$opt$max_gradient
max(abs(grad_mle))
grad1 - grad_mle
names(grad1)[which(abs(grad1) == max(abs(grad1)))]

# No divide
grad_mle2 <- ebs_run2$obj$gr(ebs_run2$opt$par)
grad2 <- numDeriv::grad(func = ebs_run2$obj$f, x = ebs_run2$opt$par)
max(abs(grad2))
ebs_run2$opt$max_gradient
max(abs(grad_mle2))
grad2 - grad_mle2
names(grad2)[which(abs(grad2) == max(abs(grad2)))]

# Divide by age
grad_mle3 <- ebs_run3$obj$gr(ebs_run3$opt$par)
grad3 <- numDeriv::grad(func = ebs_run3$obj$f, x = ebs_run3$opt$par)
max(abs(grad3))
ebs_run3$opt$max_gradient
max(abs(grad_mle3))
grad3 - grad_mle3
names(grad3)[which(abs(grad3) == max(abs(grad3)))]

# Plot
ebs_results <- data.frame(Param = names(ebs_run$opt$par),
                          MLEmodel1 = ebs_run$opt$par,
                          ADgrModel1 = c(ebs_run$obj$gr(ebs_run$opt$par)),
                          FDgrModel1 = grad1,
                          InitModel1 = ebs_run$obj$par,
                          ADgrInitModel1 = c(ebs_run$obj$gr(ebs_run$obj$par)),
                          FDgrInitModel1 = numDeriv::grad(func = ebs_run$obj$f, x = ebs_run$obj$par),
                          MLEmodel2 = ebs_run2$opt$par,
                          ADgrModel2 = c(ebs_run2$obj$gr(ebs_run2$opt$par)),
                          FDgrModel2 = grad2,
                          InitModel2 = ebs_run2$obj$par,
                          ADgrInitModel2 = c(ebs_run2$obj$gr(ebs_run2$obj$par)),
                          FDgrInitModel2 = numDeriv::grad(func = ebs_run2$obj$f, x = ebs_run2$obj$par),
                          MLEmodel3 = ebs_run3$opt$par,
                          ADgrModel3 = c(ebs_run3$obj$gr(ebs_run3$opt$par)),
                          FDgrModel3 = grad3,
                          InitModel3 = ebs_run3$obj$par,
                          ADgrInitModel3 = c(ebs_run3$obj$gr(ebs_run3$obj$par)),
                          FDgrInitModel3 = numDeriv::grad(func = ebs_run3$obj$f, x = ebs_run3$obj$par))

# -- MLEs
ebs_results_mle <- ebs_results %>%
  mutate(ParN = 1:n(),
         ParName = paste0(ParN, Param),
         GRdiffModel1 = ADgrModel1 - FDgrModel1,
         GRdiffModel2 = ADgrModel2 - FDgrModel2,
         GRdiffModel3 = ADgrModel3 - FDgrModel3) %>%
  select(ParName, GRdiffModel1, GRdiffModel2, GRdiffModel3) %>%
  rename("1. Norm by max" = GRdiffModel1, "2. No norm" = GRdiffModel2, "3. Norm by age" = GRdiffModel3) %>%
  pivot_longer(!ParName, names_to = "Model", values_to = "Difference")

ebs_results_mle %>%
  ggplot(aes(x = ParName, y = log10(abs(Difference)), colour = Model)) +
  geom_point() +
  ylab("log10 abs gradient difference at MLE") +
  facet_grid(~Model) +
  theme(legend.position="none")

# -- Inits
ebs_results_init <- ebs_results %>%
  mutate(ParN = 1:n(),
         ParName = paste0(ParN, Param),
         GRdiffModel1 = ADgrInitModel1 - FDgrInitModel1,
         GRdiffModel2 = ADgrInitModel2 - FDgrInitModel2,
         GRdiffModel3 = ADgrInitModel3 - FDgrInitModel3) %>%
  select(ParName, GRdiffModel1, GRdiffModel2, GRdiffModel3) %>%
  rename("1. Norm by max" = GRdiffModel1, "2. No norm" = GRdiffModel2, "3. Norm by age" = GRdiffModel3) %>%
  pivot_longer(!ParName, names_to = "Model", values_to = "Difference")

ebs_results_init %>%
  ggplot(aes(x = ParName, y = log10(abs(Difference)), colour = Model)) +
  geom_point() +
  ylab("log10 abs gradient difference at init") +
  facet_grid(~Model) +
  theme(legend.position="none")
