# unified rockfish model 
# statistical catch-at-age model generally based on the goa pop model, but works for dusky and northerns as well
# setup for a single area, single sex, single fleet, single survey 
# currently has fishery selectivity blocks available
# runs in conjunction with github.com/BenWilliams-NOAA/RTMButils
# ben.williams@noaa.gov
# 2026-04

# convert catch_wt to cv
# sqrt(1 / (catch_wt * 2))

urm <- function(pars, data) {
  # load ----
  require(RTMB)
  "c" <- RTMB::ADoverload("c")
  "[<-" <- RTMB::ADoverload("[<-")
   
  RTMB::getAll(pars, data)
 
  # setup ----
  # transform log parameters to natural scale
  M = exp(log_M)            # natural mortality
  q = exp(log_q)            # survey catchability
  slx_pars = exp(log_slx_pars) # selectivity
  F50 = exp(log_F50)
  F40 = exp(log_F40)
  F35 = exp(log_F35)
 	R_init = exp(log_mean_R_init)
  F_init = exp(log_F_init)

   # spawning adjustments
   spawn_fract = (spawn_mo - 1) / 12           # fraction of year before spawning
 
   # dimensions ----
   A = nrow(age_error)                         # ages in model
   A1 = length(ages)                           # ages in comps
   T = length(years)                           # fishery years
   Ts = sum(srv_ind)                           # survey years
   Tfa = sum(fish_age_ind)                     # fishery age comp years
   Tsa = sum(srv_age_ind)                      # survey age comp years
   Tfs = sum(fish_size_ind)                    # fishery size comp years
   L = length(length_bins)                     # lengths in comps
   n_curves = nrow(slx_pars) 									 # number of selectivity curves
   g = 0.00001                                 # small number to avoid division by zero
   Z_init = cum_Z_init = rep(0, A)													 # total mortality pre-model equilibrium
   
   # containers ----
   Bat = Cat = Nat = Fat = Zat = Sat = slx_fish = matrix(0, A, T)   # biomass, catch, numbers, F, Z, S, selectivity at age
   catch_pred = spawn_bio = tot_bio = rep(0, T) 
   srv_pred = srv_var = rep(0, Ts)          
   fish_age_pred = matrix(0, A1, Tfa)
   srv_age_pred = matrix(0, A1, Tsa)
   fish_size_pred = matrix(0, L, Tfs)
   slx_curves = matrix(0, A, n_curves)
 	 
   # priors ----
   nll_M = -RTMB::dnorm(log(M), log(mean_M), cv_M, log = TRUE)
   nll_q= -RTMB::dnorm(log(q), log(mean_q), cv_q, log = TRUE)
   nll_sigmaR= -RTMB::dnorm(log(sigmaR / mean_sigmaR), 0, cv_sigmaR, log = TRUE)
 
  wt_mature = waa * maa * sex_ratio

   # selectivity ----
   for(s in 1:n_curves) {
     slx_curves[,s] = RTMButils::get_slx(ages=1:A, type=slx_type[s], pars=slx_pars[s,], adj=1)
   }
   
   # assign to the fishery over time
   for(t in 1:T) {
     slx_fish[,t] = slx_curves[,fish_block_ind[t]]
   }
 
   # survey selectivity
   slx_srv = slx_curves[,srv_slx_ind]
 
   # mortality ----
   Ft = exp(log_mean_F + log_Ft)  # annual fishing mortality on natural scale
   for(t in 1:T){
     Fat[,t] = Ft[t] * slx_fish[,t]  # fishing mortality at age and year
     Zat[,t] = Fat[,t] + M           # total mortality at age and year
   }
   Sat = exp(-Zat)                   # survivorship at age and year
 
   # numbers-at-age ----
  
   for(t in 1:T) {
     # year-specific bias adjustment if using random effects bias_switch = 0	
     if(bias_switch == 1) {
     		bias_adj = bias_ramp[t] * ((sigmaR^2) / 2)
     		Nat[1,t] = exp(log_mean_R - bias_adj + log_Rt[(A-1+t)])  # recruitment in year t
     } else {
       Nat[1,t] = exp(log_mean_R + log_Rt[(A-1+t)])  # recruitment in year t
     }
   }
  
    # initial total mortality at age (historical pre-model equilibrium)
   for(a in 1:A) {
     Z_init[a] = M + F_init * slx_fish[a, 1] 
   }
  
  # cumulative mortality experienced by each cohort prior to Year 1
  for(a in 2:A) {
     cum_Z_init[a] = cum_Z_init[a-1] + Z_init[a-1]
  }
  
  # column 1: initial numbers-at-age for each cohort cells 1 to A-1
  for(a in 2:(A-1)) {
     Nat[a,1] = R_init * exp(-cum_Z_init[a] + log_Rt[a-1])
   }	
  
   # specific deviation for initial plus group
   Nat[A,1] = (R_init * exp(-cum_Z_init[A]) / (1 - exp(-Z_init[A]))) * exp(log_Rt[A-1])
 
   # forward
 for(t in 2:T) {
     for(a in 2:A) {
       Nat[a,t] = Nat[a-1,t-1] * Sat[a-1,t-1]       # survivors from previous age and year
     }
     Nat[A,t] = Nat[A,t] + Nat[A,t-1] * Sat[A,t-1]   # plus group 
   }
   recruits = Nat[1,]
   spawn_bio = colSums(Nat * wt_mature)
   tot_bio = colSums(Nat * waa)
 
   # mortality adjustment for last year spawning adjustment
   spawn_adj = Sat[,T]^spawn_fract
   spawn_bio[T] = sum(Nat[,T] * spawn_adj * wt_mature)
 
   # catch ----
   Cat = Fat / Zat * Nat * (1-Sat)
   catch_pred = colSums(Cat * waa)
   sigma_catch = sqrt(log(catch_cv^2 + 1.0))
   like_catch = -sum(RTMB::dnorm(log(catch_obs + g), log(catch_pred + g), sigma_catch, log = TRUE)) 
 
   # survey biomass ----
   # w/log-normal bias correction
   isrv = 1
   srv_like = 0.0
   
   for(t in 1:T) {
     if(srv_ind[t]==1) {
       srv_pred[isrv] = sum(Nat[,t] * slx_srv * waa) * q 
       log_sd = sqrt(log(1 + srv_cv[isrv]^2))
       mu = log(srv_pred[isrv] + g) - 0.5 * log_sd^2
       srv_like = srv_like -RTMB::dnorm(log(srv_obs[isrv]), mu, log_sd, log = TRUE)
       isrv = isrv + 1
     }
   }
   like_srv = srv_like * srv_wt
 
  # fishery age comp ----
   fish_age_lk = 0.0
   icomp = 1
   
   for(t in 1:T) {
     if(fish_age_ind[t] == 1) {
       # predicted age composition (with ageing error)
       fish_age_pred[,icomp] = as.vector(colSums((Cat[,t] / (sum(Cat[,t]))) * age_error))
       obs_count = fish_age_obs[,icomp] * fish_age_iss[icomp]  
       fish_age_lk = fish_age_lk - RTMB::dmultinom(x = obs_count,
        																			prob = fish_age_pred[,icomp],
     																					log = TRUE)
       icomp = icomp + 1
     }
   }
   like_fish_age = fish_age_lk * fish_age_wt # weighted fishery age comp likelihood
 
 
  # survey age comp ----
   srv_age_lk = 0.0
   icomp = 1
   
   for(t in 1:T) {
     if(srv_age_ind[t] == 1) {
       # predicted age composition (with ageing error)
       srv_age_pred[,icomp] = as.vector(colSums((Nat[,t] * slx_srv) / (sum(Nat[,t] * slx_srv)) * age_error))
       obs_count = srv_age_obs[,icomp] * srv_age_iss[icomp] 
       srv_age_lk = srv_age_lk - RTMB::dmultinom(x = obs_count,
        																		prob = srv_age_pred[,icomp],
     																				log = TRUE)
       icomp = icomp + 1
     }
   }
   like_srv_age = srv_age_lk * srv_age_wt # weighted survey age comp likelihood
 
  # fishery size comp ----
   fish_size_lk = 0.0
   icomp = 1
   
   for(t in 1:T) {
     if(fish_size_ind[t] == 1) {
       fish_size_pred[,icomp] = as.vector(colSums((Cat[,t] / (sum(Cat[,t]))) * saa_array[,,fish_saa_ind[t]]))
       obs_count = fish_size_obs[,icomp] * fish_size_iss[icomp] 
       fish_size_lk = fish_size_lk - RTMB::dmultinom(x = obs_count,
        																			prob = fish_size_pred[,icomp],
     																					log = TRUE)
       icomp = icomp + 1
     }
   }
   like_fish_size = fish_size_lk * fish_size_wt # weighted fishery size comp likelihood
 
   # SPR ----
   Nspr_0 = rep(1,A)
   for(a in 2:A) {
     Nspr_0[a] = Nspr_0[a-1] * exp(-M)
   }
   Nspr_0[A] = Nspr_0[A] / (1 - exp(-M))
   SB0 = sum(Nspr_0 * wt_mature * exp(-spawn_fract * M)) # unfished spawning biomass per recruit
 
   # biomass reference points
   N_50 = N_40 = N_35 = rep(1, A)
   for(a in 2:A) {
     N_50[a] = N_50[a-1] * exp(-(M + F50 * slx_fish[a-1,T]))
     N_40[a] = N_40[a-1] * exp(-(M + F40 * slx_fish[a-1,T]))
     N_35[a] = N_35[a-1] * exp(-(M + F35 * slx_fish[a-1,T]))
   }
   N_50[A] = N_50[A] / (1 - exp(-(M + F50 * slx_fish[A,T])))
   N_40[A] = N_40[A] / (1 - exp(-(M + F40 * slx_fish[A,T])))
   N_35[A] = N_35[A] / (1 - exp(-(M + F35 * slx_fish[A,T])))
 
   sbpr_50 = sum(N_50 * wt_mature * exp(-spawn_fract * (M + F50 * slx_fish[,T])))
   sbpr_40 = sum(N_40 * wt_mature * exp(-spawn_fract * (M + F40 * slx_fish[,T])))
   sbpr_35 = sum(N_35 * wt_mature * exp(-spawn_fract * (M + F35 * slx_fish[,T])))
 
   sprpen = 1000.0 * ((sbpr_50/SB0 - 0.50)^2 + (sbpr_40/SB0 - 0.40)^2 + (sbpr_35/SB0 - 0.35)^2)
 
   yrs_rec = years[years>=(1977 + ages[1]) & years<=(max(years)-ages[1])]
   n_rec = length(yrs_rec)
   pred_rec = mean(Nat[1, years %in% yrs_rec])
   stdev_rec = sqrt(sum((log_Rt[years %in% yrs_rec] - mean(log_Rt[years %in% yrs_rec]))^2) / (n_rec - 1))
 
   B0 = SB0 * pred_rec
   B50 = B0 * 0.50
   B40 = B0 * 0.40
   B35 = B0 * 0.35
 
   # likelihood/penalties ----
   like_rec = -sum(RTMB::dnorm(x = log_Rt, mean = 0, sd = sigmaR, log = TRUE)) * wt_rec_var
   f_regularity = -sum(RTMB::dnorm(x = log_Ft, mean = 0, sd = sigmaF, log = TRUE)) * wt_fmort_reg
 
   # joint negative log-likelihood
   nll = like_catch + 
     like_srv + 
     like_fish_age + 
     like_srv_age + 
     like_fish_size + 
     like_rec + 
     f_regularity + 
     nll_M + 
     nll_q + 
     nll_sigmaR +
     sprpen
   
   # reports -------------------
   RTMB::REPORT(ages)
   RTMB::REPORT(years)
   RTMB::REPORT(M)
   RTMB::ADREPORT(M)
   RTMB::REPORT(slx_pars)
   RTMB::REPORT(q)
   RTMB::ADREPORT(q)
   RTMB::REPORT(sigmaR)
   RTMB::REPORT(sigmaF)
   RTMB::REPORT(log_mean_R)
   RTMB::REPORT(log_Rt)
   RTMB::ADREPORT(log_Rt)
   RTMB::REPORT(log_mean_F)
   RTMB::REPORT(log_Ft)
   RTMB::REPORT(waa)
   RTMB::REPORT(maa)
   RTMB::REPORT(wt_mature)
   RTMB::REPORT(yield_ratio)
   RTMB::REPORT(Fat)
   RTMB::REPORT(Zat)
   RTMB::REPORT(Sat)
   RTMB::REPORT(Cat)
   RTMB::REPORT(Nat)
   RTMB::REPORT(slx_srv)
   RTMB::REPORT(slx_fish)
   RTMB::REPORT(slx_curves)
   RTMB::REPORT(Ft)
   RTMB::REPORT(catch_pred)
   RTMB::REPORT(srv_pred)
   
   RTMB::REPORT(fish_age_pred)
   RTMB::REPORT(srv_age_pred)
   RTMB::REPORT(fish_size_pred)
   
   RTMB::REPORT(tot_bio)
   RTMB::REPORT(spawn_bio)
   RTMB::REPORT(recruits)
   RTMB::ADREPORT(srv_pred)
   RTMB::ADREPORT(tot_bio)
   RTMB::ADREPORT(spawn_bio)
   RTMB::ADREPORT(recruits)
   RTMB::REPORT(spawn_fract)
   RTMB::REPORT(B0)
   RTMB::REPORT(B40)
   RTMB::REPORT(B35)
   RTMB::REPORT(F35)
   RTMB::REPORT(F40)
   RTMB::REPORT(F50)
   
   # note: ADREPORT reference points to get uncertainty estimates (delta method)
   RTMB::ADREPORT(B0)
   RTMB::ADREPORT(B50)
   RTMB::ADREPORT(B40)
   RTMB::ADREPORT(B35)
   RTMB::ADREPORT(F35)
   RTMB::ADREPORT(F40)
   RTMB::ADREPORT(F50)
   
   RTMB::REPORT(pred_rec)
   RTMB::REPORT(n_rec)
   RTMB::REPORT(yrs_rec)
   RTMB::REPORT(stdev_rec)
   
   RTMB::REPORT(like_catch)
   RTMB::REPORT(like_srv)
   RTMB::REPORT(like_fish_age)
   RTMB::REPORT(like_srv_age)
   RTMB::REPORT(like_fish_size)
   RTMB::REPORT(like_rec)
   RTMB::REPORT(f_regularity)
   RTMB::REPORT(nll_q)
   RTMB::REPORT(nll_M)
   RTMB::REPORT(nll_sigmaR)
   RTMB::REPORT(sprpen)
   RTMB::REPORT(nll)
   
   return(nll)
 
 
}
