setwd("~/GitHub/RceattleRuns/GOA/Model runs/GOA_18.5.1")
library(Rceattle)

i = 3
load("Models/18_5_1_2021-05-13.RData")
mod_fe = mod_list_all[[i]]
rm(mod_list_all)

# Update size of data by making smaller projection
data_tmp <-  mod_fe$data_list

# Update rec devs
inits_tmp <- mod_fe$estimated_params



TMBfilename = "ceattle_v01_07"
data_list = NULL
inits = NULL
map = NULL
bounds = NULL
file = NULL
debug = FALSE
random_rec = FALSE
niter = 3
msmMode = 0
avgnMode = 0
minNByage = 0
suitMode = 0
phase = NULL
silent = FALSE
recompile = TRUE
getsd = TRUE
use_gradient = TRUE
rel_tol = 1
control = list(eval.max = 1e+09,
               iter.max = 1e+09, trace = 0)
getHessian = TRUE
loopnum = 5
newtonsteps = 0


data_list = data_tmp
inits = inits_tmp # Start from ms mod
file = NULL # Don't save
debug = FALSE # Estimate
random_rec = TRUE # Random recruitment
msmMode = data_tmp$msmMode
phase = NULL 
getHessian = FALSE
niter = 3
silent = FALSE


start_time <- Sys.time()

setwd(getwd())
'%!in%' <- function(x,y)!('%in%'(x,y))

#--------------------------------------------------
# 1. DATA and MODEL PREP
#--------------------------------------------------

# STEP 1 - LOAD DATA
if (is.null(data_list)) {
  stop("Missing data_list object")
}

# - Remove years of data previous to start year
data_list$UobsWtAge <- as.data.frame(data_list$UobsWtAge)
data_list$UobsAge <- as.data.frame(data_list$UobsAge)
data_list$wt <- data_list$wt[which(data_list$wt$Year == 0 | data_list$wt$Year >= data_list$styr),]
data_list$UobsAge <- data_list$UobsAge[which(data_list$UobsAge$Year == 0 | data_list$UobsAge$Year >= data_list$styr),]
data_list$UobsWtAge <- data_list$UobsWtAge[which(data_list$UobsWtAge$Year == 0 | data_list$UobsWtAge$Year >= data_list$styr),]
data_list$srv_biom <- data_list$srv_biom[which(abs(data_list$srv_biom$Year) >= data_list$styr),]
data_list$fsh_biom <- data_list$fsh_biom[which(abs(data_list$fsh_biom$Year) >= data_list$styr),]
data_list$comp_data <- data_list$comp_data[which(abs(data_list$comp_data$Year) >= data_list$styr),]
data_list$emp_sel <- data_list$emp_sel[which(data_list$emp_sel$Year == 0 | data_list$emp_sel$Year >= data_list$styr),]
data_list$NByageFixed <- data_list$NByageFixed[which(data_list$NByageFixed$Year == 0 | data_list$NByageFixed$Year >= data_list$styr),]
data_list$Pyrs <- data_list$Pyrs[which(data_list$Pyrs$Year == 0 | data_list$Pyrs$Year >= data_list$styr),]


# - Remove years of data after to proj year
data_list$wt <- data_list$wt[which(data_list$wt$Year <= data_list$projyr),]
data_list$UobsAge <- data_list$UobsAge[which(data_list$UobsAge$Year <= data_list$projyr),]
data_list$UobsWtAge <- data_list$UobsWtAge[which(data_list$UobsWtAge$Year <= data_list$projyr),]
data_list$srv_biom <- data_list$srv_biom[which(abs(data_list$srv_biom$Year) <= data_list$projyr),]
data_list$fsh_biom <- data_list$fsh_biom[which(abs(data_list$fsh_biom$Year) <= data_list$projyr),]
data_list$comp_data <- data_list$comp_data[which(abs(data_list$comp_data$Year) <= data_list$projyr),]
data_list$emp_sel <- data_list$emp_sel[which(data_list$emp_sel$Year <= data_list$projyr),]
data_list$NByageFixed <- data_list$NByageFixed[which(data_list$NByageFixed$Year <= data_list$projyr),]
data_list$Pyrs <- data_list$Pyrs[which(data_list$Pyrs$Year <= data_list$projyr),]


# - Extend catch data to proj year for projections
if(data_list$projyr > data_list$endyr){
  for(flt in (unique(data_list$fsh_biom$Fleet_code))){
    fsh_biom_sub <- data_list$fsh_biom[which(data_list$fsh_biom$Fleet_code == flt),]
    yrs_proj <- (data_list$endyr + 1):data_list$projyr
    yrs_proj <- yrs_proj[which(yrs_proj %!in% fsh_biom_sub$Year)]
    nyrs_proj <- length(yrs_proj)
    proj_fsh_biom <- data.frame(Fleet_name = rep(fsh_biom_sub$Fleet_name[1], nyrs_proj),
                                Fleet_code = rep(flt, nyrs_proj),
                                Species = rep(fsh_biom_sub$Species[1], nyrs_proj),
                                Year = yrs_proj,
                                Month = rep(fsh_biom_sub$Month[length(fsh_biom_sub$Month)], nyrs_proj),
                                Selectivity_block = rep(fsh_biom_sub$Selectivity_block[length(fsh_biom_sub$Selectivity_block)], nyrs_proj),
                                Catch = rep(NA, nyrs_proj),
                                Log_sd = rep(fsh_biom_sub$Log_sd[length(fsh_biom_sub$Log_sd)], nyrs_proj))
    data_list$fsh_biom <- rbind(data_list$fsh_biom, proj_fsh_biom)
  }
}
data_list$fsh_biom <- data_list$fsh_biom[
  with(data_list$fsh_biom, order(Fleet_code, Year)),]

# Switches
data_list$random_rec <- as.numeric(random_rec)
data_list$debug <- debug
data_list$niter <- niter
data_list$avgnMode <- avgnMode
data_list$msmMode <- msmMode
data_list$suitMode <- as.numeric(suitMode)
data_list$minNByage <- as.numeric(minNByage)


# STEP 1 - LOAD PARAMETERS
if (is.character(inits) | is.null(inits)) {
  params <- suppressWarnings(Rceattle::build_params(
    data_list = data_list,
    inits = inits
  ))
} else{
  # inits$proj_F <- data_list$fleet_control$proj_F
  params <- inits
}
start_par <- params
message("Step 1: Parameter build complete")



# STEP 2 - BUILD MAP
if (is.null(map)) {
  map <-
    suppressWarnings(build_map(data_list, params, debug = debug > 1, random_rec = random_rec))
} else{
  map <- map
}
message("Step 2: Map build complete")


# STEP 3 - Get bounds
if (is.null(bounds)) {
  bounds <- Rceattle::build_bounds(param_list = params, data_list)
} else {
  bounds = bounds
}
message("Step 3: Param bounds complete")


# STEP 4 - Setup random effects
random_vars <- c() # c("ln_srv_q_dev_re", "ln_sel_slp_dev_re", "sel_inf_dev_re")
if (random_rec == TRUE) {
  random_vars <- c(random_vars , "rec_dev", "init_dev")
}



# Set default phasing
if(!is.null(phase)){
  if(class(phase) == "character"){
    if(tolower(phase) == "default"){
      phase = list(
        dummy = 1,
        ln_pop_scalar = 4,
        ln_mean_rec = 1,
        ln_rec_sigma = 2,
        rec_dev = 2,
        init_dev = 2,
        ln_mean_F = 1,
        ln_FSPR = 3,
        proj_F_prop = 1,
        F_dev = 1,
        ln_srv_q = 3,
        # srv_q_pow = 4,
        ln_srv_q_dev = 5,
        # ln_srv_q_dev_re = 4,
        ln_sigma_srv_q = 4,
        ln_sigma_time_varying_srv_q = 4,
        sel_coff = 3,
        sel_curve_pen = 4,
        ln_sex_ratio_sigma = 3,
        ln_sel_slp = 3,
        ln_M1 = 4,
        sel_inf = 3,
        ln_sel_slp_dev = 5,
        sel_inf_dev = 5,
        # ln_sel_slp_dev_re = 4,
        # sel_inf_dev_re = 4,
        ln_sigma_sel = 4,
        ln_sigma_srv_index = 2,
        ln_sigma_fsh_catch = 2,
        # log_gam_a = 4,
        # log_gam_b = 4,
        # log_phi = 4,
        comp_weights = 4
      )
    }
  }
  
  if(class(phase) == "character"){
    if(tolower(phase) != "default"){
      warning("phase misspecified: please set to 'default' or list with the same order as parameters.")
    }
  }
}


# STEP 5 - Compile CEATTLE is providing cpp file
# - Get cpp file if not provided
TMBfilename <- "ceattle_v01_07"

message("Step 4: Compile CEATTLE complete")


# STEP 6 - Reorganize data and build model object
Rceattle:::data_check(data_list)
data_list_reorganized <- Rceattle::rearrange_dat(data_list)
data_list_reorganized = c(list(model = "ceattle_v01_07"),data_list_reorganized)

# - Update comp weights from data
if(!is.null(data_list$fleet_control$Comp_weights)){
  params$comp_weights = data_list$fleet_control$Comp_weights
}


# STEP 7 - Set up parameter bounds
L <- c()
U <- c()
for(i in 1:length(map[[1]])){
  if(names(map[[1]])[i] %!in% random_vars){ # Dont have bounds for random effects
    L = c(L, unlist(bounds$lower[[i]])[which(!is.na(unlist(map[[1]][[i]])) & !duplicated(unlist(map[[1]][[i]])))])
    U = c(U, unlist(bounds$upper[[i]])[which(!is.na(unlist(map[[1]][[i]])) & !duplicated(unlist(map[[1]][[i]])))])
  }
}


# STEP 8 - Fit model object
step = 5
# If phased
if(!is.null(phase) & debug == 0 ){
  message(paste0("Step ", step,": Phasing begin"))
  phase_pars <- Rceattle::TMBphase(
    data = data_list_reorganized,
    parameters = params,
    map = map[[1]],
    random = random_vars,
    phases = phase,
    model_name = TMBfilename,
    silent = silent,
    use_gradient = use_gradient,
    control = control
  )
  
  start_par <- phase_pars
  
  message(paste0("Step ", step,": Phasing complete - getting final estimates"))
  step = step + 1
}


# STEP 9 - Fit final model
obj = TMB::MakeADFun(
  data_list_reorganized,
  parameters = start_par,
  DLL = TMBfilename,
  map = map[[1]],
  random = random_vars,
  silent = silent
)


library(coop)
library(TMB)


check_fn <- c()
check_he <- c()
for(yr in 8:42){
  map_tmp <- map
  map_tmp[[2]]$rec_dev[,yr:43] <- NA
  map_tmp[[1]]$rec_dev <- as.factor(map_tmp[[2]]$rec_dev)
  
  # STEP 9 - Fit final model
  obj = TMB::MakeADFun(
    data_list_reorganized,
    parameters = start_par,
    DLL = TMBfilename,
    map = map_tmp[[1]],
    random = random_vars,
    silent = silent,
    hessian = TRUE
  )
  
  check_fn[yr] <- obj$fn()
  he <- sdreport(obj, getJointPrecision = TRUE)
  hess <- he$jointPrecision
  hess <- hess[which(rownames(hess) %in% c("init_dev", "rec_dev")), which(colnames(hess) %in% c("init_dev", "rec_dev"))]
  check_he[yr] <- sparsity(as.matrix(hess))
}

# # STEP 9 - Fit final model
# obj = TMB::MakeADFun(
#   data_list_reorganized,
#   parameters = start_par,
#   DLL = TMBfilename,
#   map = map[[1]],
#   random = random_vars,
#   silent = silent
# )
# 
# message(paste0("Step ",step, ": final build complete"))
# step = step + 1
# 
# 
# opt <- nlminb(obj$par, obj$fn, obj$gr)
# # opt1 <- do.call("optim",obj)
# # # Optimize
# # if(debug == FALSE){
# #   opt = Rceattle::fit_tmb(obj = obj,
# #                           fn=obj$fn,
# #                           gr=obj$gr,
# #                           startpar=obj$par,
# #                           lower = L,
# #                           upper = U,
# #                           loopnum = loopnum,
# #                           getsd = getsd,
# #                           control = control,
# #                           getHessian = getHessian,
# #                           quiet = silent,
# #   )
# # }
# 
# # quantities <- obj$report(obj$env$last.par.best)
# # quantities
