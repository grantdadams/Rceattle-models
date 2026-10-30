
# selex can be passed to make figure of SPR over time
get_spr <- function (fit, ratio = 0.4, F = NULL, selex=NULL)
{
  maxage <- fit$data_list$nages
  if (maxage != 10) stop("SPR function not tested for maxage !=10")
  xx <- get_proj_quantities(fit)
  if(!is.null(selex)){
    stopifnot(length(selex)==maxage)
    if(max(selex)>1) warning("max selex >1")
    xx$slctfsh <- selex
  }
  R0 <- fit$quantities$R0[1]
  stopifnot(!is.null(R0))
  spr <- function(F) {
    Z <- xx$M + F * xx$slctfsh
    N <- rep(NA, maxage)
    N[1] <- R0/2
    for (i in 2:maxage) N[i] <- N[i - 1] * exp(-Z[i - 1])
    N[maxage] <- N[maxage]/(1 - exp(-Z[maxage]))
    ssb <- sum(N * xx$mat * xx$wt_spawn* exp(-0.21 * Z))
    catch <- 2 * sum(N * xx$wt_fsh*(xx$slctfsh * F/Z) * (1 - exp(-Z)))
    return(list(ssb = ssb, catch = catch))
  }
  if (!is.null(F)) {
    sprF <- spr(F)
  }
  else {
    sprF <- NULL
  }
  ssb100 <- spr(0)$ssb
  ssq <- function(ratio) {
    f <- function(F) 100 * (spr(F)$ssb - ssb100 * ratio)^2
    opt <- optimize(f = f, interval = c(0, 1))
    opt$minimum
  }
  fabc <- ssq(ratio)
  x <- spr(fabc)
  return(list(ssb100 = ssb100, fabc = fabc, abc = x$catch,
              btarget = x$ssb/ssb100, spr_at_F0 = ssb100/R0, spr_at_target = x$ssb/R0,
              spr_at_F = sprF$ssb/R0))
}



get_proj_quantities <- function(rceattle){
  q <- rceattle$quantities; d <- rceattle$data_list
  ayr <- d$endyr
  years <- d$styr:ayr
  nyrs <- length(years)
  iayr <- which(years==ayr)
  # carefully extract WAA for different purposes, only using observed WAA in
  # recent years
  ytmp <- filter(d$comp_data, Fleet_name=='Fishery' & Age0_Length1==0 ) |>
    arrange() |> pull(Year)
  wt_fsh_proj <- filter(d$weight, Wt_name=='Fishery' & Year %in% tail(ayr,1)) |>
    select(-c(Wt_name, Wt_index, Species, Sex, Year)) |>
    colMeans()

  ytmp <- filter(d$comp_data, Fleet_name=='Shelikof' & Age0_Length1==0 ) |>
    arrange() |> pull(Year)
  wt_spawn_proj <- filter(d$weight, Wt_name=='Shelikof' & Year %in% tail(ytmp,5)) |>
    select(-c(Wt_name, Wt_index, Species, Sex, Year)) |>
    colMeans()

  ytmp <- filter(d$comp_data, Fleet_name=='NMFS_BT' & Age0_Length1==0 ) |>
    arrange() |> pull(Year)
  wt_pop_proj <- filter(d$weight, Wt_name=='NMFS_BT' & Year %in% tail(ytmp,3)) |>
    select(-c(Wt_name, Wt_index, Species, Sex, Year)) |>
    colMeans()

  ytmp <- filter(d$comp_data, Fleet_name=='Fishery' & Age0_Length1==0 ) |>
    arrange() |> pull(Year)
  ind <- d$fleet_control$Fleet_code[d$fleet_control$Fleet_name=='Fishery']
  selex <- q$sel_at_age[ind,1,,]
  ind <- which(as.numeric(dimnames(selex)[[2]]) %in% tail(ytmp,5))
  slctfsh_proj <- t(selex[,ind]) |> colSums()
  slctfsh_proj <- slctfsh_proj/max(slctfsh_proj)
  data.frame(age=1:10,  M=as.numeric(rceattle$quantities$M1_at_age[1,1,,1]),
             mat=as.numeric(rceattle$data_list$maturity[-1]),
             slctfsh=slctfsh_proj, wt_pop=wt_pop_proj,
             wt_spawn=wt_spawn_proj, wt_fsh=wt_fsh_proj)
}

fit_goa_pk <- function(dat, random_rec=TRUE, random_q=TRUE,
                       q_spec=NULL, sel_spec=NULL,
                       mode=1, Ftarget=0, ...){

  message("Standardizing ESP data for DSEM..")
  dat$env_data[] <- lapply(dat$env_data, as.numeric)
  dat$env_data <- dat$env_data |>
    mutate(across(.cols=c(Spr_Larvae_Shelikof,Sum_OffYOY_Shelikof,Sum_NearYOY_Kodiak),.fns=log)) |>
    mutate(across(.fns =scale,.cols=-c(Year, Qcov, Spr_Larvae_Shelikof,Sum_OffYOY_Shelikof,Sum_NearYOY_Kodiak)))

  dat$fleet_control$Sel_norm_bin_upper <- NA

  # Model configuration ----
  SHELIKOF <- 1L; BOTTOM_TRAWL <- 2L; FISHERY <- 8L # Fleet indices

  # identical to "02-bridge.R"
  dat$fleet_control$Selectivity_index[FISHERY] <- FISHERY
  dat$fleet_control$Catchability[SHELIKOF]   <- "Estimated"
  dat$fleet_control$Time_varying_q[SHELIKOF] <- "Off"
  dat$fleet_control$Catchability[BOTTOM_TRAWL] <- "Estimated-with-prior"
  dat$fleet_control$Catchability_init[BOTTOM_TRAWL] <- 0.85
  dat$fleet_control$Catchability_prior_sd[BOTTOM_TRAWL] <- 0.1

  # Composition young-age accumulation (fishery age-1 -> 2; Shelikof ages 1-2 -> 3).
  dat$fleet_control$Comp_accum_young <- 1L
  dat$fleet_control$Comp_accum_old   <- 0L
  dat$fleet_control$Comp_accum_young[FISHERY]  <- 2L
  dat$fleet_control$Comp_accum_young[SHELIKOF] <- 3L

  # DM weights (log theta) starting values from goa_pk (fit_mod gets these from
  # fleet_control$Comp_weights); estimated freely from there.

  dat$fleet_control$Comp_weights[c(FISHERY, 1, BOTTOM_TRAWL, 3, 6)] <- 0

  # Survey-3 (adfg) time-varying q is expressed through the linkage grammar below,
  # so switch off the legacy fleet_control mode it replaces.
  ADFG <- 3L
  dat$fleet_control$Time_varying_q[ADFG] <- "Off"   # Using rw(1 | Year) on q, below

  # Fishery ascending-limb random walk also moves to the grammar; switch off the
  # legacy mode. goa_pk penalizes these deviates rather than integrating them,
  # hence integrate = FALSE. The legacy penalty uses sel_dev_sd on the slope and
  # 4x that on the inflection, so the two take different sigmas.
  SEL_RW_SD <- dat$fleet_control$Time_varying_sel_sd[FISHERY]
  dat$fleet_control$Time_varying_sel[FISHERY] <- "Off"  # -> rw(1 | Year), below

  # Linkages name their fleets (can also use Fleet_code):
  SHELIKOF_ACOUSTIC <- "Shelikof"
  ADFG_SURVEY       <- "ADFG_BT"
  # Prior fleets, restricted to the limb each curve actually has: srv2/srv3 are
  # Logistic (ascending only), srv1/srv6 are DescendingLogistic (descending only),
  # the fishery is DoubleLogistic (both). A prior on the other limb would only add
  # a constant -- fit_mod() now rejects it.
  ASC_LIMB_PRIOR_FLEETS <- c("NMFS_BT",
                             "ADFG_BT",
                             "Fishery")
  DESC_LIMB_PRIOR_FLEETS <- c("Shelikof",
                              "Summer_AT",
                              "Fishery")
  DM_PRIOR_FLEETS        <- c("Fishery",
                              "Shelikof",
                              "NMFS_BT",
                              "ADFG_BT",
                              "Summer_AT")

  # * Catchability: QAR1 (Shelikof) + random-walk q (adfg) ----
  # Shelikof is the state-space QAR1; the adfg survey is a random walk on q. The
  # random-walk SD is fixed at 0.05.
  if(is.null(q_spec)){
  q_spec <- build_catchability(linkages = list(
    q = list(
      linkage_spec(~ ar1(1 | Year),
                   fleet = SHELIKOF_ACOUSTIC,
                   observe = "Qcov",
                   obs_sd = 0.02),#exp(pl$log_Ecov_obs_sd)),
      linkage_spec(~ rw(1 | Year),
                   fleet = ADFG_SURVEY,
                   init = list(sigma = 0.05)))))
  }
  # * Selectivity priors + fishery ascending random walk ----
  # Priors mirror goa_pk exactly (both limbs on srv2 and srv6). The ascending limb
  # takes a second spec: the ~ 1 spec keeps the shared prior, the walk adds
  # fishery-only deviates around it. Ascending only -- goa_pk maps its descending
  # deviates off and estimates just slp1/inf1. (02-bridge.R walks all four, because
  # reproducing goa_pk's likelihood needs the constant penalty it still charges on
  # those fixed-at-zero deviates.)
  # The two models fix opposite ends of this walk; see A7.
  if(is.null(sel_spec)){
    sel_spec <- build_selectivity(linkages = list(
    slp_asc  = list(
      linkage_spec(~ 1,
                   fleet = ASC_LIMB_PRIOR_FLEETS,
                   priors = list(intercept = lognormal(-1, 1.5))),
      linkage_spec(~ rw(1 | Year),
                   fleet = "Fishery",
                   init = list(sigma = SEL_RW_SD),
                   integrate = FALSE)),
    inf_asc  = list(
      linkage_spec(~ 1,
                   fleet = ASC_LIMB_PRIOR_FLEETS,
                   priors = list(intercept = normal(0, 3))),
      # 4x SD: the legacy penalty weights the inflection deviate at 4 * sel_dev_sd.
      linkage_spec(~ rw(1 | Year),
                   fleet = "Fishery",
                   link = "identity",
                   init = list(sigma = 4 * SEL_RW_SD), integrate = FALSE)),
    slp_desc = linkage_spec(~ 1,
                            fleet = DESC_LIMB_PRIOR_FLEETS,
                            priors = list(intercept = lognormal(-1, 1.5))),
    inf_desc = linkage_spec(~ 1,
                            fleet = DESC_LIMB_PRIOR_FLEETS,
                            priors = list(intercept = normal(10, 3)))))
  }
  # * Dirichlet-multinomial prior ----
  # (dnorm(log_DM_pars, 0, 2)).
  comp_spec <- build_composition(linkages = list(
    theta_comp = linkage_spec(~ 1,
                              fleet = DM_PRIOR_FLEETS,
                              priors = list(intercept = lognormal(0, 2)))))

  # this setup allows me to pass an externally calculated Ftarget via the SPR
  # spreadsheet called Fabc
  #Fabc <- 1
  HCR = build_hcr(HCR = 2, # constant F
                  DynamicHCR = FALSE, # Dont dynamic reference points
                  Ftarget = Ftarget) # F40%
                  # Flimit = 0.35, # F35%
                  # Plimit = 0.2, # No fishing when SB<SB20
                  # Alpha = 0.05)

  fit <- fit_mod(
    data_list = dat,
    estimateMode = mode,
    msmMode = "SingleSpecies",
    initMode = "OffsetEquilibrium",
    random_rec = random_rec,
    random_q = random_q,
    qFun = q_spec,
    HCR = HCR,
    recFun = build_srr(proj_mean_rec = FALSE),
    selFun = sel_spec,
    compFun = comp_spec,
    # projection_uncertainty=FALSE will not estimate any parameters in projection period
    fit_control=fit_control(bias_adjust_proc=FALSE,
                            phase = TRUE,
                            projection_uncertainty = TRUE),
    ...)
  return(fit)
}


get_pk_osa <- function(fit, internal=TRUE){
  library(dplyr); library(tidyr)
  osa <- osa_residuals(fit)
  pears <- attr(osa, 'pearson')
  out <- lapply(unique(osa$fleet)[-1], \(ind)
                {
                 # print(ind)
                  x <- filter(pears, source=='comp' & fleet==ind &
                                index_label =='age' & sample_size>0)
                  if(nrow(x)==0) return()
                  tmp <- group_by(x, year) |> slice_head(n=1)
                  N <- pull(tmp, sample_size)
                  yr <- pull(tmp, year)
                  index <- unique(x$age_length_bin)
                  o <- pivot_wider(x, id_cols='year', names_from='age_length_bin', values_from=observed)
                  o <- as.matrix(o[,-1]) *N
                  e <- pivot_wider(x, id_cols='year', names_from='age_length_bin', values_from=predicted)
                  e <- as.matrix(e[,-1]) *N
                  # hack for now, first column is all zeroes and screws up indexing downstream so remove
                  # if(ind==1){
                  #   if(sum(o[,1])>0) warning("sometihng went wrong here")
                  #   o <- o[,-1]; e <- e[,-1]; index <- index[-1]
                  # }
                  x <- filter(osa, source=='comp' & fleet==ind)
                  res <- pivot_wider(x, id_cols='year', names_from='age_length_bin', values_from=residual)
                  res <- (as.matrix(res[,-1]))
                  if(!internal) res <- NULL
                  xx <- run_osa(obs=o, exp=e, N=N, index=index, years=yr, res=res,
                                fleet=x$fleet_name[ind], theta=exp(fit$estimated_params$comp_weights[ind]))
                  xx
  })
  return(out)
}




write_spm_dynamics2 <- function(rceattle, path){
  q <- rceattle$quantities; d <- rceattle$data_list
  ayr <- d$endyr
  years <- d$styr:ayr
  nyrs <- length(years)
  iayr <- which(years==ayr)
  # carefully extract WAA for different purposes, only using observed WAA in
  # recent years
  ytmp <- filter(d$comp_data, Fleet_name=='Fishery' & Age0_Length1==0 ) |>
    arrange() |> pull(Year)
  wt_fsh_proj <- 1000*filter(d$weight, Wt_name=='Fishery' & Year %in% tail(ayr,1)) |>
    select(-c(Wt_name, Wt_index, Species, Sex, Year)) |>
    colMeans()

  ytmp <- filter(d$comp_data, Fleet_name=='Shelikof' & Age0_Length1==0 ) |>
    arrange() |> pull(Year)
  wt_spawn_proj <- 1000*filter(d$weight, Wt_name=='Shelikof' & Year %in% tail(ytmp,5)) |>
    select(-c(Wt_name, Wt_index, Species, Sex, Year)) |>
    colMeans()

  ytmp <- filter(d$comp_data, Fleet_name=='NMFS_BT' & Age0_Length1==0 ) |>
    arrange() |> pull(Year)
  wt_pop_proj <- 1000*filter(d$weight, Wt_name=='NMFS_BT' & Year %in% tail(ytmp,3)) |>
    select(-c(Wt_name, Wt_index, Species, Sex, Year)) |>
    colMeans()

  ytmp <- filter(d$comp_data, Fleet_name=='Fishery' & Age0_Length1==0 ) |>
    arrange() |> pull(Year)
  selex <- q$sel_at_age[8,1,,]
  ind <- which(as.numeric(dimnames(selex)[[2]]) %in% tail(ytmp,5))
  slctfsh_proj <- t(selex[,ind]) |> colSums()
  slctfsh_proj <- slctfsh_proj/max(slctfsh_proj)
  # numbers at age at beginning of year in first projected year
  naa <- as.numeric(q$N_at_age[1,1,,iayr+1]/1e6)
  # reruitment comes from definition of mean recruit in proj, not in model
  ytmp <- as.numeric(dimnames(q$N_at_age)[[4]])
  naa[1] <- mean(q$N_at_age[1,1,1,which(ytmp %in% 1978:(ayr-1))])/1e6

  x <- c()
  x <-
    c(x,paste("# proj input file written by write_spm_inputs on", Sys.time()))
  x <- c(x, "goa_wp")
  x <- c(x, "1 # Flag to tell if this is a SSL forage species")
  x <- c(x, "0 # Flag to Dorn's version of a constant buffer")
  x <- c(x, "1 # number of fisheries")
  x <- c(x, "1 # number of sexes")
  x <- c(x, paste(mean(tail(q$F_spp[1,1:nyrs],5)), " # average F over last 5 years"))
  x <- c(x, "1 # Author's F multiplier (to MaxPermissible)")
  x <- c(x, "0.4 # ABC SPR")
  x <- c(x, "0.35 # OFL SPR")
  x <- c(x, "3.52 # Month of spawning (mid Feb, yrfrac=.21)")
  x <- c(x, paste(d$nages, " # Number of ages"))
  x <- c(x, "1 # Fratio")
  x <- c(x, "# Natural mortality")
  x <- c(x, paste(as.numeric(q$M1_at_age[1,1,,1]), collapse=' '))
  x <- c(x, "# Maturity")
  x <- c(x, paste(as.numeric(d$maturity)[-1], collapse=' '))
  x <- c(x, "# Spawning WAA")
  x <- c(x, paste(wt_spawn_proj, collapse=' '))
  x <- c(x, "# Fishery WAA")
  x <- c(x, paste(wt_fsh_proj, collapse=' '))
  x <- c(x, "# Fishery selex, averaged over last 5 years")
  x <- c(x, paste(slctfsh_proj, collapse=' '))
  x <- c(x, "# starting NAA")
  x <- c(x, paste(naa*1000, collapse=' '))
  ind <- which(years %in% 1978:(ayr-1))
  recs <- q$R[1,ind]/1000
  ssb <- as.numeric(q$ssb[1,ind-1]/1000)
  x <- c(x, paste(length(recs), " # num of recruits"))
  x <- c(x, "# Recruits from 1978 to this year -1 due to uncertain last year")
  x <- c(x, paste(recs, collapse=' '))
  x <- c(x, '# SSB from 1977 to this year-2 to match recruits')
  x <- c(x, paste(ssb, collapse=' '))
  writeLines(x, con=file.path(path, 'goa_wp.txt'))
}


write_spm_setup2 <- function(rceattle, path){

  ayr <- rceattle$data_list$endyr
  x <- c()
  x <- c(x,paste("# proj input file written by write_proj_inputs on",
                 Sys.time()))
  x <- c(x, " std # run name")
  x <- c(x, " 3 # tier")
  x <- c(x, "7 # number of alt scenarios")
  x <- c(x, paste(1:7, collapse=' '))
  x <- c(x, "1 # flag to set TAC=ABC (1 is true)")
  x <- c(x, "2 # SR type (1 ricker, 2 bholt")
  x <- c(x, "1 # proj rec form (default: 1= use obs mean/SD")
  x <- c(x, "0 # SR conditioning (0 means no)")
  x <- c(x, "0.0 # turn off prior thing")
  x <- c(x, "1 # flag to write bigfile")
  x <- c(x, "14 # number of proj years")
  x <- c(x, "1000 # number of simulations")
  x <- c(x, paste(ayr+1, " # begin year is assessment year + 1"))
  x <- c(x, "0 # number of years with specified catch")
  x <- c(x, "1 # numberof species")
  x <- c(x, "0 # OY min")
  x <- c(x, "2e6 # OY max")
  x <- c(x, "goa_wp.txt # input file name for biology")
  x <- c(x, "1 # ABC multipliers")
  x <- c(x, "1 # pop scalars")
  x <- c(x, "0.75 # new alt 4 Fabc SPRs (unused?)")
  x <- c(x, "1 # num of TAC model categories")
  x <- c(x, "1 # TAC model indices")
  #x <- c(x, '# no catch read in b/c starting in subsequent year ')
  x <- c(x, paste(ayr, 0,"# dummy catch"))
  writeLines(x, con=file.path(path, 'spm.dat'))
}




write_spm_inputs2 <- function(rceattle, path=NULL){
  #stopifnot(is.pkfit(fit))
  if(is.null(path)) {
    path <- getwd()
    #message("No path provided so writing to working directory ", path)
  }
  replist <- fit$rep
  datlist <- fit$input$dat
  ## delete old files in case it fails it'll stop there
  ff <- file.path(path, c("goa_wp.txt", "spm.dat", "spp_catch.dat"))
  trash <- lapply(ff, function(x) if(file.exists(x)) file.remove(x))
  write_spm_setup2(rceattle=rceattle, path=path)
  write_spm_dynamics2(rceattle=rceattle, path=path)
  ## old unused file, just need dummy values so it runs
  ## tacpar <- readLines(tacpar.dat'); dput(tacpar)
  write.table(file=file.path(path,'tacpar.dat'),
              x=c("7", "6", " 2.60197 0.417 0.372 0.361 0.296 0.2733 0.125",
                  " -0.300856 -0.741664 -1.26797 -1.78614 -2.15198 -2.39595 -2.59939",
                  " -0.00703968 -0.956592 -1.48909 -2.09122 -2.26005 -2.31812 -2.45321",
                  " 0.372276 -1.35066 -1.76076 -2.22163 -2.31726 -2.28107 -2.34758",
                  " 0.505535 -1.19926 -1.68812 -2.39543 -2.40752 -2.5904 -2.58726",
                  " 0.368722 -1.59025 -1.79548 -2.67422 -2.61358 -2.41092 -2.70204",
                  " 0.308248 -1.78052 -2.23264 -2.85605 -3.39375 -3.05209 -2.94219",
                  " 0.180676 -1.7586 -1.80222 -3.09402 -2.2618 -3.43215 -3.06417"))
}



get_exec_table2 <- function(rceattle, summary, detail, maxABCratio=1){
  if(maxABCratio!=1) stop("Function does not yet support an abc reduction")
  q <- rceattle$quantities; d <- rceattle$data_list
  ayr <- d$endyr
  years <- d$styr:ayr
  iayr <- which(years==ayr)
  # calculate summary biomass as 3+
  message("Manually calculating summary biomass (3+) from model outputs..")
  naa <- t(q$N_at_age[1,1,,])
  naa <- naa[which(as.numeric(dimnames(naa)[[1]]) %in% c(ayr, ayr+1)),]
  # population weight at age in future is from recent NMFS BT ages, as in spmR
  ytmp <- filter(d$comp_data, Fleet_name=='NMFS_BT' & Age0_Length1==0 ) |>
    arrange() |> pull(Year)
  waa <- filter(d$weight, Wt_name=='NMFS_BT' & Year %in% tail(ytmp,3)) |>
    select(-c(Wt_name, Wt_index, Species, Sex, Year)) |>
    colMeans()
  sumbio <- c(sum(naa[1,-(1:2)]*waa[-(1:2)]),  sum(naa[2,-(1:2)]*waa[-(1:2)]))

  M <- c(.3,.3)
  means <- detail |> filter(Year > ayr & Year <= ayr+2) |>
    group_by(Alt, Year, Stock) |>
    summarize_all(mean) |> ungroup()
  ref <- filter(summary, is.na(Alt))
  B100 <- filter(ref, variable=='SSB_100')$value |> round(-3)
  B40 <- filter(ref, variable=='SSB_40')$value |> round(-3)
  B35 <- filter(ref, variable=='SSB_ofl')$value |> round(-3)
  fofl <- filter(ref, variable=='F_ofl')$value |> round(3)
  fabc <- filter(ref, variable=='F_abc')$value |> round(3)
  ssb <- filter(summary, Alt==1 & Year %in% c(ayr+1, ayr+2) & variable == 'SSB_mean')$value
  ofl <- filter(means,  Alt==1 & Year %in% c(ayr+1, ayr+2)) |> pull(OFL) |> round(0)
  abc <- maxabc <- filter(means,  Alt==1 & Year %in% c(ayr+1, ayr+2)) |> pull(ABC) |> round(0)
  maxfabc <- fabc * maxABCratio
  tab <- rbind(M, sumbio, ssb, B100, B40, B35, fofl, maxfabc, fabc,ofl,maxabc,abc)
  tab <- as.data.frame(tab) |> setNames(c(ayr+1,ayr+2)) |>
    mutate(name=row.names(tab)) |> relocate(name)
  row.names(tab) <- NULL
  tab
  return(tab)
}

# these matched to numerical precision
# write_spm_inputs2(m6, path='../projection/test1')
# GOApollock::write_spm_inputs(fit=fit5, path='../projection/test2')


get_sigR <- function(fit){
  x <- summary(fit$dsem)
  ind <- which(x$name %in%c('sigmaR','V[recdevs1]'))
  stopifnot(length(ind)==1)
  fit$estimated_params$dsem_beta_z[ind]
}
