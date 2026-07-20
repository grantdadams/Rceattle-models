# match2_full.R — match2.R + the three composition fixes (session 3i-3k), aligned
# to ADMB/m23_rceattle_full (full-normal rec penalty, steepness off, ATS/AVO age-1
# removed from prediction, q_avo bounded).
#
# Three comp fixes vs match2.R:
#   1. Comp_loglike = "MultinomialAFSC" (ADMB offset form; default was full Multinomial).
#   2. Restore BTS comp age-1 (= BTS_1 index obs, verified identical) and turn BTS_1
#      OFF — ADMB keeps age-1 in the BTS comps and has no BTS age-1 index.
#   3. ATS/AVO Bin_first_selected = 2 (already in match2.R) — excludes age-1.
# Plus: sigma_rec_prior = 1 (matches m23_rceattle_full's full-normal rec penalty).
suppressMessages({library(dplyr); library(readxl)})
pkgload::load_all("/Users/grantadams/Documents/GitHub/Rceattle ecosystem/Rceattle", quiet = TRUE)
setwd("/Users/grantadams/Documents/GitHub/Rceattle ecosystem/Rceattle-models/EBS pollock")
SP <- "."
BAP <- as.numeric(Sys.getenv("BAP", "0"))
AD  <- "ADMB/m23_rceattle_full"
n_selages_fsh <- 12; bts_styr <- 1982; ats_styr <- 1994

mydata <- Rceattle::read_data(file = "Data/2024_EBS_pollock.xlsx")
nages <- mydata$nages; yrs <- mydata$styr:mydata$endyr
keep_age <- c("Species_name","Species","Sex","Year", paste0("Age",1:nages))
mydata$NByageFixed <- mydata$NByageFixed[, intersect(keep_age, colnames(mydata$NByageFixed))]
mydata$spawn_month <- 3

est <- mydata
est$estDynamics <- 0
est$catch_data$Log_sd <- 0.05           # xlsx index Log_sd is already a CV -- do NOT divide by Obs
fcn <- est$fleet_control$Fleet_name
est$fleet_control$Fleet_type[5:6] <- 2
est$age_error[1:nages, 3:(nages+2)] <- diag(nages)
# MATCH FISHERY SELECTIVITY: fix it at ADMB's realized sel_fsh via empirical
# selectivity (Selectivity=0). The free NonParametricPM estimate lands at slightly
# different coefficients (age-1 log-coff -1.76 vs ADMB -3.73) because the fishery
# sel + initial N + early F are a jointly-determined block in the pre-survey years;
# injecting ADMB's realized curve forces the early N to match ADMB.
est$fleet_control$Selectivity[fcn=="Fishery"] <- 0   # empirical
{
  rlf <- readLines(file.path(AD, "pm.rep"))
  i0 <- which(rlf=="sel_fsh")[1]
  sel_fsh <- t(sapply(1:length(yrs), function(k) as.numeric(strsplit(trimws(rlf[i0+k]),"[[:space:]]+")[[1]])))
  ccol <- paste0("Comp_", 1:nages)
  fc_code <- est$fleet_control$Fleet_code[fcn=="Fishery"]
  es <- est$emp_sel[est$emp_sel$Fleet_name!="Fishery", ]
  add <- est$emp_sel[0, ]; add[1:length(yrs), ] <- NA
  add$Fleet_name <- "Fishery"; add$Fleet_code <- fc_code; add$Species <- 1
  add$Sex <- 0; add$Year <- yrs
  for (a in 1:nages) add[[ccol[a]]] <- sel_fsh[, a]
  est$emp_sel <- rbind(es, add[, colnames(es)])
  cat("MATCH FSH: injected ADMB sel_fsh (empirical) for Fishery\n")
}
est$fleet_control$Selectivity[fcn=="BTS"] <- "LogisticPM"
est$fleet_control$Time_varying_sel[fcn=="BTS"] <- "RandomWalk"
est$fleet_control$Sel_curve_pen1[fcn=="BTS"] <- 2
est$fleet_control$Sel_curve_pen2[fcn=="BTS"] <- 0
est$fleet_control$Sel_curve_pen3[fcn=="BTS"] <- 8
est$fleet_control$Sel_norm_bin1[fcn=="BTS"] <- 3
est$fleet_control$Sel_norm_bin2[fcn=="BTS"] <- 14
est$fleet_control$Sel_start_year[fcn=="BTS"] <- bts_styr
est$fleet_control$Bin_first_selected[fcn=="BTS"] <- 1
est$fleet_control$Time_varying_sel_sd_prior[fcn=="BTS"] <- 1
for(fl in c("ATS","AVO")){
  est$fleet_control$Selectivity[fcn==fl] <- "NonParametricPM"
  est$fleet_control$Time_varying_sel[fcn==fl] <- "RandomWalk"
  est$fleet_control$N_sel_bins[fcn==fl] <- 8
  est$fleet_control$Sel_curve_pen1[fcn==fl] <- -1
  est$fleet_control$Sel_curve_pen2[fcn==fl] <- 1
  est$fleet_control$Sel_curve_pen3[fcn==fl] <- 0
  est$fleet_control$Sel_norm_bin1[fcn==fl] <- NA
  est$fleet_control$Bin_first_selected[fcn==fl] <- 2   # FIX 3: excludes ATS/AVO age-1
  est$fleet_control$Sel_pen_first_bin[fcn==fl] <- 2
  est$fleet_control$Sel_start_year[fcn==fl] <- ats_styr
  est$fleet_control$Time_varying_sel_sd_prior[fcn==fl] <- 0.138
}
est$index_data <- est$index_data %>% mutate(Month = case_when(Fleet_name %in% c("BTS","BTS_1","ATS","ATS_1") ~ 6, TRUE ~ 0))
est$comp_data  <- est$comp_data  %>% mutate(Month = case_when(Fleet_name=="BTS" ~ 6, Fleet_name=="ATS" ~ 6, TRUE ~ Month))
est$fleet_control$Catchability <- as.character(est$fleet_control$Catchability)
est$fleet_control$Catchability[fcn=="ATS"] <- "1"
est$fleet_control$Catchability[fcn %in% c("BTS_1","ATS_1")] <- "3"
est$index_data$Log_sd[est$index_data$Fleet_name %in% c("BTS_1","ATS_1")] <- 1
est$sigma_rec_prior <- 1              # FIX: full-normal rec penalty (m23_rceattle_full sigr=1)
est$fleet_control$Index_loglike[fcn=="BTS"] <- "MVN"
est$fleet_control$Catchability[fcn=="BTS"]  <- "AnalyticalArith"
est$index_cov <- list(BTS = as.matrix(read.table("ADMB/data/cov_2024.dat")))

# FIX 1: ADMB offset (AFSC) multinomial, not the full multinomial.
est$fleet_control$Comp_loglike <- "MultinomialAFSC"

# FIX 2: restore BTS comp age-1 (= BTS_1 index obs = raw survey age-1 count, verified
# identical) and turn BTS_1 OFF. ADMB keeps age-1 in the BTS comps and has no BTS age-1
# index; the xlsx relocated it. Not double-counted, so per the decision rule keep it in
# the comp and drop the redundant BTS_1 index (keeping both WOULD double-count).
{
  b1 <- est$index_data[est$index_data$Fleet_name=="BTS_1", c("Year","Observation")]
  bc <- which(est$comp_data$Fleet_name=="BTS")
  for (r in bc) {
    yr <- est$comp_data$Year[r]
    o  <- b1$Observation[abs(b1$Year)==abs(yr)]
    if (length(o)==1) est$comp_data[r, "Comp_1"] <- o
  }
  est$fleet_control$Fleet_type[fcn=="BTS_1"] <- 0   # ADMB has no BTS age-1 index
  cat("FIX 2: restored BTS comp age-1 for", length(bc), "rows; BTS_1 index OFF\n")
}

# FIX 4: ADMB ignore_last_ats_age1 -- the LAST-year ATS NUMBERS CV (std_ot_ats/ot_ats
# = 1.81) exceeds 0.4, so ADMB DROPS the terminal ATS age-1 observation (2024) from
# surv_like(3), fitting oa1_ats(1, n_ats_r-1). (NB: this is the ATS *numbers* CV, not
# the biomass CV 0.26.) Exclude it from Rceattle's likelihood by NEGATING the year
# (flt_yr < 0 => predicted but not fitted, and excluded from the analytical q too --
# guards at ceattle_v01_11.cpp:1821 & :2454). Only ATS_1 (age-1); ADMB still fits the
# ATS *biomass* index in 2024.
est$index_data$Year[est$index_data$Fleet_name=="ATS_1" & est$index_data$Year==2024] <- -2024
cat("FIX 4: ATS_1(2024) -> -2024 (excluded, matches ignore_last_ats_age1)\n")

# FIX 5: the xlsx ATS & ATS_1 INDEX rows carry Year = -2020 (excluded), but ADMB's
# yrs_ats_data has 2020 (obs 3617 / 350) and FITS it -- and the ATS COMPS already fit
# 2020. Flip the two index rows to +2020 to include them, matching ADMB.
est$index_data$Year[est$index_data$Fleet_name %in% c("ATS","ATS_1") & est$index_data$Year==-2020] <- 2020
cat("FIX 5: ATS/ATS_1 index -2020 -> 2020 (included, matches ADMB)\n")

cat(sprintf("=== match2_full (three comp fixes, sigma_rec=1, BAP=%g) vs m23_rceattle_full ===\n", BAP))
fit <- try(Rceattle::fit_mod(data_list=est, inits=NULL, file=NULL, estimateMode=0,
  random_rec=FALSE, msmMode=0, verbose=1, phase=TRUE, initMode=2,
  M1Fun=build_M1(updateM1=TRUE, M1_model=0),
  fit_control=fit_control(bias_adjust_proc=BAP, bias_adjust_obs=0, comp_offset=1e-3)))
if(inherits(fit,"try-error")){ cat("FIT ERROR:\n", attr(fit,"condition")$message, "\n"); quit(status=1) }
saveRDS(fit, file.path(SP, sprintf("match2_injfsh_bap%g.rds", BAP)))

g <- try(max(abs(fit$obj$gr(fit$opt$par))), silent=TRUE)
cat(sprintf("\nCONVERGENCE: max|grad| = %s | objective = %.3f\n",
    ifelse(inherits(g,"try-error"),"NA",format(g,digits=4)), fit$opt$opt$objective))
q <- fit$quantities

rl <- readLines(file.path(AD, "pm.rep"))
getblk <- function(key){ i<-grep(paste0("^",key,"$"),rl)[1]; rows<-list(); j<-i+1
  while(j<=length(rl)){ v<-suppressWarnings(as.numeric(strsplit(trimws(rl[j])," +")[[1]])); if(any(is.na(v))||length(v)<2) break; rows[[length(rows)+1]]<-v[1:2]; j<-j+1 }
  m<-as.data.frame(do.call(rbind,rows)); names(m)<-c("Year","val"); m }
cmp <- function(rvec, admb, lab){ d<-merge(data.frame(Year=yrs,R=as.numeric(rvec)),admb,by="Year"); d$pct<-100*(d$R-d$val)/d$val
  cat(sprintf("\n%s: cor=%.4f | mean|pct|=%.1f%% | max|pct|=%.1f%%\n", lab, cor(d$R,d$val), mean(abs(d$pct)), max(abs(d$pct))))
  for(y in c(1964,1978,1990,2008,2024)) cat(sprintf("  %d: Rceattle=%8.1f  ADMB=%8.1f  (%+.1f%%)\n", y, d$R[d$Year==y], d$val[d$Year==y], d$pct[d$Year==y])) }
cmp(q$ssb[1,1:length(yrs)], getblk("SSB"), "SSB")
cmp(q$R[1,1:length(yrs)],   getblk("R"),   "R  ")
cat("\nDONE\n")
