# Rceattle (ADMB-faithful bridging Model-2 config + MVN BTS + AnalyticalArith q)
# vs the STRUCTURALLY-ALIGNED ADMB (ADMB/m23_rceattle: wt submodel off, BTS devs
# non-bounded/no sum-to-zero/first-yr fixed, SrType=3 mean recruitment).
# Run AFTER the build_params sel_inf[2] LogisticPM init fix.
suppressMessages({library(dplyr); library(readxl)})
pkgload::load_all("/Users/grantadams/Documents/GitHub/Rceattle ecosystem/Rceattle", quiet = TRUE)
setwd("/Users/grantadams/Documents/GitHub/Rceattle ecosystem/Rceattle-models/EBS pollock")
SP <- "."  # was session scratchpad; now repo-local
BAP <- as.numeric(Sys.getenv("BAP", "0"))
n_selages_fsh <- 12; bts_styr <- 1982; ats_styr <- 1994

mydata <- Rceattle::read_data(file = "Data/2024_EBS_pollock.xlsx")
nages <- mydata$nages; yrs <- mydata$styr:mydata$endyr
keep_age <- c("Species_name","Species","Sex","Year", paste0("Age",1:nages))
mydata$NByageFixed <- mydata$NByageFixed[, intersect(keep_age, colnames(mydata$NByageFixed))]
mydata$spawn_month <- 3

est <- mydata
est$estDynamics <- 0
# NOTE: do NOT divide Log_sd by Observation. The xlsx index Log_sd is ALREADY a
# CV / log-sd (0.05-0.56 by fleet; ADMB's own sdnr_bts/ats/avo = 0.95/0.99/0.98
# confirm it is correctly calibrated as-is). The old "SD -> CV" division produced
# CVs of ~1e-5, i.e. demanding the AVO/ATS indices be fit to 5 decimal places:
# index jnll 743,587 (AVO 624,585 + ATS 118,280) vs a converged 108.9. That single
# line WAS the non-convergence -- see bridge_session2/dynfit.R.
est$catch_data$Log_sd <- 0.05
fcn <- est$fleet_control$Fleet_name
est$fleet_control$Fleet_type[5:6] <- 2
est$age_error[1:nages, 3:(nages+2)] <- diag(nages)
est$fleet_control$Selectivity[fcn=="Fishery"] <- "NonParametricPM"
est$fleet_control$Time_varying_sel[fcn=="Fishery"] <- "RandomWalk"
est$fleet_control$N_sel_bins[fcn=="Fishery"] <- n_selages_fsh
est$fleet_control$Sel_curve_pen1[fcn=="Fishery"] <- 12.5
est$fleet_control$Sel_curve_pen2[fcn=="Fishery"] <- 1/60
est$fleet_control$Sel_curve_pen3 <- 0
est$fleet_control$Sel_curve_pen3[fcn=="Fishery"] <- 1
est$fleet_control$Sel_norm_bin1[fcn=="Fishery"] <- NA
est$fleet_control$Time_varying_sel_sd_prior[fcn=="Fishery"] <- 0.5
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
  est$fleet_control$Bin_first_selected[fcn==fl] <- 2   # ADMB mina_ats=2: ATS sel estimated over ages 2..8 = 7 bins
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
est$sigma_rec_prior <- 0.707
est$fleet_control$Index_loglike[fcn=="BTS"] <- "MVN"
est$fleet_control$Catchability[fcn=="BTS"]  <- "AnalyticalArith"
est$index_cov <- list(BTS = as.matrix(read.table("ADMB/data/cov_2024.dat")))

cat(sprintf("=== Rceattle LogisticPM + MVN (bias_adjust_proc=%g) vs m23_rceattle ===\n", BAP))
fit <- try(Rceattle::fit_mod(data_list=est, inits=NULL, file=NULL, estimateMode=0,
  random_rec=FALSE, msmMode=0, verbose=1, phase=TRUE, initMode=2,
  M1Fun=build_M1(updateM1=TRUE, M1_model=0),
  fit_control=fit_control(bias_adjust_proc=BAP, bias_adjust_obs=0)))
if(inherits(fit,"try-error")){ cat("FIT ERROR:\n", attr(fit,"condition")$message, "\n"); quit(status=1) }
saveRDS(fit, file.path(SP, sprintf("match2_bap%g.rds", BAP)))

g <- try(max(abs(fit$obj$gr(fit$opt$par))), silent=TRUE)
cat(sprintf("\nCONVERGENCE: max|grad| = %s | objective = %.3f\n",
    ifelse(inherits(g,"try-error"),"NA",format(g,digits=4)), fit$opt$objective))
q <- fit$quantities
btscode <- est$fleet_control$Fleet_code[fcn=="BTS"]
cat(sprintf("BTS MVN jnll = %.3f (ADMB tot_like = 740.5251)\n", q$jnll_comp[1, btscode]))

rl <- readLines("ADMB/m23_rceattle/pm.rep")
getblk <- function(key){ i<-grep(paste0("^",key,"$"),rl)[1]; rows<-list(); j<-i+1
  while(j<=length(rl)){ v<-suppressWarnings(as.numeric(strsplit(trimws(rl[j])," +")[[1]])); if(any(is.na(v))||length(v)<2) break; rows[[length(rows)+1]]<-v[1:2]; j<-j+1 }
  m<-as.data.frame(do.call(rbind,rows)); names(m)<-c("Year","val"); m }
cmp <- function(rvec, admb, lab){ d<-merge(data.frame(Year=yrs,R=as.numeric(rvec)),admb,by="Year"); d$pct<-100*(d$R-d$val)/d$val
  cat(sprintf("\n%s: cor=%.4f | mean|pct|=%.1f%% | max|pct|=%.1f%%\n", lab, cor(d$R,d$val), mean(abs(d$pct)), max(abs(d$pct))))
  for(y in c(1964,1978,1990,2008,2024)) cat(sprintf("  %d: Rceattle=%8.1f  ADMB=%8.1f  (%+.1f%%)\n", y, d$R[d$Year==y], d$val[d$Year==y], d$pct[d$Year==y])) }
cmp(q$ssb[1,1:length(yrs)], getblk("SSB"), "SSB")
cmp(q$R[1,1:length(yrs)],   getblk("R"),   "R  ")
cat("\nDONE\n")
