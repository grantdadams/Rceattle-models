# Parameterised match2 variant runner: VAR selects the config under test.
suppressMessages({library(dplyr); library(readxl)})
pkgload::load_all("/Users/grantadams/Documents/GitHub/Rceattle ecosystem/Rceattle", quiet = TRUE)
setwd("/Users/grantadams/Documents/GitHub/Rceattle ecosystem/Rceattle-models/EBS pollock")
VAR <- Sys.getenv("VAR", "A")
SIG <- as.numeric(Sys.getenv("SIG", "0.707"))
BAP <- as.numeric(Sys.getenv("BAP", "0"))
FRANCIS <- Sys.getenv("FRANCIS", "0") == "1"
n_selages_fsh <- 12; bts_styr <- 1982; ats_styr <- 1994
mydata <- Rceattle::read_data(file = "Data/2024_EBS_pollock.xlsx")
nages <- mydata$nages; yrs <- mydata$styr:mydata$endyr
keep_age <- c("Species_name","Species","Sex","Year", paste0("Age",1:nages))
mydata$NByageFixed <- mydata$NByageFixed[, intersect(keep_age, colnames(mydata$NByageFixed))]
mydata$spawn_month <- 3
est <- mydata
est$estDynamics <- 0
est$catch_data$Log_sd <- 0.05           # NOTE: no Log_sd/Observation -- that was the bug
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
  est$fleet_control$Bin_first_selected[fcn==fl] <- 2
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
est$sigma_rec_prior <- SIG
est$fleet_control$Index_loglike[fcn=="BTS"] <- "MVN"
est$fleet_control$Catchability[fcn=="BTS"]  <- "AnalyticalArith"
est$index_cov <- list(BTS = as.matrix(read.table("ADMB/data/cov_2024.dat")))
# BTS1_OFF: ADMB has NO BTS age-1 index. Its survey likelihood is vector surv_like(1,3)
# = BTS biomass, ATS biomass, ATS age-1 (use_age1_ats); there is no oa1_bts/ea1_bts/
# age1_sigma_bts anywhere in pm.tpl. So Rceattle's BTS_1 fleet (42 obs, 1982-2024) is
# an EXTRA data source ADMB never fits -- turn it off to match.
if (Sys.getenv("BTS1_OFF", "0") == "1") est$fleet_control$Fleet_type[fcn == "BTS_1"] <- 0
if (FRANCIS) {   # ADMB iterative-reweighting Francis weights from m23_rceattle/pm.rep
  est$fleet_control$Comp_weights[fcn=="Fishery"] <- 0.729912
  est$fleet_control$Comp_weights[fcn=="BTS"]     <- 1.2535
  est$fleet_control$Comp_weights[fcn=="ATS"]     <- 2.11955
}
cat(sprintf("=== VAR=%s  sigma_rec_prior=%.4f  BAP=%g  FRANCIS=%s ===\n", VAR, SIG, BAP, FRANCIS))
t0 <- Sys.time()
fit <- try(Rceattle::fit_mod(data_list=est, inits=NULL, file=NULL, estimateMode=0,
  random_rec=FALSE, msmMode=0, verbose=0, phase=TRUE, initMode=2,
  M1Fun=build_M1(updateM1=TRUE, M1_model=0),
  fit_control=fit_control(bias_adjust_proc=BAP, bias_adjust_obs=0)))
cat(sprintf("elapsed %.0f s\n", as.numeric(difftime(Sys.time(), t0, units="secs"))))
if(inherits(fit,"try-error")){ cat("FIT ERROR:", attr(fit,"condition")$message, "\n"); quit(status=1) }
saveRDS(fit, sprintf("bridge_session2/var_%s.rds", VAR))
ADREF <- Sys.getenv("ADREF", "ADMB/m23_rceattle/pm.rep")
rl <- readLines(ADREF)
getblk <- function(key){ i<-grep(paste0("^",key,"$"),rl)[1]; rows<-list(); j<-i+1
  while(j<=length(rl)){ v<-suppressWarnings(as.numeric(strsplit(trimws(rl[j])," +")[[1]])); if(any(is.na(v))||length(v)<2) break; rows[[length(rows)+1]]<-v[1:2]; j<-j+1 }
  m<-as.data.frame(do.call(rbind,rows)); names(m)<-c("Year","val"); m }
cmp <- function(rvec, admb, lab){ d<-merge(data.frame(Year=yrs,R=as.numeric(rvec)),admb,by="Year"); d$pct<-100*(d$R-d$val)/d$val
  cat(sprintf("%s: cor=%.4f | mean|pct|=%.2f%% | max|pct|=%.1f%% | 1964=%+.1f%% | 2024=%+.1f%%\n",
      lab, cor(d$R,d$val), mean(abs(d$pct)), max(abs(d$pct)), d$pct[d$Year==1964], d$pct[d$Year==2024])) }
q <- fit$quantities
cmp(q$ssb[1,1:length(yrs)], getblk("SSB"), "SSB")
cmp(q$R[1,1:length(yrs)],   getblk("R"),   "R  ")
cat("objective =", format(fit$opt$opt$objective, digits=8), "\n")
jc <- q$jnll_comp; cat("comps =", round(sum(jc[3,]),1), " index =", round(sum(jc[1,]),1),
    " catch =", round(sum(jc[2,]),1), " recdev =", round(sum(jc[11,]),2), " initdev =", round(sum(jc[10,]),2), "\n")
cat("DONE_", VAR, "\n", sep="")
