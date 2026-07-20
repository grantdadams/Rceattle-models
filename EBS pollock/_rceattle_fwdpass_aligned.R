# =============================================================================
# Forward-pass jnll-component diagnostic: fix Rceattle to the ADMB/RTMB MLE
# (estimateMode = 4, single eval) and print each likelihood component beside the
# base.rds (RTMB) target. Isolates which likelihood FORMS differ. Scratch.
# =============================================================================
options(warn = -1)
MODEL_DIR <- "/Users/grantadams/Documents/GitHub/Rceattle ecosystem/Rceattle-models/EBS pollock"
PKG_DIR   <- "/Users/grantadams/Documents/GitHub/Rceattle ecosystem/Rceattle"
setwd(MODEL_DIR)
suppressMessages({
  pkgload::load_all(PKG_DIR, quiet = TRUE)
  library(dplyr); library(readxl)
})
target <- readRDS(file.path(MODEL_DIR, "base.rds"))$report

mydata <- Rceattle::read_data(file = "Data/2024_EBS_pollock.xlsx")
styr <- mydata$styr; endyr <- mydata$endyr; nages <- mydata$nages
yrs <- styr:endyr; nyr <- length(yrs)
keep_age <- c("Species_name","Species","Sex","Year", paste0("Age", 1:nages))
mydata$NByageFixed <- mydata$NByageFixed[, keep_age]
mydata$spawn_month <- 3

par_lines <- readLines("ADMB/m23_rceattle/pm.par"); rep_lines <- readLines("ADMB/m23_rceattle/pm.rep")
get_par <- function(name){ hdr <- paste0("# ",name,":"); i <- which(par_lines==hdr)[1]
  if(is.na(i)) stop("not found: ",name); vals<-c(); j<-i+1
  while(j<=length(par_lines) && !grepl("^#",par_lines[j])){ vals<-c(vals,as.numeric(strsplit(trimws(par_lines[j]),"\\s+")[[1]])); j<-j+1 }; vals }
log_avgrec<-get_par("log_avgrec"); log_avg_F<-tryCatch(get_par("log_avg_F"), error=function(e) -1.6)
log_F_devs<-get_par("log_F_devs"); log_rec_devs<-get_par("log_rec_devs")
log_q_ats<-get_par("log_q_ats"); log_q_avo<-get_par("log_q_avo")
n_selages_fsh<-12
sel_coffs_fsh<-get_par("sel_coffs_fsh")
sel_devs_fsh<-matrix(get_par("sel_devs_fsh"),ncol=n_selages_fsh,byrow=TRUE)
yrs_ch_fsh<-1965:2024
selcoff_dev<-matrix(0,nyr,n_selages_fsh); cum_dev<-rep(0,n_selages_fsh)
for(yi in 2:nyr){ k<-which(yrs_ch_fsh==yrs[yi-1]); if(length(k)==1) cum_dev<-cum_dev+sel_devs_fsh[k,]; selcoff_dev[yi,]<-cum_dev }
sel_slp_bts<-get_par("sel_slp_bts"); sel_a50_bts<-get_par("sel_a50_bts"); sel_age_one_bts<-get_par("sel_age_one_bts")
bts_slp_dev<-c(0,get_par("sel_slp_bts_dev_est")); bts_a50_dev<-c(0,get_par("sel_a50_bts_dev_est")); bts_a1_dev<-c(0,get_par("sel_age_one_bts_dev_est"))
bts_styr<-endyr-length(bts_slp_dev)+1; bts_yrs<-bts_styr:endyr
ats_styr<-1994
sel_ats_rep<-{ i0<-which(rep_lines=="sel_ats")[1]
  t(sapply((i0+1):(i0+nyr), function(L) as.numeric(strsplit(trimws(rep_lines[L]),"\\s+")[[1]]))) }

fp_data<-mydata
fp_data$estDynamics<-1
fp_data$fleet_control$Selectivity<-0
fcn<-fp_data$fleet_control$Fleet_name
fp_data$fleet_control$Selectivity[fcn=="Fishery"]<-"NonParametricPM"; fp_data$fleet_control$Time_varying_sel[fcn=="Fishery"]<-"RandomWalk"
fp_data$fleet_control$N_sel_bins[fcn=="Fishery"]<-n_selages_fsh
fp_data$fleet_control$Sel_curve_pen1[fcn=="Fishery"]<-12.5; fp_data$fleet_control$Sel_curve_pen2[fcn=="Fishery"]<-1/60
fp_data$fleet_control$Sel_curve_pen3<-0; fp_data$fleet_control$Sel_curve_pen3[fcn=="Fishery"]<-1
fp_data$fleet_control$Time_varying_sel_sd_prior[fcn=="Fishery"]<-0.5; fp_data$fleet_control$Sel_norm_bin1[fcn=="Fishery"]<-NA
fp_data$fleet_control$Selectivity[fcn=="BTS"]<-"LogisticPM"; fp_data$fleet_control$Time_varying_sel[fcn=="BTS"]<-"RandomWalk"
fp_data$fleet_control$Sel_curve_pen1[fcn=="BTS"]<-2; fp_data$fleet_control$Sel_curve_pen2[fcn=="BTS"]<-0; fp_data$fleet_control$Sel_curve_pen3[fcn=="BTS"]<-8
fp_data$fleet_control$Sel_norm_bin1[fcn=="BTS"]<-3; fp_data$fleet_control$Sel_norm_bin2[fcn=="BTS"]<-14
fp_data$fleet_control$Sel_start_year[fcn=="BTS"]<-bts_styr; fp_data$fleet_control$Bin_first_selected[fcn=="BTS"]<-1
fp_data$fleet_control$Time_varying_sel_sd_prior[fcn=="BTS"]<-1
fp_data$fleet_control$Selectivity[fcn=="ATS"]<-"NonParametricPM"; fp_data$fleet_control$Time_varying_sel[fcn=="ATS"]<-"RandomWalk"
fp_data$fleet_control$N_sel_bins[fcn=="ATS"]<-nages
fp_data$fleet_control$Sel_curve_pen1[fcn=="ATS"]<- -1; fp_data$fleet_control$Sel_curve_pen2[fcn=="ATS"]<-1; fp_data$fleet_control$Sel_curve_pen3[fcn=="ATS"]<-0
fp_data$fleet_control$Sel_norm_bin1[fcn=="ATS"]<-NA; fp_data$fleet_control$Sel_start_year[fcn=="ATS"]<-ats_styr
fp_data$fleet_control$Bin_first_selected[fcn=="ATS"]<-1; fp_data$fleet_control$Sel_pen_first_bin[fcn=="ATS"]<-2
fp_data$fleet_control$Time_varying_sel_sd_prior[fcn=="ATS"]<-0.138
fp_data$index_data<-fp_data$index_data %>% dplyr::mutate(Month=dplyr::case_when(Fleet_name %in% c("BTS","BTS_1","ATS","ATS_1")~6, TRUE~0))
fp_data$fleet_control$Catchability<-as.character(fp_data$fleet_control$Catchability)
fp_data$fleet_control$Catchability[fp_data$fleet_control$Fleet_name %in% c("BTS","ATS","AVO")]<-3
fp_data$fleet_control$Index_loglike[fcn=="BTS"]<-"MVN"
fp_data$fleet_control$Catchability[fcn=="BTS"]<-"AnalyticalArith"
fp_data$index_cov<-list(BTS=as.matrix(read.table("ADMB/data/cov_2024.dat")))
avo_code<-fp_data$fleet_control$Fleet_code[fcn=="AVO"]
ats_es<-fp_data$emp_sel[fp_data$emp_sel$Fleet_name=="ATS",]; ats_es$Fleet_name<-"AVO"; ats_es$Fleet_code<-avo_code
fp_data$emp_sel<-rbind(fp_data$emp_sel,ats_es)

inits<-build_params(fp_data)
inits$sel_coff[1,1,1:n_selages_fsh]<-sel_coffs_fsh
for(yi in 1:nyr) inits$sel_coff_dev[1,1,1:n_selages_fsh,yi]<-selcoff_dev[yi,]
btsf<-which(fcn=="BTS")
inits$log_sel_slp[1,btsf,1]<-log(sel_slp_bts); inits$sel_inf[1,btsf,1]<-sel_a50_bts; inits$sel_inf[2,btsf,1]<-sel_age_one_bts
for(k in seq_along(bts_yrs)){ yi<-which(yrs==bts_yrs[k])
  inits$log_sel_slp_dev[1,btsf,1,yi]<-bts_slp_dev[k]; inits$sel_inf_dev[1,btsf,1,yi]<-bts_a50_dev[k]; inits$sel_inf_dev[2,btsf,1,yi]<-bts_a1_dev[k] }
atsf<-which(fcn=="ATS"); inits$sel_coff[atsf,1,1:nages]<-0
for(yi in 1:nyr) if(yrs[yi]>=ats_styr) inits$sel_coff_dev[atsf,1,1:nages,yi]<-log(sel_ats_rep[yi,])
inits$log_F[1,1:nyr]<-log_avg_F+log_F_devs
inits$rec_pars[1,1]<-log_avgrec; inits$rec_dev[1,1:nyr]<-log_rec_devs
inits$index_log_q[2]<-log_q_avo

m1<-Rceattle::fit_mod(data_list=fp_data, inits=inits, file=NULL, estimateMode=4,
  random_rec=FALSE, msmMode=0, verbose=0, phase=FALSE, initMode=2,
  M1Fun=build_M1(updateM1=TRUE, M1_model=0), fit_control=fit_control(bias_adjust_proc=0, bias_adjust_obs=0))
m1$quantities$R[1,1]<-m1$quantities$N_at_age[1,1,1,1]

jc<-m1$quantities$jnll_comp
cat("\n================= FORWARD-PASS jnll_comp (Rceattle) =================\n")
print(round(jc,4))
cat("\nRowSums:\n"); print(round(rowSums(jc),4))

cat("\n================= COMPONENT-BY-COMPONENT vs base.rds =================\n")
cn<-colnames(jc)
g <- function(row, col) { ci<-which(cn==col); if(length(ci)==0) return(NA); jc[row,ci] }
cat(sprintf("%-28s %12s %12s\n","component","Rceattle","target(RTMB)"))
cat(sprintf("%-28s %12.4f %12.4f\n","Catch (Fishery)", g(2,"Fishery"), target$cat_like))
cat(sprintf("%-28s %12.4f %12.4f\n","Index BTS",       g(1,"BTS"),     target$bts_like))
cat(sprintf("%-28s %12.4f %12.4f\n","Index ATS",       g(1,"ATS"),     target$ats_like))
cat(sprintf("%-28s %12.4f %12.4f\n","Index ATS_1",     g(1,"ATS_1"),   target$ats_age1_like))
cat(sprintf("%-28s %12.4f %12.4f\n","Index AVO",       g(1,"AVO"),     target$avo_like))
cat(sprintf("%-28s %12.4f %12.4f\n","Comp Fishery",    g(3,"Fishery"), target$age_like[1]))
cat(sprintf("%-28s %12.4f %12.4f\n","Comp BTS",        g(3,"BTS"),     target$age_like[2]))
cat(sprintf("%-28s %12.4f %12.4f\n","Comp ATS",        g(3,"ATS"),     target$age_like[3]))
cat(sprintf("%-28s %12.4f %12.4f\n","Sel shape FSH",   g(5,"Fishery"), target$sel_like[1]))
cat(sprintf("%-28s %12.4f %12.4f\n","Sel shape BTS",   g(5,"BTS"),     target$sel_like[2]))
cat(sprintf("%-28s %12.4f %12.4f\n","Sel shape ATS",   g(5,"ATS"),     target$sel_like[3]))
cat(sprintf("%-28s %12.4f %12.4f\n","Sel dev FSH",     g(6,"Fishery"), target$sel_like_dev[1]))
cat(sprintf("%-28s %12.4f %12.4f\n","Sel dev BTS",     g(6,"BTS"),     target$sel_like_dev[2]))
cat(sprintf("%-28s %12.4f %12.4f\n","Sel dev ATS",     g(6,"ATS"),     target$sel_like_dev[3]))
cat(sprintf("%-28s %12.4f %12.4f\n","Rec dev penalty", g(11,1),        target$rec_like[2]))
cat(sprintf("%-28s %12.4f %12.4f\n","Init dev penalty",g(10,1),        target$rec_like[4]))
cat(sprintf("%-28s %12.4f %12.4f\n","SR penalty",      g(12,1),        target$rec_like[1]))

ssb_fp<-m1$quantities$ssb[1,1:nyr]; tssb<-as.numeric(target$SSB)
cat(sprintf("\nSSB forward-pass mean|%%diff| vs target: %.5f%%  max %.5f%%\n",
    100*mean(abs(ssb_fp/tssb-1)), 100*max(abs(ssb_fp/tssb-1))))
saveRDS(m1, file.path(MODEL_DIR,"_rceattle_fwdpass.rds"))
cat("\n================= TOTAL vs ALIGNED ADMB =================\n")
cat(sprintf("Rceattle sum(jnll_comp) at ADMB's MLE = %.4f\n", sum(jc)))
cat(sprintf("ADMB tot_like (pm.par header)         = 740.5251\n"))
cat(sprintf("DIFFERENCE                            = %+.4f\n", sum(jc)-740.5251))
rl<-readLines("ADMB/m23_rceattle/pm.rep")
gb<-function(k){i<-grep(paste0("^",k,"$"),rl)[1];r<-list();j<-i+1
  while(j<=length(rl)){v<-suppressWarnings(as.numeric(strsplit(trimws(rl[j])," +")[[1]]));if(any(is.na(v))||length(v)<2)break;r[[length(r)+1]]<-v[1:2];j<-j+1};d<-as.data.frame(do.call(rbind,r));names(d)<-c("Year","val");d}
assb<-gb("SSB"); d<-merge(data.frame(Year=yrs,R=ssb_fp),assb,by="Year")
cat(sprintf("\nSSB vs ADMB: mean|pct|=%.4f%%  max|pct|=%.4f%%\n",
  mean(abs(100*(d$R-d$val)/d$val)), max(abs(100*(d$R-d$val)/d$val))))
for(y in c(1964,1978,2024)) cat(sprintf("  %d: Rceattle=%9.2f  ADMB=%9.2f\n", y, d$R[d$Year==y], d$val[d$Year==y]))
cat("\n==== DONE ====\n")
