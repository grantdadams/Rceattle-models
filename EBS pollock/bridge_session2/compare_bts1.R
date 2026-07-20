# Component-by-component likelihood comparison: Rceattle evaluated AT the
# m23_rceattle_full MLE (identical parameters) vs ADMB's reported components.
# emp_sel bypass (selectivity penalties absent) + corrected config:
#   BTS_1 off, ATS/AVO age-1 zeroed (Bin_first_selected=2), comp_offset=1e-3,
#   Comp_weights=1 (Francis NOT applied), sigma_rec_prior=1, BAP=0.
suppressMessages({library(dplyr); library(readxl)})
pkgload::load_all("/Users/grantadams/Documents/GitHub/Rceattle ecosystem/Rceattle", quiet = TRUE)
setwd("/Users/grantadams/Documents/GitHub/Rceattle ecosystem/Rceattle-models/EBS pollock")
AD <- "ADMB/m23_rceattle_full"
pl <- readLines(file.path(AD,"pm.par")); rl <- readLines(file.path(AD,"pm.rep"))
get_par <- function(nm){ i<-which(pl==paste0("# ",nm,":"))[1]; v<-c(); j<-i+1
  while(j<=length(pl)&&!grepl("^#",pl[j])){v<-c(v,as.numeric(strsplit(trimws(pl[j]),"[[:space:]]+")[[1]]));j<-j+1}; v }
blk <- function(nm,n=61){ i<-which(rl==nm)[1]; t(sapply(1:n,function(k) as.numeric(strsplit(trimws(rl[i+k]),"[[:space:]]+")[[1]]))) }
g1 <- function(nm){ i<-which(rl==nm)[1]; as.numeric(strsplit(trimws(rl[i+1]),"[[:space:]]+")[[1]]) }

mydata <- Rceattle::read_data(file="Data/2024_EBS_pollock.xlsx")
nages <- mydata$nages; yrs <- mydata$styr:mydata$endyr; nyr <- length(yrs)
keep <- c("Species_name","Species","Sex","Year",paste0("Age",1:nages))
mydata$NByageFixed <- mydata$NByageFixed[,intersect(keep,colnames(mydata$NByageFixed))]
mydata$spawn_month <- 3
d <- mydata; d$estDynamics <- 0; d$fleet_control$Selectivity <- 0
fcn <- d$fleet_control$Fleet_name
d$fleet_control$Fleet_type[5:6] <- 2
d$age_error[1:nages,3:(nages+2)] <- diag(nages)
d$catch_data$Log_sd <- 0.05
d$sigma_rec_prior <- 1
d$fleet_control$Fleet_type[fcn=="BTS_1"] <- 0
    d$fleet_control$Comp_loglike <- "MultinomialAFSC"        # ADMB has NO BTS age-1 index
{
  b1 <- d$index_data[d$index_data$Fleet_name=="BTS_1", c("Year","Observation")]
  bc <- which(d$comp_data$Fleet_name=="BTS")
  for (r in bc) {
    yr <- d$comp_data$Year[r]
    o  <- b1$Observation[abs(b1$Year)==abs(yr)]
    if (length(o)==1) d$comp_data[r, "Comp_1"] <- o
  }
  cat("restored BTS comp age-1 for", length(bc), "rows\n")
}

sel_fsh<-blk("sel_fsh"); sel_bts<-blk("sel_bts"); sel_ats<-blk("sel_ats")
admb_sel <- list(Fishery=sel_fsh, BTS=sel_bts, ATS=sel_ats, AVO=sel_ats)
cols <- colnames(d$emp_sel); ccol <- paste0("Comp_",1:nages)
es <- d$emp_sel[!(d$emp_sel$Fleet_name %in% names(admb_sel)),]
for(fl in names(admb_sel)){ add<-d$emp_sel[0,]; add[1:nyr,]<-NA
  add$Fleet_name<-fl; add$Fleet_code<-d$fleet_control$Fleet_code[fcn==fl]
  add$Species<-1; add$Sex<-0; add$Year<-yrs
  for(a in 1:nages) add[[ccol[a]]]<-admb_sel[[fl]][,a]
  es<-rbind(es,add[,cols]) }
d$emp_sel <- es
d$fleet_control$Catchability <- as.character(d$fleet_control$Catchability)
d$fleet_control$Catchability[fcn %in% c("BTS","ATS","AVO","BTS_1","ATS_1")] <- "3"
d$fleet_control$Bin_first_selected[fcn %in% c("ATS","AVO")] <- 2   # zero age-1

inits <- build_params(d)
inits$rec_pars[1,1] <- get_par("log_avgrec")
inits$rec_dev[1,1:nyr] <- get_par("log_rec_devs")
inits$log_F[1,1:nyr] <- get_par("log_avg_F")+get_par("log_F_devs")
idv <- get_par("log_initdevs"); inits$init_dev[1,1:length(idv)] <- idv

fit <- Rceattle::fit_mod(data_list=d, inits=inits, file=NULL, estimateMode=1,
  random_rec=FALSE, msmMode=0, verbose=0, phase=FALSE, initMode=2,
  M1Fun=build_M1(updateM1=TRUE,M1_model=0),
  fit_control=fit_control(bias_adjust_proc=0, bias_adjust_obs=0, comp_offset=1e-3))
p <- fit$obj$par; nm <- names(p)
p[nm=="rec_pars"] <- get_par("log_avgrec"); p[nm=="rec_dev"] <- get_par("log_rec_devs")
p[nm=="log_F"] <- get_par("log_avg_F")+get_par("log_F_devs"); p[nm=="init_dev"] <- get_par("log_initdevs")
jc <- fit$obj$report(p)$jnll_comp
fc <- fit$data_list$fleet_control; fco <- fc$Fleet_code

cat("\n================ LIKELIHOOD COMPARISON (at m23_rceattle_full MLE) ================\n")
cat(sprintf("%-26s %14s %14s\n","component","Rceattle","ADMB_full"))
cat(strrep("-",56),"\n")
prn <- function(lab,r,a) cat(sprintf("%-26s %14.3f %14s\n",lab,r,ifelse(is.na(a),"--",sprintf("%.3f",a))))
# DATA
sl <- g1("surv_like"); 
prn("Catch",              sum(jc[2,]),  g1("cat_like"))
prn("  Index: BTS (MVN?)",jc[1,fco[fcn=="BTS"]],  sl[1])
prn("  Index: ATS",       jc[1,fco[fcn=="ATS"]],  sl[2])
prn("  Index: ATS age-1", jc[1,fco[fcn=="ATS_1"]], sl[3])
prn("  Index: AVO",       jc[1,fco[fcn=="AVO"]],  g1("avo_like"))
prn("  Index: CPUE",      jc[1,fco[fcn=="Fishery"]], g1("cpue_like"))
al <- g1("age_like")
prn("  Comp: Fishery",    jc[3,fco[fcn=="Fishery"]], al[1])
prn("  Comp: BTS",        jc[3,fco[fcn=="BTS"]], al[2])
prn("  Comp: ATS",        jc[3,fco[fcn=="ATS"]], al[3])
prn("Rec-dev penalty",    sum(jc[11,]), g1("rec_like")[2])
prn("Init-dev penalty",   sum(jc[10,]), g1("rec_like")[4])
cat(strrep("-",56),"\n")
cat("ADMB-only components (no Rceattle counterpart):\n")
cat(sprintf("   len_like (length comps)     = %.3f\n", g1("len_like")))
cat(sprintf("   wt_like  (wt-at-age submodel)= %.1f\n", g1("wt_like")))
cat(sprintf("   sel_like + sel_like_dev     = %.3f  (bypassed here via emp_sel)\n",
    sum(g1("sel_like"))+sum(g1("sel_like_dev"))))
cat(sprintf("\nADMB tot_like = %.3f  | dat_like = %.3f\n", g1("tot_like"), g1("dat_like")))
cat(sprintf("Rceattle total jnll (this eval) = %.3f\n", sum(jc)))
