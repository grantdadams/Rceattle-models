suppressMessages({library(dplyr); library(readxl)})
pkgload::load_all("/Users/grantadams/Documents/GitHub/Rceattle ecosystem/Rceattle", quiet = TRUE)
setwd("/Users/grantadams/Documents/GitHub/Rceattle ecosystem/Rceattle-models/EBS pollock")
AD <- "ADMB/m23_rceattle_full"
pl <- readLines(file.path(AD,"pm.par")); rl <- readLines(file.path(AD,"pm.rep"))
get_par <- function(nm){ i<-which(pl==paste0("# ",nm,":"))[1]; v<-c(); j<-i+1
  while(j<=length(pl)&&!grepl("^#",pl[j])){v<-c(v,as.numeric(strsplit(trimws(pl[j]),"[[:space:]]+")[[1]]));j<-j+1}; v }
blk <- function(nm,n=61){ i<-which(rl==nm)[1]; t(sapply(1:n,function(k) as.numeric(strsplit(trimws(rl[i+k]),"[[:space:]]+")[[1]]))) }
# ADMB observed/predicted P-at-age from old_rep.rep
orl <- readLines(file.path(AD,"old_rep.rep"))
readPA <- function(hdr){ i<-grep(hdr,orl)[1]; rows<-list(); j<-i+1
  while(j<=length(orl)){ v<-suppressWarnings(as.numeric(strsplit(trimws(orl[j]),"[[:space:]]+")[[1]]))
    if(length(v)<16 || any(is.na(v[1:16]))) break; rows[[length(rows)+1]]<-v[1:16]; j<-j+1 }
  m<-do.call(rbind,rows); rownames(m)<-m[,1]; m[,-1,drop=FALSE] }
bts_obs_A <- readPA("Survey Observed P at age")
bts_hat_A <- readPA("Survey Predicted P at age")

mydata <- Rceattle::read_data(file="Data/2024_EBS_pollock.xlsx")
nages<-mydata$nages; yrs<-mydata$styr:mydata$endyr; nyr<-length(yrs)
keep<-c("Species_name","Species","Sex","Year",paste0("Age",1:nages))
mydata$NByageFixed<-mydata$NByageFixed[,intersect(keep,colnames(mydata$NByageFixed))]
mydata$spawn_month<-3
d<-mydata; d$estDynamics<-0; d$fleet_control$Selectivity<-0
fcn<-d$fleet_control$Fleet_name
d$fleet_control$Fleet_type[5:6]<-2; d$age_error[1:nages,3:(nages+2)]<-diag(nages)
d$catch_data$Log_sd<-0.05; d$sigma_rec_prior<-1
d$fleet_control$Fleet_type[fcn=="BTS_1"]<-0
sel_fsh<-blk("sel_fsh"); sel_bts<-blk("sel_bts"); sel_ats<-blk("sel_ats")
admb_sel<-list(Fishery=sel_fsh,BTS=sel_bts,ATS=sel_ats,AVO=sel_ats)
cols<-colnames(d$emp_sel); ccol<-paste0("Comp_",1:nages)
es<-d$emp_sel[!(d$emp_sel$Fleet_name %in% names(admb_sel)),]
for(fl in names(admb_sel)){ add<-d$emp_sel[0,]; add[1:nyr,]<-NA
  add$Fleet_name<-fl; add$Fleet_code<-d$fleet_control$Fleet_code[fcn==fl]
  add$Species<-1; add$Sex<-0; add$Year<-yrs
  for(a in 1:nages) add[[ccol[a]]]<-admb_sel[[fl]][,a]; es<-rbind(es,add[,cols]) }
d$emp_sel<-es
d$fleet_control$Catchability<-as.character(d$fleet_control$Catchability)
d$fleet_control$Catchability[fcn %in% c("BTS","ATS","AVO","BTS_1","ATS_1")]<-"3"
inits<-build_params(d)
inits$rec_pars[1,1]<-get_par("log_avgrec"); inits$rec_dev[1,1:nyr]<-get_par("log_rec_devs")
inits$log_F[1,1:nyr]<-get_par("log_avg_F")+get_par("log_F_devs")
idv<-get_par("log_initdevs"); inits$init_dev[1,1:length(idv)]<-idv
fit<-Rceattle::fit_mod(data_list=d,inits=inits,file=NULL,estimateMode=1,
  random_rec=FALSE,msmMode=0,verbose=0,phase=FALSE,initMode=2,
  M1Fun=build_M1(updateM1=TRUE,M1_model=0),
  fit_control=fit_control(bias_adjust_proc=0,bias_adjust_obs=0,comp_offset=1e-3))
p<-fit$obj$par; nm<-names(p)
p[nm=="rec_pars"]<-get_par("log_avgrec"); p[nm=="rec_dev"]<-get_par("log_rec_devs")
p[nm=="log_F"]<-get_par("log_avg_F")+get_par("log_F_devs"); p[nm=="init_dev"]<-get_par("log_initdevs")
rep<-fit$obj$report(p)
# Rceattle comp arrays: match rows to BTS fleet
cc<-fit$data_list$comp_ctl; btscode<-fit$data_list$fleet_control$Fleet_code[fcn=="BTS"]
bts_rows<-which(cc[,1]==btscode)
co<-rep$comp_obs; ch<-rep$comp_hat
cat("=== BTS: Rceattle vs ADMB observed & predicted P-at-age ===\n")
for (yr in c(1982, 2000, 2024)) {
  yidx<-cc[bts_rows,5]
  ri<-bts_rows[which(yidx==(yr-mydata$styr+1))]
  if(length(ri)!=1){ cat("year",yr,"no unique row\n"); next }
  ro_obs<-co[ri,1:nages]; ro_hat<-ch[ri,1:nages]
  ao_obs<-bts_obs_A[as.character(yr),]; ao_hat<-bts_hat_A[as.character(yr),]
  cat(sprintf("\n--- %d --- (Rce sum obs=%.4f hat=%.4f | ADMB sum obs=%.4f hat=%.4f)\n",
      yr, sum(ro_obs), sum(ro_hat), sum(ao_obs), sum(ao_hat)))
  cat("age    Rce_obs   ADMB_obs    Rce_hat  ADMB_hat\n")
  for(a in 1:6) cat(sprintf(" %2d  %9.5f %9.5f  %9.5f %9.5f\n", a, ro_obs[a], ao_obs[a], ro_hat[a], ao_hat[a]))
}
cat("\n=== max |Rce_obs - ADMB_obs| and |Rce_hat - ADMB_hat| over all BTS years ===\n")
do<-dh<-c()
for (yr in as.numeric(rownames(bts_obs_A))) {
  ri<-bts_rows[which(cc[bts_rows,5]==(yr-mydata$styr+1))]; if(length(ri)!=1) next
  do<-c(do, max(abs(co[ri,1:nages]-bts_obs_A[as.character(yr),])))
  dh<-c(dh, max(abs(ch[ri,1:nages]-bts_hat_A[as.character(yr),]))) }
cat("observed props: max|diff| =", round(max(do),5), " (mean", round(mean(do),5),")\n")
cat("predicted props: max|diff| =", round(max(dh),5), " (mean", round(mean(dh),5),")\n")
# Reconstruct ADMB ea1_ats = N_1 * S_1^0.5, q = geomean(oa1/ea1)
N<-blk("N"); S<-blk("S")
id <- fit$data_list$index_data; id$pred <- as.numeric(fit$quantities$index_hat)
a1 <- id[id$Fleet_name=="ATS_1", c("Year","Observation","pred")]
a1 <- a1[order(abs(a1$Year)),]
# ADMB ea1_ats at each ATS_1 year
ea1 <- sapply(abs(a1$Year), function(y){ k<-which(yrs==y); N[k,1]*sqrt(S[k,1]) })
qA <- exp(mean(log(a1$Observation/ea1)))    # ADMB qtmp = geometric mean
a1$ADMB_pred <- ea1 * qA
# Rceattle analytical q for ATS_1
a1$Rce_resid  <- log(a1$Observation/a1$pred)
a1$ADMB_resid <- log(a1$Observation/a1$ADMB_pred)
cat("=== ATS_1 age-1 index: obs vs Rceattle pred vs ADMB ea1*q (at ADMB MLE) ===\n")
print(data.frame(Year=a1$Year, obs=round(a1$Observation,1),
  Rce_pred=round(a1$pred,1), ADMB_pred=round(a1$ADMB_pred,1),
  Rce_resid=round(a1$Rce_resid,3), ADMB_resid=round(a1$ADMB_resid,3)), row.names=FALSE)
cat(sprintf("\nRceattle q(ATS_1) implied = %.4g   ADMB qtmp = %.4g   ratio = %.3f\n",
  exp(mean(log(a1$Observation/a1$pred))), qA, exp(mean(log(a1$Observation/a1$pred)))/qA))
cat(sprintf("SSE: Rce=%.2f  ADMB=%.2f  (surv_like(3) = 0.5*SSE)\n", sum(a1$Rce_resid^2), sum(a1$ADMB_resid^2)))
