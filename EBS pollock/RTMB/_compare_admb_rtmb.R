# How different are the ADMB and RTMB versions? Evaluate the RTMB model AT the
# ADMB MLE parameters (parms = read_pars(pm.par)) and compare its report to the
# ADMB rep object `pm` (read_rep(pm.rep)) -- a pure structural/port comparison
# at identical parameters (no re-fitting). Scratch.
RTMB_DIR <- "/Users/grantadams/Documents/GitHub/Rceattle ecosystem/Rceattle-models/EBS pollock/RTMB"
Sys.setenv(RTMB_EBSWP_ROOT = RTMB_DIR)
Sys.setenv(POLLOCK_ROOT   = file.path(RTMB_DIR, ".pollock_root"))
setwd(RTMB_DIR)
options(warn = -1)
cfg <- readLines(file.path("R", "config.R"))
stop_at <- grep("MakeADFun", cfg)[1]
eval(parse(text = paste(cfg[seq_len(stop_at - 1)], collapse = "\n")), envir = globalenv())

data$return_nll_only <- 0
r <- rpm(parms)               # RTMB report at the ADMB MLE parameters

pd <- function(a, b) {        # mean & max abs % diff (aligned, common length)
  a <- as.numeric(a); b <- as.numeric(b); n <- min(length(a), length(b))
  a <- a[seq_len(n)]; b <- b[seq_len(n)]
  ok <- is.finite(a) & is.finite(b) & b != 0
  c(mean = 100*mean(abs(a[ok]/b[ok]-1)), max = 100*max(abs(a[ok]/b[ok]-1)))
}

cat("\n#### pm (ADMB rep) fields:", paste(names(pm), collapse=", "), "\n")

cat("\n#### RTMB-at-ADMB-params  vs  ADMB rep (pm) ####\n")
cat("--- population (should be ~0 if RTMB is a faithful port) ---\n")
cat(sprintf("SSB         mean|%%diff|=%.4f%%  max=%.4f%%\n", pd(r$SSB, pm$SSB)[1], pd(r$SSB, pm$SSB)[2]))
if (!is.null(pm$N))  cat(sprintf("N-at-age    mean|%%diff|=%.4f%%  max=%.4f%%\n", pd(as.numeric(r$N), as.numeric(pm$N))[1], pd(as.numeric(r$N), as.numeric(pm$N))[2]))
if (!is.null(pm$recruits)) cat(sprintf("recruits    mean|%%diff|=%.4f%%\n", pd(r$recruitment, pm$recruits)[1]))
if (!is.null(pm$pred_catch)) cat(sprintf("pred_catch  mean|%%diff|=%.4f%%\n", pd(r$pred_catch, pm$pred_catch)[1]))
if (!is.null(pm$sel_fsh)) cat(sprintf("sel_fsh     mean|%%diff|=%.4f%%  max=%.4f%%\n", pd(as.numeric(r$sel_fsh), as.numeric(pm$sel_fsh))[1], pd(as.numeric(r$sel_fsh), as.numeric(pm$sel_fsh))[2]))
if (!is.null(pm$eb_bts)) cat(sprintf("eb_bts      mean|%%diff|=%.4f%%\n", pd(r$eb_bts, pm$eb_bts)[1]))

cat("\n--- likelihood components (RTMB report vs ADMB rep) ---\n")
cmp <- function(nm_r, nm_p) {
  vr <- r[[nm_r]]; vp <- pm[[nm_p]]
  if (is.null(vr) || is.null(vp)) { cat(sprintf("  %-14s RTMB=%s  ADMB=%s\n", nm_r, if(is.null(vr))"NA" else paste(round(as.numeric(vr),4),collapse=","), if(is.null(vp))"NA" else paste(round(as.numeric(vp),4),collapse=","))); return(invisible()) }
  cat(sprintf("  %-14s RTMB=%s   ADMB=%s\n", nm_r,
      paste(round(as.numeric(vr),4),collapse=","), paste(round(as.numeric(vp),4),collapse=",")))
}
cmp("bts_like","surv_like"); cmp("age_like","age_like")
cmp("sel_like","sel_like"); cmp("sel_like_dev","sel_like_dev")
cmp("rec_like","rec_like"); cmp("cat_like","catch_like")
cat(sprintf("\nRTMB tot nll at ADMB params: %.3f\n", r$tot_like))
if (!is.null(pm$NLL)) cat(sprintf("ADMB sum(NLL): %.3f\n", sum(pm$NLL)))
cat("\n==== DONE ====\n")
