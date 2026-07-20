# Test: make RTMB honor DoCovBTS. Fit with IID normal BTS (DoCovBTS=0, matching
# control.dat) vs covariance MVN (DoCovBTS=1, base.rds), compare to ADMB SAFE. Scratch.
RTMB_DIR <- "/Users/grantadams/Documents/GitHub/Rceattle ecosystem/Rceattle-models/EBS pollock/RTMB"
Sys.setenv(RTMB_EBSWP_ROOT = RTMB_DIR, POLLOCK_ROOT = file.path(RTMB_DIR, ".pollock_root"))
setwd(RTMB_DIR); options(warn = -1)
cfg <- readLines(file.path("R", "config.R")); stop_at <- grep("MakeADFun", cfg)[1]
eval(parse(text = paste(cfg[seq_len(stop_at - 1)], collapse = "\n")), envir = globalenv())
# (rm(list=ls()) inside config wipes pre-set vars; (re)define after the eval)
suppressMessages(library(readxl))
EBS_DIR <- "/Users/grantadams/Documents/GitHub/Rceattle ecosystem/Rceattle-models/EBS pollock"
if (is.null(pm$NLL)) pm$NLL <- rep(1, 20)
if (is.null(pm$age_like)) pm$age_like <- rep(1, 3)
adm_ssb <- as.data.frame(read_excel(file.path(EBS_DIR, "Data", "2024_ADMB_estimate.xlsx"), sheet = "SSB"))$Est

fit_one <- function(docov) {
  data$DoCovBTS <<- docov          # modify the GLOBAL data that rpm closes over
  data$return_nll_only <<- 1
  obj <- RTMB::MakeADFun(rpm, parms, map = map_obj, silent = TRUE)
  fit <- TMBhelper::fit_tmb(obj, newtonsteps = 2, getsd = FALSE, quiet = TRUE,
                            control = list(eval.max = 1e4, iter.max = 1e4))
  pl <- obj$env$parList(obj$env$last.par.best)
  data$return_nll_only <<- 0
  r <- rpm(pl)
  data$return_nll_only <<- 1
  list(nll = fit$objective, bts = r$bts_like, ssb = as.numeric(r$SSB))
}

cat("\n#### fitting RTMB with IID BTS (DoCovBTS=0, your control.dat) ...\n")
iid <- fit_one(0)
cat("#### fitting RTMB with MVN BTS (DoCovBTS=1, base.rds) ...\n")
cov <- fit_one(1)

cat("\n################# RESULT #################\n")
cat(sprintf("%-22s %12s %12s\n", "", "IID (DoCov0)", "MVN (DoCov1)"))
cat(sprintf("%-22s %12.3f %12.3f\n", "total nll", iid$nll, cov$nll))
cat(sprintf("%-22s %12.4f %12.4f\n", "bts_like", iid$bts, cov$bts))
cat(sprintf("%-22s %12.2f %12.2f\n", "SSB 1964", iid$ssb[1], cov$ssb[1]))
cat(sprintf("%-22s %12.2f %12.2f\n", "SSB 2024", tail(iid$ssb,1), tail(cov$ssb,1)))
cat(sprintf("\nADMB SAFE SSB: 1964=%.2f  2024=%.2f\n", adm_ssb[1], tail(adm_ssb,1)))
cat(sprintf("SSB mean|%%diff| vs ADMB SAFE:  IID=%.2f%%   MVN=%.2f%%\n",
    100*mean(abs(iid$ssb/adm_ssb - 1)), 100*mean(abs(cov$ssb/adm_ssb - 1))))
cat("==== DONE ====\n")
