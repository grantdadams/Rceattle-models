# Fit the RTMB EBS pollock model locally and extract the full MLE parameter list
# + report, to inject the RTMB optimum into Rceattle for deterministic
# per-component matching. Confirms reproduction of base.rds. Scratch.
#
# NOTE: Rpm.R has leftover developer diagnostic expressions (`NLL - pm$NLL`,
# `age_like/pm$age_like`) comparing to the ADMB rep object `pm`. Their results
# are discarded, but under AD taping `advector(NULL)` throws when `pm$NLL` /
# `pm$age_like` are absent (the local m23 pm.rep has no NLL section). We inject
# dummy non-NULL values so taping succeeds; this does not affect the objective.
RTMB_DIR <- "/Users/grantadams/Documents/GitHub/Rceattle ecosystem/Rceattle-models/EBS pollock/RTMB"
Sys.setenv(RTMB_EBSWP_ROOT = RTMB_DIR)
Sys.setenv(POLLOCK_ROOT   = file.path(RTMB_DIR, ".pollock_root"))
setwd(RTMB_DIR)
options(warn = 1)

# Build parms / data / map_obj / pm exactly as config.R, but stop before MakeADFun
cfg <- readLines(file.path("R", "config.R"))
stop_at <- grep("MakeADFun", cfg)[1]
eval(parse(text = paste(cfg[seq_len(stop_at - 1)], collapse = "\n")), envir = globalenv())

# Guard the discarded diagnostic expressions in rpm (see header note)
if (is.null(pm$NLL))      pm$NLL      <- rep(1, 20)
if (is.null(pm$age_like)) pm$age_like <- rep(1, 3)

obj <- RTMB::MakeADFun(rpm, parms, map = map_obj, silent = TRUE)
cat("\n#### initial objective:", obj$fn(), "  (npar =", length(obj$par), ")\n")

# nlminb under-converges on 1350 params; polish with Newton steps to reach the
# base.rds optimum (TMBhelper::fit_tmb = nlminb + Newton iterations).
fit <- TMBhelper::fit_tmb(obj, newtonsteps = 3, getsd = FALSE, quiet = TRUE,
                          control = list(eval.max = 1e4, iter.max = 1e4))
cat("#### final objective:", fit$objective, "\n")
cat("#### max |grad|:", max(abs(obj$gr(obj$env$last.par.best[!names(obj$env$last.par.best) %in% names(obj$env$random)]))), "\n")

# Full named MLE parameter list (incl. mapped/fixed) + report at the MLE
parms_mle <- obj$env$parList(obj$env$last.par.best)
data$return_nll_only <- 0
rep_mle <- rpm(parms_mle)
data$return_nll_only <- 1

EBS_DIR <- "/Users/grantadams/Documents/GitHub/Rceattle ecosystem/Rceattle-models/EBS pollock"
base <- readRDS(file.path(EBS_DIR, "RTMB", "base.rds"))$report   # canonical copy
cat("\n#### REPRODUCTION CHECK vs base.rds ####\n")
cat(sprintf("tot_like:  fit=%.4f  rep=%.4f  base=%.4f\n",
            fit$objective, rep_mle$tot_like, base$tot_like))
relssb <- as.numeric(rep_mle$SSB) / as.numeric(base$SSB) - 1
cat(sprintf("SSB vs base: mean|%%diff|=%.6f%%  max=%.6f%%\n",
            100 * mean(abs(relssb)), 100 * max(abs(relssb))))
cat("SSB[1:5] rep :", paste(round(as.numeric(rep_mle$SSB)[1:5], 2), collapse = ", "), "\n")
cat("SSB[1:5] base:", paste(round(as.numeric(base$SSB)[1:5], 2), collapse = ", "), "\n")
for (nm in c("cat_like","bts_like","ats_like","ats_age1_like","cpue_like","avo_like"))
  cat(sprintf("  %-14s rep=%.4f  base=%.4f\n", nm, rep_mle[[nm]], base[[nm]]))

saveRDS(list(parms = parms_mle, report = rep_mle, par_mle = fit$par,
             map = map_obj, fit = fit),
        file.path(EBS_DIR, "rtmb_mle.rds"))
cat("\n#### saved ../rtmb_mle.rds (parms + report + map) ####\n")
cat("==== DONE ====\n")
