# Debug the RTMB MakeADFun NULL error: build parms/data like config.R but stop
# before MakeADFun, then call rpm() in plain R to pinpoint the NULL. Scratch.
RTMB_DIR <- "/Users/grantadams/Documents/GitHub/Rceattle ecosystem/Rceattle-models/EBS pollock/RTMB"
Sys.setenv(RTMB_EBSWP_ROOT = RTMB_DIR)
Sys.setenv(POLLOCK_ROOT   = file.path(RTMB_DIR, ".pollock_root"))
setwd(RTMB_DIR)
options(warn = 1)

cfg <- readLines(file.path("R", "config.R"))
stop_at <- grep("MakeADFun", cfg)[1]
eval(parse(text = paste(cfg[seq_len(stop_at - 1)], collapse = "\n")), envir = globalenv())

cat("\n#### parms NULL elements:",
    paste(names(parms)[vapply(parms, is.null, logical(1))], collapse = ", "), "\n")
cat("#### parms length:", length(parms), "\n")
cat("#### data has fishery_sel_form:", !is.null(data$fishery_sel_form),
    " value:", if (is.null(data$fishery_sel_form)) "NULL" else data$fishery_sel_form, "\n")
cat("#### data NULL elements:",
    paste(names(data)[vapply(data, is.null, logical(1))], collapse = ", "), "\n")

# Try rpm() in plain R to surface the offending NULL with a clear traceback
data$return_nll_only <- 1
res <- tryCatch(rpm(parms),
                error = function(e) { cat("\n#### rpm() ERROR:", conditionMessage(e), "\n"); NULL })
if (!is.null(res)) cat("\n#### rpm() plain-R nll =", res, "  (no NULL error)\n")
cat("\n==== DEBUG DONE ====\n")
