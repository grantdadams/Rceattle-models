# Isolate the MakeADFun taping error. Scratch.
RTMB_DIR <- "/Users/grantadams/Documents/GitHub/Rceattle ecosystem/Rceattle-models/EBS pollock/RTMB"
Sys.setenv(RTMB_EBSWP_ROOT = RTMB_DIR)
Sys.setenv(POLLOCK_ROOT   = file.path(RTMB_DIR, ".pollock_root"))
setwd(RTMB_DIR)
options(warn = 1)
cat("R version:", R.version.string, " RTMB:", as.character(packageVersion("RTMB")), "\n")

cfg <- readLines(file.path("R", "config.R"))
stop_at <- grep("MakeADFun", cfg)[1]
eval(parse(text = paste(cfg[seq_len(stop_at - 1)], collapse = "\n")), envir = globalenv())

# Map vs parms diagnostics
mapn <- names(map_obj)
cat("\n#### map names not in parms:", paste(setdiff(mapn, names(parms)), collapse=", "), "\n")
cat("#### map length:", length(map_obj), "\n")
for (nm in mapn) {
  pl <- length(parms[[nm]]); ml <- length(map_obj[[nm]])
  if (pl != ml) cat(sprintf("   LENGTH MISMATCH %-22s parms=%d map=%d\n", nm, pl, ml))
}

# Attempt 1: with the map
cat("\n#### MakeADFun WITH map ...\n")
o1 <- tryCatch(RTMB::MakeADFun(rpm, parms, map = map_obj, silent = TRUE),
               error = function(e) { cat("   ERR:", conditionMessage(e), "\n"); NULL })
if (!is.null(o1)) cat("   OK, fn() =", tryCatch(o1$fn(), error=function(e) paste("fn ERR:",conditionMessage(e))), "\n")

# Attempt 2: no map (all estimated)
cat("\n#### MakeADFun WITHOUT map ...\n")
o2 <- tryCatch(RTMB::MakeADFun(rpm, parms, silent = TRUE),
               error = function(e) { cat("   ERR:", conditionMessage(e), "\n"); NULL })
if (!is.null(o2)) cat("   OK, fn() =", tryCatch(o2$fn(), error=function(e) paste("fn ERR:",conditionMessage(e))), "\n")

cat("\n==== DONE ====\n")
