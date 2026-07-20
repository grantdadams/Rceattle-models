# Get the exact rpm line of the MakeADFun taping error + parm element types.
RTMB_DIR <- "/Users/grantadams/Documents/GitHub/Rceattle ecosystem/Rceattle-models/EBS pollock/RTMB"
Sys.setenv(RTMB_EBSWP_ROOT = RTMB_DIR)
Sys.setenv(POLLOCK_ROOT   = file.path(RTMB_DIR, ".pollock_root"))
setwd(RTMB_DIR)
options(warn = 1)
cfg <- readLines(file.path("R", "config.R"))
stop_at <- grep("MakeADFun", cfg)[1]
eval(parse(text = paste(cfg[seq_len(stop_at - 1)], collapse = "\n")), envir = globalenv())

cat("\n#### parms element class / length / has-dim ####\n")
for (nm in names(parms)) {
  v <- parms[[nm]]
  cat(sprintf("  %-26s class=%-9s len=%-5d dim=%s\n",
      nm, paste(class(v), collapse=","), length(v),
      if (is.null(dim(v))) "-" else paste(dim(v), collapse="x")))
}

cat("\n#### MakeADFun with full traceback ####\n")
options(error = function() { sink(stderr()); cat("---- TRACEBACK ----\n"); traceback(1, max.lines = 40); sink() })
RTMB::MakeADFun(rpm, parms, silent = TRUE)
