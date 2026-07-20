suppressMessages({library(dplyr)})
pkgload::load_all("/Users/grantadams/Documents/GitHub/Rceattle ecosystem/Rceattle", quiet=TRUE)
setwd("/Users/grantadams/Documents/GitHub/Rceattle ecosystem/Rceattle-models/EBS pollock")
src <- readLines("match2.R")
eval(parse(text=paste(src[grep("^n_selages_fsh",src):grep("^est\\$index_cov",src)], collapse="\n")))

for (drop in c(FALSE, TRUE)) {
  e <- est
  if (drop) e$fleet_control$Sel_start_year <- NA   # unset -> exercise the derived default
  d <- Rceattle::switch_check(Rceattle::clean_data(e))
  ssy <- d$fleet_control$Sel_start_year; names(ssy) <- d$fleet_control$Fleet_name
  cat(sprintf("\n=== Sel_start_year %s ===\n", ifelse(drop,"UNSET -> DERIVED from data","explicit in config")))
  print(ssy[c("Fishery","BTS","ATS","AVO")])
  f <- suppressWarnings(Rceattle::fit_mod(data_list=e, inits=NULL, file=NULL, estimateMode=3,
        random_rec=FALSE, msmMode=0, verbose=0, phase=FALSE, initMode=2,
        M1Fun=build_M1(updateM1=TRUE, M1_model=0),
        fit_control=fit_control(bias_adjust_proc=0, bias_adjust_obs=0)))
  m <- f$map
  cnt <- function(x) if(is.null(x)) NA else length(unique(na.omit(as.vector(as.integer(x)))))
  cat(sprintf("  log_sel_slp_dev=%s (ADMB 42) | sel_inf_dev=%s (ADMB 84) | sel_coff_dev=%s\n",
      cnt(m$log_sel_slp_dev), cnt(m$sel_inf_dev), cnt(m$sel_coff_dev)))
  cat(sprintf("  TOTAL estimated (obj$par) = %d   (ADMB 1225)\n", length(f$obj$par)))
}
