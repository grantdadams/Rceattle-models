suppressMessages({library(dplyr)})
pkgload::load_all("/Users/grantadams/Documents/GitHub/Rceattle ecosystem/Rceattle", quiet=TRUE)
setwd("/Users/grantadams/Documents/GitHub/Rceattle ecosystem/Rceattle-models/EBS pollock")
src <- readLines("match2.R")
eval(parse(text=paste(src[grep("^n_selages_fsh",src):grep("^est\\$index_cov",src)], collapse="\n")))
f <- suppressWarnings(Rceattle::fit_mod(data_list=est, inits=NULL, file=NULL, estimateMode=3,
      random_rec=FALSE, msmMode=0, verbose=0, phase=FALSE, initMode=2,
      M1Fun=build_M1(updateM1=TRUE, M1_model=0),
      fit_control=fit_control(bias_adjust_proc=0, bias_adjust_obs=0)))
tb <- sort(table(names(f$obj$par)), decreasing=TRUE)
cat("=== Rceattle ESTIMATED parameters (obj$par) ===\n")
for(n in names(tb)) cat(sprintf("  %-28s %5d\n", n, tb[[n]]))
cat(sprintf("  %-28s %5d   (ADMB active = 1225)\n", "TOTAL", length(f$obj$par)))
