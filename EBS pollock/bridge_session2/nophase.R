suppressMessages({library(dplyr)})
pkgload::load_all("/Users/grantadams/Documents/GitHub/Rceattle ecosystem/Rceattle", quiet=TRUE)
setwd("/Users/grantadams/Documents/GitHub/Rceattle ecosystem/Rceattle-models/EBS pollock")
SP <- "."  # was session scratchpad; now repo-local
src <- readLines(file.path(SP,"match2.R"))
eval(parse(text=paste(src[grep("^n_selages_fsh",src):grep("^est\\$index_cov",src)], collapse="\n")))
yrs <- est$styr:est$endyr

cat("=== phase=FALSE, estimateMode=1 (hindcast only), REAL objective ===\n")
t0 <- proc.time()[3]
f <- try(suppressWarnings(Rceattle::fit_mod(data_list=est, inits=NULL, file=NULL, estimateMode=1,
      random_rec=FALSE, msmMode=0, verbose=1, phase=FALSE, initMode=2,
      M1Fun=build_M1(updateM1=TRUE, M1_model=0),
      fit_control=fit_control(bias_adjust_proc=0, bias_adjust_obs=0))))
el <- proc.time()[3]-t0
if(inherits(f,"try-error")){ cat("ERROR:", attr(f,"condition")$message,"\n"); quit(status=1) }
cat(sprintf("\nWALL TIME    = %.1f s   (ADMB = 25 s)\n", el))
cat(sprintf("objective    = %.4f   (ADMB tot_like = 740.5251)\n", f$opt$objective))
g <- try(max(abs(f$obj$gr(f$opt$par))), silent=TRUE)
cat(sprintf("max|grad|    = %s\n", ifelse(inherits(g,"try-error"),"NA",format(g,digits=4))))
p <- f$opt$par
v <- f$obj$fn(p); cat(sprintf("fn(opt)      = %.4f  <- sanity: finite & nonzero\n", v))
t0<-proc.time()[3]; for(i in 1:20) f$obj$fn(p+rnorm(length(p),0,1e-6)); tf<-(proc.time()[3]-t0)/20
t0<-proc.time()[3]; for(i in 1:20) f$obj$gr(p+rnorm(length(p),0,1e-6)); tg<-(proc.time()[3]-t0)/20
cat(sprintf("fn = %.2f ms | grad = %.2f ms  (npar=%d)\n", tf*1000, tg*1000, length(p)))
saveRDS(f, file.path(SP,"nophase_fit.rds"))
q <- f$quantities
cat(sprintf("\nSSB1964=%.1f (ADMB 1988.4) | R1964=%.1f (ADMB 7020.2)\n", q$ssb[1,1], q$R[1,1]))
n <- length(yrs)
cat(sprintf("SSB2024=%.1f (ADMB 3411.5) | R2024=%.1f (ADMB 18325.2)\n", q$ssb[1,n], q$R[1,n]))
