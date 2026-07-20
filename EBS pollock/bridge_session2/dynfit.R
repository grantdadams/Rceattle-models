# ISOLATION TEST: estimate ONLY the population-dynamics parameters (rec_pars,
# rec_dev, log_F, init_dev, q). Selectivity is bypassed empirically at ADMB's
# realized values (emp_sel refreshed from m23_rceattle/pm.rep), so the 949
# selectivity deviations are NOT in the parameter vector.
# Index likelihood is the ADMB-faithful one from match2.R (MVN BTS + AnalyticalArith).
# Inits are build_params() DEFAULTS -- this asks whether the optimizer can FIND
# ADMB's solution, not merely sit on it.
suppressMessages({library(dplyr); library(readxl)})
pkgload::load_all("/Users/grantadams/Documents/GitHub/Rceattle ecosystem/Rceattle", quiet = TRUE)
setwd("/Users/grantadams/Documents/GitHub/Rceattle ecosystem/Rceattle-models/EBS pollock")
AD <- "ADMB/m23_rceattle"
rl <- readLines(file.path(AD, "pm.rep"))
blk <- function(nm, n = 61) { i <- which(rl == nm)[1]
  t(sapply(1:n, function(k) as.numeric(strsplit(trimws(rl[i+k]), "[[:space:]]+")[[1]]))) }

mydata <- Rceattle::read_data(file = "Data/2024_EBS_pollock.xlsx")
nages <- mydata$nages; yrs <- mydata$styr:mydata$endyr; nyr <- length(yrs)
keep <- c("Species_name","Species","Sex","Year", paste0("Age",1:nages))
mydata$NByageFixed <- mydata$NByageFixed[, intersect(keep, colnames(mydata$NByageFixed))]
mydata$spawn_month <- 3

d <- mydata
d$estDynamics <- 0
d$fleet_control$Selectivity <- 0          # empirical selectivity => NO sel parameters
fcn <- d$fleet_control$Fleet_name
d$fleet_control$Fleet_type[5:6] <- 2
d$age_error[1:nages, 3:(nages+2)] <- diag(nages)
# FIXED: xlsx Log_sd is ALREADY a CV/log-sd (0.05-0.56; ADMB sdnr~1). The old
# "SD -> CV" division by Observation produced CVs of ~1e-5 -> index jnll 743,587.
d$catch_data$Log_sd <- 0.05
d$sigma_rec_prior <- 0.707

sel_fsh <- blk("sel_fsh"); sel_bts <- blk("sel_bts"); sel_ats <- blk("sel_ats")
admb_sel <- list(Fishery = sel_fsh, BTS = sel_bts, ATS = sel_ats, AVO = sel_ats)
cols <- colnames(d$emp_sel); ccol <- paste0("Comp_", 1:nages)
es <- d$emp_sel[!(d$emp_sel$Fleet_name %in% names(admb_sel)), ]
for (fl in names(admb_sel)) {
  add <- d$emp_sel[0, ]; add[1:nyr, ] <- NA
  add$Fleet_name <- fl; add$Fleet_code <- d$fleet_control$Fleet_code[fcn == fl]
  add$Species <- 1; add$Sex <- 0; add$Year <- yrs
  for (a in 1:nages) add[[ccol[a]]] <- admb_sel[[fl]][, a]
  es <- rbind(es, add[, cols])
}
d$emp_sel <- es
d$index_data <- d$index_data %>% mutate(Month = case_when(Fleet_name %in% c("BTS","BTS_1","ATS","ATS_1") ~ 6, TRUE ~ 0))
d$comp_data  <- d$comp_data  %>% mutate(Month = case_when(Fleet_name=="BTS" ~ 6, Fleet_name=="ATS" ~ 6, TRUE ~ Month))
d$fleet_control$Catchability <- as.character(d$fleet_control$Catchability)
d$fleet_control$Catchability[fcn=="ATS"] <- "1"
d$fleet_control$Catchability[fcn %in% c("BTS_1","ATS_1")] <- "3"
d$fleet_control$Catchability[fcn=="AVO"] <- "1"
d$index_data$Log_sd[d$index_data$Fleet_name %in% c("BTS_1","ATS_1")] <- 1
d$fleet_control$Index_loglike[fcn=="BTS"] <- "MVN"
d$fleet_control$Catchability[fcn=="BTS"]  <- "AnalyticalArith"
d$index_cov <- list(BTS = as.matrix(read.table("ADMB/data/cov_2024.dat")))

t0 <- Sys.time()
fit <- try(Rceattle::fit_mod(data_list = d, inits = NULL, file = NULL,
  estimateMode = 1, random_rec = FALSE, msmMode = 0, verbose = 1, phase = FALSE,
  initMode = 2, M1Fun = build_M1(updateM1 = TRUE, M1_model = 0),
  fit_control = fit_control(bias_adjust_proc = 0, bias_adjust_obs = 0)))
el <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
cat(sprintf("\n=== ELAPSED: %.1f s  (full fit with sel devs = 4593 s; ADMB = 25 s) ===\n", el))
if (inherits(fit, "try-error")) { cat("FIT ERROR:", attr(fit,"condition")$message, "\n"); quit(status=1) }

cat("n estimated params:", length(fit$obj$par), "\n")
print(sort(table(names(fit$obj$par)), decreasing = TRUE))
g <- try(max(abs(fit$obj$gr(fit$opt$par))), silent = TRUE)
cat(sprintf("\nobjective = %s | max|grad| = %s\n",
  format(fit$opt$objective, digits=10),
  ifelse(inherits(g,"try-error"), paste("ERROR:", attr(g,"condition")$message), format(g, digits=4))))

ssb_a <- t(sapply(1:nyr, function(k) as.numeric(strsplit(trimws(rl[which(rl=="SSB")[1]+k]),"[[:space:]]+")[[1]])))
ssb_r <- as.numeric(fit$quantities$ssb[1, 1:nyr]); R_r <- as.numeric(fit$quantities$R[1, 1:nyr])
R_a <- blk("N")[,1]
cat("\n=== SSB: Rceattle (dynamics-only fit) vs ADMB ===\n")
for (y in c(1964,1978,1990,2008,2024)) { yi <- which(yrs==y)
  cat(sprintf("  %d: R=%9.1f  A=%9.1f (%+7.2f%%) | R(rec)=%9.1f A=%9.1f (%+7.2f%%)\n",
    y, ssb_r[yi], ssb_a[yi,2], 100*(ssb_r[yi]/ssb_a[yi,2]-1), R_r[yi], R_a[yi], 100*(R_r[yi]/R_a[yi]-1))) }
cat(sprintf("\nSSB cor = %.4f | mean|pct| = %.2f%% | min SSB = %.2f\n",
  cor(ssb_r, ssb_a[,2]), mean(abs(100*(ssb_r/ssb_a[,2]-1))), min(ssb_r)))
lf <- fit$estimated_params$log_F[1, 1:nyr]
cat(sprintf("log_F range = [%.3f, %.3f]  (bound = 10; pinned-at-bound years: %d)\n",
  min(lf), max(lf), sum(lf > 9.99)))
saveRDS(fit, "bridge_session2/dynfit_fix.rds")
