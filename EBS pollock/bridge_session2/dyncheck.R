# DYNAMICS CHECK: fix every Rceattle parameter at the aligned-ADMB MLE, let
# Rceattle COMPUTE numbers-at-age (estDynamics = 0), and compare N to ADMB's N.
# Selectivity is bypassed empirically (refreshed from m23_rceattle/pm.rep) so
# that ONLY the population dynamics are under test.
suppressMessages({library(dplyr); library(readxl)})
pkgload::load_all("/Users/grantadams/Documents/GitHub/Rceattle ecosystem/Rceattle", quiet = TRUE)
setwd("/Users/grantadams/Documents/GitHub/Rceattle ecosystem/Rceattle-models/EBS pollock")
AD <- "ADMB/m23_rceattle"

pl <- readLines(file.path(AD, "pm.par"))
get_par <- function(nm) {
  i <- which(pl == paste0("# ", nm, ":"))[1]; v <- c(); j <- i + 1
  while (j <= length(pl) && !grepl("^#", pl[j])) {
    v <- c(v, as.numeric(strsplit(trimws(pl[j]), "[[:space:]]+")[[1]])); j <- j + 1 }
  v
}
rl <- readLines(file.path(AD, "pm.rep"))
blk <- function(nm, n = 61) { i <- which(rl == nm)[1]
  t(sapply(1:n, function(k) as.numeric(strsplit(trimws(rl[i+k]), "[[:space:]]+")[[1]]))) }

mydata <- Rceattle::read_data(file = "Data/2024_EBS_pollock.xlsx")
nages <- mydata$nages; yrs <- mydata$styr:mydata$endyr; nyr <- length(yrs)
keep <- c("Species_name","Species","Sex","Year", paste0("Age",1:nages))
mydata$NByageFixed <- mydata$NByageFixed[, intersect(keep, colnames(mydata$NByageFixed))]
mydata$spawn_month <- 3

d <- mydata
d$estDynamics <- 0                      # <-- COMPUTE N (this is the point)
d$fleet_control$Selectivity <- 0        # empirical selectivity for every fleet
fcn <- d$fleet_control$Fleet_name
d$fleet_control$Fleet_type[5:6] <- 2
d$age_error[1:nages, 3:(nages+2)] <- diag(nages)

# -- refresh emp_sel from the ALIGNED run (the xlsx ships old-m23 selectivity).
# emp_sel uses Comp_1..Comp_n columns. BTS_1/ATS_1 are AGE-1 abundance indices
# with their own (1,0,0,..) selectivity and Year = 0 -> leave those rows alone.
sel_fsh <- blk("sel_fsh"); sel_bts <- blk("sel_bts"); sel_ats <- blk("sel_ats")
admb_sel <- list(Fishery = sel_fsh, BTS = sel_bts, ATS = sel_ats, AVO = sel_ats)
cols <- colnames(d$emp_sel); ccol <- paste0("Comp_", 1:nages)
es <- d$emp_sel[!(d$emp_sel$Fleet_name %in% names(admb_sel)), ]   # keep BTS_1/ATS_1
for (fl in names(admb_sel)) {
  code <- d$fleet_control$Fleet_code[fcn == fl]
  m <- admb_sel[[fl]]
  add <- d$emp_sel[0, ]
  add[1:nyr, ] <- NA
  add$Fleet_name <- fl; add$Fleet_code <- code; add$Species <- 1
  add$Sex <- 0; add$Year <- yrs
  for (a in 1:nages) add[[ccol[a]]] <- m[, a]
  es <- rbind(es, add[, cols])
}
d$emp_sel <- es
cat("emp_sel rows:", nrow(es), " fleets:", paste(unique(es$Fleet_name), collapse=","), "\n")
d$fleet_control$Catchability <- as.character(d$fleet_control$Catchability)
d$fleet_control$Catchability[fcn %in% c("BTS","ATS","AVO","BTS_1","ATS_1")] <- "3"

inits <- build_params(d)
inits$rec_pars[1, 1]    <- get_par("log_avgrec")
inits$rec_dev[1, 1:nyr] <- get_par("log_rec_devs")
inits$log_F[1, 1:nyr]   <- get_par("log_avg_F") + get_par("log_F_devs")
idv <- get_par("log_initdevs")
cat("log_avgrec =", get_par("log_avgrec"), " n(log_initdevs) =", length(idv),
    " n(init_dev slots) =", ncol(inits$init_dev), "\n")
inits$init_dev[1, 1:length(idv)] <- idv

fit <- Rceattle::fit_mod(data_list = d, inits = inits, file = NULL,
  estimateMode = 4, random_rec = FALSE, msmMode = 0, verbose = 0, phase = FALSE,
  initMode = 2, M1Fun = build_M1(updateM1 = TRUE, M1_model = 0),
  fit_control = fit_control(bias_adjust_proc = 0, bias_adjust_obs = 0))

N_r <- fit$quantities$N_at_age[1, 1, 1:nages, 1:nyr]   # [age, yr]
N_a <- t(blk("N"))                                      # [age, yr]
cat("\n=== Rceattle N vs ADMB N (params fixed at ADMB MLE) ===\n")
for (y in c(1964, 1970, 1978, 1990, 2024)) {
  yi <- which(yrs == y)
  cat(sprintf("%d age1: R=%10.1f A=%10.1f (%+.2f%%) | age5: R=%9.1f A=%9.1f (%+.2f%%)\n",
    y, N_r[1,yi], N_a[1,yi], 100*(N_r[1,yi]/N_a[1,yi]-1),
       N_r[5,yi], N_a[5,yi], 100*(N_r[5,yi]/N_a[5,yi]-1)))
}
rat <- N_r / N_a
cat("\nN ratio  mean:", round(mean(rat),5), " range:", round(range(rat),5), "\n")
ssb_a <- t(sapply(1:nyr, function(k) as.numeric(strsplit(trimws(rl[which(rl=="SSB")[1]+k]),"[[:space:]]+")[[1]])))
ssb_r <- as.numeric(fit$quantities$ssb[1, 1:nyr])
cat("\n=== SSB ===\n")
for (y in c(1964, 1978, 2024)) { yi <- which(yrs == y)
  cat(sprintf("%d: Rceattle=%9.2f  ADMB=%9.2f  (%+.2f%%)\n", y, ssb_r[yi], ssb_a[yi,2], 100*(ssb_r[yi]/ssb_a[yi,2]-1))) }
cat("\nSSB ratio mean:", round(mean(ssb_r/ssb_a[,2]),5), " range:", round(range(ssb_r/ssb_a[,2]),5), "\n")
