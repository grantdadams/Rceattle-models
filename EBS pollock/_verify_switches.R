# Verify new switches: (1) default (index_catch_bias=1) reproduces pre-change jnll
# exactly (no regression); (2) bias-off + AnalyticalArith q change catch/index.
options(warn = -1)
MODEL_DIR <- "/Users/grantadams/Documents/GitHub/Rceattle ecosystem/Rceattle-models/EBS pollock"
setwd(MODEL_DIR)
source("_rceattle_fwdpass.R")   # recompiles (picks up cpp edits), builds fp_data/inits/m1 (defaults), saves rds

base_new <- m1$quantities$jnll_comp
pre <- readRDS("_rceattle_fwdpass_prechange.rds")$quantities$jnll_comp
cat("\n######## REGRESSION CHECK (default index_catch_bias=1) ########\n")
cat("max abs diff (new default vs pre-change jnll_comp):", format(max(abs(base_new - pre)), scientific=TRUE), "\n")
cat("(expect ~0 / machine epsilon)\n")

cat("\n######## SWITCH EFFECT CHECK ########\n")
# Variant A: bias correction OFF (matches RTMB catch/ATS log-SSQ form)
fpA <- fp_data; fpA$index_catch_bias <- 0
mA <- fit_mod(fpA, inits, file=NULL, estimateMode=4, random_rec=FALSE, msmMode=0,
              verbose=0, phase=FALSE, initMode=2, M1Fun=build_M1(updateM1=TRUE,M1_model=0))
# Variant B: bias OFF + arithmetic-mean analytical q on BTS/ATS
fpB <- fpA
fpB$fleet_control$Catchability[fpB$fleet_control$Fleet_name %in% c("BTS","ATS")] <- "AnalyticalArith"
mB <- fit_mod(fpB, inits, file=NULL, estimateMode=4, random_rec=FALSE, msmMode=0,
              verbose=0, phase=FALSE, initMode=2, M1Fun=build_M1(updateM1=TRUE,M1_model=0))

cn <- colnames(base_new)
ci <- function(nm) which(cn==nm)
cat(sprintf("%-22s %14s %14s %14s\n","", "default(bias1)","biasOFF","biasOFF+arithQ"))
cat(sprintf("%-22s %14.5f %14.5f %14.5f\n","Catch (Fishery)",
    base_new[2,1], mA$quantities$jnll_comp[2,1], mB$quantities$jnll_comp[2,1]))
cat(sprintf("%-22s %14.5f %14.5f %14.5f\n","Index BTS",
    base_new[1,ci("BTS")], mA$quantities$jnll_comp[1,ci("BTS")], mB$quantities$jnll_comp[1,ci("BTS")]))
cat(sprintf("%-22s %14.5f %14.5f %14.5f\n","Index ATS",
    base_new[1,ci("ATS")], mA$quantities$jnll_comp[1,ci("ATS")], mB$quantities$jnll_comp[1,ci("ATS")]))

# Confirm arithmetic q == sum(obs)/sum(pred) for BTS via reported index_q
cat("\nBTS index_q[1]: default(geom)=", round(m1$quantities$index_q[ci("BTS"),1],5),
    " arith=", round(mB$quantities$index_q[ci("BTS"),1],5), "\n")
cat("\n==== VERIFY DONE ====\n")
