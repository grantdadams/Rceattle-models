# Export the SS3-bridged GOA Pacific cod model for the GOA circulation study.
#
# The study fits a DSEM on recruitment for four stocks. Its cod model used to be
# read from `Data/2024_GOA_pcod.xlsx`; this replaces that with the bridged model,
# whose forward pass reproduces SS3 on all 24 G1 gates and whose optimum matches
# SS3 at 330 free parameters.
#
#   Rscript export_for_circulation_study.R
#
# Writes `Bridging/_goa_cod_for_dsem.rds`, a list the study loads:
#   data_list   the cod data_list (1977-2024), env_data carrying SS3's own
#               LLSrv catchability covariate
#   inits       SS3's MLE as Rceattle parameters, for a warm start
#   map         SS3's pinning
#   growthFun, M1Fun, selFun, qFun, recFun, initMode
#               the process specs that make this SS3's model rather than a
#               default one. Pass all of them or the fit is a different model.
#
# RCE_INIT_LINK=true carries SS3's initial recruitment level as an `R_init`
# linkage. It had to be false while fit_mod() refused a recruitment linkage
# alongside a DSEM; that refusal was removed in 5.57.0.9001.
#
# Use true. The two parameterisations agree at SS3's MLE -- the forward pass is
# 1912.2067 either way, the init_dev values differing by one constant 1.388 that
# the extra linkage coefficient absorbs -- but they do NOT agree at an optimum,
# because init_dev is penalised where the linkage is not. Refitting the GOA cod
# circulation-study IID DSEM both ways: folding the level into init_dev costs
# 9.39 nats and pushes R0 to 5.71e+05 against SS3's 3.35e+05, moving SSB by up
# to 69%. The linkage lands at R0 3.15e+05, next to SS3.

.cfg <- c(RCE_SS3_DIR       = "Data/goa_pcod_caal_bins_fixed_age0",
          RCE_SS3_PHASE_DIR = "Data/goa_pcod_caal_bins_fixed_age0_est",
          RCE_SEL_PARITY    = "true",
          RCE_FIX_LINKAGE   = "false",
          RCE_SEL_DEV_ALL   = "true",
          RCE_SEL_REPLACE   = "true",
          RCE_INIT_PRIOR    = "false",
          RCE_INIT_LINK     = "true")
for (.k in names(.cfg)) do.call(Sys.setenv, as.list(.cfg[.k]))

source("Bridging/ss3_to_ceattle_forward_pass.R")

stopifnot(sum(mod0$data_list$linkage_table$process == "recruitment") == 1L)

out <- list(
  data_list = cod,
  inits     = inits,
  map       = ss3_map,
  growthFun = growthFun_spec,
  M1Fun     = M1_block,
  selFun    = selFun_spec,
  qFun      = qFun_spec,
  recFun    = recFun_spec,
  initMode  = INIT_MODE,
  built     = Sys.Date(),
  ss3_dir   = SS3_DIR)
saveRDS(out, "Bridging/_goa_cod_for_dsem.rds")

cat(sprintf("\nwrote Bridging/_goa_cod_for_dsem.rds\n  %d-%d, %d fleets, env_data: %s\n",
            cod$styr, cod$endyr, nrow(cod$fleet_control),
            paste(setdiff(names(cod$env_data), "Year"), collapse = ", ")))
cat(sprintf("  forward pass %.4f, 1 recruitment (R_init) linkage row\n", fp$quantities$jnll))
