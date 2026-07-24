# =============================================================================
# run_em_patch.R -- dispatch line to add to ASSAMC's R/run_em.R
#
# This file is NOT sourced by the demo. It documents the single edit required
# in the Age_Structured_Stock_Assessment_Model_Comparison (ASSAMC) package so
# that "RCEATTLE" can be requested as an estimation model via run_em(). It is
# kept separate so nothing in the collaborators' repo is modified from here.
#
# In ASSAMC's R/run_em.R, alongside the other EM dispatch lines
# (run_wham, run_fims, ...), add:
#
#   if ("RCEATTLE" %in% em_names) {
#     run_rceattle(
#       maindir     = maindir,
#       om_sim_num  = om_sim_num,
#       casedir     = casedir,
#       em_bias_cor = em_bias_cor
#     )
#   }
#
# and place run_rceattle.R (+ om_to_rceattle.R) in the package R/ folder. The
# translator helpers (om_to_rceattle, cv_2_sd, seed_rceattle_inits, ...) are
# exported to the parallel workers via the foreach `.export` list inside
# run_rceattle(), matching how run_fims() exports its helpers.
#
# Also add Rceattle to DESCRIPTION Suggests/Imports (as WHAM/FIMS are handled).
# =============================================================================
