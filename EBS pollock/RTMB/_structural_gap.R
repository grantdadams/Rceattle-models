# PURE STRUCTURAL GAP: apply the RTMB likelihood formulas (model_funs.R) to the
# ADMB rep's OWN predicted state (pm$...), and compare to the ADMB-reported
# component values (pm$cat_like, pm$age_like, ...). Same state, two formula sets
# -> any difference is formula/structure, not fitting. Scratch.
RTMB_DIR <- "/Users/grantadams/Documents/GitHub/Rceattle ecosystem/Rceattle-models/EBS pollock/RTMB"
Sys.setenv(RTMB_EBSWP_ROOT = RTMB_DIR)
Sys.setenv(POLLOCK_ROOT   = file.path(RTMB_DIR, ".pollock_root"))
setwd(RTMB_DIR); options(warn = -1)
cfg <- readLines(file.path("R", "config.R")); stop_at <- grep("MakeADFun", cfg)[1]
eval(parse(text = paste(cfg[seq_len(stop_at - 1)], collapse = "\n")), envir = globalenv())

row <- function(name, rtmb, admb) cat(sprintf("  %-16s RTMB-formula=%-12s ADMB-reported=%-12s  |diff|=%g\n",
      name, signif(rtmb,7), signif(admb,7), abs(rtmb-admb)))

cat("\n############ PURE STRUCTURAL GAP (RTMB formula on ADMB state vs ADMB report) ############\n")

## --- catch: catBio * sum((log(obs+1e-4)-log(pred+1e-4))^2), catBio=200 ---
catBio <- 200
rc <- catBio * catch_like(pm$obs_catch, pm$pred_catch)
row("catch", rc, pm$cat_like)

## --- age comps (multinomial, MN_const=0.001) ---
oac <- list(pm$pobs_fsh[, 2:16], pm$pobs_bts[, 2:16], pm$pobs_ats[, 2:16])
eac <- list(pm$phat_fsh,         pm$phat_bts,         pm$phat_ats)
sam <- list(data$sam_fsh, data$sam_bts, data$sam_ats)
al <- multinomial_likelihood_age(oac, eac, sam, MN_const = 0.001,
                                 mina_ats = data$mina_ats, nages = data$nages)
# subtract the obs-only offset (ADMB reports the offset-removed value)
off <- mapply(function(o, s, lo, hi) -sum(s * rowSums(o[, lo:hi, drop=FALSE] *
              log(o[, lo:hi, drop=FALSE] + 0.001))),
              oac, sam, c(1,1,data$mina_ats), c(15,15,data$nages))
for (i in 1:3) row(paste0("age_like[", c("fsh","bts","ats")[i], "]"),
                   as.numeric(al)[i] - off[i], pm$age_like[i])

## --- CPUE / AVO: natural-scale normal sum((o-p)^2/(2 var)) ---
row("cpue", CPUE_likelihood(pm$obs_cpue, pm$pred_cpue, data$obs_cpue_var), pm$cpue_like)
row("avo",  AVO_likelihood(pm$obs_avo,  pm$pred_avo,  data$obs_avo_var),  pm$avo_like)

## --- BTS: covariance natural-scale 0.5 r' Sigma^-1 r ---
rb <- BTS_likelihood(pm$ob_bts, pm$ot_bts, pm$eb_bts, pm$et_bts,
                     data$inv_bts_cov, var_ob_bts = data$ob_bts_std^2,
                     DoCovBTS = 1, do_bts_bio = TRUE)
row("bts (cov)", rb, pm$bts_like)

## --- ATS: lognormal sum((log(ob+.01)-log(eb+.01))^2/(2 lvarb)) ---
ra <- ATS_likelihood(pm$ob_ats, pm$ot_ats, pm$eb_ats, pm$et_ats,
                     data$lvar_ats, data$lvarb_ats, do_ats_bio = TRUE)
row("ats", ra, pm$ats_like)

cat("\n(Match => the ADMB and RTMB likelihoods are the SAME FORMULA; any gap is structural.)\n")
cat("==== DONE ====\n")
