# Validate the base+offset reconstruction, then ask whether either fitted basin
# leaves the box SS3 declares.
#
# Rceattle carries a block as an unbounded OFFSET on the identity link
# (Blk_Fxn = 2 REPLACES, so offset = block - base); SS3 carries it as a bounded
# VALUE. The validation: at the INJECTED SS3 MLE, base + offset must equal SS3's
# own fitted value for every block. Only then do its LO/HI mean anything here.
#
# Labels: the ctl writes _SizeSel_P_<k>_<Fleet>(<n>)_BLK..., the report writes
# Size_DblN_<pname>_<Fleet>(<n>)_BLK..., with P1..P6 = peak, top_logit, ascend_se,
# descend_se, start_logit, end_logit. Join on (pname, fleet, year).
Sys.setenv(RCE_SEL_PARITY = "true", RCE_INIT_LINK = "true")
source("Bridging/ss3_to_ceattle_forward_pass.R")
PNAME <- c("peak", "top_logit", "ascend_se", "descend_se", "start_logit",
           "end_logit")

tbl <- as.data.frame(unclass(mod0$data_list$linkage_table), stringsAsFactors = FALSE)
kid <- which(as.character(tbl$process) == "sel" &
             as.character(tbl$link) == "identity")
dc  <- as.character(tbl$design_col[kid])
# fleets sharing a Selectivity_index share ONE coefficient and appear once per
# fleet in the table, so count distinct design columns, not rows.
keep <- kid[!duplicated(dc)]
dcu  <- dc[!duplicated(dc)]
m  <- regmatches(dcu, regexec("^s([0-9]+)p([0-9]+)_blk([0-9]{4})$", dcu))
stopifnot(all(vapply(m, length, 1L) == 4L))
fl <- as.integer(vapply(m, `[`, "", 2))
pp <- as.integer(vapply(m, `[`, "", 3))
yr <- as.integer(vapply(m, `[`, "", 4))
cat(sprintf("identity sel rows %d, distinct coefficients %d\n",
            length(kid), length(keep)))

# SS3 side: LO/HI from the ctl, fitted value from the report, joined on the triple
ctl <- readLines("Data/goa_pcod_caal_lambda_on/Model19_1e.ctl")
cl <- grep("_SizeSel_P_[0-9]+_.*_BLK[0-9]+[a-z]*_[0-9]{4}", ctl)
bnd <- do.call(rbind, lapply(cl, function(i) {
  f <- strsplit(trimws(sub("#.*$", "", ctl[i])), "[ \t]+")[[1]]
  lab <- trimws(sub("^.*#\\s*", "", ctl[i]))
  g <- regmatches(lab, regexec(
    "_SizeSel_P_([0-9]+)_([A-Za-z_0-9]+)\\([0-9]+\\)_BLK[0-9]+[a-z]*_([0-9]{4})",
    lab))[[1]]
  if (length(g) != 4) return(NULL)
  data.frame(pname = PNAME[as.integer(g[2])], fleet = g[3],
             year = as.integer(g[4]), lo = as.numeric(f[1]),
             hi = as.numeric(f[2]), stringsAsFactors = FALSE)
}))
P <- ss3_rep$parameters
rp <- regmatches(P$Label, regexec(
  "^Size_DblN_([a-z_]+)_([A-Za-z_0-9]+)\\([0-9]+\\)_BLK[0-9]+[a-z]*_([0-9]{4})$",
  P$Label))
hit <- vapply(rp, length, 1L) == 4L
mle <- data.frame(pname = vapply(rp[hit], `[`, "", 2),
                  fleet = vapply(rp[hit], `[`, "", 3),
                  year  = as.integer(vapply(rp[hit], `[`, "", 4)),
                  mle   = P$Value[hit], stringsAsFactors = FALSE)
ss3 <- merge(bnd, mle, by = c("pname", "fleet", "year"))
cat(sprintf("ctl bound rows %d, report MLE rows %d, joined %d\n",
            nrow(bnd), nrow(mle), nrow(ss3)))
stopifnot(nrow(ss3) > 40)

FLN <- c("FshTrawl","FshLL","FshPot","Srv","LLSrv","IPHCLL","ADFG","SPAWN",
         "Seine","Srv")      # Rceattle fleet 10 (Srv_ae1) reads Srv's SS3 block
grab <- function(pl) {
  b  <- as.numeric(pl$beta_linkage)[keep]
  s6 <- pl$sel_dn6
  o <- NULL
  for (i in seq_along(b)) {
    j <- which(ss3$pname == PNAME[pp[i]] & ss3$fleet == FLN[fl[i]] &
               ss3$year == yr[i])
    if (length(j) != 1) next
    o <- rbind(o, data.frame(col = dcu[i], pname = PNAME[pp[i]],
      fleet = FLN[fl[i]], year = yr[i], base = s6[pp[i], fl[i], 1],
      offset = b[i], realised = s6[pp[i], fl[i], 1] + b[i],
      ss3_mle = ss3$mle[j], lo = ss3$lo[j], hi = ss3$hi[j],
      stringsAsFactors = FALSE))
  }
  o
}

v <- grab(inits)
stopifnot(nrow(v) > 0, !anyNA(v$ss3_mle))
v$err <- v$realised - v$ss3_mle
worst <- max(abs(v$err))
cat(sprintf("\n=== reconstruction at the injected SS3 MLE: %d blocks matched ===\n",
            nrow(v)))
cat(sprintf("max abs (base + offset) - SS3's fitted value: %.4e\n", worst))
if (worst > 1e-4) {
  cat("RECONSTRUCTION IS WRONG -- the bounds comparison below is meaningless.\n")
  print(utils::head(v[order(-abs(v$err)), ], 10), row.names = FALSE)
} else {
  cat("validated.\n")
}
cat(sprintf("at SS3's own MLE, realised outside its LO/HI: %d of %d\n",
            sum(v$realised < v$lo | v$realised > v$hi), nrow(v)))

r <- readRDS("Bridging/_g3_parity.rds")
for (nm in c("warm", "cold")) {
  o <- grab(r[[nm]]$estimated_params)
  o$out <- o$realised < o$lo | o$realised > o$hi
  o$excess <- ifelse(o$realised > o$hi, o$realised - o$hi,
                     ifelse(o$realised < o$lo, o$lo - o$realised, 0))
  cat(sprintf("\n=== %s: %d of %d distinct blocks OUTSIDE SS3's LO/HI ===\n",
              nm, sum(o$out), nrow(o)))
  if (any(o$out)) print(utils::head(o[order(-o$excess),
    c("fleet","pname","year","realised","lo","hi","ss3_mle","excess")], 10),
    row.names = FALSE)
}
saveRDS(list(mle = v), "Bridging/_block_recon.rds")
cat("\nRECON DONE\n")
