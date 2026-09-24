# Row-by-row check that each corrected run fits the bins the ORIGINAL file
# meant. Reads FIT_AGE_COMPS out of Report.sso directly rather than going
# through r4ss, and compares against the original data file's labels.
# Run from the Rceattle-models folder.

fit_age_comps <- function(dir) {
  f <- file.path(dir, "Report.sso")
  l <- readLines(f, warn = FALSE)
  h <- grep("^Fleet Fleet_Name Area Yr Seas Subseas Month Time Sexes Part Ageerr Lbin_lo", l)[1]
  nm <- strsplit(trimws(l[h]), " +")[[1]]
  out <- list(); i <- h + 1
  while (i <= length(l) && nzchar(trimws(l[i])) && !grepl("^#", l[i])) {
    v <- strsplit(trimws(l[i]), " +")[[1]]
    if (length(v) < 13) break
    out[[length(out) + 1]] <- v[1:13]
    i <- i + 1
  }
  d <- as.data.frame(do.call(rbind, out), stringsAsFactors = FALSE)
  names(d) <- nm[1:13]
  d$Yr <- as.integer(d$Yr)
  d$Lbin_lo <- as.numeric(d$Lbin_lo); d$Lbin_hi <- as.numeric(d$Lbin_hi)
  d
}

cat("=========== AI cod ===========\n")
# original labels, in file order
ai_orig <- read.table("AI cod - Dev/Data/M24_1_adjusted/data.ss", skip = 232, nrows = 1174,
                      fill = TRUE, comment.char = "")
ai_orig <- ai_orig[ai_orig$V7 > 0, ]
fx <- fit_age_comps("AI cod - Dev/Data/M24_1_caal_bins_fixed")
# drop the turned-off marginal age comps, which span the whole grid
fx <- fx[!(fx$Lbin_lo == 0.5 & fx$Lbin_hi == 142.5), ]
cat("original CAAL rows:", nrow(ai_orig), "   FIT_AGE_COMPS rows:", nrow(fx), "\n")
stopifnot(nrow(ai_orig) == nrow(fx), all(ai_orig$V1 == fx$Yr))
bad <- which(fx$Lbin_lo != ai_orig$V7 | fx$Lbin_hi != ai_orig$V7)
cat("rows whose fitted bin != the single bin the file labelled:", length(bad), "\n")
cat("  (want Lbin_lo = Lbin_hi = the original Lbin_lo, e.g.",
    ai_orig$V7[1], "->", fx$Lbin_lo[1], fx$Lbin_hi[1], ")\n")
if (length(bad)) print(head(data.frame(yr = ai_orig$V1[bad], orig = ai_orig$V7[bad],
                                       got_lo = fx$Lbin_lo[bad], got_hi = fx$Lbin_hi[bad]), 5))

cat("\n=========== GOA cod ===========\n")
go <- read.table("GOA cod/Data/goa_pcod-no init and ramp/GOAPcod2024Oct17_1e_5cm.dat",
                 skip = 601, nrows = 890, fill = TRUE, comment.char = "#")
go <- go[go$V7 > 0, ]
gx <- fit_age_comps("GOA cod/Data/goa_pcod_caal_bins_fixed")
gx <- gx[gx$Lbin_lo > 0 | gx$Lbin_hi < 104.5, ]
# the marginal age comps span the whole grid; drop them
gx <- gx[!(gx$Lbin_lo == 0.5 & gx$Lbin_hi == 104.5), ]
cat("original CAAL rows:", nrow(go), "   FIT_AGE_COMPS CAAL rows:", nrow(gx), "\n")
# what each 5 cm data bin SHOULD cover on the 1 cm population grid
dat_edges <- seq(4.5, 104.5, by = 5)
want_lo <- ifelse(dat_edges == 4.5, 0.5, dat_edges)          # bin 1 is a minus group
want_hi <- ifelse(dat_edges == 104.5, 104.5, dat_edges + 4)  # last is the plus bin
map <- data.frame(dat = dat_edges, lo = want_lo, hi = want_hi)
cat("intended mapping:\n"); print(head(map, 3)); print(tail(map, 2))
if (nrow(go) == nrow(gx) && all(go$V1 == gx$Yr)) {
  m <- map[match(go$V7, map$dat), ]
  bad <- which(gx$Lbin_lo != m$lo | gx$Lbin_hi != m$hi)
  cat("\nrows whose fitted bins != the data bin's population bins:", length(bad), "of", nrow(gx), "\n")
  if (length(bad)) print(head(data.frame(yr = go$V1[bad], dat_bin = go$V7[bad],
        want_lo = m$lo[bad], want_hi = m$hi[bad],
        got_lo = gx$Lbin_lo[bad], got_hi = gx$Lbin_hi[bad]), 6))
} else {
  cat("\nrow counts or years differ -- comparing the distinct bin pairs instead\n")
  got <- unique(gx[, c("Lbin_lo","Lbin_hi")]); got <- got[order(got$Lbin_lo), ]
  print(cbind(got, want_hi_for_that_lo = map$hi[match(got$Lbin_lo, map$lo)]))
}
