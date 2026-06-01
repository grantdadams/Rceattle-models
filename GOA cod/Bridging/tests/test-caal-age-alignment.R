# Standalone test for the CAAL age-column off-by-one fix in build_caal_data.
# Run via: testthat::test_file("tests/test-caal-age-alignment.R")
#
# Background: SS3 dat files often specify `agebin_vector = [1, 2, ..., max_age]`,
# meaning CAAL data covers ages 1..max_age (no age 0). When the user runs
# Rceattle at minage = 0 (i.e., includes age 0 as model age 0), the SS3 age 1
# obs must map to Rce age 1 (column CAAL_2), NOT Rce age 0 (column CAAL_1).
#
# The pre-fix converter padded trailing zeros, putting SS3 age 1 obs into
# CAAL_1 (Rce age 0). All ages shifted by one -> systematic predicted-vs-
# observed mismatch -> CAAL NLL inflated ~8-9x on the GOA Pcod 2024 model.
#
# Fix: build_caal_data now parses each SS3 age column label (e.g., "a1"),
# computes the leading-zero pad count from the gap to minage, and pads at
# the front. Test verifies the alignment.

testthat::test_that("build_caal_data pads leading zeros when SS3 ages > minage", {
  testthat::skip_if_not_installed("testthat")
  source("../R/ss3_to_rceattle.R")

  # Synthetic SS3 CAAL row with ages 1..10 (SS3 typical agebin_vector),
  # all mass at age 1 (clean signal -- if alignment is right, Rce CAAL_2
  # gets the mass since Rce age 1 = SS3 age 1)
  caal <- data.frame(
    fleet  = 1L,
    year   = 1990L,
    seas   = 1L,
    sex    = 1L,
    Nsamp  = 50,
    Lbin_lo = 30,
    Lbin_hi = 35,
    a1 = 1, a2 = 0, a3 = 0, a4 = 0, a5 = 0,
    a6 = 0, a7 = 0, a8 = 0, a9 = 0, a10 = 0
  )

  # Mock datlist: agecomp held in $agecomp, lbin_vector for length lookup
  datlist <- list(
    agecomp = caal,
    Nages   = 10,
    lbin_vector = seq(2, 100, by = 5)   # arbitrary; only used for length match
  )

  fleet_control <- data.frame(
    Fleet_name = "TestFlt",
    Fleet_code = 1L,
    stringsAsFactors = FALSE
  )

  minage <- 0L
  nages  <- 11L   # Rce age slots: 0..10

  out <- build_caal_data(datlist, fleet_control,
                         nages = nages, minage = minage,
                         nlengths = length(datlist$lbin_vector),
                         ss3_lbins = datlist$lbin_vector)

  # Pull out the CAAL_* columns -- they should be Rce slot 1..nages = age 0..10
  caal_cols <- grep("^CAAL_", colnames(out), value = TRUE)
  obs_vec   <- as.numeric(out[1, caal_cols])

  # SS3 age 1 = Rce age 1 = column CAAL_2 (slot 2 in 1-indexed)
  testthat::expect_equal(obs_vec[1], 0,
                         label = "CAAL_1 (Rce age 0) should be 0 (no SS3 age 0)")
  testthat::expect_equal(obs_vec[2], 1,
                         label = "CAAL_2 (Rce age 1) should hold SS3 age 1 obs")
  testthat::expect_equal(sum(obs_vec[-c(1, 2)]), 0,
                         label = "ages 2..10 should all be 0 since SS3 mass was at age 1")
})


testthat::test_that("build_caal_data preserves alignment when SS3 ages start at minage", {
  testthat::skip_if_not_installed("testthat")
  source("../R/ss3_to_rceattle.R")

  # If SS3 agebin starts at age 0 (matches Rce minage = 0), no padding needed
  caal <- data.frame(
    fleet  = 1L,
    year   = 1990L,
    seas   = 1L,
    sex    = 1L,
    Nsamp  = 50,
    Lbin_lo = 30,
    Lbin_hi = 35,
    a0 = 0, a1 = 1, a2 = 0, a3 = 0, a4 = 0,
    a5 = 0, a6 = 0, a7 = 0, a8 = 0, a9 = 0, a10 = 0
  )

  datlist <- list(
    agecomp = caal,
    Nages   = 10,
    lbin_vector = seq(2, 100, by = 5)
  )

  fleet_control <- data.frame(
    Fleet_name = "TestFlt",
    Fleet_code = 1L,
    stringsAsFactors = FALSE
  )

  out <- build_caal_data(datlist, fleet_control,
                         nages = 11L, minage = 0L,
                         nlengths = length(datlist$lbin_vector),
                         ss3_lbins = datlist$lbin_vector)

  caal_cols <- grep("^CAAL_", colnames(out), value = TRUE)
  obs_vec   <- as.numeric(out[1, caal_cols])

  # SS3 age 0 obs at column CAAL_1, SS3 age 1 obs at CAAL_2
  testthat::expect_equal(obs_vec[1], 0)
  testthat::expect_equal(obs_vec[2], 1)
})


testthat::test_that("build_caal_data drops over-spanning ages when SS3 has more than Rce", {
  testthat::skip_if_not_installed("testthat")
  source("../R/ss3_to_rceattle.R")

  # SS3 ages 1..15 but Rce minage=2, nages=10 (slots 2..11). Test that the
  # converter truncates correctly. (Existing code path; regression test.)
  caal <- data.frame(
    fleet  = 1L, year = 1990L, seas = 1L, sex = 1L, Nsamp = 50,
    Lbin_lo = 30, Lbin_hi = 35,
    a1 = 0, a2 = 1, a3 = 0, a4 = 0, a5 = 0,
    a6 = 0, a7 = 0, a8 = 0, a9 = 0, a10 = 0,
    a11 = 0, a12 = 0, a13 = 0, a14 = 0, a15 = 0
  )

  datlist <- list(agecomp = caal, Nages = 15,
                  lbin_vector = seq(2, 100, by = 5))
  fleet_control <- data.frame(Fleet_name = "TestFlt", Fleet_code = 1L,
                              stringsAsFactors = FALSE)

  out <- build_caal_data(datlist, fleet_control,
                         nages = 10L, minage = 2L,
                         nlengths = length(datlist$lbin_vector),
                         ss3_lbins = datlist$lbin_vector)

  caal_cols <- grep("^CAAL_", colnames(out), value = TRUE)
  obs_vec   <- as.numeric(out[1, caal_cols])

  # Rce slot k = Rce age (k-1+minage) = age (k+1)
  # SS3 age 2 (mass=1) -> Rce age 2 -> CAAL_1 (since 1 + minage = 1+2 = 3... wait)
  # Actually: Rce slot 1 = age (1-1+2) = age 2. So CAAL_1 = Rce age 2 = SS3 age 2.
  testthat::expect_equal(obs_vec[1], 1,
                         label = "CAAL_1 (Rce age 2) should hold SS3 age 2 obs")
  testthat::expect_equal(sum(obs_vec[-1]), 0)
})
