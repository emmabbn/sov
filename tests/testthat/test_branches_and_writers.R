# following line is potentially outdated
# context("Branch & validation coverage + writers")

## --- identify(): oc / wnom / mcmc / dk --------------------------------------

test_that("identify() recognizes oc, wnom, mcmc and errors on unknown", {
  # OC-like
  oc_est <- list(
    legislators = data.frame(rank = 1:2, volume = 1, coord1D = c(0.1, -0.2)),
    rollcalls  = data.frame(midpoints = 0, normVector1D = 1)
  )
  expect_equal(identify(oc_est), "oc")

  # WNOM-like
  wnom_est <- list(
    legislators = data.frame(GMP = 0.6, CC = 0.9, coord1D = c(0.2, -0.1)),
    rollcalls  = data.frame(GMP = 0.5, spread1D = 0.1, midpoint1D = 0)
  )
  expect_equal(identify(wnom_est), "wnom")

  # MCMC-like
  stats <- do.call(rbind, list(
    data.frame(Mean =  0.3, row.names = "theta.A.1"),
    data.frame(Mean = -0.2, row.names = "theta.A.2"),
    data.frame(Mean =  0.1, row.names = "beta.RC1.1")
  ))
  mcmc_est <- list(statistics = stats)
  expect_equal(identify(mcmc_est), "mcmc")

  # Unknown
  unk <- list(legislators = data.frame(x = 1), rollcalls = data.frame(y = 2))
  expect_error(identify(unk), "Specify your estimation technique")
})

## --- relative_weight(): wnom TRUE/FALSE and non-wnom --------------------------

test_that("relative_weight() returns correct weights across branches", {
  # wnom with weights
  est <- list(weights = c(2, 4, 6))
  out_true  <- relative_weight("wnom", est, TRUE)
  out_false <- relative_weight("wnom", est, FALSE)
  expect_equal(out_true,  c(1, 2, 3))   # normalized by first entry
  expect_equal(out_false, c(1, 1, 1))   # equal weights

  # non-wnom -> scalar 1
  expect_equal(relative_weight("oc",   est, TRUE), 1)
  expect_equal(relative_weight("mcmc", est, FALSE), 1)
})

## --- vs_sov_user(): normals vs midpoints; absolute vs simple; feasible.q -----

test_that("vs_sov_user works for normals path, absolute majority", {
  ideals <- matrix(c(-0.5, 0.0, 0.5), ncol = 1,
                   dimnames = list(c("A","B","C"), "coord1D"))
  normals <- matrix(1, nrow = 1, dimnames = list("RC1", "dim1"))
  votes   <- matrix(c(1,1,0), nrow = 3, dimnames = list(rownames(ideals), "RC1"))

  out <- vs_sov_user(
    ideals    = ideals,
    normals   = normals,
    votes     = votes,
    absolute  = TRUE,
    q         = 2
  )
  expect_equal(out$pivot_by_rc$Pivot[1], "B")
})

test_that("vs_sov_user works for midpoints path, simple majority with abstain; feasible.q warning for absolute", {
  ideals <- matrix(c(-0.6, 0.0, 0.6), ncol = 1,
                   dimnames = list(c("A","B","C"), "coord1D"))
  mid    <- matrix(0, nrow = 1, dimnames = list("RC1", "dim1"))

  # ---- Simple k-majority: include an abstain (9) ----
  votes_simple <- matrix(c(1, 9, 0), nrow = 3,
                         dimnames = list(rownames(ideals), "RC1"))
  out_s <- vs_sov_user(
    ideals    = ideals,
    midpoints = mid,
    votes     = votes_simple,
    absolute  = FALSE,
    pr        = 0.5001
  )
  expect_true(!is.na(out_s$pivot_by_rc$Pivot[1]))

  # ---- Absolute rule with infeasible q: need at least one 0 somewhere to pass validation ----
  # Column RCinf is infeasible (only 1 attends); RCok supplies a 0 so the matrix has both 1 and 0 overall
  votes_abs <- cbind(
    RCinf = c(1, NA, NA),
    RCok  = c(1,  0,  NA)
  )
  rownames(votes_abs) <- rownames(ideals)
  mid2 <- rbind(mid, mid)
  rownames(mid2) <- c("RCinf","RCok")

  expect_warning(
    out_a <- vs_sov_user(
      ideals    = ideals,
      midpoints = mid2,
      votes     = votes_abs,
      absolute  = TRUE,
      q         = 3,
      vw        = c(1,1,1)
    ),
    "reset to the sum of weighted votes among those attending"
  )
  expect_equal(nrow(out_a$pivot_by_rc), 2L)
  expect_true(all(colnames(out_a$pivot_by_rc)[1:2] == c("Position","RC_num")))
})


## --- Excel writers: vs_sov and sov smoke tests --------------------------------

test_that("excel export writes expected sheets (vs_sov)", {
  # Minimal OC-like 1D
  ideals <- matrix(c(-0.5,0.5), ncol = 1, dimnames = list(c("A","B"), "coord1D"))
  estimates <- list(
    legislators = data.frame(coord1D = ideals[,1], rank = 1:2, volume = 1, row.names = rownames(ideals)),
    rollcalls  = data.frame(midpoints = 0, normVector1D = 1, row.names = "RC1")
  )
  votes <- matrix(c(1,0), nrow = 2, dimnames = list(rownames(ideals), "RC1"))

  td <- file.path(tempdir(), "sov_out")
  out <- vs_sov(estimates, votes = votes, absolute = TRUE, q = 2,
                print_results = TRUE, out_dir = td)

  path <- file.path(td, "vs-sovs.xlsx")
  expect_true(file.exists(path))

  sheets <- openxlsx::getSheetNames(path)
  expect_setequal(sheets, c("sov", "name_pivots_by_rollcall", "nv_and_angles"))
})


test_that("excel export writes expected sheets (sov)", {
  ideals <- matrix(c(-0.5,0.5), ncol = 1, dimnames = list(c("A","B"), "coord1D"))

  # OC-like object: include 'rank' & 'volume' in legislators AND 'midpoints' in rollcalls
  estimates <- list(
    legislators = data.frame(
      coord1D = ideals[,1],
      rank    = 1:2,
      volume  = 1,
      row.names = rownames(ideals)
    ),
    rollcalls  = data.frame(
      midpoints = 0,            # <-- required so identify() sees this as 'oc'
      row.names = "RC1"
    )
  )

  td <- file.path(tempdir(), "sov_out2")
  out <- sov(estimates, av = c(1,1), absolute = TRUE, q = 2,
             print_results = TRUE, out_dir = td)

  path <- file.path(td, "sovs.xlsx")
  expect_true(file.exists(path))

  sheets <- openxlsx::getSheetNames(path)
  expect_setequal(sheets, c("sov", "pivots_by_angle"))
})
