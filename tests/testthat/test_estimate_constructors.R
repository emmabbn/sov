# tests/testthat/test_estimate_constructors.R

test_that("OC constructor builds an object identify() recognizes and vs_sov runs (1D)", {
  # 1D ideals, normals, and cp
  ideals <- matrix(c(-0.8, 0, 0.9), ncol = 1,
                   dimnames = list(c("A","B","C"), "coord1D"))
  normals <- matrix(1, nrow = 1, dimnames = list("RC1","dim1"))
  cps <- 0

  est <- estimates_oc(ideals, normals, cps)
  expect_equal(identify(est), "oc")

  # Include both a 1 and a 0 to satisfy validator
  votes <- matrix(c(1, 1, 0), nrow = 3, dimnames = list(c("A","B","C"), "RC1"))
  out <- vs_sov(estimates = est, votes = votes, absolute = TRUE, q = 2, print_results = FALSE)

  # With cp=0 and normal +x, pivot is the middle (0) -> "B"
  expect_equal(out$pivot_by_rc$Pivot[1], "B")
})


test_that("WNOM constructor builds an object identify() recognizes and vs_sov runs (2D)", {
  ideals <- rbind(
    p1 = c( 0.7,  0.7),
    p2 = c( 0.5, -0.5),
    p3 = c(-0.5,  0.5),
    p4 = c(-0.7, -0.7)
  )
  colnames(ideals) <- c("coord1D","coord2D")

  midpoints <- rbind(
    RC1 = c( 0.2, 0.0),  # +x
    RC2 = c( 0.0, 0.2),  # +y
    RC3 = c(-0.2, 0.0)   # -x
  )
  # Axis-aligned spreads so normals are along axes (diff = 2*spreads)
  spreads <- rbind(
    RC1 = c(0.5, 0.0),
    RC2 = c(0.0, 0.5),
    RC3 = c(0.5, 0.0)
  )

  est <- estimates_wnom(ideals, midpoints, spreads, weights = c(1, 1))
  expect_equal(identify(est), "wnom")

  votes <- cbind(
    RC1 = c(1, 1, 1, 0),
    RC2 = c(1, 0, 1, 1),
    RC3 = c(0, 1, 1, 1)
  )
  rownames(votes) <- rownames(ideals)

  out <- vs_sov(estimates = est, votes = votes, absolute = TRUE, q = 3, print_results = FALSE)
  expect_equal(out$pivot_by_rc$Pivot, c("p2","p3","p3"))
})


test_that("MCMC constructor: irt_cutpoints matches intended midpoints; vs_sov runs (2D)", {
  ideals <- rbind(
    p1 = c( 0.7,  0.7),
    p2 = c( 0.5, -0.5),
    p3 = c(-0.5,  0.5),
    p4 = c(-0.7, -0.7)
  )
  colnames(ideals) <- c("coord1D","coord2D")
  # Choose alphas/betas to yield +x (0.2,0), +y (0,0.2), -x (-0.2,0)
  alphas <- c(RC1 = -0.2, RC2 = -0.2, RC3 =  0.2)
  betas  <- rbind(RC1 = c(1,0), RC2 = c(0,1), RC3 = c(1,0))
  est <- estimates_mcmc(ideals, alphas, betas)

  # Midpoints computed by helper (before rescale) must match expected
  mids <- irt_cutpoints("mcmc", est)
  expect_equal(unname(mids[1,]), c( 0.2, 0.0), tolerance = 1e-12)
  expect_equal(unname(mids[2,]), c( 0.0, 0.2), tolerance = 1e-12)
  expect_equal(unname(mids[3,]), c(-0.2, 0.0), tolerance = 1e-12)

  votes <- cbind(
    RC1 = c(1, 1, 1, 0),
    RC2 = c(1, 0, 1, 1),
    RC3 = c(0, 1, 1, 1)
  )
  rownames(votes) <- rownames(ideals)

  # Full pipeline should identify as mcmc internally and run
  out <- vs_sov(estimates = est, votes = votes, absolute = TRUE, q = 3, print_results = FALSE)
  expect_equal(out$pivot_by_rc$Pivot, c("p2","p3","p3"))
})


