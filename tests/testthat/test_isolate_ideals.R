test_that("isolate.ideals('wnom') errors when weights length mismatches or first weight != 1", {
  # Minimal 2D ideals
  ideals <- rbind(
    a = c( 0.2, -0.3),
    b = c(-0.1,  0.5)
  )
  colnames(ideals) <- c("coord1D","coord2D")

  # One roll call; dims must match ideals
  midpoints <- matrix(c(0.0, 0.0), nrow = 1, dimnames = list("RC1", c("dim1","dim2")))
  spreads   <- matrix(c(0.5, 0.5), nrow = 1, dimnames = list("RC1", c("dim1","dim2")))

  # Build a WNOM-like estimates object using your helper
  est <- estimates_wnom(ideals, midpoints, spreads, weights = c(1,1))

  # Length mismatch (pass a single weight for 2D): should error
  expect_error(
    isolate.ideals("wnom", est, weights = c(1)),
    regexp = "The length of the weights vector must equal the number of dimensions and the first dimension must weigh 1\\."
  )

  # First weight not 1 (policy requires weights[1] == 1): should error
  expect_error(
    isolate.ideals("wnom", est, weights = c(2, 1)),
    regexp = "The length of the weights vector must equal the number of dimensions and the first dimension must weigh 1\\."
  )
})

test_that("isolate.ideals() stops for unsupported type", {
  # Reuse the same WNOM-like object, but call isolate.ideals with an unknown type
  ideals <- rbind(
    a = c(0.2, -0.3),
    b = c(-0.1, 0.5)
  )
  colnames(ideals) <- c("coord1D","coord2D")
  midpoints <- matrix(c(0.0, 0.0), nrow = 1, dimnames = list("RC1", c("dim1","dim2")))
  spreads   <- matrix(c(0.5, 0.5), nrow = 1, dimnames = list("RC1", c("dim1","dim2")))

  est <- estimates_wnom(ideals, midpoints, spreads, weights = c(1,1))

  expect_error(
    isolate.ideals("other", est, weights = c(1,1)),
    regexp = "require estimates from either `oc', `wnom', or `mcmc\\.'"
  )
})
