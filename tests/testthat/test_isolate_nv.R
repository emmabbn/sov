# tests/testthat/test_isolate_nv.R

test_that("isolate.nv('wnom') errors when weights length mismatch or first element not 1", {
  # Minimal 2D WNOM-like object (uses helpers from helpers-mocks.R)
  ideals <- matrix(c(0.2, -0.3), nrow = 1,
                   dimnames = list("v1", NULL))
  colnames(ideals) <- c("coord1D", "coord2D")

  midpoints <- matrix(c(0, 0), nrow = 1,
                      dimnames = list("RC1", c("dim1", "dim2")))
  spreads   <- matrix(c(0.5, 0.1), nrow = 1,
                      dimnames = list("RC1", c("dim1", "dim2")))

  est <- estimates_wnom(
    ideals    = ideals,
    midpoints = midpoints,
    spreads   = spreads,
    weights   = c(1, 1)   # this is the *object*'s weights; isolate.nv uses the weights argument we pass below
  )

  # Wrong length
  expect_error(
    isolate.nv(type = "wnom", estimates = est, midpoints_mcmc = NULL, weights = c(1)),
    regexp = "length of the weights vector.*number of dimensions|first dimension must weigh 1",
    ignore.case = TRUE
  )

  # First element not 1
  expect_error(
    isolate.nv(type = "wnom", estimates = est, midpoints_mcmc = NULL, weights = c(2, 1)),
    regexp = "first dimension must weigh 1",
    ignore.case = TRUE
  )
})

test_that("isolate.nv('mcmc') errors when midpoints_mcmc is missing", {
  expect_error(
    isolate.nv(type = "mcmc", estimates = list(), midpoints_mcmc = NULL, weights = NULL),
    regexp = "midpoints_mcmc.*must be provided",
    ignore.case = TRUE
  )
})

test_that("isolate.nv errors for unsupported type", {
  expect_error(
    isolate.nv(type = "other", estimates = list(), midpoints_mcmc = NULL, weights = NULL),
    regexp = "only takes 'oc', 'wnom', and 'mcmc",
    ignore.case = TRUE
  )
})
