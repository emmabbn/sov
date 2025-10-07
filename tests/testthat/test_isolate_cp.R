# tests/testthat/test_isolate_cp.R

test_that("isolate.cp() errors for WNOM when weights vector is invalid", {
  # Minimal WNOM-like rollcalls with 2D midpoints, so ncol(midpoints) == 2
  rollcalls <- data.frame(
    midpoint1D = 0.2,
    midpoint2D = -0.3,
    spread1D   = 0.1,   # not used here but typical to exist
    spread2D   = 0.2,
    row.names  = "RC1",
    check.names = FALSE
  )
  estimates <- list(
    legislators = data.frame(
      coord1D = 0, coord2D = 0, GMP = 0.5, CC = 0.5,
      row.names = "A",
      check.names = FALSE
    ),
    rollcalls = rollcalls
  )
  class(estimates) <- "nomObject"

  # Any unit normal will do (1 x 2)
  normals <- matrix(c(1, 0), nrow = 1, dimnames = list("RC1", c("dim1","dim2")))

  # Case 1: length mismatch (weights length 1 vs 2 dimensions)
  expect_error(
    isolate.cp(type = "wnom", estimates = estimates, midpoints_mcmc = NULL,
               normals = normals, weights = c(1)),
    regexp = "must equal the number of dimensions",
    ignore.case = TRUE
  )

  # Case 2: first element not 1
  expect_error(
    isolate.cp(type = "wnom", estimates = estimates, midpoints_mcmc = NULL,
               normals = normals, weights = c(0.5, 1)),
    regexp = "first dimension must weigh 1",
    ignore.case = TRUE
  )
})

test_that("isolate.cp() errors for unsupported type", {
  normals <- matrix(c(1, 0), nrow = 1, dimnames = list("RC1", c("dim1","dim2")))
  # 'estimates' and other args are ignored in this branch; still pass something minimal
  expect_error(
    isolate.cp(type = "other", estimates = list(), midpoints_mcmc = NULL,
               normals = normals, weights = c(1, 1)),
    regexp = "only takes 'oc', 'wnom', and 'mcmcPack'",
    ignore.case = TRUE
  )
})
