test_that("rescale errors when both matrices are NULL", {
  expect_error(
    rescale(),
    "At least one of 'ideals' or 'midpoints_mcmc' must be provided"
  )
})

test_that("rescale errors when column counts differ", {
  ideals <- matrix(c(1,2,3,4), ncol = 2)         # 2 cols
  mids   <- matrix(c(1,2,3),   ncol = 3)         # 3 cols -> mismatch
  expect_error(
    rescale(ideals = ideals, midpoints_mcmc = mids),
    "must match"
  )
})

test_that("rescale with only ideals scales down uniformly when needed", {
  # max row norm = 2 (row c(2,0)), so divide all rows by 2
  ideals <- rbind(a = c(2, 0),
                  b = c(0, 0.5))
  out <- rescale(ideals = ideals, midpoints_mcmc = NULL)
  expect_null(out$midpoints_mcmc)              # hit the 'all_points <- ideals' branch
  expect_equal(out$ideals["a", ], c(1, 0))
  expect_equal(out$ideals["b", ], c(0, 0.25))
  # all rows now inside unit sphere
  rn <- sqrt(rowSums(out$ideals^2))
  expect_true(all(rn <= 1 + 1e-12))
})

test_that("rescale with only midpoints_mcmc scales down uniformly when needed", {
  # max row norm = 5 (3-4-5 triangle), so divide by 5
  mids <- rbind(RC1 = c(3, 4))
  out <- rescale(ideals = NULL, midpoints_mcmc = mids)
  expect_null(out$ideals)                       # hit the 'all_points <- midpoints_mcmc' branch
  expect_equal(out$midpoints_mcmc["RC1", ], c(0.6, 0.8))
  rn <- sqrt(rowSums(out$midpoints_mcmc^2))
  expect_true(all(abs(rn - 1) < 1e-12))
})

test_that("validate.scaling warns when any row has norm > 1 (and includes object name)", {
  X <- rbind(c(2, 0),   # norm = 2 > 1  -> should warn
             c(0, 0.5)) # norm = 0.5
  # Warning fires with the expected phrasing…
  expect_warning(
    validate.scaling(X),
    regexp = "lies outside the unit hypersphere"
  )
  # …and it mentions the object name ('X') captured via deparse(substitute())
  expect_warning(
    validate.scaling(X),
    regexp = "in 'X'"
  )
})

test_that("validate.scaling does not warn when all rows are inside the unit sphere", {
  Y <- rbind(c(0.6, 0.8),  # norm = 1
             c(0.0, 0.5))  # norm = 0.5
  expect_no_warning(validate.scaling(Y))
})
