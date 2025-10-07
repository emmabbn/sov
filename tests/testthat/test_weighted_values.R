test_that("normalize_weights rejects empty / NA / negative / zero-first", {
  expect_error(normalize_weights(numeric(0)),      "length >= 1")
  expect_error(normalize_weights(c(NA_real_, 1)),  "cannot contain")
  expect_error(normalize_weights(c(Inf, 1)),       "cannot contain")
  expect_error(normalize_weights(c(-1, 0)),        "non-negative")
  expect_error(normalize_weights(c(0, 1)),         "first weight.*> 0")  # zero first element -> error
})

test_that("normalize_weights allows zero in later dimensions and normalizes", {
  expect_equal(normalize_weights(c(1, 0)),       c(1, 0))
  expect_equal(normalize_weights(c(2, 0, 4)),    c(1, 0, 2))
  expect_equal(normalize_weights(c(3, 3, 9)),    c(1, 1, 3))
})

# tests/testthat/test_relative_weight.R
test_that("relative_weight errors on non-logical-scalar weight_nom", {
  # vector logical -> error
  expect_error(
    relative_weight(type = "oc", estimates = NULL, weight_nom = c(TRUE, FALSE)),
    "logical scalar"
  )
  # non-logical -> error
  expect_error(
    relative_weight(type = "oc", estimates = NULL, weight_nom = "TRUE"),
    "logical scalar"
  )
})

test_that("relative_weight accepts logical NA (current behavior) and falls back", {
  # For non-wnom types, NA just falls through and returns 1
  expect_equal(relative_weight(type = "oc", estimates = NULL, weight_nom = NA), 1)
})

test_that("relative_weight for wnom returns normalized vs. ones", {
  est <- list(weights = c(2, 0, 4))  # first > 0, later can be 0
  # When weight_nom = TRUE -> normalize by first element
  expect_equal(relative_weight("wnom", est, TRUE), c(1, 0, 2))
  # When weight_nom = FALSE -> all ones (length = #dims)
  expect_equal(relative_weight("wnom", est, FALSE), rep(1, 3))
})

test_that("weighted_values errors when Z's columns don't match length(W)", {
  Z <- matrix(1:6, nrow = 3, ncol = 2)  # 2 columns
  W <- c(1, 2, 3)                       # length 3 -> mismatch
  expect_error(
    weighted_values(Z, W),
    "columns in matrix Z must match the length of weights W"
  )
})

test_that("weighted_values scales columns by weights", {
  Z <- matrix(c(1, 2,
                3, 4),
              nrow = 2, ncol = 2, byrow = TRUE)
  W <- c(2, 3)
  out <- weighted_values(Z, W)
  expected <- cbind(Z[, 1] * W[1], Z[, 2] * W[2])
  expect_equal(out, expected)
})
