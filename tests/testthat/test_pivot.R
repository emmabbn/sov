# tests/testthat/test_pivot.R

test_that("pivot errors if any of projv, vwmat, or votes are not matrices", {
  projv  <- matrix(c(0.1, 0.2, 0.3, 0.4), nrow = 2)
  vwmat  <- matrix(c(1, 1, 1, 1), nrow = 2)
  votes  <- c(1, 1)  # <- NOT a matrix

  qvec <- c(2, 2)

  expect_error(
    pivot(projv, vwmat, qvec, votes),
    regexp = "projv, vwmat, and votes must all be matrices",
    ignore.case = TRUE
  )
})

test_that("pivot errors when projv, vwmat, and votes do not have identical dimensions", {
  projv <- matrix(c(0.1, 0.2, 0.3, 0.4), nrow = 2)       # 2x2
  vwmat <- matrix(c(1, 1, 1, 1, 1, 1), nrow = 3)         # 3x2 (mismatch)
  votes <- matrix(c(1, 1, 1, 1), nrow = 2)               # 2x2

  qvec <- c(2, 2)

  expect_error(
    pivot(projv, vwmat, qvec, votes),
    regexp = "must have the same dimensions",
    ignore.case = TRUE
  )
})

test_that("pivot errors when length(qvec) != ncol(projv)", {
  projv <- matrix(c(0.1, 0.2, 0.3, 0.4), nrow = 2)       # 2 columns
  vwmat <- matrix(c(1, 1, 1, 1), nrow = 2)               # 2x2
  votes <- matrix(c(1, 1, 1, 1), nrow = 2)               # 2x2

  qvec_good <- c(2, 2)
  qvec_bad  <- c(2, 2, 2)  # wrong length

  # Sanity: good qvec should pass this pre-check (we don't run full pivot to avoid deeper deps)
  expect_error(
    pivot(projv, vwmat, qvec_good, votes),
    regexp = NA
  )

  expect_error(
    pivot(projv, vwmat, qvec_bad, votes),
    regexp = "qvec.*length.*ncol",
    ignore.case = TRUE
  )
})
