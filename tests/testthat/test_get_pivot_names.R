# tests/testthat/test_get_pivot_names.R

test_that("get_pivot_names errors if projv has no row names", {
  projv  <- matrix(c(-1, 0, 1, 2), nrow = 2)  # no rownames
  pivotv <- c(0, 1)
  votes  <- matrix(c(1, 0, 1, 1), nrow = 2,
                   dimnames = list(NULL, c("RC1","RC2")))

  expect_error(
    get_pivot_names(projv, pivotv, votes),
    regexp = "projv must have row names"
  )
})

test_that("get_pivot_names errors if pivotv length mismatches ncol(projv)/ncol(votes)", {
  projv <- matrix(c(-1, 0, 1, 2, 2, 3), nrow = 3,
                  dimnames = list(c("A","B","C"), NULL))
  colnames(projv) <- c("RC1","RC2")
  votes <- matrix(c(1, 1, 0,  1, 0, 1), nrow = 3,
                  dimnames = list(c("A","B","C"), c("RC1","RC2")))

  pivotv_bad <- c(0)  # wrong length
  expect_error(
    get_pivot_names(projv, pivotv_bad, votes),
    regexp = "Length of pivotv .* must have the same number of columns"
  )
})

test_that("get_pivot_names returns a single Pivot column of NA when all pivots are NA", {
  projv <- matrix(c(-1, 0,  1,
                     2, 3, -2),
                  nrow = 2, byrow = TRUE,
                  dimnames = list(c("A","B"), NULL))
  colnames(projv) <- c("RC1","RC2","RC3")
  votes <- matrix(c(1, 0,  1, 1,  0, 1),
                  nrow = 2,
                  dimnames = list(c("A","B"), c("RC1","RC2","RC3")))
  pivotv <- c(NA, NA, NA)

  out <- get_pivot_names(projv, pivotv, votes)

  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), 3L)
  expect_true(all(c("Position","RC_num","Pivot") %in% names(out)))
  expect_true(all(is.na(out$Pivot)))
  # ensure we didn't create Pivot1/Pivot2 columns when all are NA
  expect_false(any(grepl("^Pivot\\d+$", names(out))))
  # sanity: Position 1..3 and RC_num mirror vote colnames
  expect_equal(out$Position, 1:3)
  expect_equal(out$RC_num, colnames(votes))
})
