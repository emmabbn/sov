# tests/testthat/test_projections_and_polarity.R

test_that("projections_and_polarity errors when ideals and normals have different # of columns", {
  # ideals: 2 voters x 2 dims
  ideals  <- matrix(c(0.3, -0.4,
                      0.1,  0.2),
                    nrow = 2, byrow = TRUE)
  # normals: 3 rollcalls x 1 dim (COLUMN mismatch)
  normals <- matrix(c(1, -1, 1), ncol = 1)
  # votes: rows=voters (2), cols=rollcalls (3)
  votes   <- matrix(c(1,0,1, 1,1,0), nrow = 2, byrow = TRUE)
  cps     <- c(0, 0, 0)

  expect_error(
    projections_and_polarity(ideals, normals, votes, cps),
    regexp = "columns in 'ideals'.*columns in 'normals'",
    ignore.case = TRUE
  )
})

test_that("projections_and_polarity errors when #rows(ideals) != #rows(votes)", {
  # ideals: 2 voters x 2 dims
  ideals  <- matrix(c(0.3, -0.4,
                      0.1,  0.2),
                    nrow = 2, byrow = TRUE)
  # normals: 3 rollcalls x 2 dims
  normals <- matrix(c(1,0, -1,0, 0,1), nrow = 3, byrow = TRUE)
  # votes: make row-count mismatch (3 voters rows vs 2 in ideals)
  votes   <- matrix(c(1,0,1, 1,1,0, 0,1,1), nrow = 3, byrow = TRUE)
  cps     <- c(0, 0, 0)

  expect_error(
    projections_and_polarity(ideals, normals, votes, cps),
    regexp = "rows in `ideals`.*rows in `votes`",
    ignore.case = TRUE
  )
})

test_that("projections_and_polarity errors when #rows(normals) != ncol(votes) or length(cps)", {
  # ideals: 2 voters x 2 dims
  ideals  <- matrix(c(0.3, -0.4,
                      0.1,  0.2),
                    nrow = 2, byrow = TRUE)
  # normals: 2 rollcalls x 2 dims
  normals <- matrix(c(1,0, 0,1), nrow = 2, byrow = TRUE)
  # votes: 2 voters x 3 rollcalls  -> mismatch with nrow(normals)=2
  votes   <- matrix(c(1,0,1, 1,1,0), nrow = 2, byrow = TRUE)
  # cps length = 2 (also mismatched vs 3 rollcalls)
  cps     <- c(0, 0)

  expect_error(
    projections_and_polarity(ideals, normals, votes, cps),
    regexp = "rows in `normals`.*columns in `votes`.*length of `cps`",
    ignore.case = TRUE
  )
})

test_that("projections_and_polarity errors when votes contain invalid codes", {
  # Make all dimensions consistent first so we reach the validation of vote codes.
  ideals  <- matrix(c(0.3, -0.4,
                      0.1,  0.2),
                    nrow = 2, byrow = TRUE)
  normals <- matrix(c(1,0, 0,1), nrow = 2, byrow = TRUE) # 2 rollcalls x 2 dims
  # votes: 2 voters x 2 rollcalls, but include an invalid code (e.g., 2)
  votes   <- matrix(c(1, 2,
                      0, 1), nrow = 2, byrow = TRUE)
  cps     <- c(0, 0)

  expect_error(
    projections_and_polarity(ideals, normals, votes, cps),
    regexp = "vote matrix can only contain 1=yea, 0=nay, 9=.*NAs",
    ignore.case = TRUE
  )
})
