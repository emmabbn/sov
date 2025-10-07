# tests/testthat/test_validate_vote_matrix.R

# Helper: valid baseline shapes (2 voters x 1 rollcall)
base_ideals  <- matrix(c(-0.5, 0.2), ncol = 1, dimnames = list(c("A","B"), "coord1D"))
base_normals <- matrix(1, nrow = 1, ncol = 1, dimnames = list("RC1", "dim1"))
base_votes   <- matrix(c(1, 0), nrow = 2, dimnames = list(c("A","B"), "RC1"))
base_vw      <- c(1, 1)

test_that("votes must contain only {0,1,9,NA}", {
  votes_bad <- matrix(c(1, 2), nrow = 2)  # 2 is invalid
  expect_error(
    validate_vote_matrix(base_ideals, base_normals, base_vw, votes_bad),
    regexp = "The vote matrix can only contain 1=yea, 0=nay, 9=attended but abstain, and NAs"
  )
})

test_that("votes must contain at least one 0 and one 1", {
  votes_all1 <- matrix(c(1, 1), nrow = 2)
  expect_error(
    validate_vote_matrix(base_ideals, base_normals, base_vw, votes_all1),
    regexp = "must contain at least one 1=yea and one 0=nay"
  )

  votes_all0 <- matrix(c(0, 0), nrow = 2)
  expect_error(
    validate_vote_matrix(base_ideals, base_normals, base_vw, votes_all0),
    regexp = "must contain at least one 1=yea and one 0=nay"
  )
})

test_that("nrow(ideals) must equal nrow(votes)", {
  ideals_bad <- matrix(c(-0.5, 0.2, 0.3), ncol = 1)  # 3 voters vs 2 in votes
  expect_error(
    validate_vote_matrix(ideals_bad, base_normals, base_vw, base_votes),
    regexp = "rows in 'ideals'.*must match.*rows.*'votes'"
  )
})

test_that("nrow(normals) must equal ncol(votes)", {
  normals_bad <- matrix(1, nrow = 2, ncol = 1)  # 2 RCs vs 1 column in votes
  expect_error(
    validate_vote_matrix(base_ideals, normals_bad, base_vw, base_votes),
    regexp = "rows in 'normals'.*must match.*columns.*`votes`"
  )
})

test_that("length(vw) must equal nrow(votes)", {
  vw_bad <- c(1, 1, 1)  # length 3 vs 2 voters
  expect_error(
    validate_vote_matrix(base_ideals, base_normals, vw_bad, base_votes),
    regexp = "Length of `vw`.*must match the number of voters"
  )
})

test_that("happy path: valid inputs pass silently", {
  expect_silent(
    validate_vote_matrix(base_ideals, base_normals, base_vw, base_votes)
  )
})
