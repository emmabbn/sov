# tests/testthat/test_feasible_q.R

test_that("feasible.q errors when absolute != TRUE", {
  vwmat <- matrix(c(1, 1, 1, 1), nrow = 2)
  votes <- matrix(c(1, 0, 1, 0), nrow = 2)
  qvec  <- c(1, 1)

  expect_error(
    feasible.q(absolute = FALSE, vwmat = vwmat, qvec = qvec, votes = votes),
    regexp = "applies only when absolute == TRUE"
  )

  # also rejects non-logical (NA)
  expect_error(
    feasible.q(absolute = NA, vwmat = vwmat, qvec = qvec, votes = votes),
    regexp = "applies only when absolute == TRUE"
  )
})

test_that("feasible.q errors when vwmat and votes dims differ", {
  vwmat <- matrix(c(1, 1, 1, 1), nrow = 2, dimnames = list(NULL, c("RC1","RC2")))
  votes <- matrix(c(1, 0, 1),     nrow = 3, dimnames = list(NULL, "RC1"))  # different dims
  qvec  <- c(1, 1)

  expect_error(
    feasible.q(absolute = TRUE, vwmat = vwmat, qvec = qvec, votes = votes),
    regexp = "vwmat must have the same number of dimensions as matrix votes"
  )
})

test_that("feasible.q adjusts infeasible quotas and warns; leaves feasible as-is", {
  votes <- cbind(
    RC1 = c(1, NA),  # sumw = 2
    RC2 = c(1,  9)   # sumw = 3 (9 counts as attending)
  )
  rownames(votes) <- c("A", "B")
  vw <- c(2, 1)

  vwmat <- create.vwmat(absolute = TRUE, vw = vw, votes = votes)

  # Make RC1 infeasible (3 > 2) and RC2 feasible (1 <= 3)
  q_in <- c(RC1 = 3, RC2 = 1)

  expect_warning(
    q_out <- feasible.q(absolute = TRUE, vwmat = vwmat, qvec = q_in, votes = votes),
    "reset to the sum of weighted votes among those attending"
  )

  # RC1 becomes sumw = 2; RC2 remains 1 (feasible left as-is)
  expect_equal(q_out, c(RC1 = 2, RC2 = 1))
})


test_that("feasible.q with all feasible quotas emits no warning and returns input qvec", {
  vwmat <- matrix(c(2, 2,
                    1, 1), nrow = 2, byrow = TRUE,
                  dimnames = list(c("A","B"), c("RC1","RC2")))
  votes <- matrix(c(1, 1,
                    1, 1), nrow = 2, byrow = TRUE,
                  dimnames = list(c("A","B"), c("RC1","RC2")))
  q_in  <- c(2, 2)  # equal to attending weight sums

  expect_no_warning(
    q_out <- feasible.q(absolute = TRUE, vwmat = vwmat, qvec = q_in, votes = votes)
  )
  expect_equal(q_out, q_in)
})
