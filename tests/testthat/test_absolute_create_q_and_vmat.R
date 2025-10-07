# tests/testthat/test_absolute_create_q_and_vmat.R

# ---------- absolute.maj ----------
test_that("absolute.maj returns smallest strict majority of weights", {
  expect_equal(absolute.maj(c(1, 1)), 2)  # sum=2 -> (2+2)/2 = 2
  expect_equal(absolute.maj(c(2, 1)), 2)  # sum=3 -> (3+1)/2 = 2
  expect_equal(absolute.maj(c(3, 3)), 4)  # sum=6 -> (6+2)/2 = 4
})

# ---------- create.vwmat ----------
test_that("create.vwmat errors when vw length mismatches nrow(votes)", {
  votes <- matrix(
    c(
      1, NA,   # RC1: A=1, B=NA
      1,  9    # RC2: A=1, B=9
    ),
    nrow = 2, byrow = TRUE,
    dimnames = list(c("A","B"), c("RC1","RC2"))
  )
  bad_vw <- c(2, 1, 1)
  expect_error(
    create.vwmat(absolute = TRUE, vw = bad_vw, votes = votes),
    regexp = "rows.*vote matrix.*length of vw"
  )
})

test_that("create.vwmat errors when `absolute` is not a logical scalar", {
  vw <- c(2, 1)
  votes <- matrix(
    c(1, NA,
      1,  9),
    nrow = 2, byrow = TRUE,
    dimnames = list(c("A","B"), c("RC1","RC2"))
  )
  expect_error(
    create.vwmat(absolute = NA, vw = vw, votes = votes),
    regexp = "absolute.*logical scalar"
  )
  expect_error(
    create.vwmat(absolute = c(TRUE, FALSE), vw = vw, votes = votes),
    regexp = "absolute.*logical scalar"
  )
})

test_that("create.vwmat drops only absentees (NA) for simple-majority; abstain (9) is kept", {
  vw <- c(2, 1)
  votes <- cbind(
    RC1 = c(1, NA),  # B absent
    RC2 = c(1,  9)   # B abstains (present)
  )
  rownames(votes) <- c("A","B")

  vw_simple <- create.vwmat(absolute = FALSE, vw = vw, votes = votes)
  # RC1: A present (2), B absent (NA)
  # RC2: A present (2), B present (1) even if abstaining (9) — create.vwmat keeps them
  expect_equal(unname(vw_simple), matrix(c(2, NA, 2, 1), nrow = 2))
})

# ---------- create.qvec ----------
test_that("create.qvec replicates scalar q for absolute = TRUE", {
  vw <- c(2, 1)
  votes <- cbind(
    RC1 = c(1, NA),
    RC2 = c(1,  9)
  )
  rownames(votes) <- c("A","B")
  vwmat_abs <- create.vwmat(absolute = TRUE, vw = vw, votes = votes)

  qvec <- create.qvec(absolute = TRUE, q = 2, pr = 0.5001, vwmat = vwmat_abs, votes = votes)
  expect_equal(qvec, c(2, 2))
})

test_that("create.qvec uses attendance (1/0 only) for simple-majority (2 voters)", {
  vw <- c(2, 1)
  votes <- cbind(
    RC1 = c(1, NA),  # attendees with 1/0: A only -> weight 2
    RC2 = c(1,  9)   # attendees with 1/0: A only -> weight 2 (9 excluded)
  )
  rownames(votes) <- c("A","B")
  vwmat_simple <- create.vwmat(absolute = FALSE, vw = vw, votes = votes)

  qvec <- create.qvec(absolute = FALSE, pr = 0.5001, vwmat = vwmat_simple, votes = votes)
  expect_equal(qvec, c(2, 2))
})

test_that("create.qvec uses attendance (1/0 only) for simple-majority (3 voters, robust)", {
  # Weights: A=2, B=1, C=1
  vw <- c(2, 1, 1)

  # RC1: A=1 (yea), B=NA (absent), C=0 (nay)  -> attending (1/0): 2 + 1 = 3
  # RC2: A=1 (yea), B=9 (abstain), C=0 (nay)  -> attending (1/0): 2 + 1 = 3  (9 excluded)
  votes <- cbind(
    RC1 = c(1, NA, 0),
    RC2 = c(1,  9, 0)
  )
  rownames(votes) <- c("A","B","C")

  vwmat_simple <- create.vwmat(absolute = FALSE, vw = vw, votes = votes)
  qvec <- create.qvec(absolute = FALSE, pr = 0.5001, vwmat = vwmat_simple, votes = votes)

  expect_identical(as.integer(qvec), as.integer(c(2, 2)))  # ceil(.5001*3)=2 both cols
})


test_that("create.qvec errors when `absolute` is not a logical scalar", {
  vwmat <- matrix(c(2, 1, 2, 1), nrow = 2,
                  dimnames = list(c("A","B"), c("RC1","RC2")))
  votes <- matrix(c(1, 1, 1, 1), nrow = 2,
                  dimnames = list(c("A","B"), c("RC1","RC2")))

  expect_error(
    create.qvec(absolute = NA, q = 2, pr = 0.5001, vwmat = vwmat, votes = votes),
    regexp = "absolute.*logical scalar"
  )
  expect_error(
    create.qvec(absolute = c(TRUE, FALSE), q = 2, pr = 0.5001, vwmat = vwmat, votes = votes),
    regexp = "absolute.*logical scalar"
  )
})

test_that("create.qvec errors when vwmat and votes are not same-sized matrices", {
  # Mismatched dimensions (2x2 vs 2x3)
  vwmat_bad <- matrix(c(2, 1, 2, 1), nrow = 2,
                      dimnames = list(c("A","B"), c("RC1","RC2")))
  votes_bad <- cbind(
    RC1 = c(1, 1),
    RC2 = c(1, 1),
    RC3 = c(1, 1)
  )
  rownames(votes_bad) <- c("A","B")

  expect_error(
    create.qvec(absolute = TRUE, q = 2, pr = 0.5001, vwmat = vwmat_bad, votes = votes_bad),
    regexp = "vwmat and votes must be matrices of the same dimensions"
  )
})

test_that("create.qvec infers q from vwmat[,1] when absolute=TRUE and q=NULL", {
  # 3 voters, 2 rollcalls; sum(weights)=3+2+1=6 => absolute.maj = (6+2)/2 = 4
  vw <- c(3, 2, 1)
  votes <- matrix(
    c(1, 1,
      1, 1,
      1, 1),
    nrow = 3, byrow = TRUE,
    dimnames = list(c("A","B","C"), c("RC1","RC2"))
  )
  vwmat_abs <- create.vwmat(absolute = TRUE, vw = vw, votes = votes)

  qvec <- create.qvec(absolute = TRUE, q = NULL, pr = 0.5001, vwmat = vwmat_abs, votes = votes)

  expect_equal(qvec, c(4, 4))
})
