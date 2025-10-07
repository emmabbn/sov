# Branch-Coverage for OC, NOM< and MCMC
test_that("vs_sov works on WNOM-like object (1D)", {
  ideals <- matrix(c(-0.8, 0.0, 0.9), ncol = 1,
                   dimnames = list(c("A","B","C"), "coord1D"))

  # WNOM needs midpoints and spreads; 1D axis-aligned so normals are along +x
  midpoints <- matrix(0, nrow = 1, dimnames = list("RC1","dim1"))
  spreads   <- matrix(0.5, nrow = 1, dimnames = list("RC1","dim1"))

  est <- estimates_wnom(ideals, midpoints, spreads, weights = 1)

  votes <- matrix(c(1, 0, 1), nrow = 3,
                  dimnames = list(rownames(ideals), "RC1"))

  out <- vs_sov(
    estimates     = est,
    votes         = votes,
    absolute      = TRUE,
    q             = 2,
    print_results = FALSE
  )

  # With +x normal and cp=0, pivot should be B (the middle projection)
  expect_equal(out$pivot_by_rc$Pivot[1], "B")
})

test_that("vs_sov works on MCMC-like object (1D)", {
  ideals <- matrix(c(-0.8, 0.0, 0.9), ncol = 1,
                   dimnames = list(c("A","B","C"), "coord1D"))
  # Choose alpha/beta to give midpoint 0 in 1D: z = alpha/beta
  alphas <- c(RC1 = 0)
  betas  <- matrix(1, nrow = 1, dimnames = list("RC1", "dim1"))

  est <- estimates_mcmc(ideals, alphas, betas)

  votes <- matrix(c(1, 0, 1), nrow = 3,
                  dimnames = list(rownames(ideals), "RC1"))

  out <- vs_sov(
    estimates     = est,
    votes         = votes,
    absolute      = TRUE,
    q             = 2,
    print_results = FALSE
  )
  expect_equal(out$pivot_by_rc$Pivot[1], "B")
})

# Functional test for 1D, midpoint = 0, via vs_sov()
test_that("vs_sov (MCMC, 1D) handles zero midpoint by defaulting normal to +e1 and finds pivot", {
  # Ideals: ordered along +x so B is the middle voter
  ideals <- matrix(c(-0.8, 0.0, 0.9), ncol = 1,
                   dimnames = list(c("A","B","C"), "coord1D"))

  # MCMC: alpha=0, beta=1 -> midpoint z = alpha/beta = 0 (zero vector in 1D)
  alphas <- c(RC1 = 0)
  betas  <- matrix(1, nrow = 1, dimnames = list("RC1", "dim1"))

  est <- estimates_mcmc(ideals, alphas, betas)

  votes <- matrix(c(1, 0, 1), nrow = 3,
                  dimnames = list(rownames(ideals), "RC1"))

  out <- vs_sov(
    estimates     = est,
    votes         = votes,
    absolute      = TRUE,
    q             = 2,
    print_results = FALSE
  )

  # Expect B is the pivot (middle projection) rather than NA
  expect_equal(out$pivot_by_rc$Pivot[1], "B")
})


# test for isolate.nv('mcmc') zero row behavior at unit level (2D)
test_that("isolate.nv('mcmc') returns +e1 for zero midpoint rows and unit normals otherwise (2D)", {
  m <- rbind(
    z0 = c(0, 0),        # zero midpoint -> expect c(1,0)
    z1 = c(3, 4)         # norm 5 -> expect c(0.6, 0.8)
  )
  colnames(m) <- c("dim1","dim2")

  nv <- isolate.nv("mcmc", estimates = NULL, midpoints_mcmc = m, weights = NULL)

  # numeric equality ignoring names
  expect_equal(unname(nv[1, ]), c(1, 0))
  expect_equal(round(sqrt(sum(nv[2,]^2)), 10), 1)
  expect_equal(round(nv[2,1], 1), 0.6)
  expect_equal(round(nv[2,2], 1), 0.8)

  # (optional) separately assert the matrix keeps dim names
  expect_identical(colnames(nv), c("dim1","dim2"))
  expect_identical(rownames(nv), c("z0","z1"))
})


test_that("vs_sov works with OC-like object (1D)", {
  ideals <- matrix(c(-0.8, 0.0, 0.9), ncol = 1,
                   dimnames = list(c("A","B","C"), "coord1D"))
  normals <- matrix(1, nrow = 1, dimnames = list("RC1","dim1"))
  est <- estimates_oc(ideals, normals, cps = 0)

  votes <- matrix(c(1, 0, 1), nrow = 3,
                  dimnames = list(rownames(ideals), "RC1"))

  out <- vs_sov(
    estimates     = est,
    votes         = votes,
    absolute      = TRUE,
    q             = 2,
    print_results = FALSE
  )
  expect_equal(out$pivot_by_rc$Pivot[1], "B")
})

test_that("vs_sov coerces vw via as.vector() when a non-vector is supplied", {
  # OC-like 1D setup (helpers provide these)
  ideals   <- ideals_1d()
  normals  <- matrix(1, nrow = 1, dimnames = list("RC1", "dim1"))
  cps      <- 0
  estimates <- estimates_oc(ideals, normals, cps)

  votes <- cbind(RC1 = c(1, 1, 0))
  rownames(votes) <- rownames(ideals)

  # Supply vw as a 1-column matrix to force the as.vector() branch
  vw_mat <- matrix(c(1, 1, 1), ncol = 1)

  out_mat <- vs_sov(
    estimates = estimates,
    votes = votes,
    absolute = FALSE,
    vw = vw_mat,
    print_results = FALSE
  )
  out_vec <- vs_sov(
    estimates = estimates,
    votes = votes,
    absolute = FALSE,
    vw = c(1, 1, 1),
    print_results = FALSE
  )

  # Same results whether vw came in as matrix or vector
  expect_equal(out_mat$pivot_by_rc$Pivot, out_vec$pivot_by_rc$Pivot)
  expect_equal(out_mat$pivot_summary,     out_vec$pivot_summary)
})

test_that("vs_sov sets default q via absolute.maj(vw) when absolute=TRUE and q is missing", {
  ideals   <- ideals_1d()
  normals  <- matrix(1, nrow = 1, dimnames = list("RC1", "dim1"))
  cps      <- 0
  estimates <- estimates_oc(ideals, normals, cps)

  votes <- cbind(RC1 = c(1, 1, 0))  # both 1 and 0 present
  rownames(votes) <- rownames(ideals)

  # vw sums to 3 -> absolute.maj = 2; the 2nd projection (B) should be the pivot
  out_abs <- vs_sov(
    estimates = estimates,
    votes = votes,
    absolute = TRUE,
    q = NULL,               # <- exercise the branch q <- absolute.maj(vw)
    vw = c(1, 1, 1),
    print_results = FALSE
  )

  expect_equal(nrow(out_abs$pivot_by_rc), 1L)
  expect_equal(out_abs$pivot_by_rc$Pivot[1], "B")
})
