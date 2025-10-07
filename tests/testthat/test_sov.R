test_that("sov works with OC-like object (1D)", {
  # 1D OC-like object via helper
  ideals <- matrix(c(-0.8, 0.0, 0.9), ncol = 1,
                   dimnames = list(c("A","B","C"), "coord1D"))
  normals <- matrix(1, nrow = 1, dimnames = list("RC1","dim1"))
  est <- estimates_oc(ideals, normals, cps = 0)

  # sov() uses attendance vector (not votes)
  out <- sov(
    estimates   = est,
    av          = c(1,1,1),
    absolute    = TRUE,
    q           = 2,          # absolute majority of 3 voters
    nPoints1    = 8,          # coarse grid is fine for a smoke test
    nPoints2    = 1,
    print_results = FALSE
  )

  expect_true(is.list(out))
  expect_true(all(c("pivot_summary","pivot_by_angle") %in% names(out)))
  expect_true("sov" %in% names(out$pivot_summary))
})

test_that("sov coerces vw via as.vector() when a non-vector is supplied", {
  # Minimal OC-like 1D setup
  ideals    <- ideals_1d()
  normals   <- matrix(1, nrow = 1, dimnames = list("RC1", "dim1"))
  cps       <- 0
  estimates <- estimates_oc(ideals, normals, cps)

  # Everyone included
  av <- rep(1, nrow(ideals))

  # Supply vw as a 1-column matrix to exercise the as.vector() coercion
  vw_mat <- matrix(c(1, 1, 1), ncol = 1)

  out_mat <- sov(
    estimates = estimates,
    av = av,
    absolute = FALSE,
    vw = vw_mat,
    print_results = FALSE,
    nPoints1 = 8, nPoints2 = 8
  )
  out_vec <- sov(
    estimates = estimates,
    av = av,
    absolute = FALSE,
    vw = c(1, 1, 1),
    print_results = FALSE,
    nPoints1 = 8, nPoints2 = 8
  )

  expect_equal(out_mat$pivot_by_angle, out_vec$pivot_by_angle)
  expect_equal(out_mat$pivot_summary,  out_vec$pivot_summary)
})

test_that("sov sets default q via absolute.maj(vw) when absolute=TRUE and q is missing", {
  ideals    <- ideals_1d()
  normals   <- matrix(1, nrow = 1, dimnames = list("RC1", "dim1"))
  cps       <- 0
  estimates <- estimates_oc(ideals, normals, cps)

  av <- rep(1, nrow(ideals))
  vw <- c(1, 1, 1)  # sum = 3 -> absolute.maj = 2
  q_exp <- absolute.maj(vw)

  out_auto <- sov(
    estimates = estimates,
    av = av,
    absolute = TRUE,
    q = NULL,              # <- exercise the branch q <- absolute.maj(vw)
    vw = vw,
    print_results = FALSE,
    nPoints1 = 8, nPoints2 = 8
  )

  out_explicit <- sov(
    estimates = estimates,
    av = av,
    absolute = TRUE,
    q = q_exp,             # explicit quota should match auto quota
    vw = vw,
    print_results = FALSE,
    nPoints1 = 8, nPoints2 = 8
  )

  expect_equal(out_auto$pivot_by_angle, out_explicit$pivot_by_angle)
  expect_equal(out_auto$pivot_summary,  out_explicit$pivot_summary)
})
