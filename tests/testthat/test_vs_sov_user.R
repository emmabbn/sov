test_that("vs_sov_user works in 1D with midpoints (polarity handled)", {
  # A=-0.8, B=0, C=0.9; midpoint 0 means cp = 0, normal computed and polarity fixed
  ideals <- matrix(c(-0.8, 0.0, 0.9), ncol = 1,
                   dimnames = list(c("A","B","C"), "coord1D"))
  midpoints <- matrix(0, nrow = 1, dimnames = list("RC1","dim1"))

  votes <- matrix(c(1, 0, 1), nrow = 3,
                  dimnames = list(rownames(ideals), "RC1"))

  out <- vs_sov_user(
    ideals    = ideals,
    midpoints = midpoints,   # triggers the midpoints branch in vs_sov_user()
    votes     = votes,
    absolute  = TRUE,
    q         = 2
  )
  expect_equal(out$pivot_by_rc$Pivot[1], "B")
})

test_that("vs_sov_user sets default q via absolute.maj(vw) when absolute=TRUE and q is missing", {
  ideals  <- ideals_1d()
  normals <- normals_1d_right()
  votes   <- matrix(c(1, 0, 1), nrow = 3,
                    dimnames = list(rownames(ideals), "RC1"))
  vw      <- c(1, 1, 1)
  q_exp   <- absolute.maj(vw)            # = 2

  out_auto <- vs_sov_user(
    ideals = ideals, normals = normals, votes = votes,
    absolute = TRUE, q = NULL, vw = vw, print_results = FALSE
  )
  out_qexp <- vs_sov_user(
    ideals = ideals, normals = normals, votes = votes,
    absolute = TRUE, q = q_exp, vw = vw, print_results = FALSE
  )

  expect_equal(out_auto$pivot_by_rc,   out_qexp$pivot_by_rc)
  expect_equal(out_auto$pivot_summary, out_qexp$pivot_summary)
})

test_that("vs_sov_user defaults vw <- rep(1, n_voters) when user omits vw", {
  # Minimal valid 1D setup
  ideals  <- matrix(c(-0.5, 0.0, 0.6), ncol = 1,
                    dimnames = list(c("A","B","C"), "coord1D"))
  normals <- matrix(1, nrow = 1, ncol = 1, dimnames = list("RC1", "dim1"))
  # Must include at least one 0 and one 1
  votes   <- cbind(RC1 = c(1, 0, 1))
  rownames(votes) <- rownames(ideals)
  # Run once with vw omitted (should default to rep(1, n_voters))
  out_auto <- vs_sov_user(
    ideals        = ideals,
    normals       = normals,
    votes         = votes,
    absolute      = TRUE,
    q             = 2,
    print_results = FALSE
  )
  # Run again with explicit vw = rep(1, n_voters)
  out_explicit <- vs_sov_user(
    ideals        = ideals,
    normals       = normals,
    votes         = votes,
    absolute      = TRUE,
    q             = 2,
    vw            = rep(1, nrow(ideals)),
    print_results = FALSE
  )
  # If defaulting worked, these should be identical
  expect_identical(out_auto$pivot_summary, out_explicit$pivot_summary)
  expect_identical(out_auto$pivot_by_rc,   out_explicit$pivot_by_rc)
  expect_identical(out_auto$nv_and_angles, out_explicit$nv_and_angles)
})

test_that("vs_sov_user uses provided voting weights (non-uniform vw changes result)", {
  # Minimal 1D geometry
  ideals  <- matrix(c(-0.5, 0.0, 0.6), ncol = 1,
                    dimnames = list(c("A","B","C"), "coord1D"))
  normals <- matrix(1, nrow = 1, ncol = 1, dimnames = list("RC1", "dim1"))

  # IMPORTANT: include at least one 0 and one 1
  votes   <- cbind(RC1 = c(1, 0, 1))
  rownames(votes) <- rownames(ideals)

  # Absolute majority with q = 2
  # With equal weights -> pivot is the middle projection (B)
  out_eq <- vs_sov_user(
    ideals        = ideals,
    normals       = normals,
    votes         = votes,
    absolute      = TRUE,
    q             = 2,
    vw            = rep(1, nrow(ideals)),
    print_results = FALSE
  )
  expect_identical(out_eq$pivot_by_rc$Pivot, "B")

  # Heavier weight on A shifts the pivot to A
  out_wt <- vs_sov_user(
    ideals        = ideals,
    normals       = normals,
    votes         = votes,
    absolute      = TRUE,
    q             = 2,
    vw            = c(2, 1, 1),
    print_results = FALSE
  )
  expect_identical(out_wt$pivot_by_rc$Pivot, "A")
})

test_that("vs_sov_user: when absolute=TRUE and q missing, defaults to absolute.maj(vw)", {
  ideals  <- matrix(c(-0.5, 0.0, 0.6), ncol = 1,
                    dimnames = list(c("i1","i2","i3"), "coord1D"))
  normals <- matrix(1, nrow = 1, ncol = 1, dimnames = list("RC1","dim1"))
  # Must contain at least one 0 and one 1 overall:
  votes   <- cbind(RC1 = c(1, 0, 1))   # <— changed from c(1, 1, 1)
  rownames(votes) <- rownames(ideals)
  vw    <- c(1, 1, 1)
  q_exp <- absolute.maj(vw)  # = 2
  out_auto <- vs_sov_user(
    ideals   = ideals,
    normals  = normals,
    votes    = votes,
    absolute = TRUE,
    q        = NULL,  # let the function decide
    vw       = vw,
    dec      = 3
  )
  out_explicit <- vs_sov_user(
    ideals   = ideals,
    normals  = normals,
    votes    = votes,
    absolute = TRUE,
    q        = q_exp, # explicit
    vw       = vw,
    dec      = 3
  )
  expect_equal(out_auto$pivot_summary, out_explicit$pivot_summary)
  expect_equal(out_auto$pivot_by_rc,   out_explicit$pivot_by_rc)
})
