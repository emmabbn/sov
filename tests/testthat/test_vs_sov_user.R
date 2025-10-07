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

test_that("vs_sov_user uses vw = rep(1, n) when vw is NULL", {
  ideals  <- ideals_1d()                 # helpers-mocks.R
  normals <- normals_1d_right()          # unit +x
  votes   <- matrix(c(1, 0, 1), nrow = 3,
                    dimnames = list(rownames(ideals), "RC1"))

  out_null <- vs_sov_user(
    ideals = ideals, normals = normals, votes = votes,
    absolute = TRUE, q = 2, vw = NULL, print_results = FALSE
  )
  out_exp1 <- vs_sov_user(
    ideals = ideals, normals = normals, votes = votes,
    absolute = TRUE, q = 2, vw = c(1,1,1), print_results = FALSE
  )

  expect_equal(out_null$pivot_by_rc,   out_exp1$pivot_by_rc)
  expect_equal(out_null$pivot_summary, out_exp1$pivot_summary)
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

test_that("vs_sov_user defaults vw <- rep(1, n_voters) when validator returns vw = NULL", {
  # Minimal 1D inputs with at least one 0 and one 1 in votes
  ideals  <- matrix(c(-0.5, 0, 0.6), ncol = 1,
                    dimnames = list(c("A","B","C"), "coord1D"))
  normals <- matrix(1, nrow = 1, ncol = 1, dimnames = list("RC1", "dim1"))
  votes   <- cbind(RC1 = c(1, 0, 1))  # satisfies validator if our stub didn’t attach

  # Patch in the environment that vs_sov_user closes over
  env <- environment(vs_sov_user)

  to_patch <- c(
    "validate_vs_sov_user_args", "create.vwmat", "create.qvec", "feasible.q",
    "project", "pivot", "get_pivot_names", "summarize_pivots", "extract_angles"
  )
  originals <- lapply(setNames(to_patch, to_patch), function(nm) {
    if (exists(nm, envir = env, inherits = FALSE)) get(nm, envir = env) else NULL
  })
  on.exit({
    for (nm in names(originals)) {
      if (!is.null(originals[[nm]])) assign(nm, originals[[nm]], envir = env)
    }
  }, add = TRUE)

  # 1) Force validator to pass through inputs and return vw = NULL
  assign("validate_vs_sov_user_args",
         function(ideals, normals, midpoints, weight_nom, absolute, vw, q, pr, votes, dec, out_dir, print_results) {
           list(ideals = ideals, normals = normals, midpoints = NULL, vw = NULL)
         },
         envir = env)

  n_voters <- nrow(ideals)

  # 2) Intercept create.vwmat to assert defaulting occurred
  assign("create.vwmat",
         function(absolute, vw_arg, votes_arg) {
           testthat::expect_equal(vw_arg, rep(1, n_voters))  # <- proves the red line ran
           matrix(1, nrow = nrow(votes_arg), ncol = ncol(votes_arg))
         },
         envir = env)

  # Lightweight stubs to keep pipeline simple/deterministic
  assign("create.qvec",      function(absolute, q, pr, vwmat, votes) rep(1, ncol(vwmat)), envir = env)
  assign("feasible.q",       function(absolute, vwmat, qvec, votes) qvec,                envir = env)
  assign("project",          function(points, normals) matrix(0, nrow(points), nrow(normals)), envir = env)
  assign("pivot",            function(projv, vwmat, qvec, votes) rep(0, ncol(vwmat)),    envir = env)
  assign("get_pivot_names",  function(projv, pivotv, votes) data.frame(Position = 1, RC_num = "RC1", Pivot = NA_character_), envir = env)
  assign("summarize_pivots", function(ideals, pbrc) data.frame(legislator = rownames(ideals), num_pivots = 0, vs_sov = 0),    envir = env)
  assign("extract_angles",   function(normals) data.frame(Position = 1, RC_num = "RC1"), envir = env)

  # Call: vw = NULL so the code under test must default it
  invisible(vs_sov_user(
    ideals = ideals, normals = normals, votes = votes,
    absolute = TRUE, q = 1, vw = NULL, print_results = FALSE
  ))
})
