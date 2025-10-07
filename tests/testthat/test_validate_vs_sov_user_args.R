# tests/testthat/test_validate_vs_sov_user_args.R

# Helper that builds a valid baseline, then lets tests override specific args.
# Default path uses NORMALS (not midpoints).
call_validate_user <- function(overrides = list(), use_midpoints = FALSE) {
  I <- 3; D <- 2; J <- 2
  ideals <- matrix(c(
    -0.7, -0.6,
     0.0,  0.2,
     0.8, -0.1
  ), nrow = I, byrow = TRUE,
  dimnames = list(c("A","B","C"), c("coord1D","coord2D")))

  votes <- matrix(c(
    1, 0,
    1, 1,
    0, 1
  ), nrow = I, byrow = TRUE,
  dimnames = list(rownames(ideals), c("RC1","RC2")))

  normals <- matrix(c(
    1, 0,   # RC1 -> +x
    0, 1    # RC2 -> +y
  ), nrow = J, byrow = TRUE,
  dimnames = list(c("RC1","RC2"), c("dim1","dim2")))

  midpoints <- matrix(c(
    0, 0,
    0, 0
  ), nrow = J, byrow = TRUE,
  dimnames = list(rownames(normals), c("dim1","dim2")))

  base <- list(
    ideals        = ideals,
    normals       = if (!use_midpoints) normals else NULL,
    midpoints     = if (use_midpoints)  midpoints else NULL,
    weight_nom    = FALSE,
    absolute      = TRUE,
    vw            = c(1, 1, 1),
    q             = 2,
    pr            = 0.5001,
    votes         = votes,
    dec           = 3L,
    out_dir       = tempdir(),
    print_results = FALSE
  )
  args <- modifyList(base, overrides, keep.null = TRUE)
  do.call(validate_vs_sov_user_args, args)
}

# ---- Exactly one of normals/midpoints ----
test_that("exactly one of normals or midpoints must be provided", {
  # both NULL
  expect_error(
    call_validate_user(list(normals = NULL, midpoints = NULL)),
    regexp = "include 'normals' or 'midpoints'"
  )
  # both non-NULL
  I <- 3; D <- 2; J <- 2
  mids <- matrix(0, nrow = J, ncol = D)
  expect_error(
    call_validate_user(list(midpoints = mids), use_midpoints = FALSE),  # normals already present by default
    regexp = "include 'normals' or 'midpoints'"
  )
})

# ---- ideals must be numeric matrix ----
test_that("'ideals' must be a numeric matrix", {
  bad <- matrix(c("a","b","c","d","e","f"), ncol = 2)
  expect_error(
    call_validate_user(list(ideals = bad)),
    regexp = "ideals.*numeric matrix"
  )
})

# ---- normals branch: normals must be numeric matrix ----
test_that("'normals' must be a numeric matrix when provided", {
  bad_normals <- matrix(c("x","y","z","w"), ncol = 2)
  expect_error(
    call_validate_user(list(normals = bad_normals)),
    regexp = "normals.*numeric matrix"
  )
})

# ---- normals entries in [-1, 1] ----
test_that("normals entries must lie in [-1, 1]", {
  bad_normals <- matrix(c(1.5, 0, 0, 1), nrow = 2, byrow = TRUE)
  expect_error(
    call_validate_user(list(normals = bad_normals)),
    regexp = "entries of 'normals' must lie between -1 and 1"
  )
})

# ---- normals unit length; long list triggers truncation message ----
test_that("normals rows must be unit length; error lists up to 10 rows then '...and N more'", {
  I <- 3; D <- 2; J <- 12

  ideals <- matrix(c(
    -0.7, -0.6,
     0.0,  0.2,
     0.8, -0.1
  ), nrow = I, byrow = TRUE,
  dimnames = list(c("A","B","C"), c("coord1D","coord2D")))

  # All entries within [-1, 1], but each row has length 0.8 (not unit)
  normals <- matrix(rep(c(0.8, 0.0), J), nrow = J, byrow = TRUE,
                    dimnames = list(paste0("RC", seq_len(J)), c("dim1","dim2")))

  # Votes with J roll calls; ensure at least one 0 and one 1 overall
  votes <- matrix(1, nrow = I, ncol = J,
                  dimnames = list(rownames(ideals), paste0("RC", seq_len(J))))
  votes[1, 1] <- 1
  votes[2, 1] <- 0

  # 1) Basic expectation: hits the "not unit length" branch (not the [-1,1] range branch)
  expect_error(
    call_validate_user(list(ideals = ideals, normals = normals, votes = votes)),
    regexp = "not unit length"
  )

  # 2) Also assert the truncation hint “…and 2 more rows” appears (12 rows -> show 10 + “…and 2 more rows”)
  msg <- tryCatch(
    call_validate_user(list(ideals = ideals, normals = normals, votes = votes)),
    error = function(e) e$message
  )
  expect_true(grepl("\\.\\.\\.and 2 more rows", msg))
})


# ---- midpoints must be numeric matrix ----
test_that("'midpoints' must be a numeric matrix when normals are NULL", {
  mids <- matrix(c("a","b","c","d"), ncol = 2)
  expect_error(
    call_validate_user(list(midpoints = mids), use_midpoints = TRUE),
    regexp = "midpoints.*numeric matrix"
  )
})

# ---- nrow(ideals) must equal nrow(votes) ----
test_that("number of legislators in ideals must match rows in votes", {
  # 3 legislators, 1D
  ideals <- matrix(c(-0.5, 0.1, 0.4), nrow = 3,
                   dimnames = list(c("A","B","C"), "coord1D"))
  # normals: 1 rollcall, unit length
  normals <- matrix(1, nrow = 1, ncol = 1,
                    dimnames = list("RC1", "dim1"))
  # votes: 1 row x 1 rc (mismatched rows vs ideals’ 3)
  votes <- matrix(1, nrow = 1, ncol = 1,
                  dimnames = list("A", "RC1"))
  expect_error(
    validate_vs_sov_user_args(
      ideals        = ideals,
      normals       = normals,
      midpoints     = NULL,
      weight_nom    = FALSE,
      absolute      = TRUE,
      vw            = NULL,
      q             = 2,
      pr            = 0.5001,
      votes         = votes,
      dec           = 3,
      out_dir       = "out",
      print_results = FALSE
    ),
    regexp = "Number of legislators in `ideals` .* must equal number of rows in `votes`"
  )
})

# ---- votes columns must match rowcount of normals (normals path) ----
test_that("votes columns must equal rows of normals (normals path)", {
  # make votes with 3 columns but normals have 2 rows
  votes3 <- cbind(RC1 = c(1,1,0), RC2 = c(0,1,1), RC3 = c(1,0,1))
  expect_error(
    call_validate_user(list(votes = votes3)),
    regexp = "columns in `votes`.*rows in `normals`"
  )
})

test_that("ideals columns must equal normals columns (normals path)", {
  # Ideals: 2 voters, 2 dims (inside unit sphere to avoid scaling warning)
  ideals <- matrix(c(0.3, 0.2,
                     0.1, -0.2),
                   nrow = 2, byrow = TRUE,
                   dimnames = list(c("A","B"), c("coord1D","coord2D")))
  # Normals: 1 roll call, **3 dims** (unit-length row)
  normals <- matrix(rep(1/sqrt(3), 3), nrow = 1,
                    dimnames = list("RC1", paste0("dim", 1:3)))
  # Votes: 2 voters x 1 RC (so other checks pass; only the dim-mismatch trips)
  votes <- matrix(c(1, 0), nrow = 2,
                  dimnames = list(c("A","B"), "RC1"))
  expect_error(
    validate_vs_sov_user_args(
      ideals        = ideals,
      normals       = normals,
      midpoints     = NULL,
      weight_nom    = FALSE,
      absolute      = TRUE,
      vw            = NULL,
      q             = 2,
      pr            = 0.5001,
      votes         = votes,
      dec           = 3,
      out_dir       = "out",
      print_results = FALSE
    ),
    regexp = "Number of columns in `ideals` .* columns in `normals`"
  )
})


# ---- votes columns must match rowcount of midpoints (midpoints path) ----
test_that("votes columns must equal rows of midpoints (midpoints path)", {
  mids <- matrix(0, nrow = 2, ncol = 2)
  votes3 <- cbind(RC1 = c(1,1,0), RC2 = c(0,1,1), RC3 = c(1,0,1))
  expect_error(
    call_validate_user(list(midpoints = mids, normals = NULL, votes = votes3), use_midpoints = TRUE),
    regexp = "columns in `votes`.*rows in `midpoints`"
  )
})


# ---- ideals columns must match midpoints columns (midpoints path) ----
test_that("ideals columns must equal midpoints columns (midpoints path)", {
  # Ideals: 2 voters, 2 dims (inside unit sphere)
  ideals <- matrix(c(0.3, 0.2,
                     0.1, -0.2),
                   nrow = 2, byrow = TRUE,
                   dimnames = list(c("A","B"), c("coord1D","coord2D")))
  # Midpoints: 1 RC, **3 dims** (so rescale() trips on column mismatch)
  mids <- matrix(0, nrow = 1, ncol = 3,
                 dimnames = list("RC1", paste0("dim", 1:3)))
  # Votes are irrelevant here (error happens inside rescale first), but pass a valid shape
  votes <- matrix(c(1, 0), nrow = 2, dimnames = list(c("A","B"), "RC1"))
  expect_error(
    validate_vs_sov_user_args(
      ideals        = ideals,
      normals       = NULL,
      midpoints     = mids,
      weight_nom    = FALSE,
      absolute      = TRUE,
      vw            = NULL,
      q             = 2,
      pr            = 0.5001,
      votes         = votes,
      dec           = 3,
      out_dir       = "out",
      print_results = FALSE
    ),
    # match “…columns of ideals … and midpoints_mcmc…”
    regexp = "columns of ideals .* and (midpoints|midpoints_mcmc)"
  )
})


# ---- weight_nom must be logical scalar ----
test_that("`weight_nom` must be a single logical value", {
  expect_error(call_validate_user(list(weight_nom = NA)),            regexp = "weight_nom.*single logical")
  expect_error(call_validate_user(list(weight_nom = c(TRUE,FALSE))), regexp = "weight_nom.*single logical")
  expect_error(call_validate_user(list(weight_nom = "yes")),         regexp = "weight_nom.*single logical")
})

# ---- absolute must be logical scalar ----
test_that("`absolute` must be a single logical value", {
  expect_error(call_validate_user(list(absolute = NA)),              regexp = "absolute.*single logical")
  expect_error(call_validate_user(list(absolute = c(TRUE,FALSE))),   regexp = "absolute.*single logical")
  expect_error(call_validate_user(list(absolute = "yes")),           regexp = "absolute.*single logical")
})

# ---- vw validation ----
test_that("vw must be numeric vector (when provided)", {
  expect_error(call_validate_user(list(vw = c("a","b","c"))), regexp = "`vw` must be a vector of non-negative integers")
})

test_that("vw entries must be integers (or NA) and non-negative", {
  expect_error(call_validate_user(list(vw = c(1, 2.5, 1))), regexp = "positive integers or zero")
  expect_error(call_validate_user(list(vw = c(1, -1, 1))),  regexp = "positive integers or zero")
})

test_that("Length of vw must match number of voters in votes (checked later)", {
  expect_error(
    call_validate_user(list(vw = c(1,1,1,1))),  # 4 vs 3 voters
    regexp = "Length of `vw`.*match the number of voters"
  )
})

# ---- q validation ----
test_that("q must be a single positive integer when provided", {
  expect_error(call_validate_user(list(q = 0)),       regexp = "`q` must be a single positive integer")
  expect_error(call_validate_user(list(q = 1.2)),     regexp = "`q` must be a single positive integer")
  expect_error(call_validate_user(list(q = NA)),      regexp = "`q` must be a single positive integer")
  expect_error(call_validate_user(list(q = c(1,2))),  regexp = "`q` must be a single positive integer")
  expect_silent(call_validate_user(list(q = 2L)))     # valid
})

# ---- pr validation ----
test_that("pr must be within 0.5 < pr <= 1", {
  expect_error(call_validate_user(list(pr = 0.5)),    regexp = "0.5 < pr <= 1")
  expect_error(call_validate_user(list(pr = 1.1)),    regexp = "0.5 < pr <= 1")
  expect_error(call_validate_user(list(pr = NA)),     regexp = "0.5 < pr <= 1")
  expect_error(call_validate_user(list(pr = c(0.6,0.7))), regexp = "0.5 < pr <= 1")
  expect_silent(call_validate_user(list(pr = 0.75)))
})

# ---- dec validation ----
test_that("dec must be a single integer between 1 and 9", {
  expect_error(call_validate_user(list(dec = 0)),     regexp = "between 1 and 9")
  expect_error(call_validate_user(list(dec = 10)),    regexp = "between 1 and 9")
  expect_error(call_validate_user(list(dec = 2.5)),   regexp = "between 1 and 9")
  expect_error(call_validate_user(list(dec = NA)),    regexp = "between 1 and 9")
  expect_silent(call_validate_user(list(dec = 3L)))
})

# ---- print_results logical scalar ----
test_that("`print_results` must be a single logical value", {
  expect_error(call_validate_user(list(print_results = NA)),            regexp = "print_results.*single logical")
  expect_error(call_validate_user(list(print_results = c(TRUE,FALSE))), regexp = "print_results.*single logical")
  expect_error(call_validate_user(list(print_results = "yes")),         regexp = "print_results.*single logical")
})

# ---- votes values set {0,1,9,NA} ----
test_that("votes must only contain 0, 1, 9, or NA", {
  bad_votes <- matrix(c(1,2,0,1,1,1), nrow = 3)  # includes a '2'
  expect_error(
    call_validate_user(list(votes = bad_votes)),
    regexp = "can only contain 1=yea, 0=nay, 9=attended but abstain, and NAs"
  )
})

# ---- votes must contain at least one 0 and at least one 1 ----
test_that("votes must contain at least one 0 and at least one 1", {
  all_ones <- matrix(1, nrow = 3, ncol = 2)
  expect_error(
    call_validate_user(list(votes = all_ones)),
    regexp = "must contain at least one 1=yea and one 0=nay"
  )
})

# ---- testing other stop messages in validate_vs_sov_user() ----
test_that("normals entries must lie in [-1, 1]", {
  ideals <- matrix(c(0.3, -0.2,
                     0.1,  0.4),
                   nrow = 2, byrow = TRUE,
                   dimnames = list(c("A","B"), c("coord1D","coord2D")))
  # One entry outside [-1,1] -> should trigger the range check
  normals <- matrix(c(1.2, 0), nrow = 1,
                    dimnames = list("RC1", c("dim1","dim2")))
  votes <- matrix(c(1, 0), nrow = 2,
                  dimnames = list(c("A","B"), "RC1"))
  expect_error(
    validate_vs_sov_user_args(
      ideals        = ideals,
      normals       = normals,
      midpoints     = NULL,
      weight_nom    = FALSE,
      absolute      = TRUE,
      vw            = c(1,1),
      q             = 2,
      pr            = 0.5001,
      votes         = votes,
      dec           = 3,
      out_dir       = "out",
      print_results = FALSE
    ),
    regexp = "lie between -1 and 1"
  )
})

test_that("number of legislators in ideals must match rows in votes (normals path)", {
  ideals <- matrix(c(0.3, -0.2,
                     0.1,  0.4),
                   nrow = 2, byrow = TRUE,
                   dimnames = list(c("A","B"), c("coord1D","coord2D")))
  normals <- matrix(c(1, 0), nrow = 1,
                    dimnames = list("RC1", c("dim1","dim2")))
  # 3 voters in votes (A, B, C) but only 2 in ideals -> mismatch
  votes <- matrix(c(1, 0, 1), nrow = 3,
                  dimnames = list(c("A","B","C"), "RC1"))
  expect_error(
    validate_vs_sov_user_args(
      ideals        = ideals,
      normals       = normals,
      midpoints     = NULL,
      weight_nom    = FALSE,
      absolute      = TRUE,
      vw            = c(1,1,1),
      q             = 2,
      pr            = 0.5001,
      votes         = votes,
      dec           = 3,
      out_dir       = "out",
      print_results = FALSE
    ),
    regexp = "Number of legislators in `ideals` .* must equal number of rows in `votes`"
  )
})

test_that("number of legislators in ideals must match rows in votes (midpoints path)", {
  ideals <- matrix(c(0.3, -0.2,
                     0.1,  0.4),
                   nrow = 2, byrow = TRUE,
                   dimnames = list(c("A","B"), c("coord1D","coord2D")))
  midpoints <- matrix(c(0.0, 0.0), nrow = 1,
                      dimnames = list("RC1", c("dim1","dim2")))
  # 3 voters in votes (A, B, C) but only 2 in ideals -> mismatch
  votes <- matrix(c(1, 0, 1), nrow = 3,
                  dimnames = list(c("A","B","C"), "RC1"))
  expect_error(
    validate_vs_sov_user_args(
      ideals        = ideals,
      normals       = NULL,
      midpoints     = midpoints,
      weight_nom    = FALSE,
      absolute      = TRUE,
      vw            = c(1,1,1),
      q             = 2,
      pr            = 0.5001,
      votes         = votes,
      dec           = 3,
      out_dir       = "out",
      print_results = FALSE
    ),
    regexp = "Number of legislators in `ideals` \\(\\d+\\) must equal number of rows in `votes` \\(\\d+\\)\\."
  )
})
