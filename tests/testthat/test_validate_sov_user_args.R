# tests/testthat/test_validate_sov_user_args.R

# A tiny helper to call validate_sov_user_args() with good defaults and let
# individual tests override just the field under test.
call_validate_sov_user <- function(overrides = list()) {
  ideals <- matrix(c(-0.5, 0.5,
                      0.2, 0.1),
                   nrow = 2, byrow = TRUE,
                   dimnames = list(c("A","B"), c("coord1D","coord2D")))
  args <- list(
    ideals        = ideals,
    av            = c(1, 1),
    weight_nom    = FALSE,
    absolute      = TRUE,
    vw            = NULL,        # optional
    q             = NULL,        # optional
    pr            = 0.6,
    nPoints1      = 360,
    nPoints2      = 360,
    dec           = 3,
    out_dir       = tempdir(check = TRUE),
    print_results = FALSE
  )
  # apply overrides
  for (nm in names(overrides)) args[[nm]] <- overrides[[nm]]
  do.call(validate_sov_user_args, args)
}

test_that("'ideals' must be a numeric matrix", {
  expect_error(
    call_validate_sov_user(list(ideals = c(1, 2))),
    regexp = "ideals.*numeric matrix"
  )
  expect_error(
    call_validate_sov_user(list(ideals = matrix(c("a","b"), nrow = 1))),
    regexp = "ideals.*numeric matrix"
  )
})

test_that("`av` must be numeric 1/NA and length-match ideals", {
  # existing checks...
  expect_error(
    call_validate_sov_user(list(av = c("x","y"))),
    regexp = "`av` must be a numeric vector"
  )
  # direct call for av = NULL (do.call drops NULL)
  ideals <- matrix(c(-0.5, 0.5,
                      0.2, 0.1),
                   nrow = 2, byrow = TRUE,
                   dimnames = list(c("A","B"), c("coord1D","coord2D")))
  expect_error(
    validate_sov_user_args(
      ideals        = ideals,
      av            = NULL,
      weight_nom    = FALSE,
      absolute      = TRUE,
      vw            = NULL,
      q             = NULL,
      pr            = 0.6,
      nPoints1      = 360,
      nPoints2      = 360,
      dec           = 3,
      out_dir       = tempdir(check = TRUE),
      print_results = FALSE
    ),
    regexp = "`av` must be a numeric vector"
  )
  # still check bad values and length mismatch via the helper
  expect_error(
    call_validate_sov_user(list(av = c(1, 0))),
    regexp = "All entries of `av` must be either 1"
  )
  expect_error(
    call_validate_sov_user(list(av = c(1, 1, 1))),
    regexp = "Length of attendance vector av must match"
  )
})


test_that("`weight_nom` and `absolute` must be logical scalars", {
  expect_error(
    call_validate_sov_user(list(weight_nom = NA)),
    regexp = "`weight_nom` must be a single logical"
  )
  expect_error(
    call_validate_sov_user(list(weight_nom = c(TRUE, FALSE))),
    regexp = "`weight_nom` must be a single logical"
  )
  expect_error(
    call_validate_sov_user(list(absolute = NA)),
    regexp = "`absolute` must be a single logical"
  )
  expect_error(
    call_validate_sov_user(list(absolute = c(TRUE, FALSE))),
    regexp = "`absolute` must be a single logical"
  )
})

test_that("`vw` must be numeric non-negative integers and length-match (when provided)", {
  # Non-numeric
  expect_error(
    call_validate_sov_user(list(vw = c("a","b"))),
    regexp = "`vw` must be a vector of non-negative integers"
  )
  # Non-integers
  expect_error(
    call_validate_sov_user(list(vw = c(1, 1.5))),
    regexp = "All non-NA entries of `vw` must be positive integers or zero"
  )
  # Negative
  expect_error(
    call_validate_sov_user(list(vw = c(-1, 0))),
    regexp = "All non-NA entries of `vw` must be positive integers or zero"
  )
  # Length mismatch
  expect_error(
    call_validate_sov_user(list(vw = c(1))),   # 1 != nrow(ideals)=2
    regexp = "Length of voting weights `vw` must match"
  )
})

test_that("`q` must be a single positive integer when provided", {
  expect_error(
    call_validate_sov_user(list(q = 0)),
    regexp = "`q` must be a single positive integer"
  )
  expect_error(
    call_validate_sov_user(list(q = 1.2)),
    regexp = "`q` must be a single positive integer"
  )
  expect_error(
    call_validate_sov_user(list(q = NA_real_)),
    regexp = "`q` must be a single positive integer"
  )
})

test_that("`pr` must be 0.5 < pr <= 1", {
  expect_error(
    call_validate_sov_user(list(pr = 0.5)),
    regexp = "`pr` must be a single number within 0.5 < pr <= 1"
  )
  expect_error(
    call_validate_sov_user(list(pr = 1.2)),
    regexp = "`pr` must be a single number within 0.5 < pr <= 1"
  )
  expect_error(
    call_validate_sov_user(list(pr = NA_real_)),
    regexp = "`pr` must be a single number within 0.5 < pr <= 1"
  )
})

test_that("`nPoints1` and `nPoints2` must be non-negative integers", {
  expect_error(
    call_validate_sov_user(list(nPoints1 = -1)),
    regexp = "`nPoints1` must be a single non-negative integer"
  )
  expect_error(
    call_validate_sov_user(list(nPoints1 = 3.3)),
    regexp = "`nPoints1` must be a single non-negative integer"
  )
  expect_error(
    call_validate_sov_user(list(nPoints2 = -5)),
    regexp = "`nPoints2` must be a single non-negative integer"
  )
  expect_error(
    call_validate_sov_user(list(nPoints2 = 7.7)),
    regexp = "`nPoints2` must be a single non-negative integer"
  )
})

test_that("`dec` must be integer in [1..9]", {
  expect_error(
    call_validate_sov_user(list(dec = 0)),
    regexp = "`dec` must be a single integer between 1 and 9"
  )
  expect_error(
    call_validate_sov_user(list(dec = 10)),
    regexp = "`dec` must be a single integer between 1 and 9"
  )
  expect_error(
    call_validate_sov_user(list(dec = 2.5)),
    regexp = "`dec` must be a single integer between 1 and 9"
  )
  expect_error(
    call_validate_sov_user(list(dec = NA_real_)),
    regexp = "`dec` must be a single integer between 1 and 9"
  )
})

test_that("`out_dir` is validated and `print_results` must be logical scalar", {
  # OS-specific invalid path (guaranteed to fail the validator)
  bad_path <- if (.Platform$OS.type == "windows") {
    "C:bad|dir"          # '|' is illegal in Windows file names
  } else {
    "bad\x01dir"         # control char (not NUL) triggers non-Windows control-char check
  }

  # 1) Invalid path should error
  expect_error(
    call_validate_sov_user(list(out_dir = bad_path)),
    regexp = "(invalid path characters|control characters|cannot be empty)"
  )

  # 2) Empty string should error
  expect_error(
    call_validate_sov_user(list(out_dir = "")),
    regexp = "`out_dir` cannot be empty"
  )

  # 3) print_results must be logical scalar
  expect_error(
    call_validate_sov_user(list(print_results = NA)),
    regexp = "`print_results` must be a single logical value"
  )
  expect_error(
    call_validate_sov_user(list(print_results = c(TRUE, FALSE))),
    regexp = "`print_results` must be a single logical value"
  )

  # 4) A good path + proper print_results should pass silently
  good_dir <- tempdir(check = TRUE)
  expect_silent(
    call_validate_sov_user(list(out_dir = good_dir, print_results = FALSE))
  )
})
