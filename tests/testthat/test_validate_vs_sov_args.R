# tests/testthat/test_validate_vs_sov_args.R

# Handy "good" baseline so we only flip the argument under test
.good_args_vs_sov <- function() {
  list(
    estimates     = list(dummy = 1),  # only needs to be non-NULL for this validator
    weight_nom    = FALSE,
    absolute      = TRUE,
    vw            = NULL,    # omit by default
    q             = 2,
    pr            = 0.5001,
    dec           = 3,
    out_dir       = tempdir(),
    print_results = FALSE
  )
}

call_validate <- function(overrides = list()) {
  a <- .good_args_vs_sov()
  if (length(overrides)) a[names(overrides)] <- overrides
  validate_vs_sov_args(
    estimates     = a$estimates,
    weight_nom    = a$weight_nom,
    absolute      = a$absolute,
    vw            = a$vw,
    q             = a$q,
    pr            = a$pr,
    dec           = a$dec,
    out_dir       = a$out_dir,
    print_results = a$print_results
  )
}

test_that("validate_vs_sov_args errors when estimates is NULL", {
  expect_error(
    call_validate(list(estimates = NULL)),
    regexp = "You must supply the 'estimates'"
  )
})

# ---- vw checks ----
test_that("vw must be numeric (when provided)", {
  expect_error(
    call_validate(list(vw = c("a", "b"))),
    regexp = "non-negative integers"
  )
})

test_that("vw non-NA entries must be integers", {
  expect_error(
    call_validate(list(vw = c(1, 2.5, NA))),
    regexp = "must be positive integers or zero"
  )
})

test_that("vw non-NA entries must be non-negative", {
  expect_error(
    call_validate(list(vw = c(1, -1, NA))),
    regexp = "must be positive integers or zero"
  )
})

# ---- q checks ----
test_that("q must be a single positive integer", {
  expect_error(call_validate(list(q = 0)),     regexp = "'q' must be a single positive integer")
  expect_error(call_validate(list(q = 1.5)),   regexp = "'q' must be a single positive integer")
  expect_error(call_validate(list(q = NA)),    regexp = "'q' must be a single positive integer")
  expect_error(call_validate(list(q = "two")), regexp = "'q' must be a single positive integer")
})

# ---- pr checks ----
test_that("pr must be a single number with 0.5 < pr <= 1", {
  expect_error(call_validate(list(pr = 0.5)), regexp = "0.5 < pr <= 1")
  expect_error(call_validate(list(pr = 1.1)), regexp = "0.5 < pr <= 1")
  expect_error(call_validate(list(pr = NA)),  regexp = "0.5 < pr <= 1")
  expect_error(call_validate(list(pr = "x")), regexp = "0.5 < pr <= 1")
})

# ---- dec checks ----
test_that("dec must be a single integer between 1 and 9", {
  expect_error(call_validate(list(dec = 0)),    regexp = "between 1 and 9")
  expect_error(call_validate(list(dec = 10)),   regexp = "between 1 and 9")
  expect_error(call_validate(list(dec = 3.5)),  regexp = "between 1 and 9")
  expect_error(call_validate(list(dec = NA)),   regexp = "between 1 and 9")
})

# ---- print_results checks ----
test_that("print_results must be a single logical", {
  expect_error(call_validate(list(print_results = "yes")),        regexp = "single logical")
  expect_error(call_validate(list(print_results = c(TRUE, FALSE))), regexp = "single logical")
  expect_error(call_validate(list(print_results = NA)),           regexp = "single logical")
})

# ---- happy path (no error) ----
test_that("validate_vs_sov_args passes silently on valid inputs", {
  expect_silent(call_validate())
  # Also try with a valid numeric vw vector (including NAs allowed)
  expect_silent(call_validate(list(vw = c(1, 0, NA, 3))))
})

test_that("'weight_nom' must be a single logical value (TRUE or FALSE)", {
  # NA logical
  expect_error(
    call_validate(list(weight_nom = NA)),
    regexp = "weight_nom.*single logical"
  )
  # length > 1
  expect_error(
    call_validate(list(weight_nom = c(TRUE, FALSE))),
    regexp = "weight_nom.*single logical"
  )
  # not logical
  expect_error(
    call_validate(list(weight_nom = "yes")),
    regexp = "weight_nom.*single logical"
  )
})

test_that("'absolute' must be a single logical value (TRUE or FALSE)", {
  # NA logical
  expect_error(
    call_validate(list(absolute = NA)),
    regexp = "absolute.*single logical"
  )
  # length > 1
  expect_error(
    call_validate(list(absolute = c(TRUE, FALSE))),
    regexp = "absolute.*single logical"
  )
  # not logical
  expect_error(
    call_validate(list(absolute = 1L)),
    regexp = "absolute.*single logical"
  )
})
