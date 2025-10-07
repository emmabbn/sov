# tests/testthat/test_validate_sov_args.R

# Helper that calls validate_sov_args with safe defaults, allowing targeted overrides
call_validate_sov <- function(overrides = list()) {
  defaults <- list(
    estimates     = list(dummy = TRUE),  # just needs to be non-NULL
    weight_nom    = FALSE,
    absolute      = FALSE,
    vw            = NULL,
    q             = NULL,
    pr            = 0.5001,
    nPoints1      = 360L,
    nPoints2      = 360L,
    dec           = 3L,
    out_dir       = tempdir(),
    print_results = FALSE
  )
  args <- modifyList(defaults, overrides, keep.null = TRUE)
  do.call(validate_sov_args, args)
}

test_that("estimates must exist", {
  expect_error(
    call_validate_sov(list(estimates = NULL)),
    regexp = "You must supply.*estimates"
  )
})

test_that("`weight_nom` must be a single logical value (TRUE or FALSE)", {
  expect_error(call_validate_sov(list(weight_nom = NA)),          regexp = "weight_nom.*single logical")
  expect_error(call_validate_sov(list(weight_nom = c(TRUE,FALSE))),regexp = "weight_nom.*single logical")
  expect_error(call_validate_sov(list(weight_nom = "yes")),       regexp = "weight_nom.*single logical")
})

test_that("'absolute' must be a single logical value (TRUE or FALSE)", {
  expect_error(call_validate_sov(list(absolute = NA)),            regexp = "absolute.*single logical")
  expect_error(call_validate_sov(list(absolute = c(TRUE,FALSE))), regexp = "absolute.*single logical")
  expect_error(call_validate_sov(list(absolute = 1L)),            regexp = "absolute.*single logical")
})

test_that("vw must be numeric (when provided)", {
  expect_error(
    call_validate_sov(list(vw = c("a","b"))),
    regexp = "vw.*vector of non-negative integers"
  )
})

test_that("vw entries must be integers (or NA) and non-negative", {
  expect_error(
    call_validate_sov(list(vw = c(1, 2.5))),
    regexp = "positive integers or zero"
  )
  expect_error(
    call_validate_sov(list(vw = c(1, -1))),
    regexp = "positive integers or zero"
  )
})

test_that("q must be a single positive integer when provided", {
  expect_error(call_validate_sov(list(q = 1.5)), regexp = "must be a single positive integer")
  expect_error(call_validate_sov(list(q = 0)),   regexp = "must be a single positive integer")
  expect_error(call_validate_sov(list(q = NA)),  regexp = "must be a single positive integer")
  expect_error(call_validate_sov(list(q = c(1,2))), regexp = "must be a single positive integer")
  expect_error(call_validate_sov(list(q = "a")), regexp = "must be a single positive integer")
})

test_that("pr must be a single number within 0.5 < pr <= 1", {
  expect_error(call_validate_sov(list(pr = 0.5)),  regexp = "0.5 < pr <= 1")
  expect_error(call_validate_sov(list(pr = 1.1)),  regexp = "0.5 < pr <= 1")
  expect_error(call_validate_sov(list(pr = NA)),   regexp = "0.5 < pr <= 1")
  expect_error(call_validate_sov(list(pr = c(0.6,0.7))), regexp = "0.5 < pr <= 1")
})

test_that("nPoints1 must be a single non-negative integer", {
  expect_error(call_validate_sov(list(nPoints1 = -1)),    regexp = "nPoints1.*non-negative integer")
  expect_error(call_validate_sov(list(nPoints1 = 3.5)),   regexp = "nPoints1.*non-negative integer")
  expect_error(call_validate_sov(list(nPoints1 = c(1,2))),regexp = "nPoints1.*non-negative integer")
  expect_error(call_validate_sov(list(nPoints1 = NA)),    regexp = "nPoints1.*non-negative integer")
})

test_that("nPoints2 must be a single non-negative integer", {
  expect_error(call_validate_sov(list(nPoints2 = -1)),    regexp = "nPoints2.*non-negative integer")
  expect_error(call_validate_sov(list(nPoints2 = 3.5)),   regexp = "nPoints2.*non-negative integer")
  expect_error(call_validate_sov(list(nPoints2 = c(1,2))),regexp = "nPoints2.*non-negative integer")
  expect_error(call_validate_sov(list(nPoints2 = NA)),    regexp = "nPoints2.*non-negative integer")
})

test_that("dec must be a single integer between 1 and 9", {
  expect_error(call_validate_sov(list(dec = 0)),    regexp = "between 1 and 9")
  expect_error(call_validate_sov(list(dec = 10)),   regexp = "between 1 and 9")
  expect_error(call_validate_sov(list(dec = 2.5)),  regexp = "between 1 and 9")
  expect_error(call_validate_sov(list(dec = NA)),   regexp = "between 1 and 9")
})

test_that("'print_results' must be a single logical value (TRUE or FALSE)", {
  expect_error(call_validate_sov(list(print_results = NA)),            regexp = "print_results.*single logical")
  expect_error(call_validate_sov(list(print_results = c(TRUE,FALSE))), regexp = "print_results.*single logical")
  expect_error(call_validate_sov(list(print_results = "yes")),         regexp = "print_results.*single logical")
})
