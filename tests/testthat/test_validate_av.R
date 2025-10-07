# tests/testthat/test_validate_av.R

# Baseline fixtures: 3 voters x 1D
ideals3 <- matrix(c(-0.5, 0.1, 0.9), ncol = 1,
                  dimnames = list(c("A","B","C"), "coord1D"))
vw3 <- c(1, 1, 1)

test_that("defaults: av and/or vw NULL produce vectors of ones of correct length", {
  # av NULL (exercises av <- rep(1, nrow(ideals)))
  out1 <- validate_av(ideals = ideals3, vw = vw3, av = NULL)
  expect_equal(out1$av, rep(1, nrow(ideals3)))

  # both av and vw NULL (also exercises vw default path)
  out2 <- validate_av(ideals = ideals3, vw = NULL, av = NULL)
  expect_equal(out2$av, rep(1, 3))
  expect_equal(out2$vw, rep(1, 3))
})

test_that("`av` must be numeric", {
  av_bad <- c("1", NA, "x")
  expect_error(
    validate_av(ideals = ideals3, vw = vw3, av = av_bad),
    regexp = "`av` must be a numeric vector"
  )
})

test_that("`av` entries must be only 1 or NA", {
  av_bad <- c(1, 0, NA)  # 0 is not allowed
  expect_error(
    validate_av(ideals = ideals3, vw = vw3, av = av_bad),
    regexp = "either 1.*or NA"
  )
})

test_that("`av` must contain at least one 1", {
  av_none <- rep(NA_real_, 3)
  expect_error(
    validate_av(ideals = ideals3, vw = vw3, av = av_none),
    regexp = "must contain at least one 1"
  )
})

test_that("nrow(ideals) must match length(av)", {
  av_len2 <- c(1, NA)  # only 2 entries
  # Make vw length 2 as well to ensure the first mismatch hit is ideals vs av
  vw_len2 <- c(1, 1)
  expect_error(
    validate_av(ideals = ideals3, vw = vw_len2, av = av_len2),
    regexp = "rows in 'ideals'.*match.*length.*'av'"
  )
})

test_that("length(vw) must match length(av)", {
  av_len3 <- c(1, 1, NA)   # length 3 (matches ideals)
  vw_len2 <- c(1, 1)       # mismatch
  expect_error(
    validate_av(ideals = ideals3, vw = vw_len2, av = av_len3),
    regexp = "Length of voting weights `vw`.*match the length of the attendance vector 'av'"
  )
})

test_that("happy path: valid av and vw pass and are returned unchanged", {
  av_ok <- c(1, NA, 1)
  vw_ok <- c(2, 1, 3)
  out <- validate_av(ideals = ideals3, vw = vw_ok, av = av_ok)
  expect_equal(out$av, av_ok)
  expect_equal(out$vw, vw_ok)
})
