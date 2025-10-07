test_that("identify() stops with helpful message when called without estimates", {
  # Call identify() with *no* argument to trigger missing(estimates)
  # (do.call with an empty arg list is a clean way to do that in tests)
  expect_error(
    do.call(identify, list()),
    regexp = "Specify your estimation technique explicitly \\(oc, wnom, mcmcpack\\)"
  )
})

# (Optional, but nice for coverage of the 'dk' stop path)
test_that("identify() stops when object doesn't match oc/wnom/mcmc signatures", {
  bogus <- list(
    legislators = data.frame(foo = 1),
    rollcalls   = data.frame(bar = 2)
    # no 'statistics' for mcmc, no 'rank/volume/midpoints' for oc,
    # no 'GMP/CC/midpoint*/spread*' for wnom
  )
  expect_error(
    identify(bogus),
    regexp = "SOV could not identify the technique"
  )
})
