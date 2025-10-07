# tests/testthat/test_irt_cutpoints.R

# tests/testthat/test_irt_cutpoints.R
test_that("irt_cutpoints errors on non-mcmc type", {
  # Accept either irc_/irt_, and either straight or backtick quotes around mcmc
  expect_error(
    irt_cutpoints(type = "oc", estimates = list()),
    regexp = "(?:irc|irt)_cutpoints\\(\\).*type.*[`'\"]?mcmc[`'\"]?\\.?$"
  )
})


test_that("irt_cutpoints errors when |alpha| != nrow(beta)", {
  # Build a minimal MCMC-like 'statistics' table with a mismatch:
  #   - 2 alphas (RC1, RC2)
  #   - 3 betas (RC1, RC2, RC3) in 2D -> 6 beta rows
  stats <- do.call(rbind, list(
    # alphas
    data.frame(Mean = -0.2, row.names = "alpha.RC1"),
    data.frame(Mean =  0.3, row.names = "alpha.RC2"),
    # betas (2D): RC1
    data.frame(Mean = 1, row.names = "beta.RC1.1"),
    data.frame(Mean = 0, row.names = "beta.RC1.2"),
    # RC2
    data.frame(Mean = 0, row.names = "beta.RC2.1"),
    data.frame(Mean = 1, row.names = "beta.RC2.2"),
    # RC3 (extra -> creates the mismatch)
    data.frame(Mean = -1, row.names = "beta.RC3.1"),
    data.frame(Mean =  0, row.names = "beta.RC3.2")
  ))
  estimates <- list(statistics = stats)

  expect_error(
    irt_cutpoints(type = "mcmc", estimates = estimates),
    regexp = "Length of alpha must match number of rows of beta"
  )
})
