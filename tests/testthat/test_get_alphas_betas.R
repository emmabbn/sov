test_that("get_alphas_betas('mcmc') works on a minimal 2D MCMC-like object", {
  # Build a tiny MCMC-like object (2 RCs x 2 dims) using the helper
  ideals <- rbind(a = c(0.1, 0.2), b = c(-0.3, 0.4))
  alphas <- c(RC1 = -0.2, RC2 = 0.1)
  betas  <- rbind(RC1 = c(1, 0), RC2 = c(0, 1))
  est    <- estimates_mcmc(ideals, alphas, betas)

  out <- get_alphas_betas("mcmc", est)

  # Shapes and names
  expect_equal(dim(out$alphas), c(2, 1))
  expect_setequal(rownames(out$alphas), c("RC1", "RC2"))
  expect_equal(dim(out$betas), c(2, 2))
  expect_setequal(rownames(out$betas), c("RC1", "RC2"))
  # Columns from pivot_wider are named "beta_1", "beta_2"
  expect_setequal(colnames(out$betas), c("beta_1", "beta_2"))
})

test_that("get_alphas_betas('mcmc') errors if 'Mean' column is missing (alpha check)", {
  ideals <- rbind(a = c(0.1, 0.2), b = c(-0.3, 0.4))
  alphas <- c(RC1 = -0.2)
  betas  <- rbind(RC1 = c(1, 0))
  est    <- estimates_mcmc(ideals, alphas, betas)

  # Remove the 'Mean' column entirely -> triggers the first red stop (alphas)
  stats_no_mean <- est$statistics
  stats_no_mean$Mean <- NULL
  est_bad <- list(statistics = stats_no_mean)
  class(est_bad) <- "summary.mcmc"

  expect_error(
    get_alphas_betas("mcmc", est_bad),
    regexp = "must have a 'Mean' column"
  )
})

test_that("get_alphas_betas() stops for non-mcmc type", {
  expect_error(
    get_alphas_betas("oc", list()),
    regexp = "currently takes only 'mcmc'"
  )
})
