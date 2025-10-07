# tests/testthat/test_2d_cases.R

# Even number of ideal points in corners of a trapezoid with clear winner
test_that("2D case: vs_sov_user with three normals picks the expected pivots", {
  # --- 2D ideals (even number of points), named rows for pivot identification ---
  p1 <- c( 0.7,  0.7)
  p2 <- c( 0.5, -0.5)
  p3 <- c(-0.5,  0.5)
  p4 <- c(-0.7, -0.7)
  ideals <- rbind(p1, p2, p3, p4)
  rownames(ideals) <- c("p1","p2","p3","p4")
  colnames(ideals) <- c("coord1D","coord2D")  # name like your OC paths do

  # --- Normals (unit length) ---
  nv1 <- c( 1,  0)   # +x
  nv2 <- c( 0,  1)   # +y
  nv3 <- c(-1,  0)   # -x
  normals <- rbind(nv1, nv2, nv3)
  rownames(normals) <- c("RC1","RC2","RC3")
  colnames(normals) <- c("dim1","dim2")

  # --- Votes (must contain at least one 0 and one 1 somewhere) ---
  # 4 voters x 3 rollcalls
  votes <- cbind(
    RC1 = c(1, 1, 1, 0),   # p4 nays on RC1
    RC2 = c(1, 0, 1, 1),   # p2 nays on RC2
    RC3 = c(0, 1, 1, 1)    # p1 nays on RC3
  )
  rownames(votes) <- rownames(ideals)

  # Absolute simple majority with 4 voters -> q = 3
  out <- vs_sov_user(
    ideals        = ideals,
    normals       = normals,   # using normals (no midpoints path)
    votes         = votes,
    absolute      = TRUE,
    q             = 3,
    print_results = FALSE
  )

  # Compute expected pivots by hand:
  # Projections along:
  #  RC1 (+x):   0.7 (p1), 0.5 (p2), -0.5 (p3), -0.7 (p4) -> sorted: p4, p3, p2, p1 -> 3rd = p2
  #  RC2 (+y):   0.7 (p1), -0.5 (p2), 0.5 (p3), -0.7 (p4) -> sorted: p4, p2, p3, p1 -> 3rd = p3
  #  RC3 (-x):  -0.7 (p1), -0.5 (p2), 0.5 (p3),  0.7 (p4) -> sorted: p1, p2, p3, p4 -> 3rd = p3
  expected <- c("p2","p3","p3")

  expect_equal(nrow(out$pivot_by_rc), 3L)
  expect_true("Pivot" %in% names(out$pivot_by_rc))
  expect_equal(out$pivot_by_rc$Pivot, expected)

  # Also sanity-check the summary counts
  ps <- out$pivot_summary
  expect_equal(ps$num_pivots[ps$name == "p2"], 1)
  expect_equal(ps$num_pivots[ps$name == "p3"], 2)
  expect_equal(sum(ps$num_pivots), 3)
})


test_that("2D case: vs_sov with OC-like estimates returns the same pivots", {
  # --- 2D ideals ---
  p1 <- c( 0.7,  0.7)
  p2 <- c( 0.5, -0.5)
  p3 <- c(-0.5,  0.5)
  p4 <- c(-0.7, -0.7)
  ideals <- rbind(p1, p2, p3, p4)
  rownames(ideals) <- c("p1","p2","p3","p4")
  colnames(ideals) <- c("coord1D","coord2D")

  # --- Normals (OC expects normVector1D, normVector2D columns) ---
  normals <- rbind(
    c( 1,  0),   # RC1
    c( 0,  1),   # RC2
    c(-1,  0)    # RC3
  )
  rownames(normals) <- c("RC1","RC2","RC3")

  # --- OC-like estimates object (minimal fields identify() expects) ---
  estimates <- list(
    legislators = data.frame(
      coord1D = ideals[, 1],
      coord2D = ideals[, 2],
      rank    = seq_len(nrow(ideals)),
      volume  = 1,
      row.names = rownames(ideals)
    ),
    rollcalls = data.frame(
      normVector1D = normals[, 1],
      normVector2D = normals[, 2],
      midpoints    = c(0, 0, 0),          # distance from origin along the normal
      row.names    = rownames(normals)
    )
  )

  # --- Votes (must include at least one 0 and one 1 overall) ---
  votes <- cbind(
    RC1 = c(1, 1, 1, 0),
    RC2 = c(1, 0, 1, 1),
    RC3 = c(0, 1, 1, 1)
  )
  rownames(votes) <- rownames(ideals)

  out <- vs_sov(
    estimates     = estimates,
    votes         = votes,
    absolute      = TRUE,
    q             = 3,
    print_results = FALSE
  )

  # Expected pivots for these three axis-aligned normals
  expect_equal(out$pivot_by_rc$Pivot, c("p2","p3","p3"))

  # --- Check ONLY the normal-vector columns exist and are named correctly ---
  nv_cols <- grep("^normVector\\d+D$", names(out$nv_and_angles), value = TRUE)
  expect_identical(nv_cols, c("normVector1D", "normVector2D"))

  # Optional sanity checks on normals reported
  nv_mat <- as.matrix(out$nv_and_angles[, nv_cols, drop = FALSE])
  # unit length (within tight tolerance)
  row_lens <- sqrt(rowSums(nv_mat^2))
  expect_true(all(abs(row_lens - 1) < 1e-10))
  # angles column should be present for 2D
  expect_true("Angle_Degrees" %in% names(out$nv_and_angles))
})


# Tests 2D midpoints with vs_sov_user
test_that("2D case (midpoints): vs_sov_user returns expected pivots across three directions", {
  ideals <- rbind(
    p1 = c( 0.7,  0.7),
    p2 = c( 0.5, -0.5),
    p3 = c(-0.5,  0.5),
    p4 = c(-0.7, -0.7)
  )
  colnames(ideals) <- c("coord1D","coord2D")

  # Explicit midpoints (no estimate needed here)
  midpoints <- rbind(
    RC1 = c( 0.2, 0.0),  # +x
    RC2 = c( 0.0, 0.2),  # +y
    RC3 = c(-0.2, 0.0)   # -x
  )
  colnames(midpoints) <- c("dim1","dim2")

  votes <- cbind(
    RC1 = c(1, 1, 1, 0),   # p4 nay
    RC2 = c(1, 0, 1, 1),   # p2 nay
    RC3 = c(0, 1, 1, 1)    # p1 nay
  )
  rownames(votes) <- rownames(ideals)

  out <- vs_sov_user(
    ideals        = ideals,
    midpoints     = midpoints,
    votes         = votes,
    absolute      = TRUE,
    q             = 3,
    print_results = FALSE
  )

  expect_equal(nrow(out$pivot_by_rc), 3L)
  expect_true("Pivot" %in% names(out$pivot_by_rc))
  expect_equal(out$pivot_by_rc$Pivot, c("p2","p3","p3"))

  ps <- out$pivot_summary
  expect_equal(ps$num_pivots[ps$name == "p2"], 1)
  expect_equal(ps$num_pivots[ps$name == "p3"], 2)
  expect_equal(sum(ps$num_pivots), 3)
})


# Tests 2D midpoints with an intentional polarity flip in the normal vector.  Checks both pivot and reported angle.
test_that("2D (midpoints): polarity correction flips the normal when yeas are on the 'left' of the cutpoint", {
  # Same ideals
  p1 <- c( 0.7,  0.7)
  p2 <- c( 0.5, -0.5)
  p3 <- c(-0.5,  0.5)
  p4 <- c(-0.7, -0.7)
  ideals <- rbind(p1, p2, p3, p4)
  rownames(ideals) <- c("p1","p2","p3","p4")
  colnames(ideals) <- c("coord1D","coord2D")

  # Single roll call midpoint along +x (so the *initial* normal is +x)
  midpoints <- matrix(c(0.2, 0.0), nrow = 1,
                      dimnames = list("RC1", c("dim1","dim2")))

  # Votes engineered so that both yeas (p3, p4) lie to the LEFT of the cutpoint (x <= 0.2),
  # while both nays (p1, p2) lie to the RIGHT (x > 0.2). This should trigger polarity flip.
  votes <- matrix(c(0, 0, 1, 1), nrow = 4, dimnames = list(c("p1","p2","p3","p4"), "RC1"))

  # With 4 voters and absolute majority q = 2 (to make the pivot position sensitive to ordering),
  # the polarity flip will reverse projection order. The resulting 2nd pivot should be p2.
  out <- vs_sov_user(
    ideals        = ideals,
    midpoints     = midpoints,
    votes         = votes,
    absolute      = TRUE,
    q             = 2,
    print_results = FALSE
  )

  # Check pivot after flip
  expect_equal(out$pivot_by_rc$Pivot[1], "p2")

  # Check that reported angle reflects the flip (+x -> -x => ~180 degrees)
  # extract_angles() in 2D returns Angle_Degrees = atan2(y, x) in [0, 360).
  ang <- out$nv_and_angles$Angle_Degrees[1]
  expect_true(is.finite(ang))
  expect_true(abs(ang - 180) < 1e-8)
})

# verifies that irt_cutpoints() computes the intended midpoints from hand-picked alpha and beta, and runs the full vs_sov() pipeline
test_that("MCMC 2D: irt_cutpoints yields intended midpoints", {
  # --- 2D ideals (same geometry used in earlier tests) ---
  p1 <- c( 0.7,  0.7)
  p2 <- c( 0.5, -0.5)
  p3 <- c(-0.5,  0.5)
  p4 <- c(-0.7, -0.7)
  ideals <- rbind(p1, p2, p3, p4)
  rownames(ideals) <- c("p1","p2","p3","p4")

  # Desired midpoints (z_j): +x (0.2,0), +y (0,0.2), -x (-0.2,0)
  # For D>1: z_j = -(alpha_j / ||beta_j||^2) * beta_j
  # Choose beta unit vectors and solve for alpha_j.
  # RC1: beta=(1,0),  want z=( 0.2, 0) -> alpha=-0.2
  # RC2: beta=(0,1),  want z=( 0.0, 0.2) -> alpha=-0.2
  # RC3: beta=(1,0),  want z=(-0.2, 0) -> alpha= 0.2
  stats <- do.call(rbind, list(
    # thetas (two coords per voter)
    data.frame(Mean = ideals["p1",1], row.names = "theta.p1.1"),
    data.frame(Mean = ideals["p1",2], row.names = "theta.p1.2"),
    data.frame(Mean = ideals["p2",1], row.names = "theta.p2.1"),
    data.frame(Mean = ideals["p2",2], row.names = "theta.p2.2"),
    data.frame(Mean = ideals["p3",1], row.names = "theta.p3.1"),
    data.frame(Mean = ideals["p3",2], row.names = "theta.p3.2"),
    data.frame(Mean = ideals["p4",1], row.names = "theta.p4.1"),
    data.frame(Mean = ideals["p4",2], row.names = "theta.p4.2"),
    # alphas
    data.frame(Mean = -0.2, row.names = "alpha.RC1"),
    data.frame(Mean = -0.2, row.names = "alpha.RC2"),
    data.frame(Mean =  0.2, row.names = "alpha.RC3"),
    # betas (two coords per RC)
    data.frame(Mean = 1, row.names = "beta.RC1.1"),
    data.frame(Mean = 0, row.names = "beta.RC1.2"),
    data.frame(Mean = 0, row.names = "beta.RC2.1"),
    data.frame(Mean = 1, row.names = "beta.RC2.2"),
    data.frame(Mean = 1, row.names = "beta.RC3.1"),
    data.frame(Mean = 0, row.names = "beta.RC3.2")
  ))
  estimates <- list(statistics = stats)

  # Compute midpoints via your helper
  mids <- irt_cutpoints("mcmc", estimates)

  # Expected midpoints
  expect_equal(dim(mids), c(3, 2))
  rownames(mids) <- c("RC1","RC2","RC3")
  colnames(mids) <- c("dim1","dim2")
  expect_equal(unname(mids[1,]), c( 0.2, 0.0), tolerance = 1e-12)
  expect_equal(unname(mids[2,]), c( 0.0, 0.2), tolerance = 1e-12)
  expect_equal(unname(mids[3,]), c(-0.2, 0.0), tolerance = 1e-12)
})

# 2D MCMC: vs_sov end-to-end
test_that("MCMC 2D: vs_sov reproduces expected pivots using those midpoints", {
  ideals <- rbind(
    p1 = c( 0.7,  0.7),
    p2 = c( 0.5, -0.5),
    p3 = c(-0.5,  0.5),
    p4 = c(-0.7, -0.7)
  )
  colnames(ideals) <- c("coord1D","coord2D")

  betas <- rbind(
    RC1 = c( 1, 0),
    RC2 = c( 0, 1),
    RC3 = c(-1, 0)
  )
  alphas <- c(RC1 = -0.2, RC2 = -0.2, RC3 = 0.2)  # signs per your cutpoint formula

  est <- estimates_mcmc(ideals, alphas, betas)

  votes <- cbind(
    RC1 = c(1, 1, 1, 0),
    RC2 = c(1, 0, 1, 1),
    RC3 = c(0, 1, 1, 1)
  )
  rownames(votes) <- rownames(ideals)

  out <- vs_sov(
    estimates     = est,
    votes         = votes,
    absolute      = TRUE,
    q             = 3,
    print_results = FALSE
  )

  expect_equal(out$pivot_by_rc$Pivot, c("p2","p3","p3"))

  # Normal-vector columns exist; angles near {0, 90, 180}
  deg <- out$nv_and_angles$Angle_Degrees
  expect_true(all(is.finite(deg)))
  expect_true(all(round(deg) %in% c(0, 90, 180)))
})
