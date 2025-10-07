test_that("sov_user in 1D returns B as pivot for both directions", {
  ideals <- ideals_1d()
  out <- sov_user(
    ideals        = ideals,
    av            = c(1,1,1),            # everyone included
    absolute      = TRUE,
    q             = 2,
    nPoints1      = 2,                    # find_normals_nd(D=1) effectively gives +1 and -1
    nPoints2      = 1,
    print_results = FALSE
  )
  # Two directions -> two rows in pivot_by_angle
  expect_equal(nrow(out$pivot_by_angle), 2L)
  # Both pivots should be B
  piv_cols <- grep("^Pivot", names(out$pivot_by_angle), value = TRUE)
  expect_true(all(out$pivot_by_angle[[piv_cols[1]]] == "B"))
  # Summary sov for B should be 1 (pivot in all valid directions); others 0
  ps <- out$pivot_summary
  expect_equal(ps$sov[ps$name == "B"], 1)
  expect_equal(ps$sov[ps$name != "B"], c(0,0))
})

test_that("sov_user coerces vw to a vector when provided non-vector; results match explicit vector", {
  ideals <- ideals_1d()              # helper: A,B,C in 1D
  av     <- c(1, 1, 1)

  vw_vec <- c(2, 1, 1)
  vw_mat <- matrix(vw_vec, nrow = 1) # non-vector input to trigger as.vector()

  out_mat <- sov_user(
    ideals = ideals, av = av, vw = vw_mat,
    absolute = FALSE, print_results = FALSE
  )
  out_vec <- sov_user(
    ideals = ideals, av = av, vw = vw_vec,
    absolute = FALSE, print_results = FALSE
  )

  expect_equal(out_mat$pivot_summary, out_vec$pivot_summary)
  expect_equal(out_mat$pivot_by_angle, out_vec$pivot_by_angle)
})

test_that("sov_user infers q via absolute.maj(vw) when absolute=TRUE and q is missing", {
  ideals <- ideals_1d()
  av     <- c(1, 1, 1)
  vw_vec <- c(2, 1, 1)
  q_exp  <- absolute.maj(vw_vec)

  out_auto <- sov_user(
    ideals = ideals, av = av, vw = vw_vec,
    absolute = TRUE, q = NULL, print_results = FALSE
  )
  out_explicit <- sov_user(
    ideals = ideals, av = av, vw = vw_vec,
    absolute = TRUE, q = q_exp, print_results = FALSE
  )

  expect_equal(out_auto$pivot_summary, out_explicit$pivot_summary)
  expect_equal(out_auto$pivot_by_angle, out_explicit$pivot_by_angle)
})

test_that("sov_user writes Excel when print_results=TRUE (sheets: sov, pivots_by_angle)", {
  ideals <- ideals_1d()
  av     <- c(1, 1, 1)

  out_dir <- file.path(tempdir(), "sov_user_out")
  if (dir.exists(out_dir)) unlink(out_dir, recursive = TRUE, force = TRUE)

  on.exit(unlink(out_dir, recursive = TRUE, force = TRUE), add = TRUE)

  expect_silent(
    sov_user(
      ideals = ideals, av = av,
      absolute = FALSE, print_results = TRUE, out_dir = out_dir
    )
  )

  wb_path <- file.path(out_dir, "sovs.xlsx")
  expect_true(file.exists(wb_path))

  sheets <- openxlsx::getSheetNames(wb_path)
  expect_setequal(sheets, c("sov", "pivots_by_angle"))
})
