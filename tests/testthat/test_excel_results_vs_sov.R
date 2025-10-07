# tests/testthat/test_excel_results_vs_sov.R

test_that("excel_results_vs_sov errors when pivot_summary lacks vs_sov", {
  # Minimal valid inputs for other args
  pivot_by_rc <- data.frame(
    Position = 1L, RC_num = "RC1", Pivot = "A",
    check.names = FALSE
  )
  nv_and_angles <- data.frame(
    Position = 1L, RC_num = "RC1",
    normVector1D = 1, Angle_Radians = 0, Angle_Degrees = 0,
    check.names = FALSE
  )

  # Deliberately omit 'vs_sov' in pivot_summary
  bad_summary <- data.frame(
    name = "A", num_pivots = 1L,
    check.names = FALSE
  )

  out_dir <- file.path(tempdir(), "vs_sov_writer_bad")
  if (dir.exists(out_dir)) unlink(out_dir, recursive = TRUE, force = TRUE)

  expect_error(
    excel_results_vs_sov(
      pivot_summary = bad_summary,
      pivot_by_rc   = pivot_by_rc,
      nv_and_angles = nv_and_angles,
      file          = "vs-sovs.xlsx",
      dec           = 3,
      out_dir       = out_dir
    ),
    regexp = "`pivot_summary` must contain a column named 'vs_sov'"
  )
})

test_that("excel_results_vs_sov creates out_dir (if missing) and writes expected sheets", {
  pivot_summary <- data.frame(
    name = c("A", "B"),
    num_pivots = c(1L, 2L),
    vs_sov = c(1/3, 2/3),  # will be rounded internally
    check.names = FALSE
  )
  pivot_by_rc <- data.frame(
    Position = c(1L, 2L),
    RC_num   = c("RC1", "RC2"),
    Pivot    = c("A", "B"),
    check.names = FALSE
  )
  nv_and_angles <- data.frame(
    Position = c(1L, 2L),
    RC_num   = c("RC1", "RC2"),
    normVector1D = c(1, -1),
    Angle_Radians = c(0, pi),
    Angle_Degrees = c(0, 180),
    check.names = FALSE
  )

  out_dir <- file.path(tempdir(), paste0("vs_sov_writer_", as.integer(runif(1, 1, 1e9))))
  if (dir.exists(out_dir)) unlink(out_dir, recursive = TRUE, force = TRUE)

  excel_results_vs_sov(
    pivot_summary = pivot_summary,
    pivot_by_rc   = pivot_by_rc,
    nv_and_angles = nv_and_angles,
    file          = "vs-sovs.xlsx",
    dec           = 3,
    out_dir       = out_dir
  )

  out_path <- file.path(out_dir, "vs-sovs.xlsx")
  expect_true(dir.exists(out_dir))
  expect_true(file.exists(out_path))

  sheets <- openxlsx::getSheetNames(out_path)
  expect_setequal(
    sheets,
    c("sov", "name_pivots_by_rollcall", "nv_and_angles")
  )

  # (Optional) quick sanity that rounding didn’t crash write; we trust internal rounding.
  # Clean up (optional to keep temp clean)
  unlink(out_dir, recursive = TRUE, force = TRUE)
})
