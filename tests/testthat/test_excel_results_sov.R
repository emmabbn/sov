# tests/testthat/test_excel_results_sov.R

test_that("excel_results_sov errors when pivot_summary lacks sov", {
  # Minimal valid inputs for other args
  pivot_by_angle <- data.frame(
    Position = 1L,
    Angle_Degrees = 0,
    Pivot = "A",
    check.names = FALSE
  )

  # Deliberately omit 'sov' in pivot_summary
  bad_summary <- data.frame(
    name = "A",
    num_pivots = 1L,
    check.names = FALSE
  )

  out_dir <- file.path(tempdir(), "sov_writer_bad")
  if (dir.exists(out_dir)) unlink(out_dir, recursive = TRUE, force = TRUE)

  expect_error(
    excel_results_sov(
      pivot_summary = bad_summary,
      pivot_by_angle = pivot_by_angle,
      file = "sovs.xlsx",
      dec = 3,
      out_dir = out_dir
    ),
    regexp = "`pivot_summary` must contain a column named 'sov'"
  )
})

test_that("excel_results_sov creates out_dir (if missing) and writes expected sheets", {
  pivot_summary <- data.frame(
    name = c("A", "B"),
    num_pivots = c(1L, 2L),
    sov = c(0.25, 0.75),  # will be rounded internally
    check.names = FALSE
  )

  pivot_by_angle <- data.frame(
    Position = c(1L, 2L),
    Angle_Degrees = c(0, 180),
    Pivot = c("A", "B"),
    check.names = FALSE
  )

  out_dir <- file.path(tempdir(), paste0("sov_writer_", as.integer(runif(1, 1, 1e9))))
  if (dir.exists(out_dir)) unlink(out_dir, recursive = TRUE, force = TRUE)

  excel_results_sov(
    pivot_summary = pivot_summary,
    pivot_by_angle = pivot_by_angle,
    file = "sovs.xlsx",
    dec = 3,
    out_dir = out_dir
  )

  out_path <- file.path(out_dir, "sovs.xlsx")
  expect_true(dir.exists(out_dir))
  expect_true(file.exists(out_path))

  sheets <- openxlsx::getSheetNames(out_path)
  expect_setequal(sheets, c("sov", "pivots_by_angle"))

  # clean up
  unlink(out_dir, recursive = TRUE, force = TRUE)
})
