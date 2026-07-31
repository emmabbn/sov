# tests/testthat/test_plot_sov_geometry.R

# -------------------------------------------------------------------------
# Test helper: open a noninteractive, cross-platform graphics device
# -------------------------------------------------------------------------

local_pdf_device <- function() {
  path <- tempfile(fileext = ".pdf")

  grDevices::pdf(
    file   = path,
    width  = 7,
    height = 7
  )

  withr::defer(
    grDevices::dev.off(),
    envir = parent.frame()
  )

  invisible(path)
}


# -------------------------------------------------------------------------
# Shared test data
# -------------------------------------------------------------------------

make_ideals_1d <- function() {
  matrix(
    c(-0.8, 0, 0.9),
    ncol = 1,
    dimnames = list(
      c("A", "B", "C"),
      "coord1D"
    )
  )
}


make_ideals_2d <- function() {
  matrix(
    c(
      -0.6, -0.2,
       0.0,  0.4,
       0.7, -0.1
    ),
    ncol = 2,
    byrow = TRUE,
    dimnames = list(
      c("A", "B", "C"),
      c("coord1D", "coord2D")
    )
  )
}


# =========================================================================
# Validation of ideals
# =========================================================================

test_that("plot_sov_geometry requires ideals", {
  expect_error(
    plot_sov_geometry(),
    regexp = "`ideals` must be supplied"
  )
})


test_that("plot_sov_geometry requires ideals to be a matrix", {
  expect_error(
    plot_sov_geometry(
      ideals = c(-0.8, 0, 0.9)
    ),
    regexp = "`ideals` must be a numeric matrix"
  )
})


test_that("plot_sov_geometry requires ideals to be numeric", {
  bad_ideals <- matrix(
    c("A", "B", "C"),
    ncol = 1
  )

  expect_error(
    plot_sov_geometry(
      ideals = bad_ideals
    ),
    regexp = "`ideals` must be a numeric matrix"
  )
})


test_that("plot_sov_geometry requires one- or two-dimensional ideals", {
  ideals_3d <- matrix(
    c(
      -0.5, 0.0, 0.1,
       0.0, 0.2, 0.3,
       0.5, 0.4, 0.6
    ),
    ncol = 3,
    byrow = TRUE
  )

  expect_error(
    plot_sov_geometry(
      ideals = ideals_3d
    ),
    regexp = "`ideals` must have either one or two columns"
  )
})


test_that("plot_sov_geometry rejects an ideals matrix with zero rows", {
  empty_ideals <- matrix(
    numeric(0),
    nrow = 0,
    ncol = 1
  )

  expect_error(
    plot_sov_geometry(
      ideals = empty_ideals
    ),
    regexp = "`ideals` must contain at least one row"
  )
})


test_that("plot_sov_geometry rejects missing values in ideals", {
  bad_ideals <- make_ideals_1d()
  bad_ideals[2, 1] <- NA_real_

  expect_error(
    plot_sov_geometry(
      ideals = bad_ideals
    ),
    regexp = "`ideals` must contain only finite, non-missing values"
  )
})


test_that("plot_sov_geometry rejects infinite values in ideals", {
  bad_ideals <- make_ideals_2d()
  bad_ideals[1, 1] <- Inf

  expect_error(
    plot_sov_geometry(
      ideals = bad_ideals
    ),
    regexp = "`ideals` must contain only finite, non-missing values"
  )
})


# =========================================================================
# Mutual exclusivity of normals and midpoints
# =========================================================================

test_that("plot_sov_geometry rejects simultaneous normals and midpoints", {
  ideals <- make_ideals_2d()

  normals <- matrix(
    c(1, 0),
    nrow = 1,
    dimnames = list(
      "RC1",
      c("dim1", "dim2")
    )
  )

  midpoints <- matrix(
    c(0.25, 0),
    nrow = 1,
    dimnames = list(
      "RC1",
      c("dim1", "dim2")
    )
  )

  expect_error(
    plot_sov_geometry(
      ideals    = ideals,
      normals   = normals,
      midpoints = midpoints
    ),
    regexp = "Supply either `normals` or `midpoints`, not both"
  )
})


# =========================================================================
# Validation of normals
# =========================================================================

test_that("plot_sov_geometry requires normals to be a matrix", {
  expect_error(
    plot_sov_geometry(
      ideals  = make_ideals_1d(),
      normals = c(-1, 1)
    ),
    regexp = "`normals` must be a numeric matrix"
  )
})


test_that("plot_sov_geometry requires normals to be numeric", {
  bad_normals <- matrix(
    c("left", "right"),
    ncol = 1
  )

  expect_error(
    plot_sov_geometry(
      ideals  = make_ideals_1d(),
      normals = bad_normals
    ),
    regexp = "`normals` must be a numeric matrix"
  )
})


test_that("plot_sov_geometry requires normals dimensions to match ideals", {
  normals_2d <- matrix(
    c(1, 0),
    nrow = 1,
    dimnames = list(
      "RC1",
      c("dim1", "dim2")
    )
  )

  expect_error(
    plot_sov_geometry(
      ideals  = make_ideals_1d(),
      normals = normals_2d
    ),
    regexp = "`normals` must have 1 column to match `ideals`"
  )
})


test_that("plot_sov_geometry rejects normals with zero rows", {
  empty_normals <- matrix(
    numeric(0),
    nrow = 0,
    ncol = 1
  )

  expect_error(
    plot_sov_geometry(
      ideals  = make_ideals_1d(),
      normals = empty_normals
    ),
    regexp = "`normals` must contain at least one row"
  )
})


test_that("plot_sov_geometry rejects missing values in normals", {
  bad_normals <- matrix(
    c(1, NA_real_),
    ncol = 1
  )

  expect_error(
    plot_sov_geometry(
      ideals  = make_ideals_1d(),
      normals = bad_normals
    ),
    regexp = "`normals` must contain only finite, non-missing values"
  )
})


test_that("plot_sov_geometry rejects infinite values in normals", {
  bad_normals <- matrix(
    c(
      1, 0,
      0, Inf
    ),
    ncol = 2,
    byrow = TRUE
  )

  expect_error(
    plot_sov_geometry(
      ideals  = make_ideals_2d(),
      normals = bad_normals
    ),
    regexp = "`normals` must contain only finite, non-missing values"
  )
})


test_that("plot_sov_geometry rejects zero-length normals", {
  zero_normal <- matrix(
    c(0, 0),
    nrow = 1,
    dimnames = list(
      "RC1",
      c("dim1", "dim2")
    )
  )

  expect_error(
    plot_sov_geometry(
      ideals  = make_ideals_2d(),
      normals = zero_normal
    ),
    regexp = "`normals` cannot contain zero-length vectors"
  )
})


# =========================================================================
# Validation of midpoints
# =========================================================================

test_that("plot_sov_geometry requires midpoints to be a matrix", {
  expect_error(
    plot_sov_geometry(
      ideals    = make_ideals_1d(),
      midpoints = c(-0.25, 0.25)
    ),
    regexp = "`midpoints` must be a numeric matrix"
  )
})


test_that("plot_sov_geometry requires midpoints to be numeric", {
  bad_midpoints <- matrix(
    c("left", "right"),
    ncol = 1
  )

  expect_error(
    plot_sov_geometry(
      ideals    = make_ideals_1d(),
      midpoints = bad_midpoints
    ),
    regexp = "`midpoints` must be a numeric matrix"
  )
})


test_that("plot_sov_geometry requires midpoint dimensions to match ideals", {
  midpoint_1d <- matrix(
    0.25,
    nrow = 1,
    dimnames = list(
      "RC1",
      "dim1"
    )
  )

  expect_error(
    plot_sov_geometry(
      ideals    = make_ideals_2d(),
      midpoints = midpoint_1d
    ),
    regexp = "`midpoints` must have 2 columns to match `ideals`"
  )
})


test_that("plot_sov_geometry rejects midpoints with zero rows", {
  empty_midpoints <- matrix(
    numeric(0),
    nrow = 0,
    ncol = 1
  )

  expect_error(
    plot_sov_geometry(
      ideals    = make_ideals_1d(),
      midpoints = empty_midpoints
    ),
    regexp = "`midpoints` must contain at least one row"
  )
})


test_that("plot_sov_geometry rejects missing values in midpoints", {
  bad_midpoints <- matrix(
    c(0.25, NA_real_),
    ncol = 1
  )

  expect_error(
    plot_sov_geometry(
      ideals    = make_ideals_1d(),
      midpoints = bad_midpoints
    ),
    regexp = "`midpoints` must contain only finite, non-missing values"
  )
})


test_that("plot_sov_geometry rejects infinite values in midpoints", {
  bad_midpoints <- matrix(
    c(0.25, Inf),
    nrow = 1
  )

  expect_error(
    plot_sov_geometry(
      ideals    = make_ideals_2d(),
      midpoints = bad_midpoints
    ),
    regexp = "`midpoints` must contain only finite, non-missing values"
  )
})


# =========================================================================
# Basic 1D plotting behavior
# =========================================================================

test_that("plot_sov_geometry draws ideals-only geometry in one dimension", {
  local_pdf_device()

  result <- plot_sov_geometry(
    ideals = make_ideals_1d()
  )

  expect_null(result)
})


test_that("plot_sov_geometry draws one-dimensional normals", {
  local_pdf_device()

  normals <- matrix(
    c(-1, 1),
    ncol = 1,
    dimnames = list(
      c("RC-left", "RC-right"),
      "dim1"
    )
  )

  expect_invisible(
    plot_sov_geometry(
      ideals  = make_ideals_1d(),
      normals = normals
    )
  )
})


test_that("plot_sov_geometry supplies default names for unnamed 1D normals", {
  local_pdf_device()

  normals <- matrix(
    c(-1, 1),
    ncol = 1
  )

  expect_invisible(
    plot_sov_geometry(
      ideals  = make_ideals_1d(),
      normals = normals
    )
  )
})


test_that("plot_sov_geometry draws one-dimensional midpoints", {
  local_pdf_device()

  midpoints <- matrix(
    c(-0.25, 0.30),
    ncol = 1,
    dimnames = list(
      c("RC-left", "RC-right"),
      "dim1"
    )
  )

  expect_invisible(
    plot_sov_geometry(
      ideals    = make_ideals_1d(),
      midpoints = midpoints
    )
  )
})


test_that("plot_sov_geometry supplies default names for unnamed 1D midpoints", {
  local_pdf_device()

  midpoints <- matrix(
    c(-0.25, 0.30),
    ncol = 1
  )

  expect_invisible(
    plot_sov_geometry(
      ideals    = make_ideals_1d(),
      midpoints = midpoints
    )
  )
})


test_that("plot_sov_geometry constrains displayed 1D midpoints to plot limits", {
  local_pdf_device()

  midpoints <- matrix(
    c(-2, 2),
    ncol = 1,
    dimnames = list(
      c("RC-left", "RC-right"),
      "dim1"
    )
  )

  expect_invisible(
    plot_sov_geometry(
      ideals    = make_ideals_1d(),
      midpoints = midpoints
    )
  )
})


test_that("plot_sov_geometry draws 1D value labels and title", {
  local_pdf_device()

  label_values <- c(
    A = 0.125,
    B = 0.500,
    C = 0.875
  )

  expect_invisible(
    plot_sov_geometry(
      ideals       = make_ideals_1d(),
      label_values = label_values,
      digits       = 2,
      main         = "One-dimensional SOV geometry"
    )
  )
})


test_that("plot_sov_geometry assigns voter names to unnamed label values", {
  local_pdf_device()

  label_values <- c(0.125, 0.500, 0.875)

  expect_invisible(
    plot_sov_geometry(
      ideals       = make_ideals_1d(),
      label_values = label_values
    )
  )
})


test_that("plot_sov_geometry uses a default 1D axis label when absent", {
  local_pdf_device()

  ideals <- unname(make_ideals_1d())
  dim(ideals) <- c(3, 1)

  expect_invisible(
    plot_sov_geometry(
      ideals = ideals
    )
  )
})


# =========================================================================
# Basic 2D plotting behavior
# =========================================================================

test_that("plot_sov_geometry draws ideals-only geometry in two dimensions", {
  local_pdf_device()

  result <- plot_sov_geometry(
    ideals = make_ideals_2d()
  )

  expect_null(result)
})


test_that("plot_sov_geometry draws two-dimensional normals", {
  local_pdf_device()

  normals <- matrix(
    c(
      1, 0,
      0, 1,
     -1, 1
    ),
    ncol = 2,
    byrow = TRUE,
    dimnames = list(
      c("RC1", "RC2", "RC3"),
      c("dim1", "dim2")
    )
  )

  expect_invisible(
    plot_sov_geometry(
      ideals  = make_ideals_2d(),
      normals = normals
    )
  )
})


test_that("plot_sov_geometry normalizes non-unit normals for plotting", {
  local_pdf_device()

  normals <- matrix(
    c(
      2, 0,
      0, 5
    ),
    ncol = 2,
    byrow = TRUE,
    dimnames = list(
      c("RC1", "RC2"),
      c("dim1", "dim2")
    )
  )

  expect_invisible(
    plot_sov_geometry(
      ideals  = make_ideals_2d(),
      normals = normals
    )
  )
})


test_that("plot_sov_geometry supplies default names for unnamed 2D normals", {
  local_pdf_device()

  normals <- matrix(
    c(
      1, 0,
      0, 1
    ),
    ncol = 2,
    byrow = TRUE
  )

  expect_invisible(
    plot_sov_geometry(
      ideals  = make_ideals_2d(),
      normals = normals
    )
  )
})


test_that("plot_sov_geometry draws 2D midpoint chords inside the unit circle", {
  local_pdf_device()

  midpoints <- matrix(
    c(
      -0.30,  0.00,
       0.25,  0.00,
       0.00, -0.40
    ),
    ncol = 2,
    byrow = TRUE,
    dimnames = list(
      c("RC-left", "RC-right", "RC-center"),
      c("dim1", "dim2")
    )
  )

  expect_invisible(
    plot_sov_geometry(
      ideals    = make_ideals_2d(),
      midpoints = midpoints
    )
  )
})


test_that("plot_sov_geometry draws 2D midpoint lines outside the unit circle", {
  local_pdf_device()

  midpoints <- matrix(
    c(
      1.25, 0,
      0, 1.25
    ),
    ncol = 2,
    byrow = TRUE,
    dimnames = list(
      c("RC-horizontal", "RC-vertical"),
      c("dim1", "dim2")
    )
  )

  expect_invisible(
    plot_sov_geometry(
      ideals    = make_ideals_2d(),
      midpoints = midpoints
    )
  )
})


test_that("plot_sov_geometry supplies default names for unnamed 2D midpoints", {
  local_pdf_device()

  midpoints <- matrix(
    c(
      -0.30, 0.10,
       0.40, 0.15
    ),
    ncol = 2,
    byrow = TRUE
  )

  expect_invisible(
    plot_sov_geometry(
      ideals    = make_ideals_2d(),
      midpoints = midpoints
    )
  )
})


test_that("plot_sov_geometry draws 2D value labels and title", {
  local_pdf_device()

  label_values <- c(
    A = 0.20,
    B = 0.55,
    C = 0.90
  )

  expect_invisible(
    plot_sov_geometry(
      ideals       = make_ideals_2d(),
      label_values = label_values,
      digits       = 2,
      main         = "Two-dimensional SOV geometry"
    )
  )
})


test_that("plot_sov_geometry uses default 2D axis labels when absent", {
  local_pdf_device()

  ideals <- make_ideals_2d()
  colnames(ideals) <- NULL

  expect_invisible(
    plot_sov_geometry(
      ideals = ideals
    )
  )
})


# =========================================================================
# Graphics-state behavior
# =========================================================================

test_that("plot_sov_geometry restores graphical parameters after 1D plotting", {
  local_pdf_device()

  old_mar  <- graphics::par("mar")
  old_xaxs <- graphics::par("xaxs")
  old_yaxs <- graphics::par("yaxs")

  plot_sov_geometry(
    ideals = make_ideals_1d()
  )

  expect_equal(
    graphics::par("mar"),
    old_mar
  )

  expect_identical(
    graphics::par("xaxs"),
    old_xaxs
  )

  expect_identical(
    graphics::par("yaxs"),
    old_yaxs
  )
})


test_that("plot_sov_geometry restores graphical parameters after 2D plotting", {
  local_pdf_device()

  old_mar  <- graphics::par("mar")
  old_xaxs <- graphics::par("xaxs")
  old_yaxs <- graphics::par("yaxs")

  plot_sov_geometry(
    ideals = make_ideals_2d()
  )

  expect_equal(
    graphics::par("mar"),
    old_mar
  )

  expect_identical(
    graphics::par("xaxs"),
    old_xaxs
  )

  expect_identical(
    graphics::par("yaxs"),
    old_yaxs
  )
})


# =========================================================================
# Output-device behavior
# =========================================================================

test_that("plot_sov_geometry produces a nonempty graphics file", {
  output_file <- tempfile(fileext = ".pdf")

  grDevices::pdf(
    output_file,
    width = 7,
    height = 7
  )

  plot_sov_geometry(
    ideals = make_ideals_2d()
  )

  grDevices::dev.off()

  expect_true(file.exists(output_file))
  expect_gt(file.info(output_file)$size, 0)
})
