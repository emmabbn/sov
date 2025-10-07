# tests/testthat/test_projection.R

### ---------- project(): guard errors ----------

test_that("project() errors if 'points' is not a matrix", {
  pts <- c(1, 2)                                  # NOT a matrix
  nrm <- matrix(1, nrow = 1, ncol = 1)            # ok
  expect_error(
    project(pts, nrm),
    regexp = "points.*must be a matrix",
    ignore.case = TRUE
  )
})

test_that("project() errors if 'normals' is not a matrix", {
  pts <- matrix(c(1, 2), nrow = 2, ncol = 1)      # ok
  nrm <- c(1)                                      # NOT a matrix
  expect_error(
    project(pts, nrm),
    regexp = "normals.*must be a matrix",
    ignore.case = TRUE
  )
})

test_that("project() errors if #'columns(points)' != #'columns(normals)'", {
  pts <- matrix(c(1, 0, 0, 1), nrow = 2, byrow = TRUE)  # 2 x 2
  nrm <- matrix(c(1, 1), nrow = 2, ncol = 1)            # 2 x 1  (mismatch)
  expect_error(
    project(pts, nrm),
    regexp = "must match the number of columns",
    ignore.case = TRUE
  )
})


### ---------- project_diag(): guard errors ----------

test_that("project_diag() errors if 'points' is not a matrix", {
  pts <- c(0.2, -0.1)                             # NOT a matrix
  nrm <- matrix(c(1, 0, 0, 1), nrow = 2, byrow = TRUE)  # 2 x 2
  expect_error(
    project_diag(pts, nrm),
    regexp = "points.*must be a matrix",
    ignore.case = TRUE
  )
})

test_that("project_diag() errors if 'normals' is not a matrix", {
  pts <- matrix(c(0.2, -0.1, 0.3, 0.4), nrow = 2, byrow = TRUE)  # 2 x 2
  nrm <- c(1, 0)                                   # NOT a matrix
  expect_error(
    project_diag(pts, nrm),
    regexp = "normals.*must be a matrix",
    ignore.case = TRUE
  )
})

test_that("project_diag() errors if #'columns(points)' != #'columns(normals)'", {
  pts <- matrix(c(0.2, 0.1, -0.3, 0.4), nrow = 2, byrow = TRUE)  # 2 x 2
  nrm <- matrix(c(1, 0, 0), nrow = 3, ncol = 1)                  # 3 x 1  (mismatch)
  expect_error(
    project_diag(pts, nrm),
    regexp = "must match the number of columns",
    ignore.case = TRUE
  )
})
