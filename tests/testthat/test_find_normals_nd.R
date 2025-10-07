# tests/testthat/test_find_normals_nd.R

test_that("find_normals_nd validates D", {
  expect_error(find_normals_nd("2"), "single integer")
  expect_error(find_normals_nd(c(2, 3)), "single integer")
  expect_error(find_normals_nd(0), "at least 1")
  expect_error(find_normals_nd(5), "no more than 4")
})

test_that("find_normals_nd(D=1) returns two unit directions", {
  N1 <- find_normals_nd(1)
  expect_equal(dim(N1), c(2L, 1L))
  expect_equal(colnames(N1), "dim1")
  # Values must be +/-1 and unit length
  expect_true(all(abs(N1[, 1]) == 1))
  expect_true(all(abs(rowSums(N1^2) - 1) < 1e-12))
})

test_that("find_normals_nd(D=2) returns unit circle samples", {
  N2 <- find_normals_nd(2, nPoints1 = 4)  # 0, 90, 180, 270 degrees
  expect_equal(dim(N2), c(4L, 2L))
  expect_equal(colnames(N2), c("dim1", "dim2"))
  # Should match the four cardinal directions (order should be 0,90,180,270)
  expected <- rbind(
    c( 1,  0),
    c( 0,  1),
    c(-1,  0),
    c( 0, -1)
  )
  expect_equal(unname(N2), expected, tolerance = 1e-12)
  # Unit length
  expect_true(all(abs(rowSums(N2^2) - 1) < 1e-12))
})

test_that("find_normals_nd(D=3) returns spherical grid; includes poles/equator; unit length", {
  N3 <- find_normals_nd(3, nPoints1 = 4, nPoints2 = 4)  # theta: 0,pi/2,pi,3pi/2 ; phi: 0,pi/2,pi,3pi/2
  expect_equal(dim(N3), c(16L, 3L))
  expect_equal(colnames(N3), c("dim1", "dim2", "dim3"))
  # Unit length
  expect_true(all(abs(rowSums(N3^2) - 1) < 1e-12))

  # Helper to check if a row approximately exists
  has_row_close <- function(M, v, tol = 1e-12) {
    d <- sqrt(rowSums((M - matrix(v, nrow = nrow(M), ncol = ncol(M), byrow = TRUE))^2))
    any(d < tol)
  }

  # Poles (phi = 0, phi = pi)
  expect_true(has_row_close(N3, c(0, 0,  1)))
  expect_true(has_row_close(N3, c(0, 0, -1)))
  # Equator (phi = pi/2) with theta = 0 gives (1,0,0)
  expect_true(has_row_close(N3, c(1, 0, 0)))
})

test_that("find_normals_nd(D=4) returns hyperspherical grid; includes axis directions; unit length", {
  # Use 4 samples so we include 0, pi/2, pi, 3pi/2
  N4 <- find_normals_nd(4, nPoints1 = 4, nPoints2 = 4)
  expect_equal(dim(N4), c(64L, 4L))
  expect_equal(colnames(N4), c("dim1", "dim2", "dim3", "dim4"))
  # Unit length
  expect_true(all(abs(rowSums(N4^2) - 1) < 1e-12))

  has_row_close <- function(M, v, tol = 1e-12) {
    d <- sqrt(rowSums((M - matrix(v, nrow = nrow(M), ncol = ncol(M), byrow = TRUE))^2))
    any(d < tol)
  }

  # Axis directions should be present with this grid:
  expect_true(has_row_close(N4, c(1, 0, 0, 0)))  # theta1 = 0
  expect_true(has_row_close(N4, c(0, 1, 0, 0)))  # theta1 = pi/2, theta2 = 0
  expect_true(has_row_close(N4, c(0, 0, 1, 0)))  # theta1 = pi/2, theta2 = pi/2, theta3 = 0
  expect_true(has_row_close(N4, c(0, 0, 0, 1)))  # theta1 = pi/2, theta2 = pi/2, theta3 = pi/2
})
