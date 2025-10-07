# tests/testthat/test_extract_angles.R

test_that("extract_angles errors on non-matrix input and on unsupported D", {
  # not a matrix
  expect_error(extract_angles(c(1, 0)), regexp = "Input must be a matrix")

  # D > 4 (5 columns)
  too_wide <- matrix(0, nrow = 1, ncol = 5)
  expect_error(extract_angles(too_wide), regexp = "supports only 1 <= D <= 4")
})

test_that("extract_angles (3D): azimuth and inclination match spherical conventions", {
  # Rows are unit vectors; rownames become RC_num
  N3 <- rbind(
    e1 = c( 1, 0,  0),  # az=0°,    inc=90°
    e2 = c( 0, 1,  0),  # az=90°,   inc=90°
    e3 = c( 0, 0,  1),  # az=0°,    inc=0°
    e4 = c( 0, 0, -1),  # az=0°,    inc=180°
    nx = c(-1, 0,  0)   # az=180°,  inc=90°
  )
  out <- extract_angles(N3)

  # Columns exist
  expect_true(all(c("Angle_Radians1","Angle_Radians2","Angle_Degrees1","Angle_Degrees2") %in% names(out)))
  # Azimuth (deg)
  expect_equal(round(out$Angle_Degrees1, 6), c(0, 90, 0, 0, 180))
  # Inclination (deg) = acos(z)
  expect_equal(round(out$Angle_Degrees2, 6), c(90, 90, 0, 180, 90))
  # Norm vector columns present and correct
  expect_equal(unname(out$normVector1D), unname(N3[,1]))
  expect_equal(unname(out$normVector2D), unname(N3[,2]))
  expect_equal(unname(out$normVector3D), unname(N3[,3]))
})

test_that("extract_angles (4D): two azimuths + inclination computed correctly", {
  N4 <- rbind(
    e1 = c( 1, 0, 0, 0),   # az1=0,    az2=0,    inc=90
    e2 = c( 0, 1, 0, 0),   # az1=90,   az2=0,    inc=90
    e3 = c( 0, 0, 1, 0),   # az1=0,    az2=90,   inc=90
    e4 = c( 0, 0, 0, 1),   # az1=0,    az2=0,    inc=0
    nx = c(-1, 0, 0, 0)    # az1=180,  az2=0,    inc=90
  )
  out <- extract_angles(N4)

  # Columns exist
  need <- c("Angle_Radians1","Angle_Radians2","Angle_Radians3",
            "Angle_Degrees1","Angle_Degrees2","Angle_Degrees3")
  expect_true(all(need %in% names(out)))

  # Degrees (rounded for stability)
  expect_equal(round(out$Angle_Degrees1, 6), c(0, 90, 0, 0, 180))  # azimuth1
  expect_equal(round(out$Angle_Degrees2, 6), c(0, 0, 90, 0, 0))    # azimuth2
  expect_equal(round(out$Angle_Degrees3, 6), c(90, 90, 90, 0, 90)) # inclination

  # Norm vector cols
  expect_equal(unname(out$normVector1D), unname(N4[,1]))
  expect_equal(unname(out$normVector2D), unname(N4[,2]))
  expect_equal(unname(out$normVector3D), unname(N4[,3]))
  expect_equal(unname(out$normVector4D), unname(N4[,4]))
})

