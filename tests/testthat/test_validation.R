library ("openxlsx")

# valid input tests
test_that("illegal vote codes error", {
  ideals <- ideals_1d()
  bad_votes <- matrix(c(1, 2, 0), nrow = 3,
                      dimnames = list(c("A","B","C"), "RC1"))
  expect_error(
    vs_sov_user(ideals = ideals,
                midpoints = midpoints_1d_origin(),
                votes = bad_votes, absolute = TRUE, q = 2, vw = vw_3()),
    regexp = "can only contain 1=yea, 0=nay, 9=attend"
  )
})

test_that("normals must be unit length when supplied", {
  ideals <- ideals_1d()
  votes  <- votes_1rc()
  bad_normals <- matrix(c(1, 1), nrow = 1, ncol = 2, byrow = TRUE, dimnames = list("RC1", c("dim1", "dim2")) ) # length sqrt(2)
  expect_error(
    vs_sov_user(ideals = ideals,
                normals = bad_normals,
                votes = votes, absolute = TRUE, q = 2, vw = vw_3()),
    regexp = "not unit length"
  )
})

test_that("vw must be non-negative integers and length-match", {
  ideals <- ideals_1d()

  # Ensure votes have at least one 1 and one 0 (validation precondition)
  votes  <- matrix(c(1, 0, 1), nrow = 3,
                   dimnames = list(c("A","B","C"), "RC1"))

  # Non-integer weight should be rejected
  expect_error(
    vs_sov_user(
      ideals    = ideals,
      midpoints = midpoints_1d_origin(),
      votes     = votes,
      absolute  = TRUE,
      q         = 2,
      vw        = c(1, 0.5, 1)
    ),
    regexp = "non-NA entries of `vw` must be positive integers or zero"
  )
  # Length mismatch should be rejected
  expect_error(
    vs_sov_user(
      ideals    = ideals,
      midpoints = midpoints_1d_origin(),
      votes     = votes,
      absolute  = TRUE,
      q         = 2,
      vw        = c(1, 1)   # too short
    ),
    regexp = "Length of `vw`"
  )
})


test_that("av must be 1 or NA in sov_user", {
  ideals <- ideals_1d()
  expect_error(
    sov_user(ideals = ideals, av = c(1,0,1), absolute = TRUE, q = 2),
    regexp = "either 1 .* or NA"
  )
})


test_that("out_dir validation triggers error on illegal path chars", {
  ideals <- ideals_1d()
  votes  <- matrix(c(1, 0, 1), nrow = 3,
                   dimnames = list(c("A","B","C"), "RC1"))
  expect_error(
    vs_sov_user(
      ideals        = ideals,
      midpoints     = midpoints_1d_origin(),
      votes         = votes,
      absolute      = TRUE,
      q             = 2,
      out_dir       = "bad:dir",   # illegal on Windows
      print_results = TRUE
    ),
    regexp = "contains invalid path characters"
  )
})


# Excel Validation: success-path smoke test
test_that("excel export writes expected sheets (vs_sov_user)", {
  skip_on_cran()
  skip_if_not_installed("openxlsx")
  skip_if_not_installed("withr")

  td <- withr::local_tempdir()  # valid, OS-safe temp directory

  # Minimal valid input (1D), use normals to avoid zero-midpoint NaN
  ideals <- matrix(c(-0.5, 0.2, 0.9), ncol = 1,
                   dimnames = list(c("A","B","C"), "coord1D"))
  normals <- matrix(1, nrow = 1, dimnames = list("RC1","dim1"))
  votes <- matrix(c(1, 0, 1), nrow = 3,
                  dimnames = list(c("A","B","C"), "RC1"))

  vs_sov_user(
    ideals        = ideals,
    normals       = normals,
    votes         = votes,
    absolute      = TRUE,
    q             = 2,
    print_results = TRUE,
    out_dir       = td
  )

  fn <- file.path(td, "vs-sovs.xlsx")
  expect_true(file.exists(fn))
  wb <- openxlsx::loadWorkbook(fn)
  expect_setequal(openxlsx::sheets(wb),
                  c("sov", "name_pivots_by_rollcall", "nv_and_angles"))
})


# Edge - Corner Cases
# ties at the pivot
test_that("ties produce multiple Pivot columns", {
  # A=-0.5, B=0.2, C=0.2 -> tie at the pivot value 0.2
  ideals <- matrix(c(-0.5, 0.2, 0.2), ncol = 1,
                   dimnames = list(c("A","B","C"), "coord1D"))
  normals <- matrix(1, nrow = 1, dimnames = list("RC1","dim1"))

  # Valid votes: must include at least one 1 and one 0
  votes <- matrix(c(1, 1, 0), nrow = 3,
                  dimnames = list(c("A","B","C"), "RC1"))

  out <- vs_sov_user(
    ideals    = ideals,
    normals   = normals,
    votes     = votes,
    absolute  = TRUE,
    q         = 2,          # 2-of-3, the pivot is at the 2nd position -> value 0.2
    vw        = c(1,1,1)
  )

  piv_cols <- grep("^Pivot", names(out$pivot_by_rc), value = TRUE)
  expect_true(length(piv_cols) >= 1)
  expect_true(all(c("B", "C") %in% out$pivot_by_rc[1, piv_cols]))
})


test_that("abstain (9) affects q under simple majority", {
  # Geometry: A=-0.5, B=0.2, C=0.9; normal +x
  ideals  <- matrix(c(-0.5, 0.2, 0.9), ncol = 1,
                    dimnames = list(c("A","B","C"), "coord1D"))
  normals <- matrix(1, nrow = 1, dimnames = list("RC1","dim1"))

  # Case 1: all vote (1/0 present), pr=0.67 -> q = ceil(0.67*3) = 3 -> pivot at 3rd -> "C"
  votes_all <- matrix(c(1, 0, 1), nrow = 3,
                      dimnames = list(c("A","B","C"), "RC1"))
  out_all <- vs_sov_user(
    ideals   = ideals,
    normals  = normals,
    votes    = votes_all,
    absolute = FALSE,
    pr       = 0.67,
    vw       = c(1,1,1)
  )
  expect_equal(out_all$pivot_by_rc$Pivot[1], "C")

  # Case 2: C abstains (9), only A/B count in q -> q = ceil(0.67*2) = 2 -> pivot at 2nd -> "B"
  votes_9 <- matrix(c(1, 0, 9), nrow = 3,
                    dimnames = list(c("A","B","C"), "RC1"))
  out_9 <- vs_sov_user(
    ideals   = ideals,
    normals  = normals,
    votes    = votes_9,
    absolute = FALSE,
    pr       = 0.67,
    vw       = c(1,1,1)
  )
  expect_equal(out_9$pivot_by_rc$Pivot[1], "B")
})


test_that("infeasible q triggers warning and reset", {
  ideals <- ideals_1d()
  votes  <- matrix(c(1, 0, 1), nrow = 3,
                   dimnames = list(c("A","B","C"), "RC1"))
  expect_warning(
    vs_sov_user(
      ideals    = ideals,
      midpoints = midpoints_1d_origin(),
      votes     = votes,
      absolute  = TRUE,
      q         = 5,     # larger than total weight 3
      vw        = vw_3()
    ),
    regexp = "reset|greater than the sum of the weighted votes"
  )
})


# Metamorphic Checks
# Permutation invariance (row order of voters)
test_that("permuting voters does not change pivot identities", {
  ideals <- ideals_1d()
  votes  <- matrix(c(1, 0, 1), nrow = 3,
                   dimnames = list(c("A","B","C"), "RC1"))
  base <- vs_sov_user(
    ideals    = ideals,
    midpoints = midpoints_1d_origin(),
    votes     = votes,
    absolute  = TRUE,
    q         = 2,
    vw        = vw_3()
  )
  perm <- c(3, 1, 2)  # C, A, B
  ideals_p <- ideals[perm, , drop = FALSE]
  votes_p  <- votes[perm, , drop = FALSE]
  vw_p     <- vw_3()[perm]

  permuted <- vs_sov_user(
    ideals    = ideals_p,
    midpoints = midpoints_1d_origin(),
    votes     = votes_p,
    absolute  = TRUE,
    q         = 2,
    vw        = vw_p
  )
  expect_equal(base$pivot_by_rc$Pivot, permuted$pivot_by_rc$Pivot)
})

# Scaling invariance (uniform rescale of ideals & midpoints)
test_that("uniform scaling of ideals/midpoints does not change pivots", {
  ideals <- ideals_1d()
  votes  <- matrix(c(1, 0, 1), nrow = 3,
                   dimnames = list(c("A","B","C"), "RC1"))

  out1 <- vs_sov_user(
    ideals    = ideals,
    midpoints = midpoints_1d_origin(),
    votes     = votes,
    absolute  = TRUE,
    q         = 2,
    vw        = vw_3()
  )

  out2 <- vs_sov_user(
    ideals    = ideals * 5,
    midpoints = midpoints_1d_origin() * 5,
    votes     = votes,
    absolute  = TRUE,
    q         = 2,
    vw        = vw_3()
  )

  expect_equal(out1$pivot_by_rc$Pivot, out2$pivot_by_rc$Pivot)
})

# weight monotonicity
test_that("increasing a voter's weight does not reduce their chance to be a pivot (weakly)", {
  ideals  <- matrix(c(-0.5, 0.2, 0.9), ncol = 1,
                    dimnames = list(c("A","B","C"), "coord1D"))
  normals <- matrix(1, nrow = 1, dimnames = list("RC1","dim1"))
  votes   <- matrix(c(1, 0, 1), nrow = 3,
                    dimnames = list(c("A","B","C"), "RC1"))

  # Absolute majority, q=2
  base <- vs_sov_user(
    ideals    = ideals,
    normals   = normals,
    votes     = votes,
    absolute  = TRUE,
    q         = 2,
    vw        = c(1,1,1)
  )
  expect_equal(base$pivot_by_rc$Pivot[1], "B")

  more_wt <- vs_sov_user(
    ideals    = ideals,
    normals   = normals,
    votes     = votes,
    absolute  = TRUE,
    q         = 2,
    vw        = c(1,2,1)   # boost B's weight
  )
  expect_equal(more_wt$pivot_by_rc$Pivot[1], "B")
})

# tests/testthat/test_validate_out_dir.R

test_that(".validate_out_dir rejects non-character / NA / length > 1", {
  # non-character
  expect_error(.validate_out_dir(123),          regexp = "`out_dir`.*character")
  # NA
  expect_error(.validate_out_dir(NA_character_), regexp = "`out_dir`.*non-NA")
  # length > 1
  expect_error(.validate_out_dir(c("a", "b")),  regexp = "`out_dir`.*single")
})

test_that(".validate_out_dir rejects empty or whitespace-only strings", {
  expect_error(.validate_out_dir(""),     regexp = "cannot be empty")
  expect_error(.validate_out_dir("   "),  regexp = "cannot be empty")
})

test_that(".validate_out_dir enforces Windows-invalid path characters (if on Windows)", {
  skip_if(.Platform$OS.type != "windows")

  # Allow drive letter, then reject illegal chars in the remainder
  expect_error(.validate_out_dir("C:bad:dir"), regexp = "contains invalid path characters")
  expect_error(.validate_out_dir("bad<dir"),   regexp = "contains invalid path characters")

  # A benign example should pass and return invisibly
  ok <- .validate_out_dir("C:some\\ok\\path")
  expect_true(is.character(ok) && length(ok) == 1)
})

test_that(".validate_out_dir rejects control characters on non-Windows", {
  skip_if(.Platform$OS.type == "windows")
  bad <- paste0("ok", intToUtf8(1L), "bad")   # build control char at runtime
  expect_error(.validate_out_dir(bad), regexp = "control characters")
})

test_that(".validate_out_dir returns the path (invisibly) when valid", {
  # Works everywhere; no existence requirement
  p <- .validate_out_dir("some/relative/path")
  expect_identical(p, "some/relative/path")
})

# Helper: create a copy of .validate_out_dir whose environment says we're on "unix"
.mock_validate_out_dir_unix <- function() {
  f <- .validate_out_dir
  env <- new.env(parent = environment(f))
  env$.Platform <- list(OS.type = "unix")  # pretend we're on non-Windows
  environment(f) <- env
  f
}

test_that(".validate_out_dir rejects control characters on non-Windows", {
  f <- .mock_validate_out_dir_unix()
  bad <- paste0("ok", intToUtf8(1L), "bad")  # build a control char at runtime (no NULs in file)
  expect_error(f(bad), regexp = "control characters")
})

test_that(".validate_out_dir accepts normal paths on non-Windows", {
  f <- .mock_validate_out_dir_unix()
  good <- "var/tmp/output"
  expect_invisible(f(good))                 # returns invisibly
  expect_equal(f(good), good)               # value is still the same string
})

