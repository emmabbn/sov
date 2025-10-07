# tests/testthat/helpers-mocks.R

# Resolve project root both under testthat::test_dir() and plain source()
.get_proj_root <- function() {
  # Works when running via testthat
  root <- tryCatch(testthat::test_path("..", ".."), error = function(e) NULL)
  if (!is.null(root)) return(root)

  # Fallback: derive from this file's path when sourced directly
  sc <- sys.calls()
  for (i in rev(seq_along(sc))) {
    call_i <- sc[[i]]
    if (is.call(call_i) && identical(call_i[[1]], as.name("source"))) {
      src <- tryCatch(
        normalizePath(as.character(call_i[[2]]), winslash = "/", mustWork = FALSE),
        error = function(e) NULL
      )
      if (!is.null(src)) {
        return(normalizePath(file.path(dirname(src), "..", ".."), winslash = "/", mustWork = FALSE))
      }
    }
  }

  # Last resort: assume current working directory is project root
  normalizePath(".", winslash = "/", mustWork = FALSE)
}

PROJ_ROOT <- .get_proj_root()

# IMPORTANT: Do NOT re-source under coverage, or you’ll wipe covr’s instrumentation.
# Sys.getenv() returns a string; compare to the literal "true".
#if (Sys.getenv("R_COVR") != "true") {
#  source(file.path(PROJ_ROOT, "SOV_functions.R"), encoding = "UTF-8")
#}


# --- Shared tiny 1D ideals & names ---
ideals_1d <- function() {
  m <- matrix(c(-0.8, 0, 0.9), ncol = 1,
              dimnames = list(c("A","B","C"), "coord1D"))
  m
}

# Votes: 3 voters x 1 rollcall
votes_1rc <- function() {
  v <- matrix(c(1, 1, 1), nrow = 3,
              dimnames = list(c("A","B","C"), "RC1"))
  v
}

# With an abstain (9)
votes_1rc_with_9 <- function() {
  v <- matrix(c(1, 9, 0), nrow = 3,
              dimnames = list(c("A","B","C"), "RC1"))
  v
}

# Equal weights
vw_3 <- function() c(1,1,1)

# A single 1D normal pointing right
normals_1d_right <- function() {
  m <- matrix(1, ncol = 1, dimnames = list("RC1", "dim1"))
  m
}

# Midpoint exactly at the origin (distance = 0 along the normal)
midpoints_1d_origin <- function() {
  m <- matrix(0, ncol = 1, dimnames = list("RC1", "dim1"))
  m
}

# --- Minimal OC-like object (what your isolate.* expect) ---
mock_oc_1d <- function(ideals, nv = 1, cp = 0) {
  list(
    legislators = data.frame(coord1D = as.numeric(ideals[,1]),
                             row.names = rownames(ideals)),
    rollcalls  = data.frame(normVector1D = nv,
                            midpoints    = cp,
                            row.names    = "RC1")
  )
}

# --- Minimal WNOM-like object ---
# For 1D, normal from spreads will be sign(spread).
mock_wnom_1d <- function(ideals, midpoint = 0, spread = 0.5, weights = 1) {
  list(
    legislators = data.frame(coord1D = as.numeric(ideals[,1]),
                             weights = weights,
                             row.names = rownames(ideals)),
    rollcalls  = data.frame(midpoint1D = midpoint,
                            spread1D   = spread,
                            row.names  = "RC1")
  )
}

# --- Minimal MCMCpack-like object ---
# statistics must include rows: theta.<name>.1, alpha.<rc>, beta.<rc>.1 with "Mean" column
mock_mcmc_1d <- function(ideals, alpha = 0, beta = 1) {
  rn <- rownames(ideals)
  stats <- rbind(
    # thetas
    setNames(data.frame(Mean = ideals["A",1], row.names = "theta.A.1"), "Mean"),
    setNames(data.frame(Mean = ideals["B",1], row.names = "theta.B.1"), "Mean"),
    setNames(data.frame(Mean = ideals["C",1], row.names = "theta.C.1"), "Mean"),
    # alpha for RC1
    setNames(data.frame(Mean = alpha, row.names = "alpha.RC1"), "Mean"),
    # beta for RC1 (1D)
    setNames(data.frame(Mean = beta,  row.names = "beta.RC1.1"), "Mean")
  )
  list(statistics = stats)
}

# Utility: compare vectors/matrices with tolerance
expect_equal_num <- function(x, y, tol = 1e-10) {
  testthat::expect_true(all(abs(x - y) <= tol), info = paste0("Max diff = ", max(abs(x-y))))
}

# Build a minimal OC-like object that passes identify() and works with isolate.*.
# ideals:    I x D matrix with rownames (voter names), colnames "coord1D", "coord2D", ...
# normals:   J x D matrix of unit normals with rownames (RC names)
# cps:       length-J numeric vector of projected cutpoint distances (same order as normals rows)
estimates_oc <- function(ideals, normals, cps) {
  D <- ncol(ideals)
  stopifnot(ncol(normals) == D, length(cps) == nrow(normals))

  # Legislators: must include 'rank' and 'volume' and coord*D cols
  leg <- as.data.frame(ideals, stringsAsFactors = FALSE)
  if (is.null(colnames(leg))) colnames(leg) <- paste0("coord", seq_len(D), "D")
  leg$rank   <- seq_len(nrow(leg))
  leg$volume <- 1
  # Order: put non-coord columns first like OC prints (not strictly required)
  leg <- leg[, c("rank", "volume", colnames(leg)[grepl("^coord\\d+D$", colnames(leg))]), drop = FALSE]

  # Rollcalls: must include normVector*D cols and 'midpoints'
  rc <- as.data.frame(normals, stringsAsFactors = FALSE)
  nv_cols <- paste0("normVector", seq_len(D), "D")
  colnames(rc) <- nv_cols
  rc$midpoints <- as.numeric(cps)

  # identify() also often sees PRE etc., but not required; we keep it minimal.

  structure(
    list(legislators = leg, rollcalls = rc),
    class = "OCobject"
  )
}

# Build a minimal WNOM-like object for identify() and isolate.*
# ideals:    I x D matrix, rownames (voter names), colnames "coord1D", "coord2D", ...
# midpoints: J x D matrix, rownames (RC names)
# spreads:   J x D matrix, same dimnames as midpoints
# weights:   length-D vector of dimension weights (first must be 1 for your checks)
estimates_wnom <- function(ideals, midpoints, spreads, weights = rep(1, ncol(ideals))) {
  D <- ncol(ideals)
  stopifnot(
    ncol(midpoints) == D, ncol(spreads) == D,
    nrow(midpoints) == nrow(spreads),
    length(weights) == D,
    weights[1] == 1
  )

  # Legislators: must include GMP and CC plus coord*D
  leg <- as.data.frame(ideals, stringsAsFactors = FALSE)
  if (is.null(colnames(leg))) colnames(leg) <- paste0("coord", seq_len(D), "D")
  leg$GMP <- 0.5  # dummy but satisfies identify()
  leg$CC  <- 0.5
  # Reorder to resemble typical output (coord*, then GMP, CC)
  leg <- leg[, c(colnames(leg)[grepl("^coord\\d+D$", colnames(leg))], "GMP", "CC"), drop = FALSE]

  # Rollcalls: must include 'GMP' (for identify), plus midpoint*D and spread*D
  rc <- data.frame(GMP = rep(0.5, nrow(midpoints)), stringsAsFactors = FALSE)
  for (d in seq_len(D)) {
    rc[[paste0("midpoint", d, "D")]] <- midpoints[, d]
    rc[[paste0("spread",   d, "D")]] <- spreads[, d]
  }
  rownames(rc) <- rownames(midpoints)

  # Top-level 'weights' vector is required by relative_weight()
  out <- list(
    legislators = leg,
    rollcalls   = rc,
    weights     = as.numeric(weights)
  )
  class(out) <- "nomObject"
  out
}

# Build a minimal MCMCpack-like object (summary.mcmc) with 'statistics'
# ideals: I x D matrix (rownames are voter names), columns 1..D (coords)
# alphas: length-J numeric, names are RC names
# betas:  J x D matrix, rownames are RC names, colnames ignored
estimates_mcmc <- function(ideals, alphas, betas) {
  D <- ncol(ideals)
  J <- length(alphas)
  stopifnot(nrow(betas) == J, ncol(betas) == D)
  voter_names <- rownames(ideals)
  if (is.null(voter_names)) voter_names <- paste0("v", seq_len(nrow(ideals)))
  rc_names    <- names(alphas)
  if (is.null(rc_names)) rc_names <- paste0("RC", seq_len(J))
  rownames(betas) <- rc_names
  add_row <- function(key, val) {
    data.frame(
      Mean = as.numeric(val),
      SD = 0,
      `Naive SE` = 0,
      `Time-series SE` = 0,
      row.names = key,
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
  }
  stats <- do.call(rbind, c(
    # thetas
    lapply(voter_names, function(v)
      do.call(rbind, lapply(seq_len(D), function(d)
        add_row(paste0("theta.", v, ".", d), ideals[v, d])
      ))
    ),
    # alphas
    lapply(seq_len(J), function(j) add_row(paste0("alpha.", rc_names[j]), alphas[j])),
    # betas
    lapply(seq_len(J), function(j)
      do.call(rbind, lapply(seq_len(D), function(d)
        add_row(paste0("beta.", rc_names[j], ".", d), betas[j, d])
      ))
    )
  ))

  out <- list(statistics = stats)
  class(out) <- "summary.mcmc"
  out
}
