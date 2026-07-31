#' Plot SOV geometry
#'
#' This function plots voter ideal points together with roll-call normal vectors (or midpoint cutting lines) in one or two dimensions.
#'
#' @importFrom graphics abline arrows lines par points segments text title
#'
#' @param ideals Numeric matrix of voter ideal points with one or two columns.
#' @param normals Optional numeric matrix of roll-call normal vectors.
#' @param midpoints Optional numeric matrix of roll-call midpoints.
#' @param label_values Optional numeric values used to label voter points.
#' @param digits Number of decimal places used for value labels.
#' @param main Optional plot title.
#'
#' @returns Invisibly returns `NULL` after drawing the SOV geometry on the active graphics device.
#'
#' @export
#'
#' @examples
#'##--- Ideals: 5 voters in 2D -----------------------------------------------
#'i1 <- c( 0.7,  0.7)
#'i2 <- c(-0.5,  0.5)
#'i3 <- c(-0.7, -0.7)
#'i4 <- c( 0.5, -0.5)
#'i5 <- c( 0.0,  0.0)
#'ideals <- rbind(i1, i2, i3, i4, i5)
#'rownames(ideals) <- paste0("i", 1:5)
#'colnames(ideals) <- c("coord1D","coord2D")
#'
#'# Build a minimal WNOM-like 'estimates' object for sov() identification
#'spreads <- rbind(c( 1,  0), c( 0,  1), c(-1,  0))
#'midpoints <- rbind(c( 0.10,  0.00), c( 0.00, -0.10), c( 0.05,  0.00))
#'rownames(spreads)  <- rownames(midpoints) <- paste0("RC", 1:3)
#'colnames(spreads)  <- colnames(midpoints) <- c("dim1","dim2")
#'
#'leg <- data.frame(
#'  coord1D   = ideals[, 1],
#'  coord2D   = ideals[, 2],
#'  GMP       = 0.5,
#'  CC        = 0.5,
#'  row.names = rownames(ideals),
#'  check.names = FALSE
#')
#' rc <- data.frame(
#'  GMP = rep(0.5, nrow(midpoints)),
#'  midpoint1D = midpoints[, 1],
#'  midpoint2D = midpoints[, 2],
#'  spread1D   = spreads[, 1],
#'  spread2D   = spreads[, 2],
#'  row.names  = rownames(midpoints),
#'  check.names = FALSE
#')
#'weights <- c(1, 1)
#'estimates <- list(legislators = leg, rollcalls = rc, weights = weights)
#'class(estimates) <- "nomObject"
#'
#'# Attendance: exclude i5
#'av <- c(1, 1, 1, 1, NA); names(av) <- rownames(ideals)
#'vw <- rep(1, nrow(ideals))
#'
#'out_sov <- sov(
#'  estimates     = estimates,
#'  av            = av,
#'  absolute      = FALSE,
#'  pr            = 0.5001,
#'  vw            = vw,
#'  nPoints1      = 72,
#'  nPoints2      = 72,
#'  dec           = 3,
#'  print_results = FALSE
#')
#'
#' ### Plotting (2D): label with SOVs (no normals needed here) ###
#' if (interactive()) {
#'  sov_labels2d <- setNames(out_sov$pivot_summary$sov, out_sov$pivot_summary$name)
#'  sov::plot_sov_geometry(ideals, label_values = sov_labels2d, digits = 3)
#' }

plot_sov_geometry <- function(ideals = NULL,
                              normals = NULL,
                              midpoints = NULL,
                              label_values = NULL,
                              digits = 3,
                              main = NULL) {

  # VALIDATORS

  # Validate ideals
  if (is.null(ideals)) {
    stop("`ideals` must be supplied.", call. = FALSE)
  }

  if (!is.matrix(ideals) || !is.numeric(ideals)) {
    stop("`ideals` must be a numeric matrix.", call. = FALSE)
  }

  if (!ncol(ideals) %in% c(1L, 2L)) {
    stop("`ideals` must have either one or two columns.", call. = FALSE)
  }

  if (nrow(ideals) < 1L) {
    stop("`ideals` must contain at least one row.", call. = FALSE)
  }

  if (anyNA(ideals) || any(!is.finite(ideals))) {
    stop(
      "`ideals` must contain only finite, non-missing values.",
      call. = FALSE
    )
  }

  D <- ncol(ideals)

  # Validate normals and midpoints are mutually exclusive
  if (!is.null(normals) && !is.null(midpoints)) {
    stop(
      "Supply either `normals` or `midpoints`, not both.",
      call. = FALSE
    )
  }

  # Validate normals
  if (!is.null(normals)) {
    if (!is.matrix(normals) || !is.numeric(normals)) {
      stop("`normals` must be a numeric matrix.", call. = FALSE)
    }

    if (ncol(normals) != D) {
      stop(
        sprintf(
          "`normals` must have %d column%s to match `ideals`.",
          D,
          if (D == 1L) "" else "s"
        ),
        call. = FALSE
      )
    }

    if (nrow(normals) < 1L) {
      stop("`normals` must contain at least one row.", call. = FALSE)
    }

    if (anyNA(normals) || any(!is.finite(normals))) {
      stop(
        "`normals` must contain only finite, non-missing values.",
        call. = FALSE
      )
    }

    if (any(sqrt(rowSums(normals^2)) == 0)) {
      stop(
        "`normals` cannot contain zero-length vectors.",
        call. = FALSE
      )
    }
  }

  # Validate midpoints
  if (!is.null(midpoints)) {
    if (!is.matrix(midpoints) || !is.numeric(midpoints)) {
      stop("`midpoints` must be a numeric matrix.", call. = FALSE)
    }

    if (ncol(midpoints) != D) {
      stop(
        sprintf(
          "`midpoints` must have %d column%s to match `ideals`.",
          D,
          if (D == 1L) "" else "s"
        ),
        call. = FALSE
      )
    }

    if (nrow(midpoints) < 1L) {
      stop("`midpoints` must contain at least one row.", call. = FALSE)
    }

    if (anyNA(midpoints) || any(!is.finite(midpoints))) {
      stop(
        "`midpoints` must contain only finite, non-missing values.",
        call. = FALSE
      )
    }
  }


  # SUBSTANTIVE CODE (STARING WITH HELPERS)
  # utilities
  nm_or <- function(nm, fallback) if (!is.null(nm) && length(nm)) nm else fallback

  unit_vec <- function(v) {
    s <- sqrt(sum(v^2, na.rm = TRUE))
    if (is.finite(s) && s > 0) v / s else v
  }

  rc_names <- NULL
  if (!is.null(normals)) {
    rc_names <- if (is.null(rownames(normals))) paste0("RC", seq_len(nrow(normals))) else rownames(normals)
  } else if (!is.null(midpoints)) {
    rc_names <- if (is.null(rownames(midpoints))) paste0("RC", seq_len(nrow(midpoints))) else rownames(midpoints)
  }

  # Slightly-smaller RC labels and safely outside the circle
  lab_cex_norm <- 0.80
  lab_cex_mid  <- 0.80
  # Radial push beyond radius 1
  lab_out_norm <- 1.12
  lab_out_mid  <- 1.06

  # Helper: draw the chord of the unit circle for line u·x = C
  draw_chord <- function(u, C, col = "firebrick3", lwd = 2, lty = 2, label = NULL, m_for_side = NULL) {
    u <- unit_vec(u)
    p0 <- u * C
    d2 <- sum(p0^2)
    if (d2 <= 1 + 1e-12) {
      t <- c(-u[2], u[1])
      r <- sqrt(max(0, 1 - d2))
      a <- p0 + r * t
      b <- p0 - r * t
      segments(a[1], a[2], b[1], b[2], col = col, lwd = lwd, lty = lty)
      # Choose label anchor: topmost; tie -> leftmost
      sel <- if (a[2] > b[2] + 1e-12) a else if (b[2] > a[2] + 1e-12) b else if (a[1] < b[1]) a else b
      # Push slightly outside the circle
      lab_pt <- sel * lab_out_mid
      # For midpoint labels: side-justify by sign of midpoint x to avoid overlap
      if (!is.null(m_for_side) && length(m_for_side) == 2 && is.finite(m_for_side[1])) {
        pos_side <- if (m_for_side[1] < -1e-12) 2 else if (m_for_side[1] > 1e-12) 4 else 3
        text(lab_pt[1], lab_pt[2], labels = label, xpd = NA, cex = lab_cex_mid,
             col = col, pos = pos_side, offset = 0.35)
      } else {
        text(lab_pt[1], lab_pt[2], labels = label, xpd = NA, cex = lab_cex_mid, col = col)
      }
    } else {
      # No intersection: draw infinite line (rare under unit scaling)
      A <- u[1]; B <- u[2]
      if (abs(B) < 1e-12) {
        abline(v = C / A, col = col, lwd = lwd, lty = lty)
      } else {
        abline(a = C / B, b = -A / B, col = col, lwd = lwd, lty = lty)
      }
      if (!is.null(label)) {
        y <- 1.0; x <- (C - B * y) / A
        text(x, y, labels = label, col = col, xpd = NA, cex = lab_cex_mid, pos = 3)
      }
    }
  }

  ## ===== 1D =====
  if (D == 1) {
    op <- par(no.readonly = TRUE); on.exit(par(op), add = TRUE)
    par(xaxs = "i", yaxs = "i", mar = c(5, 5, 2, 5))  # keep labels inside frame

    xlab <- nm_or(colnames(ideals)[1], "coord1D")
    x <- ideals[, 1]
    names(x) <- rownames(ideals)

    plot(x, rep(0, length(x)),
         xlim = c(-1, 1), ylim = c(-0.35, 0.35),
         xlab = xlab, ylab = "", yaxt = "n",
         pch = 19, cex = 1.0, col = "black")
    abline(h = 0, col = "grey85")

    # optional numeric labels (e.g., VS-SOV) above points
    if (!is.null(label_values)) {
      if (is.null(names(label_values))) names(label_values) <- rownames(ideals)
      labs <- formatC(label_values[rownames(ideals)], format = "f", digits = digits)
      text(x, 0.08, labels = labs, cex = 0.9)
    }

    # overlaid normals (gather left- and right-pointing RCs)
    if (!is.null(normals)) {
      N <- as.matrix(normals)
      rc_names <- if (!is.null(rownames(N))) rownames(N) else paste0("RC", seq_len(nrow(N)))

      # draw a symmetric bidirectional arrow along the axis with tips inside the frame
      arr_end <- 0.94
      arrows(-arr_end, 0,  arr_end, 0, length = 0.08, angle = 20,
             col = "dodgerblue3", lwd = 2)
      arrows( arr_end, 0, -arr_end, 0, length = 0.08, angle = 20,
             col = "dodgerblue3", lwd = 2)

      left_labs  <- rc_names[which(N[, 1] < 0)]
      right_labs <- rc_names[which(N[, 1] > 0)]

      if (length(left_labs)) {
        text(-arr_end, 0, labels = paste(left_labs, collapse = ", "),
             pos = 2, xpd = NA, cex = 0.9, col = "dodgerblue3", offset = 0.4)
      }
      if (length(right_labs)) {
        text( arr_end, 0, labels = paste(right_labs, collapse = ", "),
             pos = 4, xpd = NA, cex = 0.9, col = "dodgerblue3", offset = 0.4)
      }
    }

    # optional midpoints in 1D (vertical dashed lines with RC labels near top)
    if (!is.null(midpoints)) {
      MP <- as.matrix(midpoints)
      mp <- MP[, 1]
      rc_names <- if (!is.null(rownames(MP))) rownames(MP) else paste0("RC", seq_along(mp))
      y_top <- 0.25
      for (k in seq_along(mp)) {
        x0 <- max(min(mp[k], 1.0), -1.0)
        segments(x0, -0.25, x0, 0.25, lty = 2, col = "firebrick")
        pos <- if (x0 < 0) 2 else 4
        text(x0, y_top, labels = rc_names[k],
             pos = pos, cex = 0.9, col = "firebrick", xpd = NA, offset = 0.3)
      }
    }

    if (!is.null(main)) title(main)
    return(invisible(NULL))
  }

  ## ===== 2D =====
  op <- par(no.readonly = TRUE); on.exit(par(op), add = TRUE)
  par(xaxs = "i", yaxs = "i", mar = c(5, 5, 2, 5))  # square-ish frame

  xlab <- nm_or(colnames(ideals)[1], "coord1D")
  ylab <- nm_or(colnames(ideals)[2], "coord2D")

  plot(NA, xlim = c(-1.1, 1.1), ylim = c(-1.1, 1.1),
       xlab = xlab, ylab = ylab, asp = 1)

  # unit circle
  th <- seq(0, 2 * pi, length.out = 361)
  lines(cos(th), sin(th), col = "grey70")

  # voters
  points(ideals[, 1], ideals[, 2], pch = 19)

  # optional numeric labels at voter points
  if (!is.null(label_values)) {
    if (is.null(names(label_values))) names(label_values) <- rownames(ideals)
    labs <- formatC(label_values[rownames(ideals)], format = "f", digits = digits)
    text(ideals[, 1], ideals[, 2], labels = labs, pos = 3, cex = 0.85)
  }

  # normals as dashed arrows, labels slightly outside circle
  if (!is.null(normals)) {
    N <- as.matrix(normals)
    rc_names <- if (!is.null(rownames(N))) rownames(N) else paste0("RC", seq_len(nrow(N)))
    for (j in seq_len(nrow(N))) {
      v <- N[j, ]
      v <- v / sqrt(sum(v^2))
      arrows(0, 0, 1.05 * v[1], 1.05 * v[2],
             col = "firebrick", lty = 2, lwd = 2, length = 0.08, angle = 20)
      text(1.12 * v[1], 1.12 * v[2], labels = rc_names[j],
           cex = 0.85, col = "firebrick", xpd = NA)
    }
  }

  # midpoints: mark with an “x” and label outside, left/right by x-sign to avoid overlap
  if (!is.null(midpoints)) {
    for (j in seq_len(nrow(midpoints))) {
      m <- as.numeric(midpoints[j, ])
      if (all(!is.finite(m))) next
      # If normals absent, infer direction from midpoint
      u <- if (!is.null(normals)) unit_vec(as.numeric(normals[j, ])) else {
        mm <- unit_vec(m); if (!is.finite(sum(mm))) c(1, 0) else mm
      }
      C <- sum(u * m)
      draw_chord(u, C, label = rc_names[j], m_for_side = m)
      # show midpoint marker
      points(m[1], m[2], pch = 4, lwd = 2, col = "firebrick3", cex = 1.0)
    }
  }

  if (!is.null(main)) title(main)
  invisible(NULL)
}
