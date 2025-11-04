#' R/plot_sov_geometry.R
#'
#' plots SOV/VS-SOV geometry (1D or 2D)
#' Base-graphics helper to visualize ideal points and either a normal vector
#' (and optional cut line) or a cut midpoint (with inferred cut line).
#'
#' You can pass `label_values` (e.g., SOV/VS-SOV) to replace voter labels.
#' @importFrom graphics abline arrows lines par points segments text title
#'
#' @param ideals numeric matrix (voters x dimensions), with 1 or 2 columns, i.e. dimensions = 1,2.
#' @param normals optional numeric matrix (rollcalls x dimensions).  Rows are RCs.
#' @param midpoints optional numeric matrix (rollcalls x dimensions).  Rows are RCs.
#'   If both `normals` and `midpoints` are supplied, `normals` are used.
#' @param rc integer, which roll call (row) to draw from `normals`/`midpoints`.
#' @param labels optional character vector of point labels; default is rownames(ideals).
#' @param label_values optional numeric vector to display *instead of* `labels`
#'   (e.g., SOV or VS-SOV). Will be formatted with `digits` and mapped to
#'   `rownames(ideals)` if named; otherwise assumed in row order.
#' @param digits integer digits for `label_values` formatting.
#' @param pch, cex, col aesthetics for points.
#' @param show_normal, show_cut logical; draw normal arrow and cut line.
#' @export

plot_sov_geometry <- function(ideals,
                              normals = NULL,
                              midpoints = NULL,
                              label_values = NULL,
                              digits = 3,
                              main = NULL) {
  stopifnot(is.matrix(ideals), ncol(ideals) %in% 1:2)
  D <- ncol(ideals)

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

  if (D == 1) {
    ## ===== 1D =====
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
      if (ncol(MP) > 1) MP <- MP[, 1, drop = FALSE]
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
