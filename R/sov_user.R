########## TRADITIONAL SOV FUNCTION WITH USER INPUTS ##########

# external function.  			sov_user() calculates traditional shapley owen values from data provided by the user.
#' sov_user
#'
#' @param ideals Matrix of ideal points (legislators x dimensions)
#' @param av Attendance vector coded attend=1, "not attend"=NA.
#' @param weight_nom If TRUE use weights estimated from wnom output, if FALSE use vector of 1's. ... For wnom only.
#' @param absolute If TRUE, q is a scalar of yeas needed to pass proposal.  If FALSE, pr is the proportion of yeas need to pass a proposal among voters attending.
#' @param vw Vector of weights for each voter (default 1 for each member, applied later).
#' @param q The quota of yeas need to pass a proposal under absolute k-majority rule (scalar, default absolute.maj(vw) applied later ).
#' @param pr The proportion of yea votes needed to pass a proposal among voters attending for simple k-majority rule (scalar, default: 0.5001).
#' @param nPoints1 the number of points sampled along the azimuth (the first spherical coordinate the azimuth).  Sampled from 0 to 2*pi.
#' @param nPoints2 the number of points sampled along the inclination (the second and third spherical coordinate).  Sampled from 0 to 2*pi.
#' @param dec The number of decimal places reported for vs-sovs.
#' @param out_dir The path to the output directory. If print_results = TRUE, the default is a subdirectory of the current path called "output", which the program creates.
#' @param print_results If TRUE, print results to an excel file in out_dir; if FALSE, don't print results.  In both cases, results are returned.
#'
#' @returns calculates traditional shapley owen values from data provided by the user.
#' @export
#'
#' @examples
sov_user <- function(
    ideals			= NULL,		# Matrix of ideal points (legislators x dimensions)
    av				= NULL,		# Attendance vector coded attend=1, "not attend"=NA.
    weight_nom 		= FALSE,	# If TRUE use weights estimated from wnom output, if FALSE use vector of 1's. ... For wnom only.
    absolute   		= FALSE,	# If TRUE, q is a scalar of yeas needed to pass proposal.  If FALSE, pr is the proportion of yeas need to pass a proposal among voters attending.
    vw         		= NULL,		# Vector of weights for each voter (default 1 for each member, applied later).
    q          		= NULL,		# The quota of yeas need to pass a proposal under absolute k-majority rule (scalar, default absolute.maj(vw) applied later ).
    pr         		= 0.5001,	# The proportion of yea votes needed to pass a proposal among voters attending for simple k-majority rule (scalar, default: 0.5001).
    nPoints1   		= 360,		# the number of points sampled along the azimuth (the first spherical coordinate the azimuth).  Sampled from 0 to 2*pi.
    nPoints2   		= 360,		# the number of points sampled along the inclination (the second and third spherical coordinate).  Sampled from 0 to 2*pi.
    dec		   		= 3,		# The number of decimal places reported for vs-sovs.
    out_dir    		= "output",	# The path to the output directory. If print_results = TRUE, the default is a subdirectory of the current path called "output", which the program creates.
    print_results	= FALSE		# If TRUE, print results to an excel file in out_dir; if FALSE, don't print results.  In both cases, results are returned.
) {

  # validate arguments.
  if (!is.null(av)) { av <- as.vector(av) }
  if (!is.null(vw)) { vw <- as.vector(vw) }
  ideals <- as.matrix(ideals)
  validate_sov_user_args(ideals, av, weight_nom, absolute, vw, q, pr, nPoints1, nPoints2, dec, out_dir, print_results)
  n_voters <- nrow(ideals)

  # Generate normal vectors in all directions
  normals <- find_normals_nd(
    D 		 = ncol(ideals),
    nPoints1   = nPoints1,
    nPoints2   = nPoints2
  )
  n_dirs <- nrow(normals)

  # Build attendance matrix from attendance vector
  included <- matrix(av, nrow = n_voters, ncol = n_dirs, byrow  = FALSE)

  # Default voting weights vector (if missing)
  if (is.null(vw))   vw <- rep(1, n_voters)

  # Default quota (for absolute k-majorities)
  if (absolute && is.null(q)) {
    q <- absolute.maj(vw)
  }

  # Build weight-matrix and quota-vector for each "roll call" (i.e., each direction)
  vwmat <- create.vwmat(absolute, vw, included)
  qvec  <- create.qvec(absolute, q, pr, vwmat, included)
  # If q exceeds the voting weights, reset q to the sum of the voting weights (done by rollcall).
  if (absolute){
    qvec <- feasible.q(absolute, vwmat, qvec, included)
  }

  # Project ideal points onto the normal vectors (no polarity corrections)
  projv <- project(ideals, normals)

  # Find pivot positions for those those with included==1.
  pivotv      <- pivot(projv, vwmat, qvec, included)
  pivot_by_rc <- get_pivot_names(projv, pivotv, included)
  # summarize pivots by legislator.
  pivot_summary <- summarize_pivots(ideals, pivot_by_rc)
  names(pivot_summary)[names(pivot_summary) == "vs_sov"] <- "sov"
  # summarize correct normal vectors and angles by roll call.
  nv_and_angles <- extract_angles(normals)
  # combine "nv_and_angles" and "pivot_by_rc".
  only_pivot_pbrc <- pivot_by_rc[, -(1:2), drop = FALSE]
  pivot_by_angle <- cbind(nv_and_angles, only_pivot_pbrc)
  pivot_by_angle <- pivot_by_angle[, colnames(pivot_by_angle) != "RC_num"]

  # save results to an excel file
  if(print_results==TRUE){
    excel_results_sov(pivot_summary, pivot_by_angle, file = "sovs.xlsx", dec, out_dir)
  }

  return( list(
    pivot_summary = pivot_summary,		# ideal points, number of pivots, and sovs for each voter
    pivot_by_angle = pivot_by_angle		# name of pivots for each direction sampled
  ) )

}															# CLOSE SOV_USER()
