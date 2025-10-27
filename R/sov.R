########## TRADITIONAL SOV FUNCTION ##########
# external function.  			sov() calculates traditional shapley owen values from package estimated inputs.
##########################################
#' Traditional Shapley-Owen Values
#'
#' This function calculates traditional Shapley-Owen values (SOVs) from package estimated inputs.
#'
#' @param estimates Estimation results from oc, wnom, or MCMCpack.
#' @param av Attendance vector coded attend=1, "not attend"=NA.
#' @param weight_nom If TRUE use weights estimated from wnom output, if FALSE use vector of 1's. ... For wnom only.
#' @param absolute If TRUE, q is a scalar of yeas needed to pass proposal.  If FALSE, pr is the proportion of yeas need to pass a proposal among voters attending.
#' @param vw Vector of weights for each voter (default 1 for each member, applied later).
#' @param q The quota of yeas need to pass a proposal under absolute k-majority rule (scalar, default absolute.maj(vw) applied later).
#' @param pr The proportion of yea votes needed to pass a proposal among voters attending for simple k-majority rule (scalar, default: 0.5001).
#' @param nPoints1 The number of points sampled along the azimuth (the first spherical coordinate the azimuth).  Sampled from 0 to 2*pi.
#' @param nPoints2 The number of points sampled along the inclination (the second and third spherical coordinate).  Sampled from 0 to 2*pi.
#' @param dec The number of decimal places reported for vs-sovs.
#' @param out_dir The path to the output directory. If print_results = TRUE, the default is a subdirectory of the current path called "output", which the program creates.
#' @param print_results If TRUE, print results to an excel file in out_dir; if FALSE, don't print results.  In both cases, results are returned.
#'
#' @returns A list with data frames containing ideal points, number of pivots, name of pivots for each direction sampled, and SOVs for each voter.
#' @export
#'
#' @examples
sov <- function(
    estimates		= NULL,		# Estimation results from oc, wnom, or MCMCpack.
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

  # validate arguments, except estimates (which varies by method) and av (which will be validated after we can assign default).
  if (!is.null(av)) { av <- as.vector(av) }
  if (!is.null(vw)) { vw <- as.vector(vw) }
  validate_sov_args(estimates, weight_nom, absolute, vw, q, pr, nPoints1, nPoints2, dec, out_dir, print_results)

  # ISOLATE IDEALS AND VALIDATE AVs
  # determine whether user is using oc, wnom, mcmcPack, or other.
  type <- identify(estimates)
  # prep relative weight of dimensions (for type=="wnom", dummy for other types).
  W <- relative_weight(type, estimates, weight_nom)
  # isolate ideal points
  ideals <- isolate.ideals(type, estimates, W)			# note: ideals need not be in the unit sphere if we are not dealing with midpoints.  They will project to a normal vector just fine.
  n_voters <- nrow(ideals)
  # Validate attendance vector and assign defaults to vw, and av
  val <- validate_av(ideals, vw, av)
  vw <- val$vw
  av <- val$av

  # Generate normal vectors in all directions
  normals <- find_normals_nd(
    D 		 = ncol(ideals),
    nPoints1   = nPoints1,
    nPoints2   = nPoints2
  )
  n_dirs <- nrow(normals)

  # Build attendance matrix but call it "votes" (to use previously developed functions)
  votes <- matrix(av, nrow = n_voters, ncol = n_dirs, byrow  = FALSE)

  # Default quota (for absolute k-majorities)
  if (absolute && is.null(q)) {
    q <- absolute.maj(vw)
  }

  # Build weight-matrix and quota-vector for each "roll call" (i.e., each direction)
  vwmat <- create.vwmat(absolute, vw, votes)
  qvec  <- create.qvec(absolute, q, pr, vwmat, votes)
  # If q exceeds the voting weights, reset q to the sum of the voting weights (done by rollcall).
  if (absolute){
    qvec <- feasible.q(absolute, vwmat, qvec, votes)
  }

  # Project ideal points onto the normal vectors (no polarity corrections)
  projv <- project(ideals, normals)

  # Find pivot positions among those included in the analysis (i.e., those with votes==1).
  pivotv      <- pivot(projv, vwmat, qvec, votes)
  pivot_by_rc <- get_pivot_names(projv, pivotv, votes)
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

}															# CLOSE SOV()
