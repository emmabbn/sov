########## VS-SOV FUNCTION ##########
# external function.  			vs_sov() calculates vote-specific shapley owen values from package estimated inputs.
#' vs_sov
#'
#' @param estimates Estimation results from oc, wnom, or mcmcPack.
#' @param weight_nom If TRUE use weights estimated from wnom output, if FALSE use vector of 1's. ... For wnom only.
#' @param absolute If TRUE, q is a scalar of yeas needed to pass proposal.  If FALSE, pr is the proportion of yeas need to pass a proposal among voters attending.
#' @param vw Vector of weights for each voter (default 1 for each member, applied later).
#' @param q The quota of yeas need to pass a proposal under absolute k-majority rule (scalar, default absolute.maj(vw) applied later ).
#' @param pr The proportion of yea votes needed to pass a proposal among voters attending for simple k-majority rule (scalar, default: 0.5001).
#' @param votes Vote matrix (legislators x rollcalls) coded yea=1, nay=0, and 9="attend but abstain".  All other values should be NA.
#' @param dec The number of decimal places reported for vs-sovs.
#' @param out_dir The path to the output directory. If print_results = TRUE, the default is a subdirectory of the current path called "output", which the program creates.
#' @param print_results If TRUE, print results to an excel file in out_dir; if FALSE, don't print results.  In both cases, results are returned.
#'
#' @returns Calculates vote-specific shapley owen values from package estimated inputs.
#' @export
#'
#' @examples
vs_sov <- function(
    estimates		= NULL,		# Estimation results from oc, wnom, or mcmcPack.
    weight_nom		= FALSE,	# If TRUE use weights estimated from wnom output, if FALSE use vector of 1's. ... For wnom only.
    absolute		= FALSE,	# If TRUE, q is a scalar of yeas needed to pass proposal.  If FALSE, pr is the proportion of yeas need to pass a proposal among voters attending.
    vw				= NULL,		# Vector of weights for each voter (default 1 for each member, applied later).
    q 				= NULL,		# The quota of yeas need to pass a proposal under absolute k-majority rule (scalar, default absolute.maj(vw) applied later ).
    pr				= 0.5001,	# The proportion of yea votes needed to pass a proposal among voters attending for simple k-majority rule (scalar, default: 0.5001).
    votes			= NULL,		# Vote matrix (legislators x rollcalls) coded yea=1, nay=0, and 9="attend but abstain".  All other values should be NA.
    dec				= 3,		# The number of decimal places reported for vs-sovs.
    out_dir 		= "output",	# The path to the output directory. If print_results = TRUE, the default is a subdirectory of the current path called "output", which the program creates.
    print_results	= FALSE		# If TRUE, print results to an excel file in out_dir; if FALSE, don't print results.  In both cases, results are returned.
) {

  # validate arguments, except estimates (which varies by method) and votes (validated later).
  if (!is.null(vw)) { vw <- as.vector(vw) }
  validate_vs_sov_args(estimates, weight_nom, absolute, vw, q, pr, dec, out_dir, print_results)

  # ISOLATE IDEALS, NORMALS, AND CPS
  # determine whether user is using oc, wnom, mcmcPack, or other.
  type <- identify(estimates)
  # prep relative weight of dimensions (for type=="wnom", dummy for other types).
  W <- relative_weight(type, estimates, weight_nom)
  # isolate ideal points
  ideals <- isolate.ideals(type, estimates, W)			# for mcmc, ideals are currently untransformed
  n_voters <- nrow(ideals)
  # for mcmc, attain midpoints then rescale ideals and midpoints to unit hypersphere (if needed).
  if(type=="mcmc"){
    midpoints_mcmc <- irt_cutpoints(type, estimates)	# untransformed midpoints (the intersections of the cutplanes and the normal vectors)
    trans_vals <- rescale(ideals, midpoints_mcmc)		# rescale ideal points and midpoints to unit hypersphere
    ideals <- trans_vals$ideals
    midpoints_mcmc <- trans_vals$midpoints
  } else {
    midpoints_mcmc <- NULL								# a default value set for oc and wnom
  }
  # isolate normal vectors
  normals <- isolate.nv(type, estimates, midpoints_mcmc, W)
  # isolate cut points -- poorly named, cut points are the distances from the midpoints to the origin.
  cps <- isolate.cp(type, estimates, midpoints_mcmc, normals, W)

  # Default voting weights vector (if missing)
  if (is.null(vw))   vw <- rep(1, n_voters)

  # Default quota (for absolute k-majorities)
  if (absolute && is.null(q)) {
    q <- absolute.maj(vw)
  }

  # Validate votes matrix.
  votes <- as.matrix(votes)
  validate_vote_matrix(ideals, normals, vw, votes)

  # CREATE MATRIX OF VOTING WEIGHTS AND VECTOR OF Q (differs for absolute and simple)
  # create a matrix of voting weights and a vector of voting thresholds from user inputs vw, q, and pr that are appropriate for absolute==TRUE or absolute==FALSE
  vwmat <- create.vwmat(absolute, vw, votes)			# absolute is a logical indicating absolute or simple k-majority rule, vw is a vector of voting weights, votes is a matrix
  qvec <- create.qvec(absolute, q, pr, vwmat, votes)	# absolute is a logical indicating absolute or simple k-majority rule, q is a scalar voting threshold (for absolute), pr is the proportion of yea votes among yeas and nays (for simple), and vwmat is a matrix of voting weights
  # If q exceeds voting weights, reset q to the sum of the voting weights (done by rollcall).
  if(absolute==TRUE){
    qvec <- feasible.q(absolute, vwmat, qvec, votes)
  }

  # PROJECT IDEAL POINTS AND DETERMINE PIVOTS FOR EACH ROLL CALL
  # project ideal points onto normal vector and correct polarity of normal vector
  proj <- projections_and_polarity(ideals, normals, votes, cps)
  normals2 <- proj$normals				# normal vectors with polarities updated, based on votes.  They should be reported.
  # return the value of the pivot on the projection line.  The name(s) of that pivot will be found later.
  pivotv <- pivot(as.matrix(proj$p), vwmat, qvec, as.matrix(votes))
  # deterimine the name of the pivots using pivotv and proj$p.  	Excel tab "name_pivots_by_rollcall"
  pivot_by_rc <- get_pivot_names(proj$p, pivotv, votes)
  # summarize pivots by legislator. 								Excel tab "sovs".
  pivot_summary <- summarize_pivots(ideals, pivot_by_rc)
  # summarize correct normal vectors and angles by roll call.  	Excel tab "nv_and_angles".
  nv_and_angles <- extract_angles(normals2)

  # SAVE RESULTS TO AN EXCEL FILE
  if(print_results==TRUE){
    excel_results_vs_sov(pivot_summary, pivot_by_rc, nv_and_angles, file = "vs-sovs.xlsx", dec, out_dir)
  }

  return( list(
    pivot_summary = pivot_summary,	# ideal points, number of pivots, and vs-sovs for each voter
    pivot_by_rc   = pivot_by_rc,		# name of pivot(s) for each roll call
    nv_and_angles = nv_and_angles		# normal vectors and angles for each roll call.
  ) )

}															# CLOSE VS_SOV()
