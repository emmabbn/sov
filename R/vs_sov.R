########## VS-SOV FUNCTION ##########
# external function.  			vs_sov() calculates vote-specific shapley owen values from package estimated inputs.
########################################
#' Vote-Specific Shapley-Owen Values
#'
#' This function calculates vote-specific Shapley-Owen values (vs-SOVs) from package estimated inputs.
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
#' @returns A list with data frames containing ideal points, vs-SOVs for each voter, number of pivots, name of pivot(s) for each roll call, and normal vectors and angles for each roll call.
#' @export
#'
#' @examples
#' # VS-SOVs in 2D using W-NOMINATE OUTPUT
#' # vs_sov() needs an "estimates" object from OC, WNOMINATE, or MCMCpack.
#' # Below is a minimal WNOMINATE-like object built from 2D ideals
#' # and three roll calls.
#'
#' # Ideals: 5 voters in 2D
#' i1 <- c( 0.7,  0.7)
#' i2 <- c(-0.5,  0.5)
#' i3 <- c(-0.7, -0.7)
#' i4 <- c( 0.5, -0.5)
#' i5 <- c( 0.0,  0.0)
#' ideals <- rbind(i1, i2, i3, i4, i5)
#' rownames(ideals) <- paste0("i", 1:5)
#' colnames(ideals) <- c("coord1D","coord2D")
#'
#' # Fabricate a minimal WNOM-like object called estimates
#' # Spreads pick the normal directions; midpoints place the cutplane.
#' spreads <- rbind(
#'   c( 1,  0),  # RC1: normal along +x
#'   c( 0,  1),  # RC2: normal along +y
#'   c(-1,  0)   # RC3: normal along -x
#' )
#' midpoints <- rbind(
#'   c( 0.10,  0.00),  # RC1 cut near the origin on x
#'   c( 0.00, -0.10),  # RC2 cut slightly below origin on y
#'   c( 0.05,  0.00)   # RC3 cut near the origin on x
#' )
#' rownames(spreads)  <- rownames(midpoints) <- paste0("RC", 1:3)
#'
#' # Legislators must include coord1D/coord2D plus GMP and CC fields.
#' # These help the function identify the type of estimate.
#' leg <- data.frame(
#'   coord1D = ideals[, 1],
#'   coord2D = ideals[, 2],
#'   GMP = 0.5,
#'   CC  = 0.5,
#'   row.names = rownames(ideals),
#'   check.names = FALSE
#' )
#'
#' # Roll calls must include GMP and the WNOM fields midpoint\*D and spread\*D.
#' rc <- data.frame(
#'   GMP = rep(0.5, nrow(midpoints)),
#'   midpoint1D = midpoints[, 1],
#'   midpoint2D = midpoints[, 2],
#'   spread1D   = spreads[, 1],
#'   spread2D   = spreads[, 2],
#'   row.names  = rownames(midpoints),
#'   check.names = FALSE
#' )
#'
#' # Dimensional weights (first must be 1); here both dimensions are equal.
#' weights <- c(1, 1)
#'
#' # Minimal WNOM-like object
#' estimates <- list(
#'   legislators = leg,
#'   rollcalls   = rc,
#'   weights     = weights
#' )
#' class(estimates) <- "nomObject"  # not required, but reasonable
#'
#' # Votes: 1=yea, 0=nay, 9=attend-no-vote, NA=absent
#' votes <- cbind(
#'   RC1 = c(1, 0, 0, 1, 9),  # include one '9' to illustrate attendance w/o voting
#'   RC2 = c(1, 1, 0, 0, 0),
#'   RC3 = c(0, 1, 1, 0, 0)
#' )
#' rownames(votes) <- rownames(ideals)
#'
#' # Equal voting weights
#' vw <- rep(1, nrow(ideals))
#'
#' # VS-SOV from WNOM-like estimates (simple majority among attendees)
#' out_vs <- vs_sov(
#'   estimates = estimates,
#'   votes     = votes,
#'   absolute  = FALSE,  # simple k-majority
#'   pr        = 0.5001,
#'   vw        = vw,
#'   dec       = 3
#' )
#'
#' # aggregate results by member
#' out_vs$pivot_summary
#' # Pivot names by roll call:
#' out_vs$pivot_by_rc
#' # Normals and angles (derived from spreads & midpoints):
#' out_vs$nv_and_angles
#'
#' ### Plotting (2D): one figure with ALL normals derived from 'spreads' ###
#' if (interactive()) {
#'  vs_labels2d <- setNames(out_vs$pivot_summary$vs_sov, out_vs$pivot_summary$name)
#'  sov:::plot_sov_geometry(ideals, normals = normals, label_values = vs_labels2d, digits = 3)
#' }

vs_sov <- function(
    estimates		= NULL,		# Estimation results from oc, wnom, or MCMCpack.
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
