########## VS-SOV FUNCTION WITH USER INPUTS ##########
# external function.  			vs_sov_user() calculates vote-specific shapley owen values from data provided by the user.
################
#' Vote-Specific Shapley-Owen Values with User Inputs
#'
#' This function calculates vote-specific Shapley-Owen Values (vs-SOVs) from data provided by the user.
#'
#' @param ideals Matrix of ideal points (legislators x dimensions)
#' @param normals Matrix of normal vectors (rollcalls x dimensions)
#' @param midpoints Matrix of Midpoints (rollcalls x dimensions), i.e. intersection of the cutplane and the unobserved normal vector
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
#' #############################################################################
#' ##### ##### 1D Example.  Start with Inputs ##### #####
#' ## --- Ideals: 3 voters in 1D -----------------------------------------------
#' i1 <-  0.7
#' i2 <-  0.0
#' i3 <- -0.7
#' ideals <- cbind(coord1D = c(i1, i2, i3))
#' rownames(ideals) <- paste0("i", 1:3)
#'
#' ## --- Normals: 2 roll calls (x+, x-) ---------------------------------------
#' nv1 <-  1   # points to the right
#' nv2 <- -1   # points to the left
#' normals <- cbind(dim1 = c(nv1, nv2))
#' rownames(normals) <- paste0("RC", 1:2)
#'
#' ## --- Votes: 1=yea, 0=nay, 9=attend-no-vote, NA=absent ---------------------
#' votes <- cbind(
#'   RC1 = c(1, 0, 0),   # i1 yea, i2 nay, i3 nay
#'   RC2 = c(0, 1, 1)    # i1 nay, i2 yea, i3 yea
#' )
#' rownames(votes) <- rownames(ideals)
#'
#' ## --- Equal voting weights --------------------------------------------------
#' vw <- rep(1, nrow(ideals))
#'
#' ##### EX1: Vote-specific SOV (simple majority among attendees in 1D) #####
#' out_simple <- vs_sov_user(
#'   ideals   = ideals,
#'   normals  = normals,
#'   votes    = votes,
#'   absolute = FALSE,    # simple k-majority
#'   pr       = 0.5001,   # strict majority of attendees
#'   vw       = vw,
#'   dec      = 3
#' )
#'
#' # Aggregate results
#' out_simple$pivot_summary
#' out_simple$pivot_by_rc
#' out_simple$nv_and_angles
#'
#'
#' #############################################################################
#' ##### ##### 2D Examples.  Start with Inputs ##### #####
#' ## --- Ideals: 5 voters in 2D -----------------------------------------------
#' i1 <- c( 0.7,  0.7)
#' i2 <- c(-0.5,  0.5)
#' i3 <- c(-0.7, -0.7)
#' i4 <- c( 0.5, -0.5)
#' i5 <- c( 0.0,  0.0)
#' ideals <- rbind(i1, i2, i3, i4, i5)
#' rownames(ideals) <- paste0("i", 1:5)
#' colnames(ideals) <- c("coord1D","coord2D")
#'
#' ## --- Normals: 3 roll calls (x+, y+, x-) -----------------------------------
#' nv1 <- c( 1, 0)
#' nv2 <- c( 0, 1)
#' nv3 <- c(-1, 0)
#' normals <- rbind(nv1, nv2, nv3)
#' rownames(normals) <- paste0("RC", 1:3)
#'
#' ## --- Votes: 1=yea, 0=nay, 9=attend but did not vote -----------------------
#' votes <- cbind(
#'   RC1 = c(1,0,0,1,9),
#'   RC2 = c(1,1,0,0,0),
#'   RC3 = c(0,1,1,0,0)
#' )
#' rownames(votes) <- rownames(ideals)
#'
#' ## --- Equal voting weights ---------------------------------------------------
#' vw <- rep(1, nrow(ideals))
#'
#' ##### EX2: Simple majority (2D) -- 50% + ε among attendees #####
#' out_simple <- vs_sov_user(
#'   ideals   = ideals,
#'   normals  = normals,
#'   votes    = votes,
#'   absolute = FALSE,
#'   pr       = 0.5001,
#'   vw       = vw,
#'   dec      = 3
#' )
#'
#' out_simple$pivot_summary
#' out_simple$pivot_by_rc
#' out_simple$nv_and_angles
#'
#' ### Plotting (2D): one figure with ALL normals overlaid ###
#' if (interactive()) {
#'  vs_labels2d <- setNames(out_simple$pivot_summary$vs_sov, out_simple$pivot_summary$name)
#'  sov:::plot_sov_geometry(ideals, normals = normals, label_values = vs_labels2d, digits = 3)
#' }

vs_sov_user <- function(
    ideals			= NULL,		# Matrix of ideal points (legislators x dimensions)
    normals			= NULL, 	# Matrix of normal vectors (rollcalls x dimensions)
    midpoints		= NULL,		# Matrix of Midpoints (rollcalls x dimensions), i.e. intersection of the cutplane and the unobserved normal vector
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

  # validate arguments.
  ideals <- as.matrix(ideals)
  ev <- validate_vs_sov_user_args(ideals, normals, midpoints, weight_nom, absolute, vw, q, pr, votes, dec, out_dir, print_results)
  ideals		<- ev$ideals		# returns potentially rescaled ideal points
  normals 	<- ev$normals		# returns matrix if not NULL
  midpoints   <- ev$midpoints		# returns matrix of potentially rescaled midpoints if not NULL
  vw			<- ev$vw			# returns vector if not NULL

  # identify number of voters
  n_voters <- nrow(ideals)

  # Default voting weights vector (if missing)
  if (is.null(vw))   vw <- rep(1, n_voters)

  # Default quota (for absolute k-majorities)
  if (absolute && is.null(q)) {
    q <- absolute.maj(vw)
  }

  # CREATE MATRIX OF VOTING WEIGHTS AND VECTOR OF Q
  # create a matrix of voting weights and a vector of voting thresholds from user inputs vw, q, and pr that are appropriate for absolute==TRUE or absolute==FALSE
  vwmat <- create.vwmat(absolute, vw, votes)			# absolute is a logical indicating absolute or simple k-majority rule, vw is a vector of voting weights, votes is a matrix
  qvec <- create.qvec(absolute, q, pr, vwmat, votes)	# absolute is a logical indicating absolute or simple k-majority rule, q is a scalar voting threshold (for absolute), pr is the proportion of yea votes among yeas and nays (for simple), and vwmat is a matrix of voting weights
  # If q exceeds the voting weights, reset q to the sum of the voting weights (done by rollcall).
  if(absolute==TRUE){
    qvec <- feasible.q(absolute, vwmat, qvec, votes)
  }

  # PROJECT IDEAL POINTS AND DETERMINE PIVOTS
  # if midpoints are given, derive cps (distance of midpoint to origin), project ideal points using cps, and correct normal vector polarity
  if (!is.null(midpoints)){
    # create normal vectors from midpoints
    row_norms <- sqrt(rowSums(midpoints^2))
    normals <- matrix(NA_real_, nrow = nrow(midpoints), ncol = ncol(midpoints))
    # non-zero rows -> unit normals
    nz <- row_norms > 0
    if (any(nz)) {
      normals[nz, ] <- midpoints[nz, , drop = FALSE] / row_norms[nz]
    }
    # zero rows -> default to e1 = (1, 0, 0, ...)
    if (any(!nz)) {
      normals[!nz, ] <- 0
      normals[!nz, 1] <- 1
      # cp will be 0 for these rows
    }
    # derive cps (vector) by projecting midpoints onto normal vectors
    cps <- project_diag(midpoints, normals)
    # project ideal points and correct polarity of normal vectors
    proj <- projections_and_polarity(ideals, normals, votes, cps)
    p <- proj$p											# projections of the ideal points
    normals <- proj$normals								# normal vectors with polarities updated.
    # if normals are given instead, use function that does not update normals.
  } else {
    p <- project(ideals, normals)						# projections of the ideal points
  }
  # return the value of the pivot on the projection line.  The name(s) of that pivot will be found later.
  pivotv <- pivot(as.matrix(p), vwmat, qvec, as.matrix(votes))
  # deterimine the name of the pivots using pivotv and p.  	Excel tab "name_pivots_by_rollcall"
  pivot_by_rc <- get_pivot_names(p, pivotv, votes)
  # summarize pivots by legislator. 							Excel tab "sovs".
  pivot_summary <- summarize_pivots(ideals, pivot_by_rc)
  # summarize correct normal vectors and angles by roll call. Excel tab "nv_and_angles".
  nv_and_angles <- extract_angles(normals)

  # save results to an excel file
  if(print_results==TRUE){
    excel_results_vs_sov(pivot_summary, pivot_by_rc, nv_and_angles, file = "vs-sovs.xlsx", dec, out_dir)
  }

  return( list(
    pivot_summary = pivot_summary,	# ideal points, number of pivots, and vs-sovs for each voter
    pivot_by_rc   = pivot_by_rc,		# name of pivot(s) for each roll call
    nv_and_angles = nv_and_angles		# normal vectors and angles for each roll call.
  ) )

}															# CLOSE VS_SOV_USER()
