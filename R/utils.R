#' Internal imports
#' @importFrom dplyr %>%
#' @importFrom tidyr separate pivot_wider
#' @importFrom tibble column_to_rownames
#' @importFrom openxlsx createWorkbook addWorksheet writeData createStyle addStyle saveWorkbook
#' @name utils
#' @keywords internal
NULL

##### normalize weights #####
# This function normalizes a vector of weights w.r.t. to the weight in the first dimension.  Use with wnom.
normalize_weights <- function(w) {
  if (!is.numeric(w) || length(w) < 1L) {
    stop("Weights must be a numeric vector of length >= 1.")
  }
  if (any(is.na(w)) || any(!is.finite(w))) {
    stop("Weights must be finite and cannot contain NA/Inf.")
  }
  if (any(w < 0)) {
    stop("Weights must be non-negative (>= 0).")
  }
  if (w[1] == 0) {
    stop("The first weight must be > 0 (cannot normalize by zero).")
  }
  return(as.vector(w / w[1]))
}

##### relative_weight #####
# determines the relative weight of each dimension from WNOM estimates.  Use with wnom.
# inputs: estimates -- for oc and wnom, the output object; for mcmcPack, the object produced from summary()
#			weight_nom -- logical scalar (TRUE if user wants to use the dimensional weights; FALSE if each dimensions is weighted equally)
# output: vector of relative weights for each dimension passed to isolate.ideals(), isolate.nv() and isolate.cps().
relative_weight <- function(type, estimates, weight_nom)
{
  # test whether weight_nom is a logical scalar.
  if (length(weight_nom)!=1 | !is.logical(weight_nom) ) {
    stop("weight_nom is a logical scalar.  It must have a single value of TRUE or FALSE (the default).")
  }
  # Return 1 for types other than wnom.  OC and mcmc will ignore it. For WNOM, the values will change below.
  if (type != "wnom") {
    return(1)
  } else {
    # For wnom either assign weights esimated for each dimension or assign a vector of 1s for equal weight (the default)
    if(weight_nom == TRUE){ return( normalize_weights(estimates$weights) ) }
    else{return( rep(1, length(estimates$weights)) )}
  }
}

##### weighted values #####
# This function weights the values in Z (with cols = #dims) w.r.t a vector of weights W (length=#dims). Use with wnom
weighted_values <- function(Z, W)
{
  W <- as.vector(W)
  # make sure the number of dimensions match
  if (ncol(Z) != length(W)) {
    stop("The number of columns in matrix Z must match the length of weights W.")
  }
  return(Z %*% diag(W))
}

##### rescale #####
# This function rescales ideals and midpoints_mcmc if they are outside the unit hypersphere.
# input: ideals (legislators x dimensions), midpoints_mcmc (roll calls x dimensions)
# output: both matrices rescaled
rescale <- function(ideals = NULL, midpoints_mcmc = NULL)
{
  # Require at least one matrix
  if (is.null(ideals) && is.null(midpoints_mcmc)) {
    stop("At least one of 'ideals' or 'midpoints_mcmc' must be provided.")
  }
  # Coerce inputs to matrices when input present
  if (!is.null(ideals)){         ideals         <- as.matrix(ideals) }
  if (!is.null(midpoints_mcmc)){ midpoints_mcmc <- as.matrix(midpoints_mcmc) }
  # If both provided, make sure dimensionality matches
  if (!is.null(ideals) && !is.null(midpoints_mcmc) && ncol(ideals) != ncol(midpoints_mcmc)) {
    stop(sprintf("The number of columns of ideals (%d) and midpoints_mcmc (%d) must match.", ncol(ideals), ncol(midpoints_mcmc)))
  }
  # Combine available matrices
  all_points <- if (is.null(ideals)) {
    midpoints_mcmc
  } else if (is.null(midpoints_mcmc)) {
    ideals
  } else {
    rbind(ideals, midpoints_mcmc)
  }
  # Compute maximum Euclidean norm among the matrices provided
  max_norm <- max(sqrt(rowSums(all_points^2)), na.rm=TRUE)
  # Apply uniform scaling if needed
  if (is.finite(max_norm) && max_norm > 1) {
    if (!is.null(ideals)){         ideals         <- ideals / max_norm }
    if (!is.null(midpoints_mcmc)){ midpoints_mcmc <- midpoints_mcmc / max_norm }
  }
  # Return rescaled values
  return( list(ideals = ideals, midpoints_mcmc = midpoints_mcmc) )
}

##### validate.scaling #####
# This function validates that all points in a matrix are inside the unit hypersphere.
# input: matrix (whatever x dimensions)
# output: warning message
validate.scaling <- function(matrix)
{
  matrix_name <- deparse(substitute(matrix))
  matrix <- as.matrix(matrix)
  max_norm <- max(sqrt(rowSums(matrix^2)))
  if (max_norm > 1) {
    warning(sprintf("At least one value in '%s' lies outside the unit hypersphere (i.e., distance from origin > 1).\n
	  	This should not be a problem if all normal vectors distance==1 from the origin.", matrix_name))
  }
}

##### identify #####
# Identifies whether the input is from oc, wnominate, mcmcPack, or other based on estimated object.
# input: for oc and wnom, the output object; for mcmcPack, the object produced from summary()
# output: oc, wnom, mcmc, dk (don't know) for other
identify <- function(estimates)
{
  # If estimates does not exist, return don't know
  if (missing(estimates)) {
    stop("Specify your estimation technique explicitly (oc, wnom, mcmcpack) or use functions vs_sov() or sov() instead.\n",
         "  SOV could not identify the technique that created your estimates,\n",
         "  perhaps because you did not use one of the packages accepted by this function.")
  }
  # Otherwise, identify column headers unique to the estimation source
  # oc
  rank <- grep("rank", colnames(estimates$legislators), value=TRUE)
  volume <- grep("volume", colnames(estimates$legislators), value=TRUE)
  midp <- grep("midpoints", colnames(estimates$rollcalls), value=TRUE)
  # wnom
  gmp <- grep("GMP", colnames(estimates$legislators), value=TRUE)
  cc <- grep("CC", colnames(estimates$legislators), value=TRUE)
  gmp2 <- grep("GMP", colnames(estimates$rollcalls), value=TRUE)
  spreads <- grep("spread", colnames(estimates$rollcalls), value=TRUE)
  # MCMCPack
  if ("statistics" %in% names(estimates)) {
    stats <- estimates[["statistics"]]
    members   <- stats[grep("^theta\\.", rownames(stats)), , drop = FALSE]
    rollcalls <- stats[grep("^beta\\.",  rownames(stats)), , drop = FALSE]
    thetas    <- if (nrow(members)   > 0) grep("Mean", colnames(members),   value = TRUE) else character(0)
    betas     <- if (nrow(rollcalls) > 0) grep("Mean", colnames(rollcalls), value = TRUE) else character(0)
  } else {
    thetas <- betas <- character(0)
  }
  # If all indicators match, we conclude it is the stated estimation source.
  out <- "dk"
  if(length(rank) > 0   && length(volume) > 0 && length(midp) > 0)                         out <- "oc"
  if(length(gmp) > 0    && length(cc) > 0     && length(gmp2) > 0  && length(spreads) > 0) out <- "wnom"
  if(length(thetas) > 0 && length(betas) > 0)						                         out <- "mcmc"
  # stop message
  if(out=="dk"){
    stop("Specify your estimation technique explicitly (oc, wnom, mcmcpack) or use functions vs_sov() or sov() instead.\n",
         "  SOV could not identify the technique that created your estimates,\n",
         "  perhaps because you did not use one of the packages accepted by this function.")
  }
  return(out)
}

##### isolate ideal points #####
# returns ideal points from member matrix
# inputs: type ("oc", "wnom", "mcmc"); estimates from oc, wnom, or mcmc; weights on each dimension for wnom
# output: ideal points in correct number of dimensions
isolate.ideals <- function(type, estimates, weights) {
  if (type == "oc") {
    # For 'oc', isolate columns that contain "coord"
    return(as.matrix( estimates$legislators[, grep("coord", colnames(estimates$legislators), value = TRUE), drop = FALSE] ))
  } else if (type == "wnom") {
    # For 'wnom', isolate columns that contain "coord", then apply weights
    ideals <- as.matrix( estimates$legislators[, grep("coord", colnames(estimates$legislators), value = TRUE), drop = FALSE])
    # make sure the length of weights is equal to the number of dimensions and the first dimension has weight 1 w.r.t. itself.
    if(length(weights)!=ncol(ideals) || weights[1] != 1){
      stop("The length of the weights vector must equal the number of dimensions and the first dimension must weigh 1.")
    }
    ideals <- weighted_values(ideals, weights)			# weight coordinates by the relative weight of the dimension w.r.t. D==1.
    # rename columns to coord1D, coord2D, ...
    colnames(ideals) <- paste0("coord", seq_len(ncol(ideals)), "D")
    return(ideals)

  } else if (type == "mcmc") {
    # for 'mcmc', isolate thetas, remove extra characters, and assign legislator names as row names
    members <- estimates[["statistics"]][grep("theta", rownames(estimates[["statistics"]])), , drop = FALSE ]
    # 1. Create a data frame with the "Mean" values and row names
    df <- data.frame(
      Mean    = members[, "Mean"],
      rowname = rownames(members)
    )
    # 2. Split the row name into three parts: c("theta", "group", "col")
    df <- df %>%
      separate(rowname, into = c("theta", "group", "col"), sep = "\\.")
    # 3. Pivot so that 'col' becomes the new columns: theta_1, theta_2, ... 'group' will become (or remain) the row identifier
    df_wide <- df %>%
      pivot_wider(
        names_from  = col,
        values_from = Mean,
      )
    # 4. Remove the "theta" column and convert 'group' into row names
    df_wide <- df_wide %>%
      dplyr::select(-theta) %>%
      column_to_rownames("group")
    final_theta <- as.matrix(df_wide)
    # 5. Rename columns to coord1D, coord2D
    colnames(final_theta) <- paste0("coord", seq_len(ncol(final_theta)), "D")
    return( final_theta )
  } else {
    stop("Functions vs_sov() and sov() require estimates from either `oc', `wnom', or `mcmc.'  Consider using vs_sov_user() or sov_user() instead.")
  }
}

##### get_alphas_betas for mcmc #####
# input: type =="mcmc"; estimates from mcmc
# output: alphas and betas for each roll call as separate objects
get_alphas_betas <- function(type, estimates){
  if (type == 'mcmc'){
    stats <- estimates[["statistics"]]
    # Single check covers both alphas and betas
    if (!"Mean" %in% colnames(stats)) {
      stop("MCMC 'statistics' must have a 'Mean' column.")
    }

    # --- ALPHAS ---
    rollcallsA <- stats[grep("^alpha\\.", rownames(stats)), , drop = FALSE]
    dfA <- data.frame(
      Mean     = rollcallsA[, "Mean", drop = TRUE],
      rownameA = rownames(rollcallsA),
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
    dfA <- tidyr::separate(dfA, rownameA, into = c("alpha", "group"), sep = "\\.")
    alphas <- as.matrix(dfA[, "Mean", drop = FALSE])
    rownames(alphas) <- dfA[, "group", drop = TRUE]

    # --- BETAS ---
    rollcallsB <- stats[grep("^beta\\.", rownames(stats)), , drop = FALSE]
    dfB <- data.frame(
      Mean     = rollcallsB[, "Mean", drop = TRUE],
      rownameB = rownames(rollcallsB),
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
    dfB <- tidyr::separate(dfB, rownameB, into = c("beta", "group", "col"), sep = "\\.")
    df_wide <- tidyr::pivot_wider(dfB, names_from = col, values_from = Mean, names_prefix = "beta_")
    df_wide <- dplyr::select(df_wide, -beta)
    df_wide <- tibble::column_to_rownames(df_wide, "group")
    betas <- as.matrix(df_wide)

    return(list(alphas = alphas, betas = betas))
  } else {
    stop("function get_alphas_betas() currently takes only 'mcmc' as value the type.")
  }
}

##### irt_cutpoints for mcmc #####
# input: type =="mcmc"; difficulty parameter alpha (vector length j); discrimination parameter beta (j x d matrix).
# output: midpoints - points of intersection of the cutplanes and the normal vectors.
irt_cutpoints <- function(type, estimates) {
  # check type
  if (type != 'mcmc'){
    stop("function irt_cutpoints() only works for type 'mcmc'.")
  }
  # get alphas and betas
  ab <- get_alphas_betas(type, estimates)
  alpha <- ab$alphas
  beta <- ab$betas
  # check compatibility of dimensions
  if (length(alpha) != nrow(beta)) {
    stop("Length of alpha must match number of rows of beta.")
  }
  # 1D calculation
  if(ncol(beta)==1){
    z <- alpha / beta								# no minus sign on alpha
  }
  # multiple dimensional calculation
  if(ncol(beta) > 1){
    # calculating z_j = -alpha_j /  ||beta_j||^2 )  beta_j, where j is an index for the roll call and ||beta_j||^2 is the squared Euclidean norm of beta_j.
    # compute squared norms ||beta_j||^2
    beta_norm_sq <- rowSums(beta^2)
    # compute scalar multipliers: -alpha_j / ||beta_j||^2
    scalars <- -alpha / beta_norm_sq				# minus sign on alpha
    # compute z_j vectors: scalars * beta_j
    z <- sweep(beta, 1, scalars, `*`)
  }
  return(z)  											# z is a j x d matrix
}

##### isolate normal vectors #####
# returns normal vectors from rollcalls matrix, which vary by roll call
# inputs: type ("oc", "wnom", "mcmc", "other"); estimates from oc, wnom, or mcmc; weights on each dimension for wnom
# output: normal vectors in correct number of dimensions
isolate.nv <- function(type, estimates, midpoints_mcmc, weights)
{
  if(type == "oc"){
    # For oc input, isolate columns normVector1D, normVector2D, ...
    return(as.matrix(estimates$rollcalls[, grep("normVector", colnames(estimates$rollcalls), value = TRUE), drop = FALSE]))
  } else if (type == "wnom") {
    # For wnom, normal vectors must be calculated from spreads and cutpoints
    spreads <- as.matrix(estimates$rollcalls[, grep("spread", colnames(estimates$rollcalls), value = TRUE), drop = FALSE])
    midpoints <- as.matrix(estimates$rollcalls[, grep("midpoint", colnames(estimates$rollcalls), value = TRUE), drop = FALSE])
    # make sure the length of weights is equal to the number of dimensions and the first dimension has weight 1 w.r.t. itself.
    if(length(weights)!=ncol(spreads) || weights[1] != 1){
      stop("The length of the weights vector must equal the number of dimensions and the first dimension must weigh 1.")
    }
    # calculate normal vectors following Poole (y is a yes point and n is a no point derived from midpoint and spread).
    y <- midpoints - spreads
    y <- y %*% diag(weights)
    n <- midpoints + spreads
    n <- n %*% diag(weights)
    # determine normal vector in weighted metric
    diff <- n - y
    dist <- sqrt(rowSums(diff^2))
    return ( diff/dist )
  } else if (type == "mcmc") {
    # Robust normalization: allow zero midpoints by defaulting to e1
    if (is.null(midpoints_mcmc)) {
      stop("For type='mcmc', 'midpoints_mcmc' must be provided.")
    }
    row_norms <- sqrt(rowSums(midpoints_mcmc^2))

    # start with all zeros
    normals <- matrix(0, nrow = nrow(midpoints_mcmc), ncol = ncol(midpoints_mcmc), dimnames = dimnames(midpoints_mcmc))
    nz <- row_norms > 0
    if (any(nz)) {
      # Divide each non-zero row by its own norm (true row-wise normalization)
      normals[nz, ] <- sweep(midpoints_mcmc[nz, , drop = FALSE], 1, row_norms[nz], "/")
    }
    if (any(!nz)) {
      # zero rows -> default to +e1
      normals[!nz, 1] <- 1
    }
    return(normals)
  } else {
    # stop if the function takes "OTHER" data
    stop("Function isolate.nv() only takes 'oc', 'wnom', and 'mcmcPack' type data.")
  }
}

##### project #####
# Use this to project ideal points onto a normal vector.
# input: ideals -- a maxtrix of ideal points, (legislators x dimensions);
#        normal -- a matrix of normal vectors (rollcalls x dimensions).
# output: matrix of projections (legislators x roll calls).
project <- function(points, normals) {
  # make sure 'points' is a matrix
  if (!is.matrix(points)) {
    stop("The 'points' argument must be a matrix (# observations x # dimensions).")
  }
  # make sure 'normals' is a matrix
  if (!is.matrix(normals)) {
    stop("The 'normals' argument must be a matrix (# normal vectors x # dimensions).")
  }
  # make sure the number of dimensions match
  if (ncol(points) != ncol(normals)) {
    stop("The number of columns in 'points' must match the number of columns in 'normals'.")
  }
  # normalize each row of the 'normals' matrix (just in case), each representing the normal vector for that roll call.
  row_norms <- sqrt(rowSums(normals^2))
  normals <- normals / matrix(row_norms, nrow = nrow(normals), ncol = ncol(normals))		# Extras guarantee that each row of z is divided elementwise by its norm.
  # compute the projected values -- a [legislators x roll calls] matrix.
  return( points %*% t(normals) )
}

##### project_diag #####
# The following function projects m-dimensional points onto a normal vector and reports a
#  vector of projections (length = number of roll calls)
#  Use this to project midpoints, onto a normal vector
# input: points -- a maxtrix of points, such as cutpoints (rollcalls x dimensions);
#        normal -- a matrix of normal vectors (rollcalls x dimensions).
# output: vector of projections (i.e. the distance from the origin up the normal vector).
project_diag <- function(points, normals) {
  # make sure 'points' is a matrix
  if (!is.matrix(points)) {
    stop("The 'points' argument must be a matrix (# observations x # dimensions).")
  }
  # make sure 'normals' is a matrix
  if (!is.matrix(normals)) {
    stop("The 'normals' argument must be a matrix (# normal vectors x # dimensions).")
  }
  # make sure the number of dimensions match
  if (ncol(points) != ncol(normals)) {
    stop("The number of columns in 'points' must match the number of columns in 'normals'.")
  }
  # normalize each row of the 'normals' matrix (just in case), each representing the normal vector for that roll call.
  row_norms <- sqrt(rowSums(normals^2))
  normals <- normals / matrix(row_norms, nrow = nrow(normals), ncol = ncol(normals))		# Extras guarantees that each row of normals is divided elementwise by its norm.
  # compute only the diagonal elements -- each entry is a single point for a corresponding roll call.
  return( rowSums(points * normals))
}

##### function isolate cp #####
# returns projected cp from rollcalls matrix -- the projected positions of the cut points
# input: type ("oc", "wnom", "other"),
#		   rollcalls matrix (nvoters x ?) - columns vary by estimation technique,
#		   normals - a matrix of normal vectors (rollcalls x dimensions)
#		   weight - a weight between each dimension relative to the first dimension (a vector for nominate only)
# output: cut points projected onto normal vectors (i.e., the distance on the normal vector from the origin)
isolate.cp <- function(type, estimates, midpoints_mcmc, normals, weights)
{
  if(type == "oc"){
    # For oc input, isolate column midpoints.  These are already distances from origin.
    return(as.matrix(estimates$rollcalls[, grep("midpoints", colnames(estimates$rollcalls), value = TRUE), drop = FALSE]))
  } else if (type == "wnom") {
    # For nominate input, midpoints must be weighted then projected onto normal vectors
    midpoints <- as.matrix(estimates$rollcalls[, grep("midpoint", colnames(estimates$rollcalls), value = TRUE), drop = FALSE])
    # make sure the length of weights is equal to the number of dimensions and the first dimension has weight 1 w.r.t. itself.
    if(length(weights)!=ncol(midpoints) || weights[1] != 1){
      stop("The length of the weights vector must equal the number of dimensions and the first dimension must weigh 1.")
    }
    # weight the midpoints
    midsw <- weighted_values(midpoints, weights)
    # project weighted midpoints
    return( project_diag(midsw, normals) )		# returns the projected distance from the origin, for each rollcall (can be negative)
  } else if (type == "mcmc") {
    return( project_diag(midpoints_mcmc, normals) )	# returns the projected distance from the origin, for each rollcall (can be negative)
  } else {
    # Warning if the function takes "OTHER" data
    stop("Function isolate.cp() only takes 'oc', 'wnom', and 'mcmcPack' type data.")
  }
}

##### function projections & polarity #####
# Function 1) projects the ideal points onto the normal vectors and
#			 2) assures each normal vector is pointing in the direction of the proposal.
#			 -Note: uses logical operations to avoid element-wise condition checks.
# inputs:  ideals -- a maxtrix of ideal points, (legislators x dimensions);
#          normals -- a matrix of normal vectors (rollcalls x dimensions);
#			 votes -- a vote matrix (legislators x rollcalls) with yea==1, nay==0, attend but abstain==9, missing==NA;
#			 cps -- distance of midpoints from origin, also described as cut points (one value for each roll call).
# outputs: 1) projections of ideal points
#			 2) normal vectors that point in the direction of the proposal (rollcalls x dims)
projections_and_polarity <- function(ideals, normals, votes, cps)
{
  # make sure the number of dimensions match
  if ( ncol(ideals) != ncol(normals) ){
    stop("The number of columns in 'ideals' must match the number of columns in 'normals'.")
  }
  if ( nrow(ideals) != nrow(votes) ){
    stop(sprintf("The number of rows in `ideals` (%d) must match the number of rows in `votes` (%d).", nrow(ideals), nrow(votes) ))
  }
  if ( nrow(normals) != ncol(votes) | nrow(normals) != length(cps) ){
    stop(sprintf("The number of rows in `normals` (%d) must match the number of columns in `votes` (%d) and the length of `cps` (%d).", nrow(normals), ncol(votes), length(cps) ))
  }
  # check if all elements of votes are 0, 1, 9, or NA
  if ( !all(is.na(votes) | votes %in% c(0, 1, 9)) ) {
    stop("The vote matrix can only contain 1=yea, 0=nay, 9=attended but abstain, and NAs.")
  }
  # project ideal points onto normal vectors, producing a vector of points for each roll call.
  p <- project(ideals, normals)
  # logical matrices for voting decisions
  is_yea <- votes == 1
  is_nay <- votes == 0
  # determine whether the legislator's projection is to the right or left of the cutpoint
  right_of_cp <- p > matrix(cps, nrow = nrow(p), ncol = ncol(p), byrow = TRUE)
  left_of_cp <- !right_of_cp
  # left_of_cp  <- p < matrix(cps, nrow = nrow(p), ncol = ncol(p), byrow = TRUE)
  # compute number of yeas/nays on the right and left in a vectorized way
  yeas_right <- colSums(is_yea & right_of_cp, na.rm = TRUE)
  nays_right <- colSums(is_nay & right_of_cp, na.rm = TRUE)
  yeas_left  <- colSums(is_yea & left_of_cp, na.rm = TRUE)
  nays_left  <- colSums(is_nay & left_of_cp, na.rm = TRUE)
  # Identify which rows of normals need their polarity switched
  switch_polarity <- (yeas_left > nays_left & nays_right >= yeas_right) |
    (yeas_left >= nays_left & nays_right > yeas_right)
  # if the polarity switched, flip normals (done in one operation)
  normals[switch_polarity, ] <- -normals[switch_polarity, ]
  # ... and flip projected values
  p[, switch_polarity] <- -p[, switch_polarity]
  return(list(p=p, normals=normals))
}

##### psort_apply #####
# Sorts each column of projv and vwmat in ascending order, preserving NA structure at the end.  Used in pivot().
# inputs: projv is an unsorted matrix of projected values: num of legislators x num of rollcalls),
#			vwmat is an unsorted matrix of voting weights: num of legislators x num of rollcalls).
# outputs: projv and vmwat with each column individually sorted by values of projv.
psort_apply <- function(projv, vwmat) {
  res <- lapply(seq_len(ncol(projv)), function(j) {
    col_projv <- projv[, j]
    col_vw <- vwmat[, j]
    ord <- order(col_projv, na.last = TRUE)
    sorted_projv <- col_projv[ord]
    sorted_vw <- col_vw[ord]
    list(projv = sorted_projv, vw = sorted_vw)
  })
  # Reassemble matrices from sorted columns
  projv2 <- do.call(cbind, lapply(res, `[[`, "projv"))
  vwmat2 <- do.call(cbind, lapply(res, `[[`, "vw"))
  return(list(projv = projv2, vwmat = vwmat2))
}

##### get_pivots #####
# Gets the pivots from a matrix of projected points sorted for each roll call.  Used in pivot().
# inputs: projv2 (sorted matrix of projected values: num of legislators x num of rollcalls),
#			vw2 (sorted matrix of voting weights: num of legislators x num of rollcalls),
#         qvec (vector: k-majority threshold),
# output: projected value of pivot (vector: length == num of rollcalls)
get_pivots <- function(projv2, vw2, qvec)
{
  sapply(seq_len(ncol(projv2)), function(j) {
    # Extract column j and filter out rows where either value is NA
    pj <- projv2[, j]
    wj <- vw2[, j]
    valid <- !is.na(pj) & !is.na(wj)
    pj <- pj[valid]
    wj <- wj[valid]
    # Compute cumulative weights
    cum_wj <- cumsum(wj)
    # Find pivot index where cumulative weight first >= qvec[j]
    pivot_index <- which(cum_wj >= qvec[j])[1]
    # Return pivot value if it is found, otherwise return NA
    return(if (!is.na(pivot_index)) pj[pivot_index] else NA)
  })
}													# close function

##### pivot #####
# Determines the pivots from a 1D projection of the ideal points, cut points, and proper nayside of the vote
# inputs: projv (matrix of projected values: num of legislators x num of rollcalls),
#			vwmat (matrix: legislators x rollcalls),
#         qvec (vector: k-majority threshold for each rollcall),
#			votes (matrix of yea=1, nay=0, and "attended but abstain"=9: num of legislators x num of rollcalls).
# output: projected value of pivot (vector: length == num of rollcalls)
pivot <- function(projv, vwmat, qvec, votes)
{
  # basic checks.
  if (!is.matrix(projv) || !is.matrix(vwmat) || !is.matrix(votes)) {
    stop("projv, vwmat, and votes must all be matrices.")
  }
  if (!all(dim(projv) == dim(vwmat)) || !all(dim(projv) == dim(votes))) {
    stop("projv, vwmat, and votes must have the same dimensions.")
  }
  if (length(qvec) != ncol(projv)) {
    stop("qvec must be a vector with length equal to the number of rollcalls (ncol of projv).")
  }
  # to account for attendance, delete projected value for anyone absent on a particular roll call.
  projv[is.na(votes)] <- NA
  # remove row names so no one gets confused after sorting
  rownames(projv) <- NULL
  # Sort projections and weights in ascending order or projv by individual roll call
  ps <- psort_apply(projv, vwmat)
  # Compute pivot values
  pivotvs <- get_pivots(as.matrix(ps$projv), as.matrix(ps$vwmat), qvec)
  return(pivotvs)
}								# close function

##### absolute maj #####
# Determines the absolute majority of voting weights.
# inputs: voting weights vector
# output: the absolute majority of the weights (scalar), used for default value of q.
absolute.maj <- function(vw)
{
  sumw <- sum(vw, na.rm = TRUE)
  return( ifelse((sumw %% 2 == 0), (sumw+2)/2, (sumw+1)/2) )
}

##### create.vwmat #####
# Create a matrix of the voting weights (legislatures x rollcalls).
# inputs: absolute (logical scalar), vw (vector of voting weights), votes (legislatures x rollcalls).
# output: a legislatures x rollcalls matrix of the voting weights
create.vwmat <- function(absolute, vw, votes)
{
  # make sure length of vw is equal to number of rows of votes matrix and absolute is logical.
  if ( (nrow(votes) != length(vw)) ) {
    stop("The number of rows in the vote matrix must equal the length of vw (vector of voting weights).")
  }
  if (length(absolute) != 1 || !is.logical(absolute) || is.na(absolute)) {
    stop("absolute is a logical scalar.  It must have a single value of TRUE or FALSE.")
  }
  # determine some dimensions
  i <- nrow(votes); j<- ncol(votes)
  # repeat vw for j columns
  vwmat <- matrix(rep(vw, j), nrow = i, ncol = j)
  # adjust vwmat for attendance if voting rule is a simple k-majority rule
  if(!absolute){
    vwmat[is.na(votes)] <- NA	  		# remove non-attenders from vwmat by assigning them NAs for appropriate rollcalls.
  }
  return(vwmat)
}

##### create.qvec #####
# Create a vector of voting thresholds q (length == num rollcalls).
# inputs: absolute (logical scalar), q (scalar of voting threshold), pr (proportion of yea votes needed for passage), vwmat (matrix of voting weights, legislatures x rollcalls), votes (legislatures x rollcalls).
# output: a vector of voting thresholds used in the pivots function (length == num rollcalls).
create.qvec <- function(absolute=TRUE, q=NULL, pr=0.5001, vwmat, votes)
{
  if (length(absolute) != 1 || !is.logical(absolute) || is.na(absolute)) {
    stop("absolute is a logical scalar.  It must have a single value of TRUE or FALSE.")
  }
  if (!is.matrix(vwmat) || !is.matrix(votes) || nrow(vwmat) != nrow(votes) || ncol(vwmat) != ncol(votes)) {
    stop("vwmat and votes must be matrices of the same dimensions.")
  }
  if (absolute) {
    # If q is not provided, infer it from the weights in vwmat (any column works).
    if (is.null(q)) {
      q <- absolute.maj(vwmat[, 1])
    }
    qvec <- rep(q, ncol(vwmat))
  } else {										# base qvec on proportion attending who cast 0/1 if voting rule is a simple k-majority rule
    vw_attend <- vwmat							# vw_attend is the voting weights of those that attend
    vw_attend[!(votes %in% c(0, 1))] <- NA  		# drop 9 + anything not 0/1
    tattend <- colSums(vw_attend, na.rm=TRUE)		# total number of weighted votes among 0/1 (length == num rollcalls)
    qvec <- ceiling(pr * tattend)					# determine qvec based on proportion of attenders needed to pass proposals
  }
  return(qvec)
}

##### .validate_out_dir #####
# validates the path to the output directory
# inputs: out_dir (character string).
# output: an invisible version of the out_dir (which can be captured).
.validate_out_dir <- function(out_dir)
{
  # Must be a single, non-NA character string
  if (!is.character(out_dir) || length(out_dir) != 1L || is.na(out_dir)) {
    stop("`out_dir` must be a single, non-NA character string.")
  }
  # Not empty or whitespace-only
  if (!nzchar(out_dir) || nchar(trimws(out_dir)) == 0L) {
    stop("`out_dir` cannot be empty.")
  }
  if (.Platform$OS.type == "windows") {
    # Allow Windows drive letters like "C:" at the start; check the remainder
    rest <- sub("^[A-Za-z]:", "", out_dir)
    if (grepl("[<>:\"|?*]", rest)) {
      stop("`out_dir` contains invalid path characters (e.g. <>:\"|?*).")
    }
  } else {
    # Be conservative on non-Windows: reject control characters (includes NUL)
    if (grepl("[[:cntrl:]]", out_dir)) {
      stop("`out_dir` cannot contain control characters.")
    }
  }
  invisible(out_dir)
}

##### validate_vs_sov_args #####
# This function validates the arguments used in vs_sov().  Note, 'votes' is validated later.
# input: most arguments
# output: produces stop message if invalid
validate_vs_sov_args <- function(estimates, weight_nom, absolute, vw, q, pr, dec, out_dir, print_results)
{
  # estimates must exist
  if (is.null(estimates)) {
    stop("You must supply the 'estimates' (i.e., output) from oc, wnom, or MCMCpack for this function to work.")
  }
  # weight_nom must be a logical scalar
  if (!is.logical(weight_nom) || length(weight_nom) != 1 || is.na(weight_nom)) {
    stop("'weight_nom' must be a single logical value (TRUE or FALSE).")
  }
  # absolute must be a logical scalar
  if (!is.logical(absolute) || length(absolute) != 1 || is.na(absolute)) {
    stop("'absolute' must be a single logical value (TRUE or FALSE).")
  }
  # vw, if provided, must be a non-negative integer vector with length matching the number of voters.  NAs are permitted.
  if (!is.null(vw)) {
    if (!is.numeric(vw)) {
      stop("'vw' must be a vector of non-negative integers.  If missing, each member will be assigned an equal voting weight of 1.")
    }
    # check whether non-NA entries are non-negative integers
    bad_int  <- !is.na(vw) & (vw %% 1 != 0)
    bad_sign <- !is.na(vw) & (vw < 0)
    if (any(bad_int) || any(bad_sign)) {
      stop("All non-NA entries of `vw` must be positive integers or zero.")
    }
  }
  # q, if provided, must be a single positive integer
  if (!is.null(q)) {
    if (!is.numeric(q) || length(q) != 1 || is.na(q) || q <= 0 || (q %% 1 != 0)) {
      stop("'q' must be a single positive integer. For absolute==TRUE, the default is the smallest majority of the voting weights vw.")
    }
  }

  # pr must be a single numeric value within (.5, 1]
  if (!is.numeric(pr) || length(pr) != 1 || is.na(pr) || pr <= 0.5 || pr > 1) {
    stop("'pr' must be a single number within 0.5 < pr <= 1. For absolute==FALSE, the default is .5001.")
  }

  # dec must be a single integer between 1 and 9
  if (!is.numeric(dec) || length(dec) != 1 || is.na(dec) || dec <= 0 || dec >= 10 || (dec %% 1 != 0)) {
    stop("'dec' must be a single integer between 1 and 9.  The default is 3.")
  }

  # validate out_dir
  .validate_out_dir(out_dir)

  # print_results must be a logical scalar
  if (!is.logical(print_results) || length(print_results) != 1 || is.na(print_results)) {
    stop("'print_results' must be a single logical value (TRUE or FALSE).")
  }
}


##### validate_sov_args #####
# This function validates the arguments used in sov().
# input: all arguments except estimates
# output: produces stop message if invalid
validate_sov_args <- function(estimates, weight_nom, absolute, vw, q, pr, nPoints1, nPoints2, dec, out_dir, print_results)
{
  # estimates must exist
  if (is.null(estimates)) {
    stop("You must supply the `estimates` (i.e., output) from oc, wnom, or MCMCpack for this function to work.\n If you have ideal points already, consider another function.")
  }

  #  weight_nom must be a logical scalar
  if (!is.logical(weight_nom) || length(weight_nom) != 1 || is.na(weight_nom)) {
    stop("`weight_nom` must be a single logical value (TRUE or FALSE).")
  }
  # absolute must be a logical scalar
  if (!is.logical(absolute) || length(absolute) != 1 || is.na(absolute)) {
    stop("'absolute' must be a single logical value (TRUE or FALSE).")
  }
  # vw, if provided, must be a non-negative integer vector with length matching the number of voters.  NAs are permitted.
  if (!is.null(vw)) {
    if (!is.numeric(vw)) {
      stop("'vw' must be a vector of non-negative integers.  If missing, each member will be assigned an equal voting weight of 1.")
    }
    # check whether non-NA entries are non-negative integers
    bad_int  <- !is.na(vw) & (vw %% 1 != 0)
    bad_sign <- !is.na(vw) & (vw < 0)
    if (any(bad_int) || any(bad_sign)) {
      stop("All non-NA entries of 'vw' must be positive integers or zero.")
    }
  }
  # q, if provided, must be a single positive integer
  if (!is.null(q)) {
    if (!is.numeric(q) || length(q) != 1 || is.na(q) || q <= 0 || (q %% 1 != 0)) {
      stop("'q' must be a single positive integer. For absolute==TRUE, the default is the smallest majority of the voting weights vw.")
    }
  }

  # pr must be a single numeric value within (.5, 1]
  if (!is.numeric(pr) || length(pr) != 1 || is.na(pr) || pr <= 0.5 || pr > 1) {
    stop("'pr' must be a single number within 0.5 < pr <= 1. For absolute==FALSE, the default is .5001.")
  }

  # nPoints1 must be a single non-negative integer
  if (!is.numeric(nPoints1) || length(nPoints1) != 1 || is.na(nPoints1) || nPoints1 < 0 || (nPoints1 %% 1 != 0)) {
    stop("'nPoints1' must be a single non-negative integer. The default is 360.")
  }

  # nPoints2 must be a single non-negative integer
  if (!is.numeric(nPoints2) || length(nPoints2) != 1 || is.na(nPoints2) || nPoints2 < 0 || (nPoints2 %% 1 != 0)) {
    stop("'nPoints2' must be a single non-negative integer. The default is 360.")
  }

  # dec must be a single integer between 1 and 9
  if (!is.numeric(dec) || length(dec) != 1 || is.na(dec) || dec <= 0 || dec >= 10 || (dec %% 1 != 0)) {
    stop("'dec' must be a single integer between 1 and 9. The default is 3.")
  }

  # validate out_dir
  .validate_out_dir(out_dir)

  # print_results must be a logical scalar
  if (!is.logical(print_results) || length(print_results) != 1 || is.na(print_results)) {
    stop("'print_results' must be a single logical value (TRUE or FALSE).")
  }
}

##### validate_vs_sov_user_args #####
# This function validates the arguments used in vs_sov_user().
# input: most arguments
# output: produces stop message if invalid
validate_vs_sov_user_args <- function(ideals, normals, midpoints, weight_nom, absolute, vw, q, pr, votes, dec, out_dir, print_results)
{
  # tolerance for comparing length of normal vectors, normals, to 1.
  tol <- 1e-5
  # exactly one of normals and midpoints must be non-NULL
  if ((is.null(normals) && is.null(midpoints)) || (!is.null(normals) && !is.null(midpoints))) {
    stop("You must include 'normals' or 'midpoints' as an argument.  Omit the other one.")
  }
  # ideals must be a matrix
  if (!is.matrix(ideals) || !is.numeric(ideals)){
    stop("'ideals' must be a numeric matrix of ideal points (legislators x dimensions).")
  }

  # If normals are active, it must be a matrix; if midpoints are active, it must be a matrix
  if (!is.null(normals)){
    # with normals supplied, we will warn user if any ideal points are outside the unit hypersphere
    validate.scaling(ideals)
    # normals must be a matrix with values in [-1, 1]
    normals <- as.matrix(normals)
    if (!is.matrix(normals) || !is.numeric(normals)) {
      stop("'normals' must be a numeric matrix of normal vectors (rollcalls x dimensions).")
    }
    if ( any(normals < (-1-tol) | normals > (1+tol), na.rm = TRUE)){
      stop("All entries of 'normals' must lie between -1 and 1, or be NA; and the length of each normal vector must equal 1.")
    }
    # Each row of normals must be a unit vector with length roughly equal to 1.
    row_lengths <- apply(normals, 1, function(r) {
      if ( all(is.na(r)) ) return(NA_real_)
      sqrt(sum(r^2, na.rm = TRUE))
    })
    # find bad rows, i.e., those with length other than roughly 1.
    bad <- ( !is.na(row_lengths) ) & ( abs(row_lengths - 1) > tol )
    if (any(bad)){
      bad_idx <- which(bad)
      bad_lengths <- row_lengths[bad_idx]
      # build a vector of "row length" strings
      reports <- sprintf("%d: %0.*f", bad_idx, 3, bad_lengths)  	# 3 decimals
      # if too many, truncate to first 10 and note how many more
      max_show <- 10
      if (length(reports) > max_show) {
        extra <- length(reports) - max_show
        reports <- c(reports[1:max_show], sprintf("...and %d more rows", extra))
      }
      stop("The following rows of `normals` are not unit length:\n", paste(reports, collapse = "\n"))
    }
  } else {
    # midpoints must be a numeric matrix
    midpoints <- as.matrix(midpoints)
    if (!is.matrix(midpoints) || !is.numeric(midpoints)) {
      stop("'midpoints' must be a numeric matrix of the intersections of the cut plane and the normal vector (rollcalls x dimensions).")
    }
    # with midpoints supplied, we will rescale ideal points and midpoints to the "unit" hypersphere (if necessary)
    trans_val <- rescale(ideals, midpoints)
    ideals <- trans_val$ideals
    midpoints <- trans_val$midpoints
  }

  # Number of rows in ideals must match number of rows in votes
  if (nrow(ideals) != nrow(votes)) {
    stop(sprintf("Number of legislators in `ideals` (%d) must equal number of rows in `votes` (%d).", nrow(ideals), nrow(votes) ))
  }

  # Number of columns of votes should match number of rows of normals/midpoints; Number of columns of ideals should match number of columns of normals/midpoints
  if (!is.null(normals)) {
    # rollcalls in votes should match rows of normals
    if (ncol(votes) != nrow(normals)) {
      stop(sprintf("Number of columns in `votes` (%d) must equal number of rows in `normals` (%d) and the number of rollcalls.", ncol(votes), nrow(normals) ))
    }
    # dimensions of ideals should match dimensions of normals
    if (ncol(ideals) != ncol(normals)) {
      stop(sprintf("Number of columns in `ideals` (%d) must equal number of columns in `normals` (%d) and the number of dimensions.", ncol(ideals), ncol(normals) ))
    }
  } else {												# In this case, the user inputed midpoints, not normals
    # rollcalls in votes should match rows of midpoints
    if (ncol(votes) != nrow(midpoints)) {
      stop(sprintf("Number of columns in `votes` (%d) must equal number of rows in `midpoints` (%d) and the number of rollcalls.", ncol(votes), nrow(midpoints) ))
    }
  }

  #  weight_nom must be a logical scalar
  if (!is.logical(weight_nom) || length(weight_nom) != 1 || is.na(weight_nom)) {
    stop("`weight_nom` must be a single logical value (TRUE or FALSE).")
  }
  # absolute must be a logical scalar
  if (!is.logical(absolute) || length(absolute) != 1 || is.na(absolute)) {
    stop("`absolute` must be a single logical value (TRUE or FALSE).")
  }
  # vw, if provided, must be a non-negative integer vector with length matching the number of voters.  NAs are permitted.
  if (!is.null(vw)) {
    if (!is.numeric(vw)) {
      stop("`vw` must be a vector of non-negative integers.  If missing, each member will be assigned an equal voting weight of 1.")
    }
    bad_int  <- !is.na(vw) & (vw %% 1 != 0)
    bad_sign <- !is.na(vw) & (vw < 0)
    if (any(bad_int) || any(bad_sign)) {
      stop("All non-NA entries of `vw` must be positive integers or zero.")
    }
  }
  # Check length when the user actually supplies vw
  if (!is.null(vw) && length(vw) != nrow(votes)) {
    stop(sprintf(
      "Length of `vw` (%d) must match the number of voters in the vote matrix `votes` (nrow(votes) = %d).",
      length(vw), nrow(votes)
    ))
  }
  # q, if provided, must be a single positive integer
  if (!is.null(q)) {
    if (!is.numeric(q) || length(q) != 1 || is.na(q) || q <= 0 || (q %% 1 != 0)) {
      stop("`q` must be a single positive integer. For absolute==TRUE, the default is the smallest majority of the voting weights vw.")
    }
  }

  # pr must be a single numeric value within (.5, 1]
  if (!is.numeric(pr) || length(pr) != 1 || is.na(pr) || pr <= 0.5 || pr > 1) {
    stop("`pr` must be a single number within 0.5 < pr <= 1. For absolute==FALSE, the default is .5001.")
  }

  # dec must be a single integer between 1 and 9
  if (!is.numeric(dec) || length(dec) != 1 || is.na(dec) || dec <= 0 || dec >= 10 || (dec %% 1 != 0)) {
    stop("`dec` must be a single integer between 1 and 9.  The default is 3.")
  }

  # validate out_dir
  .validate_out_dir(out_dir)

  # print_results must be a logical scalar
  if (!is.logical(print_results) || length(print_results) != 1 || is.na(print_results)) {
    stop("`print_results` must be a single logical value (TRUE or FALSE).")
  }

  # Check votes
  # check if all elements are 0, 1, 9, or NA
  if (!all( is.na(votes) | votes %in% c(0, 1, 9) )) {
    stop("The vote matrix can only contain 1=yea, 0=nay, 9=attended but abstain, and NAs.")
  }
  # check if the matrix contains at least one 0 and one 1.
  if (!any(votes == 0, na.rm = TRUE) || !any(votes == 1, na.rm = TRUE)) {
    stop("The vote matrix must contain at least one 1=yea and one 0=nay.")
  }
  # if the matrix does not contain at least one 9, provide a message to the user.
  #      if ( !any(votes == 9, na.rm=TRUE) ) {
  #  	    message("Your vote matrix does not contain any 9s.\n The function works, just know that individuals who attend but do not vote (9s) can affect pivots.")
  #	  }
  # return ideals (potentially modified), matrix of normals or midpoints (whichever not NULL), and vector vw
  return(list(ideals=ideals, normals=normals, midpoints=midpoints, vw=vw))
}

##### validate_sov_user_args #####
# This function validates the arguments used in sov_user().
# input: all arguments except estimates
# output: produces stop message if invalid
validate_sov_user_args <- function(ideals, av, weight_nom, absolute, vw, q, pr, nPoints1, nPoints2, dec, out_dir, print_results)
{
  # ideals must be a matrix
  if (!is.matrix(ideals) || !is.numeric(ideals)){
    stop("'ideals' must be a numeric matrix of ideal points (legislators x dimensions).")
  }

  # av must be a vector containing only 1 or NA
  if ( !is.numeric(av) ||  is.null(av) ) {
    stop("`av` must be a numeric vector with all voters included in the analysis coded as 1 and all excluded voters coded NA.")
  }
  if ( any(!(is.na(av) | av == 1)) ) {
    stop("All entries of `av` must be either 1 (included in the analysis) or NA (not included).")
  }
  # number of voters in av must match number of voters in ideals
  if (length(av) != nrow(ideals)) {
    stop("Length of attendance vector av must match the number of rows in ideals; both represent the number of voters.")
  }

  # weight_nom must be a logical scalar
  if (!is.logical(weight_nom) || length(weight_nom) != 1 || is.na(weight_nom)) {
    stop("`weight_nom` must be a single logical value (TRUE or FALSE).")
  }

  # absolute must be a logical scalar
  if (!is.logical(absolute) || length(absolute) != 1 || is.na(absolute)) {
    stop("`absolute` must be a single logical value (TRUE or FALSE).")
  }

  # vw, if provided, must be a non-negative integer vector with length matching the number of voters.  NAs are permitted.
  if (!is.null(vw)) {
    if (!is.numeric(vw)) {
      stop("`vw` must be a vector of non-negative integers.  If missing, each member will be assigned an equal voting weight of 1.")
    }
    # check whether non-NA entries are non-negative integers
    bad_int  <- !is.na(vw) & (vw %% 1 != 0)
    bad_sign <- !is.na(vw) & (vw < 0)
    if (any(bad_int) || any(bad_sign)) {
      stop("All non-NA entries of `vw` must be positive integers or zero.")
    }
    # number of voters in vw must match number of voters in ideals
    if (length(vw) != nrow(ideals)) {
      stop("Length of voting weights `vw` must match the number of rows in ideals; both represent the number of voters.")
    }
  }
  # q, if provided, must be a single positive integer
  if (!is.null(q)) {
    if (!is.numeric(q) || length(q) != 1 || is.na(q) || q <= 0 || (q %% 1 != 0)) {
      stop("`q` must be a single positive integer. For absolute==TRUE, the default is the smallest majority of the voting weights vw.")
    }
  }

  # pr must be a single numeric value within (.5, 1]
  if (!is.numeric(pr) || length(pr) != 1 || is.na(pr) || pr <= 0.5 || pr > 1) {
    stop("`pr` must be a single number within 0.5 < pr <= 1. For absolute==FALSE, the default is .5001.")
  }

  # nPoints1 must be a single non-negative integer
  if (!is.numeric(nPoints1) || length(nPoints1) != 1 || is.na(nPoints1) || nPoints1 < 0 || (nPoints1 %% 1 != 0)) {
    stop("`nPoints1` must be a single non-negative integer. The default is 360.")
  }

  # nPoints2 must be a single non-negative integer
  if (!is.numeric(nPoints2) || length(nPoints2) != 1 || is.na(nPoints2) || nPoints2 < 0 || (nPoints2 %% 1 != 0)) {
    stop("`nPoints2` must be a single non-negative integer. The default is 360.")
  }

  # dec must be a single integer between 1 and 9
  if (!is.numeric(dec) || length(dec) != 1 || is.na(dec) || dec <= 0 || dec >= 10 || (dec %% 1 != 0)) {
    stop("`dec` must be a single integer between 1 and 9. The default is 3.")
  }

  # validate out_dir
  .validate_out_dir(out_dir)

  # print_results must be a logical scalar
  if (!is.logical(print_results) || length(print_results) != 1 || is.na(print_results)) {
    stop("`print_results` must be a single logical value (TRUE or FALSE).")
  }
}

##### validate_vote_matrix #####
# Evaluates whether the vote matrix provided by the user contains acceptable values and at least one yea==1 and one nay==0 code
# input: votes matrix, among others
# output: produces stop message if not correct
validate_vote_matrix <- function(ideals, normals, vw, votes)
{
  # check if all elements are 0, 1, 9, or NA
  if(!all(is.na(votes) | votes %in% c(0, 1, 9) )) {
    stop("The vote matrix can only contain 1=yea, 0=nay, 9=attended but abstain, and NAs.")
  }
  # check if the matrix contains at least one 0 and one 1.
  if(!any(votes == 0, na.rm = TRUE) || !any(votes == 1, na.rm = TRUE)) {
    stop("The vote matrix must contain at least one 1=yea and one 0=nay.")
  }
  # if the matrix does not contain at least one 9, provide a message to the user.
  #      if( !any(votes == 9, na.rm = TRUE)) {
  # 	    message("Your vote matrix does not contain any 9s.\n The function works, just know that individuals who attend but abstain (9s) can affect pivots.")
  #	  }
  # make sure the number of dimensions match other matrices
  if( nrow(ideals) != nrow(votes) ){
    stop(sprintf("The number of rows in 'ideals' (%d) must match number of rows in the vote matrix 'votes' (%d).", nrow(ideals), nrow(votes)))
  }
  if( nrow(normals) != ncol(votes) ){
    stop(sprintf("The number of rows in 'normals' (%d) must match the number of columns in the vote matrix `votes` (%d).", nrow(normals), ncol(votes)))
  }
  if(length(vw) != nrow(votes)) {
    stop(sprintf("Length of `vw` (%d) must match the number of voters in the vote matrix `votes` (nrow(votes) = %d).", length(vw), nrow(votes)))
  }
}

##### validate_av #####
# Evaluates attendance vector and code av=1 for each voter if av is null.
# input: votes matrix, among others
# output: produces stop message if not correct
validate_av <- function(ideals, vw, av)
{
  # assign default values to av and vw if NULL
  if(is.null(av)) {
    av <- rep(1, length=nrow(ideals))
  }
  if(is.null(vw)) {
    vw <- rep(1, length=nrow(ideals))
  }
  # av must be a vector containing only 1 or NA
  if ( !is.numeric(av) ) {
    stop("`av` must be a numeric vector with all voters included in the analysis coded 1 and all excluded voters coded NA.")
  }
  if ( any(!(is.na(av) | av == 1)) ) {
    stop("All entries of `av` must be either 1 (included in the analysis) or NA (excluded).")
  }
  # check if the vector contains at least one 1.
  if (!any(av == 1, na.rm = TRUE)) {
    stop("The attendance vector must contain at least one 1 (included in the analysis).")
  }
  # make sure the number of dimensions match other matrices
  if ( nrow(ideals) != length(av) ){
    stop(sprintf("The number of rows in 'ideals' (%d) must match the length of the attendance vector 'av' (%d).", nrow(ideals), length(av)))
  }
  if (length(vw) != length(av)) {
    stop(sprintf("Length of voting weights `vw` (%d) must match the length of the attendance vector 'av' (%d).", length(vw), length(av)))
  }
  return(list(vw=vw,av=av))
}

##### feasible q #####
# Determine whether the number of weighted votes is at least as great as q (for absolute==TRUE).
# inputs: absolute (logical scalar), vwmat (legislatures x rollcalls matrix of the voting weights), qvec (vector of voting thresholds q), votes (legislatures x rollcalls).
# output: qvec with infeasible cases set to the sum of weighted votes attending.  A warning is also produced if such cases exist.
feasible.q <- function(absolute, vwmat, qvec, votes)
{
  if (!isTRUE(absolute)) {
    stop("feasible.q() applies only when absolute == TRUE. It cannot be applied to simple k-majority percentages.")
  }
  if ( !identical( dim(vwmat), dim(votes) ) ){
    stop("vwmat must have the same number of dimensions as matrix votes.")
  }
  # calculate total number of weighted votes by rollcall (with weights from non-voters removed)
  vwmat[is.na(votes)] <- NA
  sumw <- colSums(vwmat, na.rm = TRUE)
  # deterimine feasible q, i.e. the sum of weighted votes is at least as great as q and all weights are not NAs.
  feasible <- (sumw >= qvec) & (sumw > 0)
  # for infeasible roll calls, replace qvec with sum of weights
  qvec[!feasible] <- sumw[!feasible]
  # identify the names of infeasbile votes
  infeasible_votes <- colnames(votes)[!feasible]
  # warn users that they have infeasible votes.
  if(length(infeasible_votes) > 0){
    warning(paste(c("Some of your quotas are greater than the sum of the weighted votes among those attending.\n  For those rollcalls, they have been reset to the sum of weighted votes among those attending.\n  The roll calls are: ", infeasible_votes), collapse=" ")  )
  }
  return(qvec = qvec)
}

##### get_pivot_names #####
# identifies the names of the voters with ideal points at pivotv and saves an excel ready dataframe
# inputs: projv (projected values of ideal points for each rollcall: #voters x #rollcalls); pivotv (projected pivot-value for each rollcall: length = num_rc); votes (for rollcall names)
# output: roll call position, roll call names, name of voter(s) who pivoted the roll call -- need not be unique.
#	note:	rollcalls without pivots, typically because a normal vector was not estimated, are marked NA.
get_pivot_names <- function(projv, pivotv, votes)
{
  if (is.null(rownames(projv))) {
    stop("projv must have row names (voter names) in order to identify pivots.")
  }
  if (length(pivotv) != ncol(projv) | length(pivotv) != ncol(votes)) {
    stop(sprintf("Length of pivotv (%d) must have the same number of columns in projv (%d) and votes (%d).", length(pivotv), ncol(projv), ncol(votes)))
  }
  rc_names <- colnames(votes)
  if (is.null(rc_names)) rc_names <- paste0("V", seq_len(ncol(votes)))
  # Shell
  result <- data.frame(
    Position = seq_len(ncol(votes)),
    RC_num   = rc_names,
    row.names = NULL,
    check.names = FALSE
  )
  valid_cols_idx <- which(!is.na(pivotv))
  if (length(valid_cols_idx) == 0L) {
    # No valid pivots at all -> single Pivot col of NA
    result$Pivot <- NA_character_
    return(result)
  }
  # Build list of pivot names per valid RC
  tied_lists <- vector("list", length(valid_cols_idx))
  names(tied_lists) <- rc_names[valid_cols_idx]
  for (k in seq_along(valid_cols_idx)) {
    j <- valid_cols_idx[k]
    eq <- projv[, j, drop = TRUE] == pivotv[j]
    tied_lists[[k]] <- rownames(projv)[which(eq)]
  }
  max_ties <- max(vapply(tied_lists, length, integer(1)))
  if (max_ties <= 1) {
    # Single "Pivot" column (or NA) for all RCs
    piv <- rep(NA_character_, ncol(votes))
    for (k in seq_along(valid_cols_idx)) {
      j <- valid_cols_idx[k]
      vn <- tied_lists[[k]]
      piv[j] <- if (length(vn)) vn[1] else NA_character_
    }
    result$Pivot <- piv
    return(result)
  }
  # Multiple pivots possible: Pivot1..PivotK, assign RC-by-RC
  pivot_mat <- matrix(NA_character_, nrow = ncol(votes), ncol = max_ties)
  for (k in seq_along(valid_cols_idx)) {
    j  <- valid_cols_idx[k]
    vn <- tied_lists[[k]]
    row_vals <- c(vn, rep(NA_character_, max_ties - length(vn)))
    pivot_mat[j, ] <- row_vals
  }
  colnames(pivot_mat) <- paste0("Pivot", seq_len(max_ties))
  cbind(result, pivot_mat, row.names = NULL)
}

##### extract angles #####
# Determines the angles of each normal vector in both radians and degrees (for excel output).
# inputs: matrix of normal vectors (rollcalls x dimensions).
# output: dataframe with position, rc_num, normal vector, angle radians, angle in degrees
# Note: this function assumes standard spherical coordinates, where in 3D:
# 	phi: is the inclination from +z-axis downward
#		phi = 0 degrees is pointing straight up (z-axis)
#		phi = 90 degrees is the x-y plane
#		phi = 180 degrees is straight down (-z)
extract_angles <- function(normals)
{
  if (!is.matrix(normals)) { stop("Input must be a matrix.") }

  # Basic setup
  D <- ncol(normals)
  if (D < 1 | D > 4) stop("Function supports only 1 <= D <= 4.")
  J <- nrow(normals)
  row_ids <- rownames(normals)
  if (is.null(row_ids)) row_ids <- paste0("v", seq_len(J))
  # generate dataframe, additional columns added dynamically
  result <- data.frame(Position = seq_len(J),
                       RC_num = row_ids)
  # Add normal vectors to result with dynamic names
  for (d in seq_len(D)) {
    result[[paste0("normVector", d, "D")]] <- normals[, d]
  }
  # angle calculations
  if (D == 1) {
    # For 1D, angle is 0 if positive, pi if negative
    angle_rad <- ifelse(normals[,1] >= 0, 0, pi)
    angle_deg <- angle_rad * 180 / pi
    result$Angle_Radians <- angle_rad
    result$Angle_Degrees <- angle_deg
  } else if (D == 2) {
    angle_rad <- atan2(normals[,2], normals[,1])
    angle_rad <- ifelse(angle_rad < 0, angle_rad + 2*pi, angle_rad)
    angle_deg <- angle_rad * 180 / pi
    result$Angle_Radians <- angle_rad
    result$Angle_Degrees <- angle_deg
  } else if (D == 3) {
    x <- normals[,1]
    y <- normals[,2]
    z <- normals[,3]
    azimuth_rad <- atan2(y, x)
    azimuth_rad <- ifelse(azimuth_rad < 0, azimuth_rad + 2*pi, azimuth_rad)
    inclination_rad <- acos(z)
    result$Angle_Radians1 <- azimuth_rad
    result$Angle_Radians2 <- inclination_rad
    result$Angle_Degrees1 <- azimuth_rad * 180 / pi
    result$Angle_Degrees2 <- inclination_rad * 180 / pi
  } else if (D == 4) {
    x1 <- normals[,1]
    x2 <- normals[,2]
    x3 <- normals[,3]
    x4 <- normals[,4]
    azimuth1 <- atan2(x2, x1)
    azimuth1 <- ifelse(azimuth1 < 0, azimuth1 + 2*pi, azimuth1)
    azimuth2 <- atan2( x3, sqrt(x1^2 + x2^2) )
    azimuth2 <- ifelse(azimuth2 < 0, azimuth2 + 2*pi, azimuth2)
    inclination <- acos(x4)  # since unit vector
    result$Angle_Radians1 <- azimuth1
    result$Angle_Radians2 <- azimuth2
    result$Angle_Radians3 <- inclination
    result$Angle_Degrees1 <- azimuth1 * 180 / pi
    result$Angle_Degrees2 <- azimuth2 * 180 / pi
    result$Angle_Degrees3 <- inclination * 180 / pi
  }
  return(result)
}

##### find_normals_nd #####
# Generate all unit normal vectors over a hypersphere for 1-4 dimensions.
# inputs: D, the number of dimensions;
#			nPoints1, the number of points sampled along the azimuth (the first spherical coordinate the azimuth);
#			nPoints2, the number of points sampled along the inclination (the second and third spherical coordinate).
# output: normal vectors for all combinations of angles in increments nPoints1 and nPoints2
# Note: this function assumes standard spherical coordinates, similar to extract_angles():
find_normals_nd <- function(D, nPoints1 = 360, nPoints2 = 360)
{
  if (!is.numeric(D) || length(D) != 1) {
    stop("Dimensions (D) must be a single integer.")
  }
  if (D < 1 || D > 4) {
    stop("Dimensions must be at least 1 and no more than 4.")
  }

  if (D == 1) {
    # 1D line: two directions at +1 and -1
    normals <- matrix(c(1, -1), ncol = 1)

  } else if (D == 2) {
    # 2D circle: angle from 0 to 2*pi, avoiding duplicate endpoints.
    theta <- seq(0, 2 * pi, length.out = nPoints1 + 1)[- (nPoints1 + 1)]
    normals <- cbind(cos(theta), sin(theta))

  } else if(D == 3) {
    # 3D Sphere: azimuth and inclination each from 0 to 2*pi, avoiding duplicate endpoints.
    theta <- seq(0, 2 * pi, length.out = nPoints1 + 1)[- (nPoints1 + 1)]  # azimuth
    phi   <- seq(0, 2 * pi, length.out = nPoints2 + 1)[- (nPoints2 + 1)]  # inclination
    grid  <- expand.grid(theta = theta, phi = phi)

    # Standard spherical coords, with phi measured from +z downward
    x <- with(grid, sin(phi) * cos(theta))
    y <- with(grid, sin(phi) * sin(theta))
    z <- with(grid, cos(phi))
    normals <- cbind(x, y, z)

  } else if (D == 4) {
    # Hypersphere: three angles each from 0 to 2*pi, avoiding duplicate endpoints
    theta1 <- seq(0, 2 * pi, length.out = nPoints1 + 1)[- (nPoints1 + 1)]
    theta2 <- seq(0, 2 * pi, length.out = nPoints2 + 1)[- (nPoints2 + 1)]
    theta3 <- seq(0, 2 * pi, length.out = nPoints2 + 1)[- (nPoints2 + 1)]
    grid   <- expand.grid(theta1 = theta1, theta2 = theta2, theta3 = theta3)

    x1 <- with(grid, cos(theta1))
    x2 <- with(grid, sin(theta1) * cos(theta2))
    x3 <- with(grid, sin(theta1) * sin(theta2) * cos(theta3))
    x4 <- with(grid, sin(theta1) * sin(theta2) * sin(theta3))
    normals <- cbind(x1, x2, x3, x4)
  }

  # Normalize to unit length (to compensate for numerical drift)
  normals <- normals / sqrt(rowSums(normals^2))
  colnames(normals) <- paste0("dim", seq_len(D))

  return(normals)
}


##### summarize_pivots #####
# determines the number of pivots across rollcalls and the vs-sov, which it reports along with information from ideals.
# inputs: ideals (#voters x #dimensions); pivot_by_rc (name of pivot(s) for each rollcall: (#rollcalls x 3+)
# output: dataframe with: name, coord1D, coord2D, ... coordkD, num_pivots, vs-sov
summarize_pivots <- function(ideals, pivot_by_rc)
{
  # Extract legislator names
  names_vec <- rownames(ideals)
  # Extract pivot columns from pivot_by_rc (those that start with "Pivot")
  pivot_cols <- grep("^Pivot", colnames(pivot_by_rc), value = TRUE)
  pivot_data <- pivot_by_rc[, pivot_cols, drop = FALSE]
  # Flatten pivot values into a single vector and tabulate counts
  pivot_names <- unlist(pivot_data, use.names = FALSE)
  pivot_counts <- table(pivot_names)
  # Initialize num_pivots and vs-sov for all legislators
  num_pivots <- sapply(names_vec, function(name) {
    if (name %in% names(pivot_counts)) pivot_counts[[name]] else 0
  })
  # Count how many roll calls have at least one pivot
  valid_rcs <- rowSums(!is.na(pivot_data)) > 0
  num_valid_rcs <- sum(valid_rcs)
  # Compute proportion of valid roll calls that a legislator pivoted
  vs_sov <- num_pivots / num_valid_rcs
  # Combine everything into a data frame
  output_df <- data.frame(
    name = names_vec,
    ideals,
    num_pivots = num_pivots,
    vs_sov = vs_sov,
    row.names = NULL
  )
  return(output_df)
}

##### excel_results_vs_sov #####
# this function saves results in three different tabs of an excel file  -- for use in vs_sov().
# inputs:
#     pivot_summary: data.frame of sov summary
#     pivot_by_rc:   data.frame of pivots by rollcall
#     nv_and_angles: data.frame of normal vectors & angles
#     file:          output filename
#     dec:           number of decimals for rounding sov
#		out_dir:	   subdirectory in which the output files will be saved.
# output: excel file with three tabs saved to "file"
excel_results_vs_sov <- function(pivot_summary, pivot_by_rc, nv_and_angles, file = "vs-sovs.xlsx", dec = 3, out_dir  = "output")
{
  ## Warnings and general.
  # make sure vs_sov is a column name in pivot_summary
  if (!"vs_sov" %in% names(pivot_summary)) {
    stop("`pivot_summary` must contain a column named 'vs_sov'")
  }
  # make sure out_dir exists on the user's machine
  if (!dir.exists(out_dir)) {
    dir.create(out_dir, recursive = TRUE)
  }
  # round the column vs_sov in pivot_summary to `dec` decimals
  pivot_summary$vs_sov <- round(pivot_summary$vs_sov, dec)
  # create workbook
  wb <- createWorkbook()
  ## Sheet 1: sov
  addWorksheet(wb, "sov")
  # caption
  writeData(wb, "sov", x = "The Number of Pivots and VS-SOV by Legislator", startRow = 1, startCol = 1)
  # data (from row 2)
  writeData(wb, "sov", pivot_summary, startRow = 2, startCol = 1, headerStyle = createStyle(textDecoration = "bold"))
  # bold the caption
  addStyle(wb, "sov", style = createStyle(textDecoration = "bold"), rows = 1, cols = 1, gridExpand = TRUE)
  ## Sheet 2: name_pivots_by_rollcall
  addWorksheet(wb, "name_pivots_by_rollcall")
  # caption
  writeData(wb, "name_pivots_by_rollcall", x = "Pivots by Roll Call", startRow = 1, startCol = 1)
  # data (from row 2)
  writeData(wb, "name_pivots_by_rollcall", pivot_by_rc, startRow = 2, startCol = 1, headerStyle = createStyle(textDecoration = "bold"))
  # bold the caption
  addStyle(wb, "name_pivots_by_rollcall", style = createStyle(textDecoration = "bold"), rows = 1, cols = 1, gridExpand = TRUE)
  ## Sheet 3: nv_and_angles
  addWorksheet(wb, "nv_and_angles")
  # caption
  writeData(wb, "nv_and_angles", x = "Normal Vectors and Roll Call Angles (pointing toward yea)", startRow = 1, startCol = 1)
  # data (from row 2)
  writeData(wb, "nv_and_angles", nv_and_angles, startRow = 2, startCol = 1, headerStyle = createStyle(textDecoration = "bold"))
  # bold the caption
  addStyle(wb, "nv_and_angles", style = createStyle(textDecoration = "bold"), rows = 1, cols = 1, gridExpand = TRUE)
  # Save workbook
  out_path <- file.path(out_dir, file)
  saveWorkbook(wb, out_path, overwrite = TRUE)
}

##### excel_results_sov #####
# this function saves results in two different tabs of an excel file -- for use in sov().
# inputs:
#     pivot_summary: 	data.frame of sov summary
#     pivot_by_angle:	data.frame of pivots by angle
#     file:          output filename
#     dec:           number of decimals for rounding sov
#		out_dir:	   subdirectory in which the output files will be saved.
# output: excel file with two tabs saved to "file"
excel_results_sov <- function(pivot_summary, pivot_by_angle, file = "sovs.xlsx", dec = 3, out_dir  = "output")
{
  ## Warnings and general.
  # make sure sov is a column name in pivot_summary
  if (!"sov" %in% names(pivot_summary)) {
    stop("`pivot_summary` must contain a column named 'sov'")
  }
  # make sure out_dir exists on the user's machine
  if (!dir.exists(out_dir)) {
    dir.create(out_dir, recursive = TRUE)
  }
  # round the column sov in pivot_summary to `dec` decimals
  pivot_summary$sov <- round(pivot_summary$sov, dec)
  # create workbook
  wb <- createWorkbook()
  ## Sheet 1: sov
  addWorksheet(wb, "sov")
  # caption
  writeData(wb, "sov", x = "The Number of Pivots and SOV by Legislator", startRow = 1, startCol = 1)
  # data (from row 2)
  writeData(wb, "sov", pivot_summary, startRow = 2, startCol = 1, headerStyle = createStyle(textDecoration = "bold"))
  # bold the caption
  addStyle(wb, "sov", style = createStyle(textDecoration = "bold"), rows = 1, cols = 1, gridExpand = TRUE)
  ## Sheet 2: pivots_by_angle
  addWorksheet(wb, "pivots_by_angle")
  # caption
  writeData(wb, "pivots_by_angle", x = "Pivots by Angle", startRow = 1, startCol = 1)
  # data (from row 2)
  writeData(wb, "pivots_by_angle", pivot_by_angle, startRow = 2, startCol = 1, headerStyle = createStyle(textDecoration = "bold"))
  # bold the caption
  addStyle(wb, "pivots_by_angle", style = createStyle(textDecoration = "bold"), rows = 1, cols = 1, gridExpand = TRUE)
  # Save workbook
  out_path <- file.path(out_dir, file)
  saveWorkbook(wb, out_path, overwrite = TRUE)
}
