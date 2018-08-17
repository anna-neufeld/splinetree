
#' Custom rpart split function for spline random forests
#'
#' Wrapper for split function required for the random forest functionality. This function is called once per covariate at each potential split.
#' Implements the random selection of variables; each variable is randomly selected to be included or excluded.
#'
#' @param y the responses at this node
#' @param wt the weight of the responses
#' @param x the X data for this covariate
#' @param parms the basis matrix for the spline and the proportion of variables randomly sampled (diceProb)
#' @param continuous value is handled internally by rpart - tells us if this covariate is continuous or categorical (factor).
#' @keywords internal
splineforest_split <- function(y, wt, x, parms = NULL, continuous) {
  diceProb <- parms[[2]]
  dice <- runif(1)

  if(dice < diceProb) {
    # If chosen as a possible split var, call regular split function.
    res <- spline_split(y, wt, x, parms, continuous)
    goodness <- res[[1]]
    direction <- res[[2]]
  } else {
    # If not chosen as a split var, set goodness to be 0 so that it won't get chosen
    goodness <- rep(0, length(x) - 1)
    direction <- rep(-1, length(x) - 1)
  }
  list(goodness = goodness, direction = direction)
}




