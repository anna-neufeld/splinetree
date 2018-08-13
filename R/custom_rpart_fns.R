#' Custom rpart init function
#'
#' The init function is required for custom rpart functionality. This function initializes every node. The init function is responsible for defining the summary function
#' that will be used by rpart's summary function if you call summary() on this tree object. The init function also passes forward its arguments and tells rpart
#' the dimension of the response variable. This function is called internally by rpart; the details are not important for the end user.
#'
#' @param y Response data, which will be estimated spline coefficients
#' @param  offset Required by rpart, but never used by splinetree, so its value will always be NULL
#' @param parms rpart's custom split functionality allows optional parameters to be passed through the
#' splitting functions. In the splinetree package, the parms parameter is used to hold a list of length
#' 1 or 2 containing a spline basis matrix and the probability that a variable will be selected at a split.
#' The probability is only used in splineforests. For splinetrees, only the basis matrix is needed.
#' @param wt Used to weight observations differently. Required by rpart, but not supported by splinetree, so its value will always be NULL.
#' @keywords internal
#' @return A list of information for this node that is used internally by rpart.
spline_init <- function(y, offset = NULL, parms = NULL, wt = NULL) {
  y <- as.data.frame(y)

  # sfun is the function that will be called during summary(model) to print out the tree.
  # yval is the quantities that label each terminal node (the spline coefficients)
  sfun <- function(yval, dev, wt, ylevel, digits) {
    Ysummary <- rep(0, nrow(yval))
    for (i in 1:nrow(yval)) {
      Ysummary[i] <- paste(round(yval[i,], 2), collapse = ', ')
    }
    paste(" mean =", Ysummary, ", MSE =" , format(signif(dev/wt, digits)), sep = '')
  }

  environment(sfun) <- .GlobalEnv

  # In the case when there is only one spline coefficient, y will be a vector. We convert y to a matrix in this case
  # to be consistent with cases when there is more than one spline coefficient.
  if (is.vector(y)) {
    list(y = as.matrix(y), parms = parms, numresp = 1, numy = 1, summary = sfun)
  } else {
    list(y = y, parms = parms, numresp = NCOL(y), numy = NCOL(y), summary = sfun)
  }
}


#' Custom rpart eval function.
#'
#' The eval function is required for custom rpart functionality. The split criterion is the total sum of squared errors of the projected or smoothed outcome values around their mean.
#' Note that this is the node purity measure introduced by Yu and Lambert, 1999.
#' The calling of this function is always handled internally by rpart; the user will never directly call this function.
#' @param y the responses at this node, which will be estimated spline coefficients for individuals in the node.
#' @param wt Used to weight observations differently. Required by rpart, but not supported by splinetree, so its value will always be NULL.
#' @param parms rpart's custom split functionality allows optional parameters to be passed through the splitting functions.
#' In the splinetree package, the parms parameter is used to hold a list of length 1 or 2 containing either just a spline basis matrix (for a tree), or
#' a spline basis matrix and the probability that a variable will be selected at a split (for a random forest).
#' @keywords internal
#' @return A description for the node. This description includes the label, which is the mean response at the node,
#' and the deviance, which in this case is the total projected sum of squares.
spline_eval <- function(y, wt = NULL, parms = NULL) {
  y <- matrix(y, ncol = ncol(parms[[1]]))

  # Step 1: Compute the mean row of the data y.
  # If y only has 1 row, the mean is just this row.
  if (NROW(y) == 1) {
    meanVec <- drop(y)
  # Otherwise, compute mean coefficient vector
  } else {
    meanVec <- colMeans(y)
  }

  # Step 2: Calculate total projected sum of squares distance
  # X is the spline basis matrix.
  X <- parms[[1]]

    # Subtract the mean coefficient vector from each vector of coefficients.
    resids <- t(y) - meanVec

    # Compute total projected sum of squares
    total_dist <- sum((X%*%resids)^2)

 # }
  list(label = meanVec, deviance = total_dist)
}


#' Custom rpart split function.
#'
#' The split function is required for the custom rpart functionality. This function is called once per covariate per node during the tree construction,
#' and is responsible for choosing the covariate and threshold for the best split point. This implements the split function suggested by Yu and Lambert.
#' When the covariate is categorical, this code uses a shortcut for computational efficiency. Instead of trying
#' every possible combination of categories as a potential split point, the categories are ordered using the first principal component of the average spline coefficient vector.
#' @importFrom stats prcomp
#' @param y The responses at this node
#' @param wt Used to weight observations differently. Required by rpart, but not supported by splinetree, so its value will always be NULL.
#' @param x The data for a particular covariate
#' @param parms rpart's custom split functionality allows optional parameters to be passed through the splitting functions.
#' In the splinetree package, the parms parameter is used to hold a list of length 1 or 2 containing either just a spline basis matrix (for a tree), or
#' a spline basis matrix and the probability that a variable will be selected at a split (for a random forest).
#' @param continuous Value is handled internally by rpart - tells us if this covariate is contintuous (TRUE) or categorical (FALSE).
#' @return A list with two components, goodness and direction, describing the goodness of fit and direction for each possible split for this covariate.
#' The goodness component holds the utility of the split (projected sum of squares) for each possible split.
#' If the continuous parameter is TRUE, goodness and direction each have length n-1, here n is the length of x.
#' The ith value of goodness describes utility of splitting observations 1 to i from i + 1 to n.
#' The values of direction will be \eqn{-1} and \eqn{+1}, where \eqn{-1} suggests that values with y < cutpoint be sent to the left side of the tree,
#' and a value of +1 that values with y cutpoint be sent to the right. This is not really an important choice,
#' it only matters for tree reading conventions.
#' If the continuous parameter is FALSE, then the predictor variable x is categorical with
#' k classes and there are potentially almost 2k different ways to split the node.
#' When invoking custom split functions, rpart assumes that a reasonable approximation can be
#' computed by first ordering the groups by their
#' first principal component of the average y vector and then using the
#' usual splitting rule on this ordered variable.
#' In this case, the direction vector has k values giving the ordering of the groups, and the goodness vector
#' has k-1 values giving the utility of the splits.
#' @keywords internal
spline_split <- function(y, wt, x, parms = NULL, continuous) {
  goodness <- vector()
  direction <- vector()
  #dev <- vector()

  y <- matrix(y, ncol = ncol(parms[[1]]))

  totalScore <- spline_eval(y, parms = parms)$deviance[1]

  if (continuous) {

    for (i in 1:(length(x)-1)) {
      yLeft <- y[1:i,]
      yRight <- y[(i+1):length(x),]

      if (NCOL(y) > 1 && (NROW(yLeft) < 2 || NROW(yRight) < 2)) {
          goodness <- c(goodness, 0)
        } else {
          leftScore <- spline_eval(y = yLeft, parms = parms)$deviance[1]
          rightScore <- spline_eval(y = yRight, parms = parms)$deviance[1]
          split_score <- totalScore - leftScore - rightScore
          goodness <- c(goodness, split_score)
        }
        direction <- c(direction, -1)
    }

  } else {
    # Categorical variables
    xUnique <- unique(x)
    Yscore <- matrix(0, nrow = length(xUnique), ncol = ncol(y))

    # Putting categories in order by PC1 of average spline coefficients (multidimensional scaling to 1D from average vectors)
    for(i in 1:length(xUnique)){
      Yscore[i,] <- colMeans(y[x == xUnique[i],], na.rm = TRUE)
    }
    dir <- order(prcomp(Yscore)$x[,1]) # Linear combo of spline coefficients that explain the most variation

    for(i in 1:(length(dir) - 1)) {
        yLeft <- y[x%in%dir[1:i], ]
        yRight <- y[x%in%dir[(i+1):length(dir)], ]
        if (is.vector(yRight) || is.vector(yLeft)) {
          goodness <- c(goodness,0)
          next
        }
        if (NROW(yLeft) == 0 || NROW(yRight) == 0) {
          goodness <- c(goodness,0)
          next
        }
        leftScore <- spline_eval(y = yRight, parms = parms)$deviance[1]
        rightScore <- spline_eval(y = yLeft, parms = parms)$deviance[1]
        split_score <- totalScore - leftScore - rightScore
        goodness <- c(goodness, split_score)
    }
    direction <- dir
  }
  list(goodness = goodness, direction = direction)
}

