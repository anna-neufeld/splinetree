#' Predict spline coefficients for a testset using a splineforest.
#'
#' Uses the forest to predict spline coefficients. Returns a matrix of predicted spline coefficients where the columns
#' of the returned matrix correspond to rows of the testdata. The number of rows of the returned matrix is equal to the
#' degrees of freedom of the forest. If no testdata is provided, forest$flat_data is used. When testdata is not provided,
#'  predictions will be made according to one of three methods. The "method" parameter must be either
#' "oob", "itb", or "all". This parameter specifies which trees are used in making a prediction for a certain datapoint.
#' This parameter is not relevant when predicting for a testset that is distinct from the training set.
#'
#' @param forest A splineforest object
#' @param method A string; either "oob", "itb", or "all".
#' If "oob" (the default), predictions for a given data point are made only using trees for which this
#' data point was "out of the bag" (not in the bootstrap sample). If "itb", predictions for
#' a given data point are made using onle the trees for which this datapoint was "in the bag"
#' (in the bootstrap sample). If "all", all trees are used for every datapoint.
#' @param testdata The test data to make preditions for. If this is provided, then
#' all trees are used for all datapoints.
#' @return A matrix of predicted spline coefficients. The dimensions are forest$df x nrow(testdata). Each column of the matrix
#' corresponds to a row of the testdata.
#' @export
predictCoeffsForest = function(forest, method = "oob", testdata=NULL) {
  idvar <- forest$idvar
  innerKnots <- forest$innerKnots
  boundaryKnots <- forest$boundaryKnots
  degree <- forest$degree
  intercept <- forest$intercept
  flat_data <- forest$flat_data

  t <- forest$Trees[[1]]
  coeffDims = NCOL(t$frame$yval2)


  if (is.null(testdata)) {

    predictions <- array(NA, c(length(forest$Trees),
                               coeffDims, NROW(flat_data)))

    if (method == "oob") {
      indices <- forest$oob_indices
    }

    if (method == "all") {
      indices <- list()
      for (tree in 1:length(forest$Trees)) {
        indices[[tree]] <- 1:NROW(forest$flat_data)
      }
    }

    if (method == "itb") {
      indices <- forest$index
    }

  for (tree in 1:length(forest$Trees)) {
    preds <- array(NA, c(coeffDims, NROW(flat_data)))
    test_indices <- indices[[tree]]

    testset <- flat_data[test_indices, ]

    preds[, test_indices] <- predictCoeffs(forest$Trees[[tree]],
                                                 testset)
    predictions[tree, , ] <- preds
  }
  actualpredictions <- data.frame(apply(predictions,
                                        c(2, 3), mean, na.rm = TRUE))
  names(actualpredictions) <- flat_data[[idvar]]
  actualpredictions
  }
  ### If testdata is not null and you actually want to predict on a new dataset.
  ### In this case, no "oob/itb" distinction because every datapoint is out of bag
  ### Use every tree for every datapoint.
  else {

    predictions <- array(NA, c(length(forest$Trees),
                               coeffDims, NROW(testdata)))

    for (tree in 1:length(forest$Trees)) {
      preds <- array(NA, c(coeffDims, NROW(testdata)))
      predictions[tree, , ] <- predictCoeffs(forest$Trees[[tree]],
                                                   testdata)
    }

  actualpredictions <- data.frame(apply(predictions,
                                        c(2, 3), mean, na.rm = TRUE))
  names(actualpredictions) <- testdata[[idvar]]
  }
  actualpredictions
}

#' Predict responses for a testset using a splineforest.
#'
#' Uses the forest to make predictions of responses for individuals. This method should only be used
#' on forests where forest$intercept=TRUE. If the testdata parameter is
#' null, makes predictions for each row of the training data. In this case, the methods parameter (which should
#' be set to "oob", "itb", or "all") determines the method used for prediction. If the testdata parameter is not
#' null, the methods parameter is ignored and all trees are used for the prediction of every datapoint.
#'
#' @param forest A splineforest object
#' @param method A string. Must be either "oob", "itb", or "all". Only relevant when testdata is NULL.
#' The default value is "oob". If "oob", predictions for a given data point are made only using
#' trees for which this data point was "out of the bag" (not in the bootstrap sample).
#'  If "itb", predictions for a given data point are made using only the trees for which this datapoint
#'  was in the bag (in the bootstrap sample). If "all", all trees are used for every datapoint.
#' @param testdata the Test data to make preditions for. If this is provided, then
#' all trees are used for all datapoints.
#' @return A vector of predicted responses. The indices of the vector correspond to rows of the testdata.
#' @export
predictYForest <- function(forest, method = "oob", testdata=NULL) {
  if (!forest$intercept) {
    stop("You should not try to predict response values with a no-intercept model")
  }

  innerKnots <- forest$innerKnots
  boundaryKnots <- forest$boundaryKnots
  flat_data <- forest$flat_data
  degree <- forest$degree
  tvar <- forest$tvar
  idvar <- forest$idvar
  dat <- forest$data

  if (is.null(testdata)) {
    coeffPreds <- t(predictCoeffsForest(forest, method))
    preds <- rep(NA, NROW(forest$data))

    for (i in 1:NROW(forest$flat_data)) {
      ID <- flat_data[i, ][[idvar]]
      personDat = dat[dat[[idvar]] == ID, ]
      coeffs <- coeffPreds[i,]

      ### Assumes that forest includes intercept.
      basisMat <- cbind(1, bs(personDat[[tvar]],
                              knots = innerKnots, Boundary.knots = boundaryKnots,
                              degree = degree))

      try1 <- try({
        pred <- basisMat %*% t(as.matrix(coeffs))
      }, silent = TRUE)
      if (class(try1) == "try-error") {
        try2 <- try({
          pred <- basisMat %*% as.matrix(coeffs)
        }, silent = TRUE)
      }
      preds[which(dat[[idvar]]== ID)] <- pred
    }
  }

  else {

    coeffPreds <- t(predictCoeffsForest(forest, method, testdata))
    preds <- rep(NA, NROW(testdata))

    for (i in 1:NROW(testdata)) {
      coeffs <- coeffPreds[i,]

      ### Assumes that forest includes intercept.
      basisMat <- cbind(1, bs(testdata[i,][[tvar]],
                              knots = innerKnots, Boundary.knots = boundaryKnots,
                              degree = degree))

      try1 <- try({
        pred <- basisMat %*% t(as.matrix(coeffs))
      }, silent = TRUE)
      if (class(try1) == "try-error") {
        try2 <- try({
          pred <- basisMat %*% as.matrix(coeffs)
        }, silent = TRUE)
      }
      preds[i] <- pred
    }
  }
  preds
}
