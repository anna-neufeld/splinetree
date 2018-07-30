#' Predict spline coefficients using random forest.
#'
#' Uses the forest to make predictions of spline coefficients for individuals
#' in the training sample or on a new sample. If the testdata parameter is null,
#' then predictions are given on the training sample according to one of three possible
#' methods. The supplied method parameter must be either "oob", "itb", or "all".
#'
#' @param forest a splinetree forest object
#' @param method a string; either "oob", "itb", or "all". "oob" is the default value.
#' if "oob", predictions for a given data point are made only using trees for which this
#' data point was "out of the bag" (not in the bootstrap sample). If "itb", predictions for
#' a given data point are made using onle the trees for which this datapoint was in the bag (in the bootstrap sample).
#' If "all", all trees are used.
#' @param testdata the test data to make preditions for. If this is provided, then
#' all trees are used for all datapoints.
#' @return a matrix of predicted coefficients.
#' @export
predict_coeffs_RF = function(forest, method = "oob", testdata=NULL) {
  idvar <- forest$idvar
  innerKnots <- forest$innerKnots
  boundaryKnots <- forest$boundaryKnots
  degree <- forest$degree
  intercept <- forest$intercept
  Xdata <- forest$Xdata

  t <- forest$Trees[[1]]
  coeffDims = NCOL(t$frame$yval2)


  if (is.null(testdata)) {

    predictions <- array(NA, c(length(forest$Trees),
                               coeffDims, NROW(Xdata)))

    if (method == "oob") {
      indices <- forest$oob_indices
    }

    if (method == "all") {
      indices <- list()
      for (tree in 1:length(forest$Trees)) {
        indices[[tree]] <- 1:NROW(forest$Xdata)
      }
    }

    if (method == "itb") {
      indices <- forest$index
    }

  for (tree in 1:length(forest$Trees)) {
    preds <- array(NA, c(coeffDims, NROW(Xdata)))
    test_indices <- indices[[tree]]

    testset <- Xdata[test_indices, ]

    preds[, test_indices] <- predict_coeffs_tree(forest$Trees[[tree]],
                                                 testset)
    predictions[tree, , ] <- preds
  }
  actualpredictions <- data.frame(apply(predictions,
                                        c(2, 3), mean, na.rm = TRUE))
  names(actualpredictions) <- Xdata[[idvar]]
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
      predictions[tree, , ] <- predict_coeffs_tree(forest$Trees[[tree]],
                                                   testdata)
    }

  actualpredictions <- data.frame(apply(predictions,
                                        c(2, 3), mean, na.rm = TRUE))
  names(actualpredictions) <- testdata[[idvar]]
  }
  actualpredictions
}

#' Predict responses using random forest.
#'
#' Uses the forest to make predictions of responses for individuals at given times
#' in the training sample or on a new sample. If the testdata parameter is null,
#' then predictions are given on the training sample according to one of three possible
#' methods. The supplied method parameter must be either "oob", "itb", or "all". Note that this method
#' should only be used on trees that have an intercept. Otherwise, the Y predictions will
#' not be accurate at all.
#'
#' @param forest a splinetree forest object
#' @param method a string; either "oob", "itb", or "all". "oob" is the default value.
#' if "oob", predictions for a given data point are made only using trees for which this
#' data point was "out of the bag" (not in the bootstrap sample). If "itb", predictions for
#' a given data point are made using onle the trees for which this datapoint was in the bag (in the bootstrap sample).
#' If "all", all trees are used.
#' @param testdata the test data to make preditions for. If this is provided, then
#' all trees are used for all datapoints.
#' @return a matrix of predicted responses.
#' @export
predict_y_RF <- function(forest, method = "oob", testdata=NULL) {
  if (!forest$intercept) {
    stop("You should not try to predict response values with a no-intercept model")
  }

  innerKnots <- forest$innerKnots
  boundaryKnots <- forest$boundaryKnots
  Xdata <- forest$Xdata
  degree <- forest$degree
  tvar <- forest$tvar
  idvar <- forest$idvar
  dat <- forest$data

  if (is.null(testdata)) {
    coeffPreds <- t(predict_coeffs_RF(forest, method))
    preds <- rep(NA, NROW(forest$data))

    for (i in 1:NROW(forest$Xdata)) {
      ID <- Xdata[i, ][[idvar]]
      personDat = dat[dat[[idvar]] == ID, ]

      personBasis <- cbind(1, bs(personDat[[tvar]],
                                   knots =  innerKnots, Boundary.knots = boundaryKnots,
                                   degree = degree))

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

    coeffPreds <- t(predict_coeffs_RF(forest, method, testdata))
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


#' Predict spline coefficients for a testset using a single tree.
#'
#' @param tree a splinetree object
#' @param testset the dataset to return predictions for. If omitted, defaults
#' to the data used to build this tree.
#' @return a matrix of predicted coefficients
#' @importFrom treeClust rpart.predict.leaves
#' @export
predict_coeffs_tree <- function(tree, testset = tree$data) {
  #wheres <- rpart:::pred.rpart(tree, rpart:::rpart.matrix(testset))
  wheres <- rpart.predict.leaves(tree, newdata=testset)


  coeffDims <- NCOL(tree$frame$yval2)

  preds <- array(NA, c(coeffDims, NROW(testset)))
  for (i in 1:NROW(testset)) {
    node <- wheres[i]
    coeffs <- tree$frame[node, ]$yval2
    preds[, i] = coeffs
  }
  preds
}

#' Predict responses for a testset using a single tree.
#'
#' @param tree a splinetree object
#' @param testset the dataset to return predictions for. Defaults
#' to the dataaset used to build this tree.
#' @return a matrix of predicted responses
#' @export
predict_Y_tree <- function(tree, testset=tree$data) {
  #wheres <- rpart:::pred.rpart(tree, rpart:::rpart.matrix(testset))
  wheres <- rpart.predict.leaves(tree, newdata=testset)


  tvar <- tree$parms$tvar

  preds <- rep(NA, NROW(testset))
  for (i in 1:NROW(testset)) {
    node <- wheres[i]
    coeffs <- tree$frame[node, ]$yval2
    basisMat <- cbind(1, bs(testset[i, ][[tvar]],
                            knots = tree$parms$innerKnots, Boundary.knots = tree$parms$boundaryKnots,
                            degree = tree$parms$degree))
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
  preds
}








