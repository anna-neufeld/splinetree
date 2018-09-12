#' Computes percent of variation in response explained by spline tree.
#'
#' Computes the percentage of variation in response explained by the spline tree.
#' This metric is only meaningful if model$intercept==TRUE.
#' If the tree includes an intercept, the measure will be between 0 and 1.
#'
#' @param model a splinetree tree object
#' @export
#' @return An R^2 goodness measure. 1-SSE/SST where SSE is the sum of squared errors between predicted responses and true
#' responses, and SST is sum of squared errors of true responses around population mean. Note that if the tree passed in was built
#' without an intercept, this function will return NULL.
#' @examples
#' \donttest{
#' \donttest{
#' split_formula <- ~HISP + WHITE + BLACK + SEX + Num_sibs + HGC_FATHER + HGC_MOTHER
#' tree <- splineTree(split_formula, tformula, idvar = "ID",
#'    data = nlsySample, degree = 1, df = 3,
#'    intercept = TRUE, cp = 0.005)
#' }
#' yR2(tree)
yR2 <- function(model) {
    if (model$parms$intercept) {
    yvar <- model$parms$yvar
    realResp <- model$parms$data[[yvar]]
    predResp <- predict_y_training(model)
    meanResp <- mean(model$parms$data[[yvar]])
    SSE = sum((predResp - realResp)^2)
    SST = sum((realResp - meanResp)^2)
    1 - SSE/SST
    }
  else {
    NULL
  }
}


#' Predictions from a splitted splinetree object
#'
#' Returns a vector of predicted responses for the testData. If testData is ommitted,
#' returns predictions for the training data. This function is most meaningful if model$intercept==TRUE.
#'
#' @param model A splinetree object.
#' @param testData The data to return predictions for. If ommitted, uses the training data.
#' @return A vector of predictions with rows corresponding to the testdata.
#' @importFrom treeClust rpart.predict.leaves
#' @export
#' @examples
#' \donttest{
#' split_formula <- ~HISP + WHITE + BLACK + SEX + Num_sibs + HGC_FATHER + HGC_MOTHER
#' tree <- splineTree(split_formula, tformula, idvar = "ID",
#'    data = nlsySample, degree = 1, df = 3,
#'    intercept = TRUE, cp = 0.005)
#' }
#' plot(predictY(tree), tree$parms$data[[tree$parms$yvar]])
predictY <- function(model, testData = NULL) {

    ### Calls a different version of the function that returns predicted Ys for training dataset
    if (is.null(testData)) {
      predict_y_training(model)
    }

    else {
      degree = model$parms$degree
      df = model$parms$df
      intercept = model$parms$intercept
      basisMatrix = model$parms$basisMatrix
      boundaryKnots = model$parms$boundaryKnots
      innerKnots = model$parms$innerKnots
      tvar = model$parms$tvar
      yvar = model$parms$yvar
      idvar = model$parms$idvar

      preds = rep(NA, NROW(testData))

      nodes <- rpart.predict.leaves(model, newdata=testData)
      ### Loop through every test unit.
      for (i in 1:NROW(testData)) {
        node = nodes[i]
        predCoeffs = model$frame[node, ]$yval2

        if (intercept) {
            basisMat = cbind(1, bs(testData[i, ][[tvar]], Boundary.knots = boundaryKnots,
                knots = innerKnots, degree = degree))
        } else {
            basisMat = bs(testData[i, ][[tvar]],
                Boundary.knots = boundaryKnots,
                knots = innerKnots, degree = degree)
        }
        preds[i] = basisMat %*% t(predCoeffs)
      }
      preds
    }
}


#' Computes percent of variation in projected response explained by a splinetree.
#'
#' Computes an R^2 measure for a splinetree based on the projected sum of squared errors. Returns 1-SSE/SST.
#' SSE is the sum of projection squared errors between individual smoothed trajectories and predicted smoothed
#' trajectories evaluated on a fixed grid. SST is the sum of projection squared errors between individual smoothed
#' trajectories and the overall population mean trajectory, evaluated on the same fixed grid.
#' If model$intercept==TRUE, then there is the option to ignore the intercept coefficient when computing this metric.
#' When the intercept is ignored, the metric captures how well the model explains variation in shape, and ignores
#' any variation in intercept explained by the model.
#' @export
#' @param model a splinetree tree object
#' @param includeIntercept If FALSE and if the model was built with an intercept, the projected squared errors are computed
#' while ignoring the intercept. If the model was built without an intercept, this parameter does not do anything.
#' @return The percentage of variation in projected trajectory explained by the model. Computed as 1-SSE/SST. See description.
#' @examples
#' r2 <- projectedR2(tree)
projectedR2 <- function(model, includeIntercept = FALSE) {

    ## Goal is to capture how well the predicted spline coefficients approximate the actual spline coefficients.
    real_coeffs = as.matrix(model$parms$flat_data$Ydata)
    preds_coeffs = as.matrix(t(predictCoeffs(model)))
    beta = model$parms$basisMatrix

    ### REm
    if (model$parms$intercept == TRUE & includeIntercept ==
        FALSE) {
        preds_coeffs = preds_coeffs[, -1]
        real_coeffs = real_coeffs[, -1]
        beta = beta[, -1]
    }


    mean_coeffs = as.matrix(apply(real_coeffs,
        2, mean))

    SSE = 0
    SST = 0
    for (i in 1:NROW(preds_coeffs)) {
        resid = preds_coeffs[i, ] - real_coeffs[i,
            ]
        resid2 = real_coeffs[i, ] - mean_coeffs
        SSE = SSE + t(resid) %*% t(beta) %*% beta %*%
            resid
        SST = SST + t(resid2) %*% t(beta) %*% beta %*%
            resid2
    }
    1 - SSE/SST
}







#' Predict spline coefficients for a testset using a splinetree object
#'
#' Returns a matrix of spline coefficients for each observation in the testset. If no testset is provided,
#' returns predicted coefficients for the individuals in training set; in this case, the columns of the
#' returned predictions correspond to the rows of the flattened training dataset (found in tree$parms$flat_data).
#'
#' importFrom treeClust rpart.predict.leaves
#' @param tree A splinetree object
#' @param testset The dataset to predict coefficients for. Default is the flattened dataset used to make the tree.
#' @export
#' @return A matrix of spline coefficients. The dimension of the matrix is the degrees of freedom of
#' the spline by the number of units in the test set. The ith column of the matrix holds the predicted
#' coefficients for the ith row in the testset.
#' @examples
#' \donttest{
#' split_formula <- ~HISP + WHITE + BLACK + SEX + Num_sibs + HGC_FATHER + HGC_MOTHER
#' tree <- splineTree(split_formula, tformula, idvar = "ID",
#'    data = nlsySample, degree = 1, df = 3,
#'    intercept = TRUE, cp = 0.005)
#' }
#' preds <- predictCoeffs(tree)
predictCoeffs <- function(tree, testset = tree$parms$flat_data) {
    ## Holds assigned node for every row of testset.
    wheres <- treeClust::rpart.predict.leaves(tree, newdata=testset)
    coeffDims = NCOL(tree$frame$yval2)

    preds <- array(NA, c(coeffDims, NROW(testset)))
    for (i in 1:NROW(testset)) {
        node = wheres[i]
        coeffs = tree$frame[node, ]$yval2
        preds[, i] = coeffs
    }
    preds
}



#' Predict responses for the training data
#'
#' Calling predictY(model) and predict_y_training(model) return identical results, because when no test data is
#' provided to predictY(), the default is to use the training set. This is a slightly faster version that
#' can be used when you know that you wish to predict on the training data. It is faster because it takes advantage
#' of the relationship between model$parms$flat_data and model$parms$data.
#'
#' @param model a splinetree object
#' @return A vector of predicted responses where each element in the vector corresponds to a row in model$parms$data.
#' @export
#' @keywords internal
#' @importFrom treeClust rpart.predict.leaves
predict_y_training <- function(model) {

  testData = model$parms$flat_data
  dat = model$parms$data
  nodes <- rpart.predict.leaves(model, newdata=testData)

  degree = model$parms$degree
  df = model$parms$df
  intercept = model$parms$intercept
  basisMatrix = model$parms$basisMatrix
  boundaryKnots = model$parms$boundaryKnots
  innerKnots = model$parms$innerKnots
  tvar = model$parms$tvar
  yvar = model$parms$yvar
  idvar = model$parms$idvar

  preds = rep(0, nrow(dat))

  for (i in 1:nrow(testData)) {
    node = nodes[i]
    predCoeffs = model$frame[node, ]$yval2

    ### Get all data associated with this person's ID
    ID <- testData[i, ][[idvar]]
    personDat <- dat[dat[[idvar]] == ID, ]

    ### Build basis matrix using same parameters as the common forest-wide basis matrix, but tailored
    ### to this person's individual time points.
    if (intercept) {
      personBasis <- cbind(1, bs(personDat[[tvar]],
                               knots =  innerKnots, Boundary.knots = boundaryKnots,
                               degree = degree))
    }
    else {
      personBasis <- bs(personDat[[tvar]],
                        knots =  innerKnots, Boundary.knots = boundaryKnots,
                        degree = degree)
    }

    preds[which(dat[[idvar]] == ID)] <- personBasis %*% t(predCoeffs)
  }
  preds
}


