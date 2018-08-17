#' Percent of variation in response explained by spline tree.
#'
#' Computes an R^2 measure for the spline tree with respect to prediction. Note that this metric is only meaningful if the spline tree object includes an intercept.
#' If the tree includes an intercept, the measure will be between 0 and 1.
#'
#' @param model a splinetree tree object
#' @export
#' @return An R^2 goodness measure. 1-SSE/SST where SSE is the sum of squared errors between predicted responses and true
#' responses, and SST is sum of squared errors of true responses around population mean. Note that if the tree passed in was built
#' without an intercept, this function will return NULL.
#' @examples
#' \dontrun{
#' split_formula <- BMI ~ HISP + WHITE + BLACK + SEX + Dad_Full_Work
#'   + Mom_Full_Work   + Age_first_weed + Age_first_smoke + Age_first_alc
#'   + Num_sibs + HGC_FATHER + HGC_MOTHER + Mag + News + Lib + Two_Adults_14
#'   + Mother_14 + Father_14 + STABLE_RESIDENCE + URBAN_14 + South_Birth
#'  tree <- splineTree(split_formula, BMI~AGE, 'ID', nlsySample, degree=1,
#'   df=3, intercept=TRUE, cp=0.006, minNodeSize=20)
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


#' Returns predicted responses.
#'
#' Returns predicted responses. Note that this function is most meaningful when used on spline tree objects that have an intercept.
#'
#' @param model a SplineTree object
#' @param testData The data to predict on. By default, uses the training set.
#' @return A vector of predictions with rows corresponding to the testdata.
#' @importFrom treeClust rpart.predict.leaves
#' @export
#' @examples
#' \dontrun{
#' split_formula <- BMI ~ HISP + WHITE + BLACK + SEX + Dad_Full_Work
#'   + Mom_Full_Work   + Age_first_weed + Age_first_smoke + Age_first_alc
#'   + Num_sibs + HGC_FATHER + HGC_MOTHER + Mag + News + Lib + Two_Adults_14
#'   + Mother_14 + Father_14 + STABLE_RESIDENCE + URBAN_14 + South_Birth
#' tree <- splineTree(split_formula, BMI~AGE, 'ID', nlsySample, degree=1,
#'   df=3, intercept=TRUE, cp=0.006, minNodeSize=20)
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


#' Computes an R^2-like measure that is based on the projected sum of squared errors.
#'
#' Computes an R^2-like measure that is based on the projected sum of squared errors. Can be used on trees whether or not they
#' were built with an intercept. If the tree was built with an intercept, there is the option to ignore the intercept in this
#' projection to isolate how well the tree clusters based on shape.
#' @export
#' @param model a splinetree tree object
#' @param includeIntercept If FALSE and if the model was built with an intercept, the projected squared errors are computed
#' while ignoring the intercept. If the model was built without an intercept, this parameter does not do anything.
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
#' \dontrun{
#' split_formula <- BMI ~ HISP + WHITE + BLACK + SEX + Dad_Full_Work
#'   + Mom_Full_Work   + Age_first_weed + Age_first_smoke + Age_first_alc
#'   + Num_sibs + HGC_FATHER + HGC_MOTHER + Mag + News + Lib + Two_Adults_14
#'   + Mother_14 + Father_14 + STABLE_RESIDENCE + URBAN_14 + South_Birth
#' tree <- splineTree(split_formula, BMI~AGE, 'ID', nlsySample, degree=1,
#'   df=3, intercept=TRUE, cp=0.006, minNodeSize=20)
#' }
#' predictCoeffs(tree)
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
#' Returns a vector of predicted responses for the dataset used to build the tree
#'
#' Calling predictY(model) and predict_y_training(model) return identical results, because when no test data is
#' provided to predictY(), the default is to use the training set. This is a slightly faster version that
#' can be used when you know that you wish to predict on the training data.
#'
#' @param model a splinetree object
#' @return A vector of predicted responses
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


