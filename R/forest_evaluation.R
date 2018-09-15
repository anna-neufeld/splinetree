#' Computes a level-based or shape-based evaluation metric for a splineforest.
#'
#' Computes an R-squared-like evaluation metric for a splineforest object. Goal is to see how well the predicted spline coefficients for each individual
#' match the spline coefficients obtained when fitting a spline only to this individual's data (we call these coefficients the true coefficients). Computes 1-SSE/SST, where SSE is the total sum of squared projection errors of the true coefficients compared
#' to the predicted coefficients, and SST is the total sum of squared projection erros of the true coefficients compared to
#' the population mean coefficients. If this is an intercept forest, have the option to compute these sum of squares either with the intercept included or with the intercept ignored to isolate the shape.
#'
#' @param forest A splineforest object
#' @param method How would you like to compute this metric? The choices are "oob", "itb", or "all".
#' "oob" means that predictions for a datapoint can only be made using trees for which that datapoint was
#' "out of the bag" (not in the bootstrap sample). "all" means that all trees are used in the prediction for every
#' datapoint. "itb" means that predictions for a datapoint are made using only the trees for which this datapoint was IN the bootstrap sample.
#' @param removeIntercept If true, the projection sum of squared error is computed while ignoring the intercept coefficient.
#' This will help capture the tree's performance at clustering based on shape, not based on level.
#' This parameter is only meaningful if this tree was built using an intercept.
#' @return Returns 1-SSE/SST, where SSE is the total sum of squared projection errors of the true coefficients compared
#' to the predicted coefficients, and SST is the total sum of squared projection erros of the true coefficients compared to
#' the population mean coefficients.
#' @export
#' @examples
#' projectedR2Forest(forest, method="all", removeIntercept=TRUE)
#' \donttest{
#' projectedR2Forest(forest, method="all", removeIntercept=FALSE)
#' }
projectedR2Forest <- function(forest, method = "oob", removeIntercept = TRUE) {
    # First, get the predicted spline coefficients for every datapoint using the desired method.
    forest_pred_coeffs <- t(predictCoeffsForest(forest, method))
    # Goal will be to compare these to the true spline coefficients for every datapoint.
    true_coeffs <- forest$flat_data$Ydata ### Note to self- change this to flat_data to be consistent with tree.

    ### Goal is to see how closely the forest_pred_coeffs approximate the true_coeffs.
    ### To measure this, we use the projected sum of squares.

    ### For projected sum of squares, need the basis matrix.
    beta <- forest$Trees[[1]]$parms[[1]]

    if (removeIntercept && forest$intercept) {
        forest_pred_coeffs <- as.matrix(forest_pred_coeffs[, -1])
        true_coeffs <- as.matrix(true_coeffs[, -1])
        beta <- beta[, -1]
    }

    ### Because this measure is "R2-like", we use a Sum-Squares-Total and Sum-Squares-Error
    ### to compute it.
    mean_coeffs <- apply(true_coeffs, 2, mean)
    SST <- 0
    SSE <- 0
    for (i in 1:NROW(true_coeffs)) {
        resid_T <- true_coeffs[i, ] - mean_coeffs
        resid_E <- forest_pred_coeffs[i, ] - true_coeffs[i,
            ]
        SST <- SST + t(resid_T) %*% t(beta) %*%
            beta %*% resid_T
        SSE <- SSE + t(resid_E) %*% t(beta) %*%
            beta %*% resid_E
    }
    1 - SSE/SST
}


#' Computes a level-based evaluation metric for a splineforest that was built WITH an intercept.
#'
#' Computes the R-squared metric for a splineforest object. Goal is to see how well the predicted response values match the
#' actual response values. Note that this function should only be used on forests where the intercept parameter is TRUE.
#' A simple 1-SSE/SST calculation.
#'
#' @param forest A splineforest object
#' @param method How would you like to compute this metric? The choices are "oob", "itb", or "all".
#' "oob" means that predictions for a datapoint can only be made using trees for which that datapoint was
#' "out of the bag" (not in the bootstrap sample). "all" means that all trees are used in the prediction for every
#' datapoint. "itb" means that predictions for a datapoint are made using only the trees for which this datapoint was IN the bootstrap sample.
#' @return Returns 1-SSE/SST, where SSE is the total sum of squared errors of the true responses and predicted responses,
#' and SST is the total sum of squared erros of the responses around their mean. If this forest was not built with an intercept, returns NULL.
#' @export
#' @examples
#' yR2Forest(forest, method="all")
yR2Forest <- function(forest, method = "oob") {
    if (!forest$intercept) {
      ### If this forest was built without an intercept,
      ### inappropriate to try to predict responses.
      NULL
    }
    else {
    # First step is to get predicted coefficients for all individuals using appropriate method.
    forest_pred_coeffs = t(predictCoeffsForest(forest, method))

    dat <- forest$data
    meanYs <- mean(dat[[forest$yvar]])


    SST <- 0
    SSE <- 0
    ### Loop through each person in dataset.
    for (i in 1:NROW(forest$flat_data)) {

        ### Compute each person's predicted Y values. This requires getting their predicted coefficients
        ### And the appropriate time points for this individual.
        predCoeffs <- as.matrix(forest_pred_coeffs)[i, ]

        ### Get all data associated with this person's ID
        ID <- forest$flat_data[i, ][[forest$idvar]]
        personDat <- dat[dat[[forest$idvar]] == ID, ]

        ### Build basis matrix using same parameters as the common forest-wide basis matrix, but tailored
        ### to this person's individual time points.
        personBasis <- cbind(1, bs(personDat[[forest$tvar]],
            knots =  forest$innerKnots, Boundary.knots = forest$boundaryKnots,
            degree = forest$degree))

        ### Compute this person's predicted responses at all the same time points that they have real responses at.
        ### Two cases because depending on vector/matrix stuff, sometimes you need to transpose and sometimes you don't.
        ### Probably able to get rid of these cases - from Anna 7/12.
        try1 <- try({
            predYs <- personBasis %*% t(predCoeffs)
        }, silent = TRUE)
        if (class(try1) == "try-error") {
            try2 = try({
                predYs <- personBasis %*% predCoeffs
            }, silent = TRUE)
        }

        ### Compute SSE and SST between real responses and preducted responses.
        realYs <- personDat[[forest$yvar]]
        SST <- SST + sum((realYs - meanYs)^2)
        SSE <- SSE + sum((realYs - predYs)^2)
    }
    1 - SSE/SST
    }
}
