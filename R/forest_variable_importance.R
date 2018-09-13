#' Random Forest Variable Importance based on Y
#'
#' Returns the random forest variable importance based on the permutation accruacy measure, which is calculated as the difference in mean squared error between the original data and from randomly permutating the values of a variable.
#'
#' The "method" parameter deals with the way in which forest performance should be measured. Since varaible importance is based on a change
#' in performance, the "method" parameter is necessary for a variable importance measure. The choices are "oob" (out of bag), "all", or "itb" (in the bag).
#'
#' @param forest a random forest, generated from splineForest()
#' @param method the method to be used. This must be one of "oob" (out of bag), "all", "itb" (in the bag).
#' @return A matrix storing variable importance metrics. The rows correspond to split variables.
#' The columns are different methods of measuring importance. The first column is the absolute importance
#' (mean difference in performance between permuted and unpermuted datasets). The second column measures the
#' mean percent difference in performance. The third column standardizes the differences by dividing them
#' by their standard deviation.
#' @export
#' @importFrom mosaic shuffle
#' @examples
#' \donttest{
#' importanceMatrix <- varImpY(forest)
#' plotImp(importanceMatrix[,3])
#' }
varImpY = function(forest, method = "oob") {

    vars = attr(terms(forest$formula), "term.labels")
    trees = forest$Trees
    yvar = forest$yvar
    idvar = forest$idvar
    data = forest$data

    varDifs = list()
    percDifs = list()
    for (v in vars) {
        varDifs[[v]] = rep(0, length(trees))
        percDifs[[v]] = rep(0, length(trees))
    }

    if (method == "all") {
      indices <- list()
      for (tree in 1:length(forest$Trees)) {
        indices[[tree]] <- 1:NROW(forest$flat_data)
      }

    }

    if (method == "oob") {
      indices = forest$oob_indices
    }

    if (method == "itb") {
      indices = forest$index
    }

    full_basis_Mat <- cbind(1, bs(data[[forest$tvar]],
                            knots = forest$innerKnots, Boundary.knots = forest$boundaryKnots,
                            degree = forest$degree))


    print("Working on tree: ")
    for (i in 1:length(trees)) {
        print(i)
        tree = trees[[i]]

        IDS = forest$flat_data[indices[[i]], ][[idvar]]

        ID_indices = which(data[[idvar]] %in% IDS)
        testset = data[ID_indices,]
        basisMat <- full_basis_Mat[ID_indices,]

        #### Get the unpermuted predictions.
        wheres <- treeClust::rpart.predict.leaves(tree, testset)
        preds <- apply(array(1:NROW(testset)), 1, function(x) tree$frame[wheres[x], ]$yval2%*%basisMat[x,])

        MSE_real <- sum((testset[[yvar]] - preds)^2)/NROW(testset)

        for (var in vars) {
            permuted <- testset
            permuted[[var]] <- shuffle(permuted[[var]])


            ### Get the permuted predictions.
            perm_wheres <- treeClust::rpart.predict.leaves(tree, permuted)
            perm_preds <- apply(array(1:NROW(permuted)), 1, function(x) tree$frame[perm_wheres[x], ]$yval2%*%basisMat[x,])


            MSE_permuted <- sum((permuted[[yvar]] -
                perm_preds)^2)/NROW(permuted)

            varDifs[[var]][i] <- MSE_permuted - MSE_real
            percDifs[[var]][i] <- (MSE_permuted - MSE_real)/MSE_real
        }
    }

    absolute_importance = t(data.frame(lapply(varDifs, mean)))
    percent_importance = t(data.frame(lapply(percDifs, mean)))
    standardized_importance = t(data.frame(lapply(varDifs, function(x) mean(x)/sd(x))))
    imp = cbind(absolute_importance, percent_importance,
        standardized_importance)
    names(imp) < c("Absolute_Difference", "Percent_Difference", "Standardized_Difference")
    imp
}

#' Random Forest Variable Importance based on spline coefficients
#'
#' Returns the random forest variable importance based on the permutation accruacy measure, which is calculated as the difference in mean squared error between the original data and from randomly permutating the values of a variable.
#'
#'
#' @param forest a random forest, generated from splineForest()
#' @param removeIntercept a boolean value, TRUE if you want to exclude the intercept in the calculations, FALSE otherwise.
#' @param method the method to be used. This must be one of "oob" (out of bag), "all", "itb" (in the bag).
#' @return a matrix of variable importance metrics.
#' @examples
#' \donttest{
#' importanceMatrix <- varImpCoeff(forest, removeIntercept=TRUE)
#' }
#' @export
#' @importFrom mosaic shuffle
varImpCoeff <- function(forest, removeIntercept = TRUE,
    method = "oob") {
    formula = forest$formula
    vars = attr(terms(formula), "term.labels")

    trees = forest$Trees
    yvar = forest$yvar
    idvar = forest$idvar
    tvar = forest$tvar

    beta = trees[[1]]$parms[[1]]

    innerKnots = forest$innerKnots
    boundaryKnots = forest$boundaryKnots
    degree = forest$degree
    intercept = forest$intercept

    flat_data = forest$flat_data
    difs = rep(0, length(vars))

    names(difs) = vars
    varDifs = list()
    percDifs = list()
    for (v in vars) {
        varDifs[[v]] = rep(0, length(trees))
        percDifs[[v]] = rep(0, length(trees))
    }

    cols = 1:NCOL(beta)
    if (intercept == TRUE & removeIntercept ==
        TRUE) {
        cols = 2:NCOL(beta)
        beta = beta[, -1]
    }

    if (method == "oob") {
      indices = forest$oob_indices
    }

    if (method == "all") {
      indices <- list()
      for (tree in 1:length(forest$Trees)) {
        indices[[tree]] <- 1:NROW(forest$flat_data)
      }

    }

    if (method == "itb") {
      indices = forest$index
    }

    print("working on tree")
    for (i in 1:length(trees)) {
        print(i)
        tree <- trees[[i]]

        IDS <- forest$flat_data[indices[[i]], ][[idvar]]
        testset <- flat_data[flat_data[[idvar]] %in% IDS,]

        wheres <- treeClust::rpart.predict.leaves(tree, testset)
        preds_coeffs <- t(sapply(1:NROW(testset), function(i) tree$frame[wheres[i], ]$yval2))

        real_coeffs <- testset$Ydata

        ### Deal with removing intercept if necessary
        preds_coeffs = preds_coeffs[,cols]
        real_coeffs = real_coeffs[,cols]

        mean_coeffs = apply(real_coeffs, 2, mean)

        SSE_tree <- 0
        SSE_total <- 0
        for (j in 1:NROW(preds_coeffs)) {
            resid <- preds_coeffs[j, ] - real_coeffs[j,]
            SSE_tree <- SSE_tree + t(resid) %*%
                t(beta) %*% beta %*% resid
        }

        MSE_real <- SSE_tree/NROW(preds_coeffs)

        for (var in vars) {
            permuted <- testset
            permuted[[var]] <- shuffle(permuted[[var]])

            wheres <- treeClust::rpart.predict.leaves(tree, permuted)
            perm_preds <- t(sapply(1:NROW(permuted), function(x) tree$frame[wheres[x], ]$yval2))


            perm_preds <- perm_preds[, cols]
            ### COMPUTE SSE USING PERM_PRDS COEFFS.
            MAHALA_perm = 0
            for (j in 1:NROW(perm_preds)) {
                resid = perm_preds[j, ] - real_coeffs[j,
                  ]
                MAHALA_perm = MAHALA_perm + t(resid) %*%
                  t(beta) %*% beta %*% resid
            }

            MSE_perm = MAHALA_perm/NROW(preds_coeffs)

            dif = MSE_perm - MSE_real
            perc_dif = (MSE_perm - MSE_real)/MSE_real
            varDifs[[var]][i] = dif
            percDifs[[var]][i] = perc_dif
        }
    }

    absolute_importance = t(data.frame(lapply(varDifs, mean)))
    percent_importance = t(data.frame(lapply(percDifs, mean)))
    standardized_importance = t(data.frame(lapply(varDifs, function(x) mean(x)/sd(x))))
    imp = cbind(absolute_importance, percent_importance,
                standardized_importance)
    names(imp) < c("Absolute_Difference", "Percent_Difference", "Standardized_Difference")
    imp
}
