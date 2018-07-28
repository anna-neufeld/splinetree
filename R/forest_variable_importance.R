#' Random Forest Variable Importance based on Y
#'
#' Returns the random forest variable importance based on the permutation accruacy measure, which is calculated as the difference in mean squared error between the original data and from randomly permutating the values of a variable.
#'

#'
#' @param forest a random forest, generated from splineForest()
#' @param method the method to be used. This must be one of "oob" (out of bag), "all", "itb" (in the bag).
#' @return
#' @importFrom mosaic shuffle
varImp_Y_RF = function(forest, method = "oob") {


    formula = forest$formula
    vars = attr(terms(formula), "term.labels")
    trees = forest$Trees
    # t=trees[[1]]
    yvar = forest$yvar
    idvar = forest$idvar
    tvar = forest$tvar
    innerKnots = forest$innerKnots
    boundaryKnots = forest$boundaryKnots
    degree = forest$degree
    data = forest$data


    difs = rep(0, length(vars))
    names(difs) = vars
    varDifs = list()
    percDifs = list()
    for (v in vars) {
        varDifs[[v]] = c()
        percDifs[[v]] = c()
    }

    for (i in 1:length(trees)) {
        print("working on tree")
        print(i)
        tree = trees[[i]]
        if (method == "oob") {
            indices = forest$oob_indices[[i]]
        }
        if (method == "all") {
            indices = c(1:NROW(forest$Xdata))
        }
        if (method == "itb") {
            indices = forest$index[[i]]
        }
        IDS = forest$Xdata[indices, ][[idvar]]
        testset = data[data[[idvar]] %in% IDS,
            ]

        #### Figure out this function and the arguements
        #### it is supposed to have!!!
        preds = predict_Y_helper(tree, testset,
            innerKnots, boundaryKnots, degree,
            tvar = tvar, idvar = idvar, yvar = yvar)

        MSE_real = sum((testset[[yvar]] - preds)^2)/NROW(testset)

        for (var in vars) {
            permutedOOB = testset
            permutedOOB[[var]] = shuffle(permutedOOB[[var]])
            perm_preds = predict_Y_helper(tree = tree,
                testset = permutedOOB, innerKnots = innerKnots,
                boundaryKnots = boundaryKnots,
                degree = degree, tvar = tvar, yvar = yvar,
                idvar = idvar)
            MSE_permuted = sum((permutedOOB[[yvar]] -
                perm_preds)^2)/NROW(permutedOOB)

            dif = MSE_permuted - MSE_real
            perc_dif = (MSE_permuted - MSE_real)/MSE_real
            varDifs[[var]] = c(varDifs[[var]],
                dif)
            percDifs[[var]] = c(percDifs[[var]],
                perc_dif)
        }
    }

    absolute_importance = rep(0, length(vars))
    names(absolute_importance) = vars
    percent_importance = rep(0, length(vars))
    names(percent_importance) = vars
    standardized_importance = rep(0, length(vars))
    names(standardized_importance) = vars
    for (var in vars) {
        absolute_importance[[var]] = mean(varDifs[[var]])
        percent_importance[[var]] = mean(percDifs[[var]])
        standardized_importance[[var]] = mean(varDifs[[var]])/sd(varDifs[[var]])
    }

    imp = cbind(absolute_importance, percent_importance,
        standardized_importance)
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
#' @return
#' @importFrom mosaic shuffle
#' @importFrom mosaic shuffle
varImp_coeff_RF <- function(forest, removeIntercept = TRUE,
    method = "oob") {
    formula = forest$formula
    vars = attr(terms(formula), "term.labels")

    trees = forest$Trees
    yvar = forest$yvar
    idvar = forest$idvar
    tvar = forest$tvar

    beta = forest$Trees[[1]]$parms[[1]]

    innerKnots = forest$innerKnots
    boundaryKnots = forest$boundaryKnots
    degree = forest$degree
    intercept = forest$intercept

    Xdata = forest$Xdata
    difs = rep(0, length(vars))

    names(difs) = vars
    varDifs = list()
    percDifs = list()
    for (v in vars) {
        varDifs[[v]] = c(0)
        percDifs[[v]] = c(0)
    }

    if (intercept == TRUE & removeIntercept ==
        TRUE) {
        beta = beta[, -1]
    }

    for (i in 1:length(trees)) {
        print("working on tree")
        print(i)
        tree = trees[[i]]

        if (method == "oob") {
            oobIndices = forest$oob_indices[[i]]
        }
        if (method == "all") {
            oobIndices = c(1:NROW(forest$Xdata))
        }
        if (method == "itb") {
            oobIndices = forest$index[[i]]
        }

        oobIDS = forest$Xdata[oobIndices, ][[idvar]]
        testset = Xdata[Xdata[[idvar]] %in% oobIDS,
            ]


        preds_coeffs = t(predict_COEFF_helper(tree = tree, testset = testset))
        real_coeffs = testset$Ydata

        if (intercept == TRUE & removeIntercept ==
            TRUE) {
            preds_coeffs = preds_coeffs[, -1]
            real_coeffs = real_coeffs[, -1]
        }
        mean_coeffs = apply(real_coeffs, 2, mean)

        SSE_tree = 0
        SSE_total = 0
        for (i in 1:NROW(preds_coeffs)) {
            resid = preds_coeffs[i, ] - real_coeffs[i,
                ]
            SSE_tree = SSE_tree + t(resid) %*%
                t(beta) %*% beta %*% resid
        }

        MSE_real = SSE_tree/NROW(preds_coeffs)

        for (var in vars) {
            permutedOOB = testset
            permutedOOB[[var]] = shuffle(permutedOOB[[var]])
            perm_preds = t(predict_COEFF_helper(tree = tree,
                testset = permutedOOB))
            if (intercept == TRUE & removeIntercept ==
                TRUE) {
                perm_preds = perm_preds[, -1]
            }
            ### COMPUTE SSE USING PERM_PRDS COEFFS.
            MAHALA_perm = 0
            for (i in 1:NROW(perm_preds)) {
                resid = perm_preds[i, ] - real_coeffs[i,
                  ]
                MAHALA_perm = MAHALA_perm + t(resid) %*%
                  t(beta) %*% beta %*% resid
            }

            MSE_perm = MAHALA_perm/NROW(preds_coeffs)

            dif = MSE_perm - MSE_real
            perc_dif = (MSE_perm - MSE_real)/MSE_real
            varDifs[[var]] = c(varDifs[[var]],
                dif)
            percDifs[[var]] = c(percDifs[[var]],
                perc_dif)
        }
    }

    absolute_importance = rep(0, length(vars))
    names(absolute_importance) = vars
    percent_importance = rep(0, length(vars))
    names(percent_importance) = vars
    standardized_importance = rep(0, length(vars))
    names(standardized_importance) = vars
    for (var in vars) {
        absolute_importance[[var]] = mean(varDifs[[var]][-1])
        percent_importance[[var]] = mean(percDifs[[var]][-1])
        standardized_importance[[var]] = mean(varDifs[[var]][-1])/sd(varDifs[[var]][-1])
    }

    imp = cbind(absolute_importance, percent_importance,
        standardized_importance)
    imp
}



predict_Y_helper <- function(tree, testset, innerKnots,
    boundaryKnots, degree, tvar, idvar, yvar) {
    #wheres <- rpart:::pred.rpart(tree, rpart:::rpart.matrix(testset))
    wheres <- treeClust::rpart.predict.leaves(tree, testset)

    preds <- rep(NA, NROW(testset))
    for (i in 1:NROW(testset)) {
        node = wheres[i]
        coeffs = tree$frame[node, ]$yval2
        basisMat = cbind(1, bs(testset[i, ][[tvar]],
            knots = innerKnots, Boundary.knots = boundaryKnots,
            degree = degree))
        try1 = try({
            pred = basisMat %*% t(as.matrix(coeffs))
        }, silent = TRUE)
        if (class(try1) == "try-error") {
            try2 = try({
                pred = basisMat %*% as.matrix(coeffs)
            }, silent = TRUE)
        }
        preds[i] = pred
    }
    preds
}


predict_COEFF_helper <- function(tree, testset) {
    #wheres <- rpart:::pred.rpart(tree, rpart:::rpart.matrix(testset))
    wheres <- treeClust::rpart.predict.leaves(tree, testset)

    if (is.null(tree$frame$yval2)) {
        tree$frame$yval2 = tree$frame$yval
    }

    preds <- sapply(1:NROW(testset), function(i) tree$frame[wheres[i], ]$yval2)

    #coeffDims = NCOL(tree$frame$yval2)
    #preds <- array(NA, c(coeffDims, NROW(testset)))
    #for (i in 1:NROW(testset)) {
    #    node = wheres[i]
    #    coeffs = tree$frame[node, ]$yval2
    #    preds[, i] = coeffs
    #}
    preds
}



