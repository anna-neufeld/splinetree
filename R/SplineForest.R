#' Build a spline random forest.
#'
#' Builds an ensemble of regression trees for longitudinal or functional data using the spline projection method. The resulting model
#' contains a list of spline trees along with some additional information. All parameters are used in the same way that they are used in
#' the splineTree() function. The additional parameter ntree specifies how many trees should be in the ensemble, and prob controls the
#' probability of selecting a given variable for split consideration at a node. This method may take several minutes to run- saving the forest after
#' building it is recommended.
#'
#' The ensemble method is highly similar to the random forest methodology of Breiman (2001). Each tree in the ensemble is fit to a random sample
#' of 63.5% of the data (sampled without replacement). At each node of each tree, only a subset of the split variables are considered candidates for the split. In our methodology,
#' the subset of variables considered at each node is determined by a random process. The prob parameter specifies the probability that a given variable
#' will be selected at a certain node. Because the method is based on probability, the same number of variables are not considered for splitting at each node
#' (as in the randomForest package). Note that if prob is small and the number of variables in the splitFormula is also small, there is a high probability that
#' no variables will be considered for splitting at a certain node, which is problematic. The fewer total variables there are, the larger prob should be to
#' ensure good results.
#'
#' @param splitFormula Formula specifying the longitudinal response variable and the time-constant variables that will be used for splitting in the tree.
#' @param tformula Formula specifying the longitudinal response variable and the variable that acts as the time variable.
#' @param idvar The name of the variable that serves as the ID variable for grouping observations. Must be in quotes
#' @param data dataframe that contains all variables specified in the formulas- in long format.
#' @param knots Specified locations for internal knots in the spline basis. Defaults to NULL, which corresponds to no internal knots.
#' @param df Degrees of freedom of the spline basis. If this is specified but the knots parameter is NULL, then the appropriate number of internal knots
#' will be added at quantiles of the training data. If both df and knots are unspecified, the spline basis will have no internal knots.
#' @param degree Specifies degree of spline basis used in the tree.
#' @param intercept Specifies whether or not the splitting process will consider the intercept coefficient of the spline projections.
#' Defaults to FALSE, which means that the tree will split based on trajectory shape, ignoring response level.
#' @param nGrid Number of grid points to evaluate projection sum of squares at. If gridPoints is not supplied, then this is the
#' number of grid points that will be automatically placed at quantiles of the time variable. The default is 7.
#' @param gridPoints Optional. A vector of numbers that will be used as the grid on which to evaluate the projection
#' sum of squares. Should fall roughly within the range of the time variable.
#' @param minNodeSize Minimum number of observational units that can be in a terminal node. Controls tree size and helps avoid overfitting.
#' Default is 10.
#' @param cp Complexity parameter passed to the rpart building process. Default is the rpart default of 0.01
#' @param ntree Number of trees in the forest.
#' @param prob Probability of selecting a variable to included as a candidate for each split.
#' @param bootstrap Boolean specifying whether bootstrap sampling should be used when choosing data to
#' use for each tree. When set to FALSE (the default), sampling without replacement is used and 63.5% of the data
#' is used for each tree. When set to TRUE, a bootstrap sample is used for each tree.
#' @return A spline forest model, which is a named list with 15 components.
#' The list stores a list of trees (in model$Trees), along with information about the
#' spline basis used (model$intercept, model$innerKnots, model$boundaryKnots, etc.), and information about which datapoints were
#' used to build each tree (model$oob_indices and model$index). Note that each element in model$Trees is an rpart object but
#' it is not the same as a model returned from splineTree() because it does not store all relevant information in model$parms.
#' @export
#' @import nlme
#' @import rpart
#' @import splines
#' @importFrom graphics barplot layout par plot points rect text
#' @importFrom stats complete.cases formula lm quantile runif sd terms time
#' @examples
#' \donttest{
#' nlsySubset <- nlsySample[nlsySample$ID %in% sample(unique(nlsySample$ID), 400),]
#' splitForm <-~HISP+WHITE+BLACK+HGC_MOTHER+HGC_FATHER+SEX+Num_sibs
#' sampleForest <- splineForest(splitForm, BMI~AGE, 'ID', nlsySubset, degree=1, cp=0.005, ntree=10)
#' }
splineForest <- function(splitFormula, tformula,
    idvar, data, knots = NULL, df = NULL, degree = 3,
    intercept = FALSE, nGrid = 7, gridPoints = NULL, ntree = 50, prob = 0.3,
    cp = 0.001, minNodeSize=1, bootstrap=FALSE) {
    #### Once per forest, need to do all of the
    #### preprocessing spline steps.
    yvar <- attr(terms(getResponseFormula(tformula)),
               "term.labels")
    tvar <- attr(terms(tformula), "term.labels")
    splitvars <- attr(terms(splitFormula), "term.labels")

    #Add an error if (1-prob)^(length(splitvars)) < .01 or so - Increase the prob

    ### Check for time-varying covariates in
    ### splitvars
    if (length(unique(data[, c(idvar, splitvars)])[[idvar]]) !=
        length(unique(data[[idvar]]))) {
        stop("Split variables must be non-time-varying.")
    }

    flat_data <- flatten_predictors(idvar, data)

    results <- getBasisMat(yvar, tvar, idvar, data,
        knots, df, degree, intercept, gridPoints, nGrid)

    basisMatrix <- results[[1]]
    innerKnots <- results[[2]]
    boundaryKnots <- results[[3]]


    Ydata <- sapply(unique(data[[idvar]]), individual_spline,
        idvar, yvar, tvar, data, boundaryKnots,
        innerKnots, degree, intercept)
    intercept_coeffs <- Ydata[1,]
    if (!intercept) {
      Ydata <- Ydata[-1,]
    }
    if (is.vector(Ydata)) {
      flat_data$Ydata <- Ydata
    } else {
      flat_data$Ydata <- t(Ydata)
    }
    flat_data$intercept_coeffs <- intercept_coeffs

    ### In new data frame, remove the original y data
    flat_data <- flat_data[, names(flat_data) != yvar]


    ### Another step of data processing - get rid of
    ### any row that has NA coeffs in the Y variable
    ### If we don't, RPART will do it for us, but our
    ### data structures will not match later
    flat_data <- flat_data[complete.cases(Ydata),]
    Ydata <- as.matrix(Ydata)[complete.cases(Ydata),]
    data <- data.frame(data[data[[idvar]] %in% flat_data[[idvar]],])


    #### Now all forest computation happens with respect to
    #### the flat_data dataframe
    ulist <- list(eval = spline_eval, split = splineforest_split,
        init = spline_init)

    control = rpart.control(cp = cp)
    form = formula(paste("Ydata ~ ", paste(attr(terms(formula(splitFormula)),
        "term.labels"), collapse = "+")))

    #### Now preprocessing done: begin forest building.
    if (bootstrap) {sampleSize = NROW(flat_data)}
    else {sampleSize = 0.632*NROW(flat_data)}

    myForest = list()
    itbIndices = list()
    oobIndices = list()

    splits = c()
    print("Building Tree:")
    for (j in c(1:ntree)) {
        print(j)

        indices = sample(1:NROW(flat_data), sampleSize,
            replace = bootstrap)
        sample = flat_data[indices, ]

        #### Since data is already processed, just
        #### directly build rpart tree.
        fit <- rpart(form, data = sample,
            method = ulist, control = control,
            maxcompete = 0, parms = list(basisMatrix,
                prob))

        if (is.null(fit$frame$yval2)) {
          fit$frame$yval2 <- fit$frame$yval
        }
        ### Save information from this iteration to forest.
        itbIndices[[j]] = unique(indices)
        myForest[[j]] = fit
        oobIndices[[j]] = (1:NROW(flat_data))[-unique(indices)]
        splits = c(splits, row.names(fit$splits))
    }

    results = list(myForest, itbIndices, splits, data,
        flat_data, splitFormula, oobIndices, degree,
        intercept, df, boundaryKnots, innerKnots,
        idvar, yvar, tvar)
    names(results) = c("Trees", "index", "splits",
        "data", "flat_data", "formula", "oob_indices",
        "degree", "intercept", "df", "boundaryKnots",
        "innerKnots", "idvar", "yvar", "tvar")
    results
}
