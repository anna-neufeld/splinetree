#' Get spline coefficients for a single person
#'
#' @param person ID of this person
#' @param idvar name of the id variable (string)
#' @param tvar name of time variable (string)
#' @param data full dataset
#' @param boundaryKnots the boundary knots for the bspline
#' @param innerKnots the inner knots for the bspline
#' @param degree the degree of the bspline
#' @param intercept whether or not to include an intercept
#' @param yvar the name of the response variable
#' @keywords internal
individual_spline <- function(person, idvar, yvar,
    tvar, data, boundaryKnots, innerKnots, degree,
    intercept) {

    originalydat = data[[yvar]]

    ### Get this person's responses and times.
    Ys = originalydat[which(data[[idvar]] == person)]
    times = data[which(data[[idvar]] == person),
        ][[tvar]]

    ### Convert person's responses and times to
    ### spline coefficients (with or without
    ### intercept coefficient)
    X = bs(times, knots = innerKnots, Boundary.knots = boundaryKnots,
        degree = degree)

    # returns the spline coefficients
    # CHANGE: returns all coefficients even if intercept is FALSE.
    #if (intercept) {
    lm(Ys ~ X)$coefficients
    #} else {
     #   lm(Ys ~ X)$coefficients[-1]
    #}
}

#' Flattens predictor variable data into one row per person
#'
#' Assumes that splitting explanatory variables do not vary with time. Spline Tree is not meant to handle time-varying covariates.
#' @param idvar The string name of the ID variable (used to group observations)
#' @param data The full dataset to be flattened (long form)
#' @return A wide format dataset with spline coefficients as the responses.
#' @export
#' @keywords internal
flatten_predictors <- function(idvar, data) {
    ## Assuming that the splitting explanatory
    ## variables are not varying with time
    people = unique(data[[idvar]])
    X_data = data %>%
      group_by(get(idvar)) %>%
      slice(1)
    return(as.data.frame(X_data))
}

#' Get the basis matrix to be used for this spline tree
#'
#' Using the user-specified parameters or the default parameters, computes the basis matrix that will be used for building the tree.
#'
#' @param yvar Name of response variable (string)
#' @param tvar Name of time variable (string)
#' @param idvar Name of ID variable (string)
#' @param data Full dataset
#' @param knots Knots argument specified by user. Specifies location of INTERNAL knots.
#' @param df Degrees of freedom argument specified by user
#' @param intercept Whether or not to use an intercept
#' @param nGrid Number of grid points to evaluate split function at.
#' @param gridPoints Optional. A vector of numbers that will be used as the grid on which to evaluate the
#' projection sum of squares. Should fall roughly within the range of the time variable.
#' @param degree The degree of the spline polynomial
#' @return The basis matrix to be used for the tree building process
#' @keywords internal
getBasisMat <- function(yvar, tvar, idvar, data,
    knots = NULL, df, degree, intercept, gridPoints, nGrid = 7) {

    ## Boundary knots are always the endpoints of
    ## the dataset.
    boundaryKnots <- sort(range(data[[tvar]], na.rm = TRUE))

    ## Internal knots depend on user-specified
    ## parameters.
    if (is.null(knots)) {
        if (is.null(df)) {
            #### If both KNOTS and df are NULL, assume you
            #### want no internal knots
            num_internal_knots <- 0
            df <- degree + intercept
            innerKnots <- NULL
        } else {
            ### If knots = null but df specified, place
            ### default knots at data quantiles.
            num_internal_knots <- df - degree -
                intercept
            if (num_internal_knots > 0) {
                innerQuantiles <- seq(0, 1, length.out = num_internal_knots +
                  2)[2:(num_internal_knots + 1)]
                innerKnots <- quantile(data[[tvar]],
                  innerQuantiles, na.rm = TRUE)
            } else {
                innerKnots <- NULL
            }
        }
        #### When knots are provided, they are treated as
        #### the INTERNAL KNOTS.
    } else {
        innerKnots <- knots
    }
    #### Choosing common basis matrix for evaluating
    #### at all nodes during tree building Number of
    #### points specified by parameter nGrid.

    #Form a grid of quantiles- don't include endpoints.
    if (is.null(gridPoints)) {
      gridPoints <- quantile(data[[tvar]], probs = seq(0, 1, length.out = nGrid+2))[-c(1, nGrid+2)]
    }

    ## If the intercept parameter is TRUE, then we
    ## need to append a column of 1s to our
    ## basisMatrix
    if (intercept) {
        basisMatrix <- cbind(1, bs(gridPoints,
            knots = innerKnots, Boundary.knots = boundaryKnots,
            degree = degree))
    } else {
        basisMatrix <- bs(gridPoints, knots = innerKnots,
            Boundary.knots = boundaryKnots, degree = degree)
    }

    return(list(basisMatrix, innerKnots, boundaryKnots))

}

#' Build a splinetree model.
#'
#' Builds a regression tree for longitudinal or functional data using the spline projection method. The underlying tree building process uses the rpart package,
#' and the resulting spline tree is an rpart object with additional stored information. The parameters df, knots, degree, intercept allow for flexibility
#' in customizing the spline basis used for projection. The parameters nGrid and gridPoints allow for flexibility in the grid on which the
#' projection sum of squares is evaluated. The parameters minNodeSize and cp allow for flexibility in controlling the size of the final tree.
#'
#' @param splitFormula Formula specifying the longitudinal response variable and the time-constant variables that will be used for splitting in the tree.
#' @param tformula Formula specifying the longitudinal response variable and the variable that acts as the time variable.
#' @param idvar The name of the variable that serves as the ID variable for grouping observations. Must be a string.
#' @param data dataframe in long format that contains all variables specified in the formulas.
#' @param knots Specified locations for internal knots in the spline basis. Defaults to NULL, which corresponds to no internal knots.
#' @param df Degrees of freedom of the spline basis. If this is specified but the knots parameter is NULL, then the appropriate number of internal knots
#' will be added at quantiles of the training data. If both df and knots are unspecified, the spline basis will have no internal knots. If knots is specified,
#' this parameter will be ignored.
#' @param degree Specifies degree of spline basis used for projection.
#' @param intercept Specifies whether or not the set of basis functions will include the intercept function.
#' Defaults to FALSE, which means that the tree will split based on trajectory shape, ignoring response level.
#' @param gridPoints Optional. A vector of numbers that will be used as the grid on which to evaluate the
#' projection sum of squares. Should fall roughly within the range of the time variable.
#' @param nGrid Number of grid points to evaluate projection sum of squares at. If gridPoints
#' is not supplied, this argument will be used and the appropriate number of grid points will be placed at
#' equally spaced quantiles of the time variable. The default is 7.
#' @param minNodeSize Minimum number of observational units that can be in a terminal node. Controls tree size
#' and helps avoid overfitting. Defaults to 10.
#' @param cp Complexity parameter passed to the rpart building process. Controls tree size. Defaults to
#' the rpart default of 0.01.
#' @return An rpart object with additional splinetree-specific information stored in model$parms. The important
#' attributes of the rpart object include model$frame, model$where, and model$cptable. model$frame holds information
#' about each node in the tree. The ith entry in model$where tells us which row of model$frame describes the node that
#' the ith individual in the flattened dataset falls into. model$parms$flat_data holds the flattened dataset that
#' was used to build the tree. model$cptable displays the complexity parameters that would be needed to prune the tree
#' to various desired sizes. Apart from holding the flattened dataset, model$parms holds the boundary knots and the internal
#' knots of the spline basis used to build the tree. These are sometimes important to recover later.
#' @export
#' @import nlme
#' @import rpart
#' @import splines
#' @importFrom graphics barplot layout par plot points rect text
#' @importFrom stats complete.cases formula lm quantile runif sd terms time
#' @examples
#' nlsySample_subset <- nlsySample[nlsySample$ID %in% sample(unique(nlsySample$ID), 500),]
#' splitForm <- ~HISP+WHITE+BLACK+HGC_MOTHER+HGC_FATHER+SEX+Num_sibs
#' tree1 <- splineTree(splitForm, BMI~AGE, 'ID', nlsySample_subset, degree=3, intercept=TRUE, cp=0.005)
#' stPrint(tree1)
#' stPlot(tree1)
splineTree <- function(splitFormula, tformula,
    idvar, data, knots = NULL, df = NULL, degree = 3,
    intercept = FALSE, nGrid = 7, gridPoints = NULL, minNodeSize = 10,
    cp = 0.01) {

    ### First step: Preprocess the data by finding
    ### spline basis, flattening data, and finding
    ### coefficients for all people.
    yvar <- attr(terms(getResponseFormula(tformula)),
        "term.labels")
    tvar <- attr(terms(tformula), "term.labels")
    splitvars <- attr(terms(splitFormula), "term.labels")

    ### Check for time-varying covariates in
    ### splitvars
    if (length(unique(data[, c(idvar, splitvars)])[[idvar]]) !=
        length(unique(data[[idvar]]))) {
        stop("Split variables must be non-time-varying.")
    }

    ### Flatten predictors into 1 row per person
    flat_data <- flatten_predictors(idvar, data)

    ### Get the basis matrix
    results <- getBasisMat(yvar, tvar, idvar, data,
        knots, df, degree, intercept, gridPoints, nGrid)

    basisMatrix <- results[[1]]
    innerKnots <- results[[2]]
    boundaryKnots <- results[[3]]

    ### Convert original responses into spline
    ### coefficients.
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
    ### Note that intercept_coeffs is redundant (stored twice) if the tree was built
    ### with intercept=TRUE.

    ### In new data frame, remove the original y data
    flat_data <- flat_data[, names(flat_data) !=
        yvar]


    ### Another step of data processing - get rid of
    ### any row that has NA coeffs in the Y variable
    ### If we don't, RPART will do it for us, but our
    ### data structures will not match later
    flat_data <- flat_data[complete.cases(Ydata),
        ]
    Ydata <- as.matrix(Ydata)[complete.cases(Ydata),
        ]

    ### Just in case the preprocess step threw out
    ### some data for not having enough observations
    ### need to make sure the full dataset is updated
    ### to match this.
    data <- data[data[[idvar]] %in% flat_data[[idvar]],
        ]

    #### Now ready to actually build a tree using the
    #### flat data.
    ulist <- list(eval = spline_eval, split = spline_split,
        init = spline_init)
    control <- rpart.control(minbucket = minNodeSize,
        cp = cp)  # Controls tree size using user-specified controls or defaults.
    form <- formula(paste("Ydata ~ ", paste(attr(terms(formula(splitFormula)),
        "term.labels"), collapse = "+")))
    result <- rpart(form, data = flat_data, method = ulist,
        control = control, parms = list(basisMatrix))

    if (is.null(result$frame$yval2)) {
      result$frame$yval2 <- result$frame$yval
    }

    ### Save extra information to the rpart object for
    ### later use in plotting/eval functions
    saveparms <- list(flat_data, data, yvar, tvar,
        basisMatrix, idvar, df, degree, intercept,
        splitFormula, boundaryKnots, innerKnots)
    names(saveparms) <- c("flat_data", "data",
        "yvar", "tvar", "basisMatrix", "idvar",
        "df", "degree", "intercept", "splitFormula",
        "boundaryKnots", "innerKnots")
    result$parms <- saveparms
    return(result)
}



