


#' Build a spline random forest
#'
#' Builds an rpart object for the responses projected onto a specified spline basis. Either knots or df (but not both) should be specified. If neither is specified, the default is to have no internal knots in the spline.
#' Knots is the location of the internal knots, whereas df specifies indirectly how many internal knots should be used.
#'
#' @param splitFormula Formula specifying the longitudinal response variable and the time-constant variables that will be used for splitting in the tree
#' @param tformula Formula specifying the longitudinal response variable and the variable that acts as the time variable
#' @param idvar The name of the variable that serves as the ID variable. Must be in quotes
#' @param data dataframe that contains all variables specified in the formulas (long format)
#' @param knots defaults to NULL, specified locations for INTERNAL knots.
#' @param df If this is specified but knots is not, we will add the approriate number of internal knots. Defaults to NULL, which corresponds to no internal knots.
#' @param degree Specifies degree of splines used in the tree
#' @param intercept Should the spline tree be built with or without the spline intercept coefficient? Default to FALSE.
#' @param nGrid Number of grid points along time variable in which trajectories are compared.
#' @param ntree Number of trees in the forest
#' @param diceProb Probability of selecting a variable to included as a candidate for each split.
#' @param cp Complexity parameter passed to rpart building process.
#' @return An rpart object with extra information stored in model$parms.
#' @export
#' @import nlme
#' @import rpart
#' @import splines
#' @importFrom graphics barplot layout par plot points rect text
#' @importFrom stats complete.cases formula lm quantile runif sd terms time
#' @examples
#' form1 <-BMI~HISP+WHITE+BLACK+HGC_MOTHER+SEX
#' model1 <- splineForest(form1, BMI~AGE, 'ID', nlsySample, degree=1, intercept=FALSE, cp=0.005)
#' model2 <- splineForest(form1, BMI~AGE, 'ID', nlsySample, degree=3, intercept=TRUE, cp=0.005)
splineForest <- function(splitFormula, tformula,
    idvar, data, knots = NULL, df = NULL, degree = 3,
    intercept = FALSE, nGrid = 7, ntree = 50, diceProb = 0.3,
    cp = 0.001) {
    #### Once per forest, need to do all of the
    #### preprocessing spline steps.
    yvar <- attr(terms(getResponseFormula(splitFormula)),
        "term.labels")
    tvar <- attr(terms(tformula), "term.labels")
    splitvars <- attr(terms(splitFormula), "term.labels")

    #Add an error if (1-diceProb)^(length(splitvars)) < .01 or so - Increase the diceProb

    ### Check for time-varying covariates in
    ### splitvars
    if (length(unique(data[, c(idvar, splitvars)])[[idvar]]) !=
        length(unique(data[[idvar]]))) {
        stop("Split variables must be non-time-varying.")
    }

    flat_data <- flatten_predictors(idvar, data)

    results <- getBasisMat(yvar, tvar, idvar, data,
        knots = NULL, df, degree, intercept, nGrid)

    basisMatrix <- results[[1]]
    innerKnots <- results[[2]]
    boundaryKnots <- results[[3]]


    Ydata <- sapply(unique(data[[idvar]]), individual_spline,
        idvar, yvar, tvar, data, boundaryKnots,
        innerKnots, degree, intercept)
    if (is.vector(Ydata)) {
        flat_data$Ydata <- Ydata
    } else {
        flat_data$Ydata <- t(Ydata)
    }

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
    data <- data[data[[idvar]] %in% flat_data[[idvar]],
        ]


    #### Now all forest stuff happens with respect to
    #### the flat data dataframe
    ulist <- list(eval = spline_eval, split = splineforest_split,
        init = spline_init)
    # assign('diceProb', diceProb, envir
    # =.GlobalEnv)

    control = rpart.control(cp = cp)
    form = formula(paste("Ydata ~ ", paste(attr(terms(formula(splitFormula)),
        "term.labels"), collapse = "+")))

    #### FOREST SETUP
    sampleSize = NROW(flat_data)

    myForest = list()
    index = list()
    oobIs = list()

    splits = c()
    print("Building Tree:")
    for (j in c(1:ntree)) {
        print(j)
        indices = sample(1:NROW(flat_data), sampleSize,
            replace = TRUE)
        oobIndices = (1:NROW(flat_data))[-unique(indices)]
        bootstrap_sample = flat_data[indices, ]

        #### Since data is already processed, just
        #### directly build rpart tree.
        fit <- rpart(form, data = bootstrap_sample,
            method = ulist, control = control,
            maxcompete = 0, parms = list(basisMatrix,
                diceProb))

        ### Save information from this iteration to forest.
        index[[j]] = unique(indices)
        myForest[[j]] = fit
        oobIs[[j]] = oobIndices
        splits = c(splits, row.names(fit$splits))
    }

    results = list(myForest, index, splits, data,
        flat_data, splitFormula, oobIs, degree,
        intercept, Ydata, df, boundaryKnots, innerKnots,
        idvar, yvar, tvar)
    names(results) = c("Trees", "index", "splits",
        "data", "Xdata", "formula", "oob_indices",
        "degree", "intercept", "Ydata", "df", "boundaryKnots",
        "innerKnots", "idvar", "yvar", "tvar")
    results
}
