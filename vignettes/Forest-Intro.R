## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  fig.width = 7
)

## ------------------------------------------------------------------------
library(splinetree)
split_formula <- ~HISP+WHITE+BLACK+SEX+Num_sibs+HGC_FATHER+HGC_MOTHER
tformula <- BMI~AGE

## ---- eval=FALSE---------------------------------------------------------
#  forest <- splineForest(split_formula, tformula, idvar="ID", data=nlsySample, degree=1, df=3, intercept=TRUE,ntree=50, prob=0.5, cp=0.005)

## ------------------------------------------------------------------------
names(forest)

## ------------------------------------------------------------------------
forest$formula
forest$idvar
forest$yvar
forest$tvar

## ------------------------------------------------------------------------
mean_coeffs <- apply(forest$Ydata, 1, mean)

times <- sort(unique(forest$data[[forest$tvar]]))


basisMatrix <- bs(times, degree=forest$degree, Boundary.knots = forest$boundaryKnots, knots = forest$innerKnots)
if (forest$intercept) {
  basisMatrix <- cbind(1, basisMatrix)
}
 
preds <- basisMatrix %*% mean_coeffs

plot(times, preds, type='l', main="Population Average Trajectory")

## ------------------------------------------------------------------------
stPrint(forest$Trees[[17]])

