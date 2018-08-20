## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  fig.width = 7
)

## ------------------------------------------------------------------------
library(splinetree)
split_formula <- BMI~HISP+WHITE+BLACK+SEX+Num_sibs+HGC_FATHER+HGC_MOTHER
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

## ------------------------------------------------------------------------
sample_tree <- splineTree(split_formula, tformula, idvar="ID", data=nlsySample, degree=1, df=2, intercept=TRUE, cp=0.005)

### Try to evaluate both trees
yR2(sample_tree)
test <- try(yR2(forest$Trees[[17]]), silent=TRUE)
class(test)

### Try to access additional information from both trees
sample_tree$parms$degree
forest$Trees[[17]]$parms$degree

## ---- eval=FALSE---------------------------------------------------------
#  itb17 <- forest$flat_data[forest$index[[17]],]
#  oob17 <- forest$flat_data[forest$oob_indices[[17]],]

## ------------------------------------------------------------------------
freqs <- table(forest$splits)/sum(table(forest$splits))
par(las = 2)
barplot(freqs)

## ------------------------------------------------------------------------
avSize(forest)
avSize(pruneForest(forest, cp=0.01))

## ---- eval=FALSE---------------------------------------------------------
#  newData <- data.frame("WHITE" = 0, "BLACK"=1, "HISP"=0, "Num_sibs"=3, "HGC_MOTHER"=12, "HGC_FATHER"=12, "SEX"=1)
#  preds <- predictCoeffsForest(forest, testdata = newData)

## ------------------------------------------------------------------------
AGE <- c(16,18,20,22,25,30,37,39,50)
newData2 <- cbind(AGE, newData)
predictions <- predictYForest(forest, testdata=newData2)
plot(AGE, predictions, type='l')

## ------------------------------------------------------------------------
cor(nlsySample$BMI, predictYForest(forest, method="oob"))
cor(nlsySample$BMI, predictYForest(forest, method="all"))
cor(nlsySample$BMI, predictYForest(forest, method="itb"))

## ------------------------------------------------------------------------
yR2Forest(forest, method="oob")
yR2Forest(forest, method="all")
yR2Forest(forest, method="itb")

## ------------------------------------------------------------------------
projectedR2Forest(forest, method="oob", removeIntercept = FALSE)
projectedR2Forest(forest, method="all", removeIntercept = FALSE)
projectedR2Forest(forest, method="itb", removeIntercept = FALSE)

## ------------------------------------------------------------------------
projectedR2Forest(forest, method="oob", removeIntercept = TRUE)
projectedR2Forest(forest, method="all", removeIntercept = TRUE)
projectedR2Forest(forest, method="itb", removeIntercept = TRUE)

## ------------------------------------------------------------------------
Y_imps <- varImpY(forest, method="oob")
coeff_imps <- varImpCoeff(forest, method="oob", removeIntercept=FALSE)
shape_imps <- varImpCoeff(forest, method="oob", removeIntercept=TRUE)

## ------------------------------------------------------------------------
par(mfrow=c(1,3))
plotImp(Y_imps[,3], main="Response")
plotImp(coeff_imps[,3], main = "Coeff w/ Intercept")
plotImp(shape_imps[,3], main = "Coeff w/out Intercept")

