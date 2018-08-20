## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  fig.width = 7
)

## ---- include=FALSE------------------------------------------------------
library(splinetree)

## ---- echo=FALSE---------------------------------------------------------
nlsySample[c(1:8,13:20),c(1,26,31, 12,14, 27)]

## ------------------------------------------------------------------------
data <-nlsySample
tformula <- BMI~AGE
idvar <- "ID"
split_formula <- BMI ~ HISP + WHITE + BLACK + SEX + Dad_Full_Work + Mom_Full_Work   + Age_first_weed + Age_first_smoke + Age_first_alc + Num_sibs + HGC_FATHER + HGC_MOTHER + Mag + News + Lib + Two_Adults_14 + Mother_14 + Father_14 + STABLE_RESIDENCE + URBAN_14 + South_Birth
first_tree <- splineTree(split_formula, tformula, idvar, data)

## ------------------------------------------------------------------------
stPrint(first_tree)

## ------------------------------------------------------------------------
stPlot(first_tree)

## ------------------------------------------------------------------------
spaghettiPlot(first_tree)

## ------------------------------------------------------------------------
treeSummary(first_tree)

## ------------------------------------------------------------------------
treeSummary(first_tree)["4",]$coeffs

## ------------------------------------------------------------------------
terminalNodeSummary(first_tree, 4)

## ------------------------------------------------------------------------
node4dat <- getNodeData(first_tree, 4)

## ------------------------------------------------------------------------
range(node4dat$Num_sibs)
range(node4dat$HGC_MOTHER, na.rm=TRUE)

## ------------------------------------------------------------------------
plotNode(first_tree, node=4, includeData=FALSE, estimateIntercept=FALSE)

## ------------------------------------------------------------------------
node4coeffs <- treeSummary(first_tree)["4",]$coeffs

### Define age range we want trajectory predicted over
xvals <- seq(18,50,length.out=50)

### Reconstruct basis matrix. 
basisMat <- bs(xvals, degree=first_tree$parms$degree, Boundary.knots = first_tree$parms$boundaryKnots, knots = first_tree$parms$innerKnots)

### Compute predicted trajectory.
node4preds <- basisMat%*%t(node4coeffs)

### Compare automatic plot to reconstructed plot
require(ggplot2)
par(mfrow=c(1,2))
plotNode(first_tree, 4, includeData=FALSE, estimateIntercept=FALSE)
ggplot()+geom_line(aes(x=xvals, y=node4preds, color="red", size=1))+theme(legend.position="none")+xlab(tree$parms$tvar)+ylab(tree$parms$yvar)

## ------------------------------------------------------------------------
node4flatdata <- getNodeData(first_tree, 4, dataType="flat")
coeffs <- node4flatdata$Ydata
head(coeffs)

## ------------------------------------------------------------------------
second_tree <- splineTree(split_formula, tformula, idvar, data, minNodeSize=20, cp=0.001)
treeSize(second_tree)

## ------------------------------------------------------------------------
third_tree <- splineTree(split_formula, tformula, idvar, data, minNodeSize=20, cp=0.005)
treeSize(third_tree)  
stPrint(third_tree)
stPlot(third_tree, colors = c("red", "green", "blue", "cyan", "magenta", "grey"))

## ------------------------------------------------------------------------
stPlot(prune(second_tree, cp=0.005), colors = c("red", "green", "blue", "cyan", "magenta", "grey"))

## ------------------------------------------------------------------------
fourth_tree <- splineTree(split_formula, tformula, idvar, data, minNodeSize=20, cp=0.005, nGrid=50)
treeSimilarity(third_tree, fourth_tree)

## ------------------------------------------------------------------------
fifth_tree <- splineTree(split_formula, tformula, idvar, data, minNodeSize=20, cp=0.005, gridPoints=c(18))
treeSimilarity(third_tree, fifth_tree)

## ------------------------------------------------------------------------
linear_tree <- splineTree(split_formula, tformula, idvar, data, minNodeSize=20, cp=0.007, degree=1, df=2) 
stPlot(linear_tree, colors=c("red", "orange", "yellow", "green", "blue"))

## ------------------------------------------------------------------------
linear_tree_intercept <- splineTree(split_formula, tformula, idvar, data, minNodeSize=20, cp=0.007, degree=1, df=3, intercept=TRUE) 
stPlot(linear_tree_intercept)

## ------------------------------------------------------------------------
predictions <- predictY(linear_tree_intercept)
cor(predictions, nlsySample$BMI)
plot(predictions, (predictions-nlsySample$BMI), xlab="Predicted BMI", ylab = "Residual")

## ------------------------------------------------------------------------
yR2(linear_tree_intercept)

## ------------------------------------------------------------------------
linear_tree_intercept_big <- splineTree(split_formula, tformula, idvar, data, minNodeSize=5, cp=0.0001, degree=1, df=3, intercept=TRUE) 
treeSize(linear_tree_intercept_big)
yR2(linear_tree_intercept_big)

## ------------------------------------------------------------------------
projectedR2(linear_tree_intercept, includeIntercept = TRUE)
projectedR2(linear_tree_intercept, includeIntercept = FALSE)
projectedR2(linear_tree)

## ------------------------------------------------------------------------
### Get appropriate data
persondata <- nlsySample[nlsySample$ID==559,]

### Get the individual coefficients for this individual. Stored in "flat_data". 
personcoeffs <- linear_tree_intercept$parms$flat_data[linear_tree_intercept$parms$flat_data$ID==559,]$Ydata

### Get the predicted coefficients for this individual
personpreds <- predictCoeffs(linear_tree_intercept, persondata[1,])

### Form a basis matrix. We must append a column of 1s because this tree has an intercept.
basisMat <- cbind(1, bs(persondata$AGE, degree=linear_tree_intercept$parms$degree, Boundary.knots = linear_tree_intercept$parms$boundaryKnots, knots = linear_tree_intercept$parms$innerKnots))

### Calculate points along the individual smoothed trajectory and predicted trajectory
smoothed <- basisMat %*% t(personcoeffs)
predicted <- basisMat %*% personpreds

plot(BMI~AGE, data=persondata, pch=19)
points(persondata$AGE, smoothed, type='l', col="red")
points(persondata$AGE, predicted, type='l', col="blue")
#### Add the points obtained from "predictY" to show that they match the blue line
points(persondata$AGE, predictY(linear_tree_intercept, persondata), col="blue", pch=19)

