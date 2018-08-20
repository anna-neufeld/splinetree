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

