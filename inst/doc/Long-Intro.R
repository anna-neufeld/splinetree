## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  fig.width = 7
)

## ---- include=FALSE------------------------------------------------------
library(splinetree)
split_formula <- BMI~HISP+WHITE+BLACK+SEX+Num_sibs+HGC_FATHER+HGC_MOTHER
tformula <- BMI~AGE
sample_tree <- splineTree(split_formula, tformula, idvar="ID", data=nlsySample, degree=1, df=2, intercept=FALSE, cp=0.005)
sample_tree_intercept<- splineTree(split_formula, tformula, idvar="ID", data=nlsySample, degree=1, df=3, intercept=TRUE, cp=0.005)

## ------------------------------------------------------------------------
stPrint(sample_tree)
stPlot(sample_tree)

## ------------------------------------------------------------------------
stPrint(sample_tree_intercept)
stPlot(sample_tree_intercept, colors=c("red", "orange", "yellow", "blue", "cyan"))

## ---- include=FALSE, eval=FALSE------------------------------------------
#  split_formula_2 <- BMI ~ HISP + WHITE + BLACK + SEX + Dad_Full_Work + Mom_Full_Work + Age_first_weed + Age_first_smoke + Age_first_alc + Num_sibs +  HGC_FATHER + HGC_MOTHER + Mag + News + Lib + Two_Adults_14 + Mother_14 + Father_14 + STABLE_RESIDENCE + URBAN_14 + South_Birth
#  sample_tree_large <- splineTree(split_formula_2, BMI~AGE, "ID", nlsySample, degree=1, df=2, intercept=TRUE, cp=0.0005)
#  sample_tree_small <- splineTree(split_formula_2, BMI~AGE, "ID", nlsySample, degree=1, df=2, intercept=TRUE, cp=0.01)
#  

## ---- include=FALSE, eval=FALSE------------------------------------------
#  vars = attr(terms(split_formula_2), "term.labels")
#  imp=sample_tree_large$variable.importance
#  imp_pruned=sample_tree_small$variable.importance
#  
#  all_imps = rep(0,2)
#  for (var in vars) {
#    thisRow = rep(0,2)
#    if (length(imp[names(imp)==var])>0) thisRow[1] = imp[names(imp)==var]
#    if (length(imp[names(imp_pruned)==var])>0) thisRow[2] = imp_pruned[names(imp_pruned)==var]
#    all_imps = rbind(all_imps, thisRow)
#  }
#  all_imps = all_imps[-1,]
#  row.names(all_imps)=vars
#  all_imps=data.frame(all_imps)
#  names(all_imps) = c("Large Tree", "Small Tree")
#  
#  par(mfrow=c(1,2))
#  par(las=2) # make label text perpendicular to axis
#  par(mar=c(1,7,3,1)) # increase y-axis margin.
#  barplot(all_imps[,2]/sum(all_imps[,2]), horiz=TRUE, names.arg=row.names(all_imps), cex.names=0.7, main="Small Tree", axes=FALSE)
#  barplot(all_imps[,1]/sum(all_imps[,1]), horiz=TRUE, names.arg=row.names(all_imps), cex.names=0.7, main="Large Tree", axes=FALSE)

## ---- eval=FALSE---------------------------------------------------------
#  forest <- splineForest(split_formula, tformula, idvar="ID", data=nlsySample, degree=1, df=3, intercept=TRUE,ntree=50, prob=0.5, cp=0.005)

## ---- include=FALSE------------------------------------------------------
coeff_imps <- importance[[2]] 
shape_imps <- importance[[3]]
Y_imps <- importance[[1]]

## ---- include=FALSE, eval=FALSE------------------------------------------
#  save(forest, file="./data/forest.rda", compress="xz")

## ------------------------------------------------------------------------
stPrint(forest$Trees[[1]])

## ------------------------------------------------------------------------
fullpreds <- predictYForest(forest, method="all")
oobpreds <- predictYForest(forest, method="oob")
itbpreds <- predictYForest(forest, method="itb")

## ------------------------------------------------------------------------
cor(fullpreds, nlsySample$BMI)
cor(itbpreds, nlsySample$BMI)
cor(oobpreds, nlsySample$BMI)

## ---- eval=FALSE---------------------------------------------------------
#  Y_imps <- varImpY(forest)
#  coeff_imps <- varImpCoeff(forest, removeIntercept=FALSE)
#  shape_imps <- varImpCoeff(forest, removeIntercept=TRUE)

## ---- eval=FALSE, include=FALSE------------------------------------------
#  save(Y_imps, coeff_imps, shape_imps, file='importance.rda')

## ------------------------------------------------------------------------
par(mfrow=c(1,3))
plotImp(Y_imps[,3], main="Response")
plotImp(coeff_imps[,3], main = "Coeff w/ Intercept")
plotImp(shape_imps[,3], main = "Coeff w/out Intercept")

