## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 8
)

## ---- include=FALSE------------------------------------------------------
library(splinetree)

## ------------------------------------------------------------------------
set.seed(123)
splitform <- BMI~HISP+WHITE+BLACK+HGC_FATHER+Num_sibs+HGC_MOTHER+SEX+News+Mag+Lib
#forest <- splineForest(splitform, BMI~AGE, "ID", nlsySample, degree=1, df=2, intercept=FALSE, cp=0.001, ntree=50)
#intercept_forest <- splineForest(BMI~HISP+WHITE+BLACK+HGC_FATHER+Num_sibs+HGC_MOTHER+SEX+News+Mag+Lib, BMI~AGE, "ID", nlsySample, degree=1, df=3, intercept=TRUE, cp=0.001, ntree=50)

