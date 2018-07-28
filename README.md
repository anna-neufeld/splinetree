<!-- README.md is generated from README.Rmd. Please edit that file -->
splinetree
==========

The goal of this package is to create regression trees and random forests for longitudinal data through the spline projection method described in (link to paper). The package also allows users to evaluate and plot their models.

Example
-------

``` r
#library(splinetree)
#tree1 <- splineTree(BMI ~ HISP + WHITE + BLACK + HGC_MOTHER + SEX, BMI ~ AGE, "ID", nlsySample, degree = 1, intercept = FALSE, cp = 0.005)
#R2 = R2_projected(tree1)
#stPlots(tree1)
#treeSummary(tree1)
```
