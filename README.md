<!-- README.md is generated from README.Rmd. Please edit that file -->
splinetree
==========

This package allows users to create, visualize, and evaluate regression trees and random forests for longitudinal or functional data through a spline projection method first suggested by Yu and Lambert (1999). 

Example
-------

```{r}
library(splinetree)
tree <- splineTree(BMI ~ HISP + WHITE + BLACK + HGC_MOTHER + SEX, BMI ~ AGE, "ID", nlsySample, degree = 1, intercept = FALSE, cp = 0.005)
R2 = projectedR2(tree1)
stPlot(tree1)
stPrint(tree1)
```
