
<!-- README.md is generated from README.Rmd. Please edit that file -->
splinetree
==========

[![Build Status](https://travis-ci.org/anna-neufeld/splinetree.svg?branch=master)](https://travis-ci.org/anna-neufeld/splinetree)

This package allows users to create, visualize, and evaluate regression trees and random forests for longitudinal or functional data through a spline projection method first suggested by Yu and Lambert (1999).

Installation
------------

You can install splinetree from github with:

``` r
# install.packages("devtools")
devtools::install_github("anna-neufeld/splinetree")
```

Example Tree
------------

``` r
library(splinetree)
#> Loading required package: rpart
#> Loading required package: nlme
#> Loading required package: splines
tree1 <- splineTree(~HISP+WHITE+BLACK+HGC_MOTHER+HGC_FATHER+SEX+Num_sibs, 
        BMI ~ AGE, "ID", nlsySample, degree = 1, df=2, intercept = FALSE, cp = 0.005)
stPrint(tree1)
#> n= 1000,  
#> 
#> node), split, n , coefficients 
#>       * denotes terminal node
#> 
#> 1) root, 1000,  (4.961796, 8.091247) 
#>   2) WHITE< 0.5, 505,  (5.882807, 9.006190)*
#>   3) WHITE>=0.5, 495,  (4.022179, 7.157821) 
#>     6) HGC_FATHER< 8.5, 78,  (5.198284, 8.642817)*
#>     7) HGC_FATHER>=8.5, 417,  (3.802188, 6.880053)*
```

``` r
stPlot(tree)
```

![](https://raw.githubusercontent.com/anna-neufeld/splinetree/master/README-unnamed-chunk-3-1.png)

Example Forest
--------------

``` r
set.seed(1234)
forest1 <- splineForest(~HISP+WHITE+BLACK+HGC_MOTHER+HGC_FATHER+SEX+Num_sibs, 
        BMI ~ AGE, "ID", nlsySample, degree = 1, df=2, intercept = FALSE, ntree=50, prob=1/2)
varImps <- varImpCoeff(forest1)
```

``` r
plotImp(varImps[,3])
```

![](https://raw.githubusercontent.com/anna-neufeld/splinetree/master/README-unnamed-chunk-5-1.png)
