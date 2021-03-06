% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tree_print.R
\name{stPrint}
\alias{stPrint}
\title{Print a spline tree in the style of print.rpart}
\usage{
stPrint(t, cp, digits = getOption("digits"))
}
\arguments{
\item{t}{A model returned by splineTree()}

\item{cp}{Optional- if provided, a pruned version of the tree will be printed. The tree will be
pruned using the provided cp as the complexity parameter.}

\item{digits}{Specifies how many digits of each coefficient should be printed}
}
\description{
The printout provides numbered labels for the terminal nodes,
a description of the split at each node, the number of observations found at each node,
and the predicted spline coefficients for each node. This code is primarily taken from rpart base code for print.rpart. It has been modified to
ensure that the full vector of coefficients is printed for each node.
}
\examples{
\donttest{
split_formula <- ~HISP + WHITE + BLACK + SEX + Num_sibs + HGC_FATHER + HGC_MOTHER
tree <- splineTree(split_formula, BMI~AGE, idvar = "ID",
   data = nlsySample, degree = 1, df = 3,
   intercept = TRUE, cp = 0.005)
}
stPrint(tree)
}
