% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/forest_variable_importance.R
\name{varImpY}
\alias{varImpY}
\title{Random Forest Variable Importance based on Y}
\usage{
varImpY(forest, method = "oob")
}
\arguments{
\item{forest}{a random forest, generated from splineForest()}

\item{method}{the method to be used. This must be one of "oob" (out of bag), "all", "itb" (in the bag).}
}
\value{
A matrix storing variable importance metrics. The rows correspond to split variables.
The columns are different methods of measuring importance. The first column is the absolute importance
(mean difference in performance between permuted and unpermuted datasets). The second column measures the
mean percent difference in performance. The third column standardizes the differences by dividing them
by their standard deviation.
}
\description{
Returns the random forest variable importance based on the permutation accuracy measure, which is calculated as the difference in mean squared error between the original data and from randomly permuting the values of a variable.
}
\details{
The "method" parameter deals with the way in which forest performance should be measured. Since variable importance is based on a change
in performance, the "method" parameter is necessary for a variable importance measure. The choices are "oob" (out of bag), "all", or "itb" (in the bag).
}
\examples{
\donttest{
importanceMatrix <- varImpY(forest, method="oob")
plotImp(importanceMatrix[,3])
}
}
