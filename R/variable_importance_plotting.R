#' Create a barplot of relative variable importance scores.
#'
#' Given a named vector of variable importance measures, this function makes a barplot of the relative importances.
#' The importances are scaled to sum to 1. An appropriate input is one column of the output from
#' varImpY() or varImpCoeff().
#'
#' @param importance_vector a named vector where the names are the variables and the vector stores the importances.
#' @param ... additional arguments to plot, such as "main", "cex", etc.
#' @export
#' @examples
#' \donttest{
#' imp <- varImpCoeff(forest)[,3]
#' }
#' \dontshow{
#' imp <- importance[[2]][,3]
#' }
#' plotImp(imp, main="Standardized Variable Importance")
plotImp <- function(importance_vector, ...) {
    importance_vector[importance_vector<0]=0
    try({
    par(las = 2)
    par(mar = c(1, 7, 1, 1))
    barplot(importance_vector/sum(importance_vector),
        horiz = TRUE, names.arg = row.names(importance_vector),
        cex.names = 0.7,
        axes = FALSE, ...)
    }, silent=TRUE)
}
