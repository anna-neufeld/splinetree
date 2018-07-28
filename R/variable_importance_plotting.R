#' Create a variable importance plot
#' 
#' Pass in a named vector of variable importance measures. This function will make a barplot of the importances. The importances are scaled, so
#' only relative importance is shown.
#' 
#' @param importance_vector a named vector where the names are the variables and the vector stores the importances. 
plot_varimp <- function(importance_vector) {
    try({
    par(las = 2)
    par(mar = c(1, 7, 1, 1))
    barplot(importance_vector/sum(importance_vector), 
        horiz = TRUE, names.arg = row.names(importance_vector), 
        cex.names = 0.7, main = "Variable Importance", 
        axes = FALSE)
    }, silent=TRUE)
}
