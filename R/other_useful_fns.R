#' Returns number of terminal nodes in a tree.
#' @param model A splinetree object, or any rpart object
#' @return Number of terminal nodes in tree
treeSize <- function(model) {
    NROW(unique(model$where))
}

#' Prints the tree frame.
#'
#' @param model A splinetree object.
treeSummary <- function(model) {
    frame <- model$frame
    frame$coeffs = frame$yval2
    summary <- data.frame(cbind(data.frame(frame$var),
        frame$n, frame$dev, frame$coeffs))
    names(summary) = cbind("var", "n", "dev", "coeffs")
    summary
}
