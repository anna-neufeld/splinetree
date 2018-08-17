#' Average tree size in forest
#'
#' Returns the average number of terminal nodes for tree in forest
#'
#' @param forest A splineforest object
#'
#' @return avergae number of terminal nodes
#' @export
#' @examples
#' avSize(sample_forest)
avSize <- function(forest) {
    print(mean(as.numeric(as.matrix(lapply(forest$Trees,
        function(x) NROW(unique(x$where)))))))
}

#' Prunes each tree in forest using a given complexity parameter.
#'
#' @return A new splinetree forest object, where each tree has a new size
#' @param forest A spline forest object
#' @param cp The complexity parameter that will be used to prune each tree (see rpart package documentation for detailed description of complexity parameter)
#' @examples
#' print(avSize(sample_forest))
#' print(avSize(pruneForest(sample_forest, cp=0.007)))
#' print(avSize(pruneForest(sample_forest, cp=0.01)))
#' @export
pruneForest <- function(forest, cp) {
    new_forest = forest
    for (i in c(1:length(new_forest$Trees))) {
        new_forest$Trees[[i]] <- prune(forest$Trees[[i]],
            cp = cp)
    }
    new_forest
}
