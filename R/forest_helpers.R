#' Compute the average tree size in a forest
#'
#' Returns the average number of terminal nodes for trees in a forest
#'
#' @param forest A splineforest object
#'
#' @return The avergae number of terminal nodes in forest
#' @export
#' @examples
#' avSize(forest)
avSize <- function(forest) {
    print(mean(as.numeric(as.matrix(lapply(forest$Trees,
        function(x) NROW(unique(x$where)))))))
}

#' Prune each tree in forest using a given complexity parameter.
#'
#' Prunes each tree in the list forest$Trees according to the provided complexity parameter. Returns a new forest.
#'
#' @return A new splineforest object where each tree has been pruned to the desired level.
#' @param forest A spline forest object
#' @param cp The complexity parameter that will be used to prune each tree (see rpart package documentation for detailed description of complexity parameter)
#' @examples
#' print(avSize(forest))
#' print(avSize(pruneForest(forest, cp=0.007)))
#' print(avSize(pruneForest(forest, cp=0.01)))
#' @export
pruneForest <- function(forest, cp) {
    new_forest = forest
    for (i in c(1:length(new_forest$Trees))) {
        new_forest$Trees[[i]] <- prune(forest$Trees[[i]],
            cp = cp)
    }
    new_forest
}
