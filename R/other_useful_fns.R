#' Returns number of terminal nodes in a tree.
#' @param model A splinetree object, or any rpart object
#' @return The number of terminal nodes in the tree
#' @export
#' @examples
#' \dontrun{
#' split_formula <- ~ HISP + WHITE + BLACK + SEX + HGC_FATHER + HGC_MOTHER + Num_sibs
#' tree <- splineTree(split_formula, BMI~AGE, 'ID', nlsySample, degree=1,
#'   df=3, intercept=TRUE, cp=0.006, minNodeSize=20)
#' }
#' treeSize(tree)
treeSize <- function(model) {
    NROW(unique(model$where))
}


#' Returns a measure of how similar the two trees are.
#'
#' Computes the Adjusted Rand Index of the clusterings of the population created by the two trees.
#' In the case of correlated covariates, two trees that split on entirely different variables may actually
#' describe similar partitions of the population. This metric allows us to detect when two trees are partitioning
#' the population similarly. A value close to 1 indicates a similar clustering.
#' @param tree1 a splinetree object
#' @param tree2 a splinetree object
#' @return The Adjusted Rand Index of the clusterings created by the two trees.
#' @importFrom mclust adjustedRandIndex
#' @seealso mclust::adjustedRandIndex
#' @examples
#' \donttest{
#' splitForm <- ~SEX+Num_sibs+HGC_MOTHER+HGC_FATHER
#' nlsySubset <- nlsySample[nlsySample$ID %in% sample(unique(nlsySample$ID), 400),]
#' tree1 <- splineTree(splitForm, BMI~AGE, "ID", nlsySubset, degree=1, df=2, intercept=FALSE, cp=0.005)
#' tree2 <- splineTree(splitForm, BMI~AGE, "ID", nlsySubset, degree=1, df=3, intercept=TRUE, cp=0.005)
#' treeSimilarity(tree1, tree2)
#' }
#' @export
treeSimilarity <- function(tree1, tree2) {
  adjustedRandIndex(tree1$where, tree2$where)
}

#' Retrieve the subset of the data found at a given terminal node
#'
#' Given a terminal node number, this function returns the data belonging to
#' this terminal node. If the dataType argument is 'all', returns all rows of data from the
#' original dataset that fall in this node.  Otherwise, the flattened data that belongs to
#' this node is returned (one row of data per ID, original responses replaced by spline coefficients).
#'
#' @param tree a splinetree object
#' @param node The number of the node to retrieve data from. Must be valid
#' number of a terminal node. Node numbers can be seen using stPrint(tree)
#' or treeSummary(tree).
#' @param dataType If "all", the data returned is from the original dataset (one row per individual observation
#' with original response values). If "flat", the data returned is the flattened data (one row per person/unit),
#' with individual spline coefficients instead of response values.
#' @return A dataframe which holds all the data that falls into this node of the tree.
#' @export
#' @examples
#' \dontrun{
#' split_formula <- BMI ~ HISP + WHITE + BLACK + SEX +
#'   Num_sibs + HGC_FATHER + HGC_MOTHER
#' tree <- splineTree(split_formula, BMI~AGE, 'ID', nlsySample, degree=1,
#'   df=3, intercept=TRUE, cp=0.006, minNodeSize=20)
#' }
#' node6data <- getNodeData(tree, 6, dataType = 'all')
#' plot(BMI~AGE, data=node6data)
getNodeData <- function(tree, node, dataType = 'all') {
  nodeIndex <- which(row.names(tree$frame)==node)
  if (tree$frame[nodeIndex,]$var != "<leaf>") stop("This node number does not correspond to a terminal node.
                                                   Please look at the numbers provided in the
                                                   stPrint() printout printed tree and try again.")

  flat_node_data = tree$parms$flat_data[tree$where==nodeIndex,]
  if (dataType=="flat") {
    flat_node_data
  }
  else {
    tree$parms$data[tree$parms$data[[tree$parms$idvar]] %in% flat_node_data[[tree$parms$idvar]],]
  }
}


