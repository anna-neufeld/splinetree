#' Returns number of terminal nodes in a tree.
#' @param model A splinetree object, or any rpart object
#' @return Number of terminal nodes in tree
#' @export
#' @examples
#' \dontrun{
#' split_formula <- BMI ~ HISP + WHITE + BLACK + SEX + Dad_Full_Work
#'   + Mom_Full_Work   + Age_first_weed + Age_first_smoke + Age_first_alc
#'   + Num_sibs + HGC_FATHER + HGC_MOTHER + Mag + News + Lib + Two_Adults_14
#'   + Mother_14 + Father_14 + STABLE_RESIDENCE + URBAN_14 + South_Birth
#' tree <- splineTree(split_formula, BMI~AGE, 'ID', nlsySample, degree=1,
#'   df=3, intercept=TRUE, cp=0.006, minNodeSize=20)
#' }
#' treeSize(tree)
treeSize <- function(model) {
    NROW(unique(model$where))
}

#' Prints a summary of a terminal node in a tree
#'
#' If no arguement is provided for the parameter \code{node}, summaries are printed for every
#' terminal node. Otherwise, the summary of just the requested node is printed.
#'
#' @param tree A splinetree object
#' @param node The number of the node that you want summarized. To see which nodes correspond to
#' which numbers, see stPrint(tree).
#' @export
#' @example
#' \dontrun{
#' split_formula <- BMI ~ HISP + WHITE + BLACK + SEX + Dad_Full_Work
#'   + Mom_Full_Work   + Age_first_weed + Age_first_smoke + Age_first_alc
#'   + Num_sibs + HGC_FATHER + HGC_MOTHER + Mag + News + Lib + Two_Adults_14
#'   + Mother_14 + Father_14 + STABLE_RESIDENCE + URBAN_14 + South_Birth
#' tree <- splineTree(split_formula, BMI~AGE, 'ID', nlsySample, degree=1,
#'   df=3, intercept=TRUE, cp=0.006, minNodeSize=20)
#' }
#' terminalNodeSummary(tree)
terminalNodeSummary <- function(tree, node=NULL) {
  if (is.null(node)) {
    for (i in 1:nrow(tree$frame)) {
      if (tree$frame[i,]$var == "<leaf>") {
        path.rpart(tree, row.names(tree$frame)[i])
        cat(paste("\n N:", tree$frame[i,]$n))
        coeffs  <- paste(tree$frame[i,]$yval2, collapse=',')
        cat(paste("\n Coefficients:",coeffs))
        cat('\n----------\n')
      }
    }
  }
  else {
    path.rpart(tree, node)
    nodeIndex = which(row.names(tree$frame)==toString(node))
    cat(paste("\n N:", tree$frame[nodeIndex,]$n))
    coeffs  <- paste(tree$frame[nodeIndex,]$yval2, collapse=',')
    cat(paste("\n Coefficients:",coeffs))
  }
}


#' Returns the portion of the data found at a given terminal node
#'
#' Given a terminal node number, this function returns the dataset found at this terminal node.
#' If the dataType arguement is 'all', then all rows of data (with original response values) that
#' fall in this node are returned.  Otherwise, the flattened data is returned (one row of data per
#' person/unit, original responses replaced by spline coefficients).
#'
#' @param tree a splinetree object
#' @param node The number of the node that you want the data for.
#' Node numbers for your model can be seen using stPrint(tree)
#' or treeSummary(tree). Note that this node number should correspond to
#' a terminal node.
#' @param dataType If "all", the data returned is the original data (one row per individual observation
#' with original response values). If "flat", the data returned is the flattened data (one row per person/unit),
#' with spline coefficients instead of response values.
#' @return A dataframe which holds all the data that falls into this node of the tree.
#' @export
#' @example
#' \dontrun{
#' split_formula <- BMI ~ HISP + WHITE + BLACK + SEX + Dad_Full_Work
#'   + Mom_Full_Work   + Age_first_weed + Age_first_smoke + Age_first_alc
#'   + Num_sibs + HGC_FATHER + HGC_MOTHER + Mag + News + Lib + Two_Adults_14
#'   + Mother_14 + Father_14 + STABLE_RESIDENCE + URBAN_14 + South_Birth
#' tree <- splineTree(split_formula, BMI~AGE, 'ID', nlsySample, degree=1,
#'   df=3, intercept=TRUE, cp=0.006, minNodeSize=20)
#' }
#' node10data <- getNodeData(model1, 10, dataType = 'all)
#' plot(BMI~AGE, data=node10data)
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


#' Plot the predicted trajectory for a single node
#'
#' Make a simple plot to view the trajectory predicted at a given node. Option to include or not include
#' the individual trajectories of people in the node as well.
#'
#' @importFrom ggplot2 ggplot xlab ylab
#' @export
#' @param tree a splinetree object
#' @param node a node number that must correspond to a terminal node
#' @param includeData would you like to see the data from the node
#' plotted along with the predicted trajectory?
#' @example
#' \dontrun{
#' split_formula <- BMI ~ HISP + WHITE + BLACK + SEX + Dad_Full_Work
#'   + Mom_Full_Work   + Age_first_weed + Age_first_smoke + Age_first_alc
#'   + Num_sibs + HGC_FATHER + HGC_MOTHER + Mag + News + Lib + Two_Adults_14
#'   + Mother_14 + Father_14 + STABLE_RESIDENCE + URBAN_14 + South_Birth
#' tree <- splineTree(split_formula, BMI~AGE, 'ID', nlsySample, degree=1,
#'   df=3, intercept=TRUE, cp=0.006, minNodeSize=20)
#' }
#' plotNodeTraj(tree, 10, includeData=TRUE)
plotNodeTraj <-  function(tree, node, includeData = FALSE) {
  nodeIndex <- which(row.names(tree$frame)==toString(node))
  nodeCoeffs <- tree$frame[nodeIndex,]$yval2
  if (tree$frame[nodeIndex,]$var != "<leaf>") stop("This node number does not correspond to a terminal node.
                                                   Please look at the numbers provided in the
                                                   stPrint() printout printed tree and try again.")

  flat_node_data = tree$parms$flat_data[tree$where==nodeIndex,]
  data = tree$parms$data[tree$parms$data[[tree$parms$idvar]] %in% flat_node_data[[tree$parms$idvar]],]

  xRange = range(data[[tree$parms$tvar]])
  xGrid = seq(xRange[1], xRange[2], length.out=20)
  if (tree$parms$intercept) {
    newxmat <- cbind(1, bs(xGrid, knots = tree$parms$innerKnots,
                           Boundary.knots = tree$parms$boundaryKnots,
                           degree = tree$parms$degree))
    preds <- newxmat %*% t(nodeCoeffs)
  } else {
    newxmat <- bs(xGrid, knots = tree$parms$innerKnots,
                           Boundary.knots = tree$parms$boundaryKnots,
                           degree = tree$parms$degree)
    mean_int = mean(tree$parms$data[(tree$parms$data[[tree$parms$tvar]] -
                                  min(tree$parms$data[[tree$parms$tvar]])) < 1, ][[tree$parms$yvar]])
    preds <- newxmat %*% t(nodeCoeffs) + mean_int
  }

    ggplot() +geom_line(data=data, mapping = aes_string(x = tree$parms$tvar, y = tree$parms$yvar,
                                    group = tree$parms$idvar))+theme(legend.position="none")+xlab(tree$parms$tvar)+ylab(tree$parms$yvar)+ geom_line(aes(x=xGrid, y=preds, color="red", size=2))
}
