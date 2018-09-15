#' Print a spline tree object in the style of print.rpart
#'
#' The printout provides numbered labels for the terminal nodes,
#' a description of the split at each node, the number of observations found at each node,
#' and the predicted spline coefficients for each node. This code is primarily taken from rpart base code for print.rpart. It has been modified to
#' ensure that the full vector of coefficients is printed for each node.
#'
#' @param t A splinetree object
#' @param digits Specifies how many digits of each coefficient should be printed
#' @param cp Optional- if provided, a pruned version of the tree will be printed. The tree will be
#' pruned using the provided cp as the complexity parameter.
#' @importFrom stats naprint
#' @examples
#' \donttest{
#' split_formula <- ~HISP + WHITE + BLACK + SEX + Num_sibs + HGC_FATHER + HGC_MOTHER
#' tree <- splineTree(split_formula, BMI~AGE, idvar = "ID",
#'    data = nlsySample, degree = 1, df = 3,
#'    intercept = TRUE, cp = 0.005)
#' }
#' stPrint(tree)
#' @export
stPrint <- function(t, cp, digits = getOption("digits"))
{
  minlength = 0L
  spaces = 2L
  if (!inherits(t, "rpart")) stop("Not a legitimate \"splinetree\" object")

  if (!missing(cp)) t <- prune.rpart(t, cp = cp)
  frame <- t$frame
  ylevel <- attr(t, "ylevels")
  node <- as.numeric(row.names(frame))
  depth <- tree.depth(node)
  indent <- paste(rep(" ", spaces * 32L), collapse = "")
  ## 32 is the maximal depth
  indent <- if (length(node) > 1L) {
    indent <- substring(indent, 1L, spaces * seq(depth))
    paste0(c("", indent[depth]), format(node), ")")
  } else paste0(format(node), ")")

  if (!is.matrix(frame$yval2)) {
    yval <- sapply(frame$yval2,  function(x) format(signif(x, digits)))
    #yval <- yval2
  }
  else {
    yval2 <- apply(frame$yval2, 2, function(x) format(signif(x, digits)))
    yval <- apply(yval2, 1, paste, collapse=', ')
  }
  term <- rep(" ", length(depth))
  term[frame$var == "<leaf>"] <- "*"
  z <- labels(t, digits = digits, minlength = minlength)
  n <-paste(frame$n, ", ", sep='')
  z <- paste(indent, ' ', z, ", ", n, " (", yval, ")", term, sep='')


  omit <- t$na.action
  if (length(omit)) cat("n=", n[1L], " (", naprint(omit), ")\n\n", sep = "")
  else cat("n=", n[1L], "\n\n")

  cat("node), split, n , coefficients \n")
  cat("      * denotes terminal node\n\n")

  cat(z, sep = "\n")
  invisible(t)
}

#' Prints the tree frame.
#'
#' Provides a similar output to model$frame, but with the redundant information of yval and
#' yval2 removed. Also omits the deviance, the complexity, and the weight. Useful for viewing
#' node numbers and for extracting coefficients for a given node.
#'
#' @param model A splinetree object.
#' @return A dataframe. The number of rows is the same as the number of nodes in the tree.
#' The row names display the node labels of each node. The "var" attribute either displays
#' the split variable selected at each node, or <leaf> if this node is a terminal node. The "n"
#' attribute displays the number of individuals in the node. The "dev" attribute reports the
#' projected sum of squares at this node; terminal nodes have the smallest values for "dev" because
#' this is what the tree building process is supposed to minimize. The "coeffs" attribute displays
#' the coefficients predicted for each node.
#' @examples
#' \donttest{
#' split_formula <- ~HISP + WHITE + BLACK + SEX + Num_sibs + HGC_FATHER + HGC_MOTHER
#' tree <- splineTree(split_formula, BMI~AGE, idvar = "ID",
#'    data = nlsySample, degree = 1, df = 3,
#'    intercept = TRUE, cp = 0.005)
#' }
#' treeSummary(tree)
#' @export
treeSummary <- function(model) {
  frame <- model$frame
  summary <- data.frame(cbind(data.frame(frame$var),
                              frame$n, frame$dev))
  names(summary) <- cbind("var", "n", "dev")
  summary$coeffs <- frame$yval2
  row.names(summary) <- row.names(frame)
  summary
}

#' Prints a summary of a terminal node in a tree
#'
#' If no argument is provided for the parameter \code{node}, summaries are printed for every
#' terminal node. Otherwise, the summary of just the requested node is printed.
#'
#' @param tree A splinetree object
#' @param node The number of the node that you want summarized. To see which nodes correspond to
#' which numbers, see stPrint(tree) or treeSummary(tree). If this parameter is provided, must correspond
#' to a valid terminal node in the tree.
#' @export
#' @examples
#' \donttest{
#' split_formula <- ~HISP + WHITE + BLACK + SEX + Num_sibs + HGC_FATHER + HGC_MOTHER
#' tree <- splineTree(split_formula, BMI~AGE, idvar = "ID",
#'    data = nlsySample, degree = 1, df = 3,
#'    intercept = TRUE, cp = 0.005)
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
    cat(paste("\n Coefficients:",coeffs, "\n"))
  }
}

#' Given a list of node numbers, returns the depth at which these appear in the tree.
#'
#' Used in printing and plotting.
#' Source: rpart
#' @keywords internal
tree.depth <- function (nodes)
{
  depth <- floor(log(nodes, base = 2) + 1e-7)
  depth - min(depth)
}


