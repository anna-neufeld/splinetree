#' Print a spline tree object
#'
#' Code adapted only slightly from the rpart base code for print.rpart to support the printing of
#' all coefficients.
#'
#' @param tree A splinetree object
#' @param digits Specifies how many digits of each coefficient should be printed
#' @param cp Optional- if provided, a pruned version of the tree will be printed. The tree will be
#' pruned using the provided cp as the complexity parameter.
#' @return A printout of the tree. The printout provides numbered labels for the terminal nodes,
#' a description of the split at each node, the number of observations found at each node, and the
#' predicted spline coefficients for each node.
#' @importFrom stats naprint
#' @examples
#' \dontrun{
#' split_formula <- BMI ~ HISP + WHITE + BLACK + SEX + Dad_Full_Work
#'   + Mom_Full_Work   + Age_first_weed + Age_first_smoke + Age_first_alc
#'   + Num_sibs + HGC_FATHER + HGC_MOTHER + Mag + News + Lib + Two_Adults_14
#'   + Mother_14 + Father_14 + STABLE_RESIDENCE + URBAN_14 + South_Birth
#'  tree <- splineTree(split_formula, BMI~AGE, 'ID', nlsySample, degree=1,
#'    df=3, intercept=TRUE, cp=0.006, minNodeSize=20)
#' }
#' stPrint(tree)
#' @export
stPrint <- function(tree, cp, digits = getOption("digits"))
{
  minlength = 0L
  spaces = 2L
  if (!inherits(tree, "rpart")) stop("Not a legitimate \"splinetree\" object")

  if (!missing(cp)) tree <- prune.rpart(tree, cp = cp)
  frame <- tree$frame
  ylevel <- attr(tree, "ylevels")
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
  z <- labels(tree, digits = digits, minlength = minlength)
  n <-paste(frame$n, ", ", sep='')
  z <- paste(indent, ' ', z, ", ", n, " (", yval, ")", term, sep='')


  omit <- tree$na.action
  if (length(omit)) cat("n=", n[1L], " (", naprint(omit), ")\n\n", sep = "")
  else cat("n=", n[1L], "\n\n")

  ## This is stolen, unabashedly, from print.tree
  cat("node), split, n , coefficients \n")
  cat("      * denotes terminal node\n\n")

  cat(z, sep = "\n")
  invisible(tree)
}

#' Prints the tree frame.
#'
#' @param model A splinetree object.
#' @example
#' \dontrun{
#' split_formula <- BMI ~ HISP + WHITE + BLACK + SEX + Dad_Full_Work
#'   + Mom_Full_Work   + Age_first_weed + Age_first_smoke + Age_first_alc
#'   + Num_sibs + HGC_FATHER + HGC_MOTHER + Mag + News + Lib + Two_Adults_14
#'   + Mother_14 + Father_14 + STABLE_RESIDENCE + URBAN_14 + South_Birth
#' tree <- splineTree(split_formula, BMI~AGE, 'ID', nlsySample, degree=1,
#'   df=3, intercept=TRUE, cp=0.006, minNodeSize=20)
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
#' If no arguement is provided for the parameter \code{node}, summaries are printed for every
#' terminal node. Otherwise, the summary of just the requested node is printed.
#'
#' @param tree A splinetree object
#' @param node The number of the node that you want summarized. To see which nodes correspond to
#' which numbers, see stPrint(tree).
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


