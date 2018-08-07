#' Print a spline tree object
#'
#' Code adapted only slightly from the rpart base code for print.rpart to support the printing of
#' all coefficients.
#'
#' @param x The splinetree object
#' @param digits Specifies how many digits of each coefficient should be printed
#' @param cp Optional- if provided, a pruned version of the tree will be printed. The tree will be
#' pruned using the provided cp as the complexity parameter.
#'
#' @return A printout of the tree. The printout provides numbered labels for the terminal nodes,
#' a description of the split at each node, the number of observations found at each node, and the
#' predicted spline coefficients for each node.
#' @export
print.splinetree <- function(x, minlength = 0L, spaces = 2L, cp,
                        digits = getOption("digits"), ...)
{
  if (!inherits(x, "rpart")) stop("Not a legitimate \"splinetree\" object")

  if (!missing(cp)) x <- prune.rpart(x, cp = cp)
  frame <- x$frame
  ylevel <- attr(x, "ylevels")
  node <- as.numeric(row.names(frame))
  depth <- tree.depth(node)
  indent <- paste(rep(" ", spaces * 32L), collapse = "")
  ## 32 is the maximal depth
  indent <- if (length(node) > 1L) {
    indent <- substring(indent, 1L, spaces * seq(depth))
    paste0(c("", indent[depth]), format(node), ")")
  } else paste0(format(node), ")")

  yval2 <- apply(frame$yval2, 2, function(x) format(signif(x, digits)))
  yval <- apply(yval2, 1, paste, collapse=', ')
  term <- rep(" ", length(depth))
  term[frame$var == "<leaf>"] <- "*"
  z <- labels(x, digits = digits, minlength = minlength, ...)
  n <-paste(frame$n, ", ", sep='')
  z <- paste(indent, ' ', z, ", ", n, " (", yval, ")", term, sep='')


  omit <- x$na.action
  if (length(omit)) cat("n=", n[1L], " (", naprint(omit), ")\n\n", sep = "")
  else cat("n=", n[1L], "\n\n")

  ## This is stolen, unabashedly, from print.tree
  cat("node), split, n , coefficients \n")
  cat("      * denotes terminal node\n\n")

  cat(z, sep = "\n")
  invisible(x)
}
