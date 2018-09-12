#' Plots a splinetree.
#'
#' Creates a two paneled plot of a splinetree object that shows both the tree and the trajectories side by side.
#' Note that this function has trouble when the plot window is not wide enough. If nothing shows up in RStudio,
#' try increasing the size of the plot window and trying again. For a tree without an intercept, intercepts are
#' estimated after-the-fact for each node using the average starting value in the data so that the plotted
#' trajectories have reasonable response values.
#'
#' @param model A splinetree object
#' @param colors A list of colors that will be used for the trajectories (if NULL, will automatically select colors from
#' rainbow color scheme.
#' @importFrom grDevices dev.off
#' @examples
#' \donttest{
#' split_formula <- ~HISP + WHITE + BLACK + SEX + Num_sibs + HGC_FATHER + HGC_MOTHER
#' tree <- splineTree(split_formula, tformula, idvar = "ID",
#'    data = nlsySample, degree = 1, df = 3,
#'    intercept = TRUE, cp = 0.005)
#' }
#' stPlot(tree, colors = c("red", "orange", "green", "blue", "cyan", "magenta"))
#' @export
stPlot <- function(model, colors = NULL) {
    if (is.null(colors)) {
        colors = rainbow(dim(model$frame)[1], v = 0.9)
    }
    try1 <- try({
    layout(matrix(c(1, 2), 1, 2), widths = c(2, 1))
    par(mar = c(1, 1, 1, 1), xpd = NA)
    splineTreePlot(model, colors = colors)
    par(mar = c(4.4, 4, 2, 2))
    nodePlot(model, colors = colors)
    }, silent=TRUE)
}



#' Creates a tree plot of a splinetree object.
#'
#' Creates a tree plot of a splinetree object. This corresponds to plotting only the first panel of
#' stPlot(). Code for this function was borrowed from the longRPart package on github.
#'
#' @param model a splinetree object
#' @param colors a list of colors that will be used for the terminal nodes (if NULL, will use a rainbow)
#' @export
#' @importFrom grDevices dev.cur gray rainbow
splineTreePlot <- function(model, colors = NULL) {
    use.n = TRUE
    data = model$parms$data
    tvar = model$parms$tvar
    yvar = model$parms$yvar
    idvar = model$parms$idvar
    degree = model$parms$degree
    df = model$parms$df
    intercept = model$parms$intercept
    boundaryKnots = model$parms$boundaryKnots
    innerKnots = model$parms$innerKnots

    ### Code largely copied from the longRpart github page
    indexes = rpartco(model)
    plot(model, uniform = TRUE, ylim = range(indexes$y) -
        c(diff(range(indexes$y)) * 0.125, 0), xlim = range(indexes$x) +
        c(-1, 1) * diff(range(indexes$x) * 0.1))

    f = model$frame
    leaves = (1:dim(f)[1])[f$var == "<leaf>"]
    timeVar = data[, tvar]

    nodeindices = unique(model$where)
    terminalNodes = model$frame[nodeindices, ]
    nodes = as.numeric(row.names(terminalNodes))
    nodelist = as.numeric(row.names(model$frame))

    timeValues = unique(timeVar)

    ### For plotting each person's individual curve
    plotList = list()
    peopleIndices = unique(data[[idvar]])
    for (i in peopleIndices) {
        personData = data[which(data[[idvar]] == i), ]
        ys <- personData[[yvar]]
        xs <- personData[[tvar]]
        pts = ys
        plotList[[i]] = pts
    }

    ### For plotting the node average curve.
    plotList2 = list()
    for (i in 1:length(leaves)) {
        nodeMembers = peopleIndices[which(model$where == leaves[i])]
        personData = data[which(data[[idvar]] %in% nodeMembers), ]

        realcoeffs = model$frame[leaves[i], ]$yval2

        newx <- seq(min(personData[[tvar]]), max(personData[[tvar]]),
            length = 20)
        if (intercept == TRUE) {
            newxmat <- cbind(1, bs(newx, knots = innerKnots,
                Boundary.knots = boundaryKnots,
                degree = degree))
        } else {
            newxmat <- bs(newx, knots = innerKnots,
                Boundary.knots = boundaryKnots,
                degree = degree)
        }

        ### Add mean intercept if this model was a no-intercept model
        if (intercept == FALSE) {
            mean_int = mean(personData[(personData[[tvar]] -
                min(personData[[tvar]])) < 1, ][[yvar]])
            preds <- newxmat %*% t(realcoeffs) +
                mean_int
        } else {
            preds <- newxmat %*% t(realcoeffs)
        }
        plotList2[[i]] = preds
    }


    #### Takes care of plotting. Mostly taken from
    #### longRPart code
    subYLim = range(plotList, na.rm = TRUE)
    xRange = range(indexes$x)
    yRange = range(indexes$y)
    if (is.null(colors)) {
        colors = rainbow(dim(f)[1], v = 0.9)
    }

    for (i in 1:length(leaves)) {
        nodeMembers = peopleIndices[which(model$where ==
            leaves[i])]
        limx = indexes$x[leaves[i]] + c(-1, 1) *
            diff(xRange) * 0.075
        limy = indexes$y[leaves[i]] + c(-2, 0) *
            diff(yRange) * 0.075
        rect(limx[1], limy[1], limx[2], limy[2],
            density = -1, col = gray(0.9))

        for (person in nodeMembers) {
            pts = plotList[[person]]
            pts = (pts - subYLim[1])/(subYLim[2] -
                subYLim[1])
            pts = pts * (limy[2] - limy[1]) + limy[1]
            points(x = seq(limx[1], limx[2], length.out = length(pts)),
                pts, lwd = 2, type = "l", col = "black")
        }

        pts = plotList2[[i]]
        pts = (pts - subYLim[1])/(subYLim[2] -
            subYLim[1])
        pts = pts * (limy[2] - limy[1]) + limy[1]
        points(x = seq(limx[1], limx[2], length.out = length(pts)),
            pts, lwd = 3, type = "l", col = colors[i])
    }
    try(text(model, use.n = TRUE, all = TRUE, cex = 0.6),
        silent = TRUE)
}


#' Plots the trajectories of each terminal node side by side.
#'
#' Corresponds to plotting only the second panel of stPlot(). If model$intercept==FALSE, estimated
#' intercepts are added to each trajectory so that the trajectories are plotted at the level of reasonable
#' response values.
#'
#' @param model A splinetree object
#' @param colors A list of colors to use. By default, uses colors drawn from a rainbow.
#' @export
nodePlot <- function(model, colors = NULL) {
    data = model$parms$data
    tvar = model$parms$tvar
    yvar = model$parms$yvar
    idvar = model$parms$idvar
    degree = model$parms$degree
    df = model$parms$df
    intercept = model$parms$intercept
    innerKnots = model$parms$innerKnots
    boundaryKnots = model$parms$boundaryKnots

    f = model$frame
    leaves = (1:dim(f)[1])[f$var == "<leaf>"]
    peopleIndices = unique(data[[idvar]])

    plotList2 = list()
    xplotList = list()
    for (i in 1:length(leaves)) {
        nodeMembers = peopleIndices[which(model$where ==
            leaves[i])]
        personData = data[which(data[[idvar]] %in%
            nodeMembers), ]
        newx <- sort(unique(model$parms$data[[tvar]]))

        if (intercept == TRUE) {
            newxmat <- cbind(1, bs(newx, knots = innerKnots,
                Boundary.knots = boundaryKnots,
                degree = degree))
            mean_int = 0
        } else {
            newxmat <- bs(newx, knots = innerKnots,
                Boundary.knots = boundaryKnots,
                degree = degree)
            mean_int = mean(personData[(personData[[tvar]] -
                                          min(personData[[tvar]])) < 1, ][[yvar]])
        }

        realcoeffs = model$frame[leaves[i], ]$yval2
        preds2 <- newxmat %*% t(realcoeffs)

        xplotList[[i]] = newx
        plotList2[[i]] = preds2 + mean_int
    }

    f = model$frame
    if (is.null(colors)) {
        colors = rainbow(dim(f)[1], v = 0.9)
    }

    xmax = max(unlist(xplotList))
    xmin = min(unlist(xplotList))
    ymax = max(unlist(plotList2))
    ymin = min(unlist(plotList2))

    nodePlot <- plot(0, 0, col = "white", xlim = c(xmin,
        xmax), ylim = c(ymin, ymax), xlab = tvar,
        ylab = yvar)
    for (i in 1:length(leaves)) {
        xpts = xplotList[[i]]
        ypts = plotList2[[i]]
        points(x = xpts, y = ypts, lwd = 3, type = "l",
            col = colors[i])
    }
}

#' Create a facetted spaghetti plot of a splinetree model
#'
#' Uses ggplot to create a paneled spaghetti plot of the data, where each panel corresponds to a terminal node in the tree.
#' Allows users to visualize homogeneity of trajectories within the terminal nodes of the tree while also looking
#' at the trajectories of different nodes side by side.
#'
#' @param model a splinetree object
#' @param colors optional arguement specifying colors to be used for each panel.
#' @export
#' @importFrom ggplot2 ggplot aes aes_string facet_grid geom_line geom_point scale_color_manual theme
#' @examples
#' \donttest{
#' split_formula <- ~HISP + WHITE + BLACK + SEX + Num_sibs + HGC_FATHER + HGC_MOTHER
#' tree <- splineTree(split_formula, tformula, idvar = "ID",
#'    data = nlsySample, degree = 1, df = 3,
#'    intercept = TRUE, cp = 0.005)
#' }
#' spaghettiPlot(tree)
spaghettiPlot <- function(model, colors = NULL) {
    wheres <- data.frame(cbind(unique(model$parms$data$ID),
        as.numeric(model$where)))
    names(wheres) <- c("ID", "where")

    dat <- model$parms$data
    tvar <- model$parms$tvar
    yvar <- model$parms$yvar
    idvar <- model$parms$idvar
    intercept <- model$parms$intercept

    internalKnots <- model$parms$innerKnots
    boundaryKnots <- model$parms$boundaryKnots

    df <- model$parms$df
    degree <- model$parms$degree

    newDat <- data.frame(cbind(dat[[tvar]], dat[[yvar]],
        dat[[idvar]], NA))
    names(newDat) <- c(tvar, yvar, idvar, "where")
    for (i in 1:NROW(newDat)) {
        newDat$where[i] <- wheres$where[wheres[[idvar]] ==
            newDat[[idvar]][i]]
    }

    plotDat = data.frame()
    for (i in 1:length(unique(model$where))) {
        leaf = unique(model$where)[i]
        coeffs = model$frame[leaf, ]$yval2
        if (is.null(coeffs)) {
            coeffs = model$frame[leaf, ]$yval
        }

        newx <- sort(unique(model$parms$data[[tvar]]))
        if (intercept == TRUE) {
            newxmat <- cbind(1, bs(newx, knots = internalKnots,
                Boundary.knots = boundaryKnots,
                degree = degree))
            preds <- newxmat %*% t(coeffs)
        } else {
            newxmat <- bs(newx, knots = internalKnots,
                Boundary.knots = boundaryKnots,
                degree = degree)
            nodeData = newDat[newDat$where == leaf,
                ]
            meanInt = mean(nodeData[nodeData[[tvar]] ==
                min(nodeData[[tvar]]), ][[yvar]])
            preds <- newxmat %*% t(coeffs) + meanInt
        }
        thisPanel = data.frame("where" = leaf, "time" = sort(unique(model$parms$data[[tvar]])),
            "response" = preds)
        plotDat = rbind(plotDat, thisPanel)
    }

    p1 <- ggplot(data = newDat, aes_string(x = tvar,
        y = yvar, group = idvar))

    f = model$frame
    colors = rainbow(dim(f)[1], v = 0.9)

    plotDat$colors = rep(NA, NROW(plotDat))
    leaves = unique(model$where)
    for (i in 1:NROW(plotDat)) {
        plotDat$colors[i] = colors[match(plotDat$where[i],
            leaves)]
    }
    p1 + geom_line() + facet_grid(. ~ where) +
        geom_point(mapping = aes_string(x = "time", y = "response",
            group = 1, color = as.factor(plotDat$where)),
            data = plotDat) + scale_color_manual(values = colors) +
        theme(legend.position = "none")
}

#' Plot the predicted trajectory for a single node
#'
#' Creates a simple plot of the predicted trajectory at a given node. Option to include
#' the data that falls in the node on the same plot.
#'
#' @importFrom ggplot2 ggplot xlab ylab
#' @export
#' @param tree A splinetree object
#' @param node A node number. Must be a valid terminal node for the given splinetree object.
#' To view valid terminal node numbers, use stPrint() or treeSummary().
#' @param includeData Would you like to see the data from the node
#' plotted along with the predicted trajectory?
#' @param estimateIntercept If the tree was built without an intercept, should
#' the average starting response of all the individuals in the node be added to the trajectory
#' to give the plot interpretable values? Or should the shape of the
#' trajectory be plotted without any regard to the intercept?
#' @examples
#' \donttest{
#' split_formula <- ~HISP + WHITE + BLACK + SEX + Num_sibs + HGC_FATHER + HGC_MOTHER
#' tree <- splineTree(split_formula, tformula, idvar = "ID",
#'    data = nlsySample, degree = 1, df = 3,
#'    intercept = TRUE, cp = 0.005)
#' }
#' plotNode(tree, 6, includeData=TRUE)
plotNode <-  function(tree, node, includeData = FALSE, estimateIntercept = TRUE) {
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
    if (estimateIntercept) {
      mean_int = mean(tree$parms$data[(tree$parms$data[[tree$parms$tvar]] -
                                         min(tree$parms$data[[tree$parms$tvar]])) < 1, ][[tree$parms$yvar]])
    }
    else {mean_int=0}
    preds <- newxmat %*% t(nodeCoeffs) + mean_int
  }
  if (includeData) {
    ggplot() +geom_line(data=data, mapping = aes_string(x = tree$parms$tvar, y = tree$parms$yvar,group = tree$parms$idvar))+
      theme(legend.position="none")+xlab(tree$parms$tvar)+ylab(tree$parms$yvar)+ geom_line(aes(x=xGrid, y=preds, color="red", size=2))
  }
  else {
    ggplot()+geom_line(aes(x=xGrid, y=preds, color="red", size=1))+theme(legend.position="none")+xlab(tree$parms$tvar)+ylab(tree$parms$yvar)
  }
}



