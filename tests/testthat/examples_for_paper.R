### Load the larger dataset. 
load("~/Spline-Trees/SplineTree/data/nlsySample_large.RData")
split_formula <- BMI ~ HISP + WHITE + BLACK + SEX + Dad_Full_Work + Mom_Full_Work + 
  Age_first_weed + Age_first_smoke + Age_first_alc + Num_sibs + 
  HGC_FATHER + HGC_MOTHER + Mag + News + Lib + Two_Adults_14 + 
  Mother_14 + Father_14 + STABLE_RESIDENCE + URBAN_14 + South_Birth

### Build a tree with an intercept and a tree without an intercept. 
### Both build as linear trees with 1 internal knot due to initial exploration / precedent

tree_INT <- splineTree(split_formula, BMI~AGE, "ID", nlsySample_large, degree=1, df=3, intercept=TRUE, minNodeSize=20, cp=0.001)
tree <- splineTree(split_formula, BMI~AGE, "ID", nlsySample_large, degree=1, df=2, intercept=FALSE, minNodeSize=20, cp=0.001)
biggest_tree <- splineTree(split_formula, BMI~AGE, "ID", nlsySample_large, degree=1, df=2, intercept=FALSE, cp=0.0005)

### Are these trees too big to visualize?
treeSize(tree)
treeSize(tree_INT)

### Yes, too big, let's prune
pruned_tree <- prune(tree, cp=0.005)
treeSize(pruned_tree)

pruned_tree_int <-  prune(tree_INT, cp=0.0043)
treeSize(pruned_tree_int)

### Summarize

treeSummary(pruned_tree)
treeSummary(pruned_tree_int)

### Now let's plot
colors = c("red","orange","yellow", "violetred" , "rosybrown1", "purple", "blue" ,"cyan"      
           ,"gray","seagreen","green", "darkolivegreen4")
stPlots(pruned_tree, colors)
stPlots(pruned_tree_int, colors)


### Evaluate trees
R2_y(tree_INT)
R2_projected(tree_INT, includeIntercept=TRUE)
R2_projected(tree_INT, includeIntercept=FALSE)
R2_y(pruned_tree_int)
R2_projected(pruned_tree_int, includeIntercept=TRUE)
R2_projected(pruned_tree_int, includeIntercept=FALSE)

R2_projected(tree)
R2_projected(pruned_tree)


### Plot trees

### Variable importance from trees
imp=tree$variable.importance
imp_int=tree_INT$variable.importance
imp_pruned=pruned_tree$variable.importance
imp_pruned_int=pruned_tree_int$variable.importance


vars = attr(terms(split_formula), "term.labels")

all_imps = rep(0,4)
for (var in vars) {
  thisRow = rep(0,4)
  if (length(imp[names(imp)==var])>0) thisRow[1] = imp[names(imp)==var]
  if (length(imp[names(imp_int)==var])>0) thisRow[2] = imp_int[names(imp_int)==var]
  if (length(imp[names(imp_pruned)==var])>0) thisRow[3] = imp_pruned[names(imp_pruned)==var]
  if (length(imp[names(imp_pruned_int)==var])>0) thisRow[4] = imp_pruned_int[names(imp_pruned_int)==var]
  all_imps = rbind(all_imps, thisRow)
}
all_imps = all_imps[-1,]
row.names(all_imps)=vars
all_imps=data.frame(all_imps)
names(all_imps) = c("No int, full tree", "Int, Full Tree", "Pruned", "Int, Pruned")

par(mfrow=c(2,2))
par(las=2) # make label text perpendicular to axis
par(mar=c(1,7,3,1)) # increase y-axis margin.
barplot(all_imps[,2]/sum(all_imps[,2]), horiz=TRUE, names.arg=row.names(all_imps), cex.names=0.7, main="Intercept, Full Tree", axes=FALSE)
barplot(all_imps[,1]/sum(all_imps[,1]), horiz=TRUE, names.arg=row.names(all_imps), cex.names=0.7, main="No Intercept, Full Tree", axes=FALSE)
barplot(all_imps[,3]/sum(all_imps[,3]), horiz=TRUE, names.arg=row.names(all_imps), cex.names=0.7, main="No Intercept, Pruned", axes=FALSE)
barplot(all_imps[,4]/sum(all_imps[,4]), horiz=TRUE, names.arg=row.names(all_imps), cex.names=0.7, main="No Intercept, Pruned", axes=FALSE)






### Variable importance from trees
imp=biggest_tree$variable.importance
imp_medium_pruned=tree$variable.importance
imp_extra_pruned=pruned_tree$variable.importance

vars = attr(terms(split_formula), "term.labels")
new_imps = rep(0,3)
for (var in vars) {
  thisRow = rep(0,3)
  if (length(imp[names(imp)==var])>0) thisRow[1] = imp[names(imp)==var]
  #if (length(imp_pruned[names(imp_pruned)==var])>0) thisRow[2] = imp_pruned[names(imp_pruned)==var]
  if (length(imp_extra_pruned[names(imp_extra_pruned)==var])>0) thisRow[2] = imp_extra_pruned[names(imp_extra_pruned)==var]
  if (length(imp_medium_pruned[names(imp_medium_pruned)==var])>0) thisRow[3] = imp_medium_pruned[names(imp_medium_pruned)==var]
  new_imps = rbind(new_imps, thisRow)
}
new_imps = new_imps[-1,]
row.names(new_imps)=vars
new_imps=data.frame(new_imps)

par(mfrow=c(1,3))
par(las=2) # make label text perpendicular to axis
par(mar=c(1,7,3,1)) # increase y-axis margin.
barplot(new_imps[,1]/sum(new_imps[,1]), horiz=TRUE, names.arg=row.names(new_imps), cex.names=0.7, main="54 Terminal Nodes", axes=FALSE)
barplot(new_imps[,3]/sum(new_imps[,3]), horiz=TRUE, names.arg=row.names(new_imps), cex.names=0.7, main="28 Terminal Nodes", axes=FALSE)
barplot(new_imps[,2]/sum(new_imps[,2]), horiz=TRUE, names.arg=row.names(new_imps), cex.names=0.7, main="9 Terminal Nodes", axes=FALSE)

