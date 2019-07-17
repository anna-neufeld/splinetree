## Resubmission
This is a resubmission. In this version we

* Updated package vignettes. 

* Modified the method of plotting trajectories for trees built without an intercept. Previously, the intercept-less trajectories were shifted by the average individual baseline response to put them in the range of reasonable response values. This is inappropriate when the "baseline" does not correspond to a value of 0 of the time variable. Now, average individual estimated intercept coefficients are used instead.

* Eliminated redundant information return by a call to splineForest. Previously, individual smoothing coefficients were in the named attribute Ydata even though they are also included in the named attribute flat_data. The named attribute Ydata no longer exists. 

* Fixed an issue with node labeling in the "spaghettiPlot" function. Now the panel labels match the node labels in the tree printout. 

## Test environments
* local OS X install, R version 3.5.0
* Linux using travis-ci
* win-builder, R version 3.5.1


## R CMD check results
There were no ERRORs or WARNINGs or NOTEs on any of the tested platforms.

## Reverse Dependencies
There are no reverse dependencies. 
