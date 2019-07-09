## Resubmission
This is a resubmission. In this version we

* Modified the method of "adding the baseline" back into a model that was built with no intercept. Previously, individual-specific baseline responses were added to each trajectory. This may be inappropriate when the trajectories do not begin near 0. Now, smoothed coefficients are added back in instead. 

* Eliminated redundant information return by a call to splineForest. Previously, individual smoothing coefficients were in the named attribute Ydata even though they are also included in the named attribute flat_data. The named attribute Ydata no longer exists. 

## Test environments
* local OS X install, R version 3.5.0
* Linux using travis-ci
* win-builder, R version 3.5.1


## R CMD check results
There were no ERRORs or WARNINGs on any of the tested platforms.
On local OS X and on travis-ci, there are no NOTEs. 

On win-builder, there is 1 NOTE because this is a new submission.

## Reverse Dependencies
There are no reverse dependencies. 
