## Resubmission
This is a resubmission. In this version we

* Added an additional parameter to the ``splineForest()`` function to allow users to build spline forests using either bootstrap sampling or sampling without replacement. Previous versions of the package always used bootstrap sampling, but previous research has shown that sampling  without replacement is preferable. 

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
