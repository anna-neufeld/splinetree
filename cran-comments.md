## Test environments
* local OS X install, R version 3.5.0
* Linux using travis-ci
* 

## R CMD check results
There were no ERRORs or WARNINGs

There was 1 NOTE:

The data directory is larger than 5MB because it contains a pre-built spline random forest object. These objects are large. Storing a pre-built random forest allows examples and vignettes to run faster, so there is a tradeoff between package size and checking efficiency. The forest object is compressed, but when the package is loaded it is still taking up a lot of room. 

## Reverse Dependencies
This is a new package, there are no reverse dependencies. 
