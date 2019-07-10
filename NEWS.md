splinetree 0.1.1
================

This is a minor release. Typesetting issues in vignettes have been fixed. 


splinetree 0.1.2
================

This is a minor release. An optional boolean parameter, ``bootstrap`` has been added to the ``splineForest`` function. This parameter allows users to specify whether they would like to build an ensemble of regression trees using different bootstrap samples of the data or different random samples drawn without replacement from the population. The previous version of the package always used bootstrap samples, but the work of Strobl et al (2007) suggests that samples drawn without replacement are preferable. No functionality of the package has changed; all old code will still run but will produce slightly different results.

splinetree 0.2.1
================

This is a minor release. The following changes were made:

* Modified the method of plotting trajectories for trees built without an intercept. Previously, the intercept-less trajectories were shifted by the average individual baseline response to put them in the range of reasonable response values. This is inappropriate when the "baseline" does not correspond to a value of 0 of the time variable. Now, average individual estimated intercept coefficients are used instead.

* Eliminated redundant information return by a call to splineForest. Previously, individual smoothing coefficients were in the named attribute Ydata even though they are also included in the named attribute flat_data. The named attribute Ydata no longer exists. 

* Fixed an issue with node labeling in the "spaghettiPlot" function. Now the panel labels match the node labels in the tree printout. 
