splinetree 0.1.1
================

This is a minor release. Typesetting issues in vignettes have been fixed. 


splinetree 0.1.2
================

This is a minor release. An optional boolean parameter, ``bootstrap`` has been added to the ``splineForest`` function. This parameter allows users to specify whether they would like to build an ensemble of regression trees using different bootstrap samples of the data or different random samples drawn without replacement from the population. The previous version of the package always used bootstrap samples, but the work of Strobl et al (2007) suggests that samples drawn without replacement are preferable. No functionality of the package has changed; all old code will still run but will produce slightly different results. 