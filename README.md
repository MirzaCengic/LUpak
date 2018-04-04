
<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- output: rmarkdown::github_document -->
<!-- output: html_notebook -->
LUpak package
=============

This is a repo for package that facilitates analysis for the agricultural conversion project.

To install the package run:

``` r
devtools::install_github("mirzacengic/LUpak")
```

**Made for personal use only!**

#### Functions currenly added:

-   `variable_importance()` - calculate importance of predictor variables.

-   `get_rasters()` - load predictors for specific region.

-   `load_PA()`/`load_PA2()` - load response variable data (*needs to be fixed*)

-   `format_data()` - prepare data for modeling (return training and evaluation data). There are some redundant VIF stuff still here.

-   `vif_stepwise()`/`vif_select_vars()` - calculate VIF by stepwise elimination procedure.

-   `evaluate_model()`/`get_evaluations()` - Get model assessment results (AUC/TSS).

-   `fit_model()` - Fit linear model to the data.
