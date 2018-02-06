# IRRsim

This package provides functions and a shiny application to simulate inter-rater 
reliability statistics based on various scoring and response models. The initial motivation for 
this package is to understand the relationship between percent rater agreement and interclass correlation.

**Vignette**: [vignettes/IRRsim.Rmd](http://htmlpreview.github.io/?https://github.com/jbryer/IRRsim/blob/master/inst/doc/IRRsim.html)
**Demo**: [demo/irr_simulation_test.R](https://github.com/jbryer/IRRsim/blob/master/demo/irr_simulation_test.R) Can also be run from R: `demo('irr_simulation_test')`

## Install

The development version of `IRRsim` can be downloaded from Github using the `devtools` package:

```
devtools::install_github('jbryer/IRRsim')
```

## Shiny App

Many of features of this package can be explored using the included shiny applications. The `IRRsim_shiny()` function will run the application.

```
IRRsim::IRRsim_shiny()
```



