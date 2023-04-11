
# `IRRsim`: An R Package for Simulating Inter-Rater Reliability

<!-- badges: start -->

[![R-CMD-check](https://github.com/jbryer/IRRsim/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/jbryer/IRRsim/actions/workflows/R-CMD-check.yaml)
[![](https://img.shields.io/badge/devel%20version-1.0.0-blue.svg)](https://github.com/jbryer/IRRsim)
[![](https://www.r-pkg.org/badges/version/IRRsim)](https://cran.r-project.org/package=IRRsim)
[![CRAN
Status](https://badges.cranchecks.info/flavor/release/IRRsim.svg)](https://cran.r-project.org/web/checks/check_results_IRRsim.html)
<!-- badges: end -->

This package provides functions and a shiny application to simulate
inter-rater reliability statistics based on various scoring and response
models. The initial motivation for this package is to understand the
relationship between percent rater agreement and intraclass correlation.

Documentation is available at <http://irrsim.bryer.org>

Slides for NCME 2019 talk are here:
<a href="http://irrsim.bryer.org/IRRsim-Presentation.html" target="_blank">http://irrsim.bryer.org/IRRsim-Presentation.html</a>

## Install

The development version of `IRRsim` can be downloaded from Github using
the `devtools` package:

``` r
devtools::install_github('jbryer/IRRsim')
```

## Shiny App

Many of features of this package can be explored using the included
shiny applications. The `IRRsim_shiny()` function will run the
application.

``` r
IRRsim::IRRsim_demo()
```

<figure>
<img src="man/figures/IRRsimShinyApp.png" alt="IRRsim Shiny App" />
<figcaption aria-hidden="true">IRRsim Shiny App</figcaption>
</figure>

## Data

The analysis done in the vignette requires a large number of simulated
datasets. Not only does it take a long time for those datasets to be
generated, the file sizes exceed what is allowed in R packages. The data
are stored as [Github
releases](https://github.com/jbryer/IRRsim/releases/tag/IRRsimData). See
the [data-releases/IRRsimulations.R](data-releases/IRRsimulations.R) R
script for how these data files are generated. Alternatively, you can
download the data using the following commands in R:

``` r
download_dir <- getwd()
download.file('https://github.com/jbryer/IRRsim/releases/download/IRRsimData/IRRsimData.rda', 
              paste0(download_dir, '/IRRsimData.rda'))
load(paste0(download_dir, '/IRRsimData.rda))
```
