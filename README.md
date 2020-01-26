<!-- README.md is generated from README.Rmd. Please edit that file -->

# dccvalidator

[![Travis-CI Build
Status](https://travis-ci.org/Sage-Bionetworks/dccvalidator.svg?branch=master)](https://travis-ci.org/Sage-Bionetworks/dccvalidator)
[![CRAN
status](https://www.r-pkg.org/badges/version/dccvalidator)](https://CRAN.R-project.org/package=dccvalidator)
[![lifecycle](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)

dccvalidator is a package and Shiny app to perform data validation and
QA/QC. It’s used in the [AMP-AD](https://ampadportal.org/) and
[PsychENCODE](http://www.psychencode.org) consortia to validate data
prior to data releases.

## Installation

You can install dccvalidator from CRAN:

``` r
install.packages("dccvalidator")
```

To install the development version from GitHub, run:

``` r
devtools::install_github("Sage-Bionetworks/dccvalidator")
```

Many functions in dccvalidator use reticulate and the [Synapse Python
client](https://pypi.org/project/synapseclient/). See the [reticulate
documentation](https://rstudio.github.io/reticulate/#python-version) for
information on how to set R to use a specific version of Python if you
don’t want to use the default Python installation on your machine.
Whichever Python installation you choose should have synapseclient
installed.

Because dccvalidator uses reticulate, it is not compatible with the
[synapser](https://r-docs.synapse.org/) package..

## Check data

dccvalidator provides functions for checking the following common data
quality issues:

  - Annotation keys and values conform to a controlled vocabulary
  - Column names match those of an associated metadata template
  - Certain columns are not empty
  - Certain columns are complete
  - Identifiers match between two metadata files (e.g. all individuals
    in one file are also present in another)
  - Check that identifiers are unique within a file

# Data submission validation

This package contains a Shiny app to validate manifests and metadata for
AMP-AD studies. It uses the dccvalidator package to check for common
data quality issues and gives realtime feedback to the data contributor
on errors that need to be fixed. The reporting UI is heavily inspired by
the [MetaDIG project’s metadata quality
reports](https://knb.ecoinformatics.org/quality/s=knb.suite.1/doi%3A10.5063%2FF12V2D1V).

The application also allows users to submit documentation of their
study, a description of the methods used, etc.

See the [customizing
dccvalidator](https://sage-bionetworks.github.io/dccvalidator/articles/customizing-dccvalidator.html)
vignette for information on how to spin up a customized version of the
application
