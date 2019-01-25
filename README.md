<!-- README.md is generated from README.Rmd. Please edit that file -->

# dccvalidator

[![Travis-CI Build
Status](https://travis-ci.org/Sage-Bionetworks/dccvalidator.svg?branch=master)](https://travis-ci.org/Sage-Bionetworks/dccvalidator)
[![Coverage
status](https://codecov.io/gh/Sage-Bionetworks/dccvalidator/branch/master/graph/badge.svg)](https://codecov.io/github/Sage-Bionetworks/dccvalidator?branch=master)
[![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)

Validate CNS Data and Metadata

## Installation

``` r
devtools::install_github("Sage-Bionetworks/dccvalidator")
```

## Check annotations

You can check whether a Synapse file has valid annotation keys and
values. You can also check file views and data frames to see if their
columns correspond to valid annotations, and if the values in the
columns are valid.

``` r
library("synapser")
library("dccvalidator")
synLogin()

## File
my_file <- synGet("syn17038065", downloadFile = FALSE)
check_annotation_keys(my_file)
check_annotation_values(my_file)

## File view
fv <- synTableQuery("SELECT * FROM syn17038067")
check_annotation_keys(fv)
check_annotation_values(fv)

## Data frame
dat <- data.frame(assay = "foo", b = 2)
check_annotation_keys(dat)
check_annotation_values(dat)
```

Please note that the dccvalidator project is released with a
[Contributor Code of Conduct](.github/CODE_OF_CONDUCT.md). By
contributing to this project, you agree to abide by its terms.
